import assert from "node:assert";
import test from "node:test";
import { McpHybridWebSocket } from "../hybridSocket";
import type { McpTunnelWebSocket, ToolCaller } from "../tunnelSocket";
import { McpTunnelWebSocket as Tunnel } from "../tunnelSocket";

// A controllable fake WebSocket constructor that opens immediately
function makeFakeWs() {
  const instances: FakeWs[] = [];
  class FakeWs {
    url: string;
    binaryType = "";
    onopen: (() => void) | null = null;
    onerror: (() => void) | null = null;
    onclose: ((e: { code: number }) => void) | null = null;
    onmessage: ((e: { data: unknown }) => void) | null = null;
    sent: Array<string | ArrayBuffer> = [];
    constructor(url: string) {
      this.url = url;
      instances.push(this);
      setTimeout(() => this.onopen?.(), 1);
    }
    send(data: string | ArrayBuffer): void {
      this.sent.push(data);
    }
    close(): void {
      this.onclose?.({ code: 1000 });
    }
  }
  return { ctor: FakeWs as unknown as typeof WebSocket, instances };
}

function makeTunnelFactory() {
  const sentFrames: Array<{ connectionId: string; frames: unknown[] }> = [];
  const caller: ToolCaller = async (name, args) => {
    if (name === "_shiny_connect") return { connectionId: "tid" };
    if (name === "_shiny_send") {
      sentFrames.push(args as { connectionId: string; frames: unknown[] });
      return {};
    }
    if (name === "_shiny_receive") {
      return await new Promise(() => {
        /* hang */
      });
    }
    return {};
  };
  let created: McpTunnelWebSocket | null = null;
  const make = () => {
    created = new Tunnel(caller);
    return created;
  };
  return {
    make,
    get created() {
      return created;
    },
    sentFrames,
  };
}

void test("injects pendingRestore into the init frame (direct transport)", async () => {
  const { ctor, instances } = makeFakeWs();
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
  });
  sock.pendingRestore = "_inputs_&n=200";

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  // Send an init frame
  sock.send(JSON.stringify({ method: "init", data: { foo: "bar" } }));

  assert.strictEqual(instances[0].sent.length, 1);
  const parsed = JSON.parse(instances[0].sent[0] as string) as {
    method: string;
    data: Record<string, unknown>;
  };
  assert.strictEqual(parsed.method, "init");
  assert.strictEqual(parsed.data[".clientdata_mcp_restore"], "_inputs_&n=200");
  assert.strictEqual(parsed.data["foo"], "bar");
});

void test("injects pendingRestore into the init frame (tunnel transport)", async () => {
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: null,
    makeTunnel: tunnels.make,
  });
  sock.pendingRestore = "_inputs_&n=200";

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  // Send an init frame
  sock.send(JSON.stringify({ method: "init", data: { foo: "bar" } }));

  // The tunnel sends frames via callTool("_shiny_send", ...)
  assert.strictEqual(tunnels.sentFrames.length, 1);
  const frame = (
    tunnels.sentFrames[0].frames as Array<{ data: string; binary?: boolean }>
  )[0];
  const parsed = JSON.parse(frame.data) as {
    method: string;
    data: Record<string, unknown>;
  };
  assert.strictEqual(parsed.method, "init");
  assert.strictEqual(parsed.data[".clientdata_mcp_restore"], "_inputs_&n=200");
  assert.strictEqual(parsed.data["foo"], "bar");
});

void test("does not inject into non-init string frames", async () => {
  const { ctor, instances } = makeFakeWs();
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
  });
  sock.pendingRestore = "_inputs_&n=200";

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  const updateFrame = JSON.stringify({ method: "update", data: { x: 1 } });
  sock.send(updateFrame);

  assert.strictEqual(instances[0].sent.length, 1);
  assert.strictEqual(instances[0].sent[0], updateFrame);
});

void test("passes ArrayBuffer frames through untouched", async () => {
  const { ctor, instances } = makeFakeWs();
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
  });
  sock.pendingRestore = "_inputs_&n=200";

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  const buf = new ArrayBuffer(4);
  sock.send(buf);

  assert.strictEqual(instances[0].sent.length, 1);
  assert.strictEqual(instances[0].sent[0], buf);
});

void test("injects only once (first init frame)", async () => {
  const { ctor, instances } = makeFakeWs();
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
  });
  sock.pendingRestore = "_inputs_&n=200";

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  // First init frame gets injection
  sock.send(JSON.stringify({ method: "init", data: {} }));
  // Second init frame does NOT get injection
  sock.send(JSON.stringify({ method: "init", data: {} }));

  assert.strictEqual(instances[0].sent.length, 2);

  const first = JSON.parse(instances[0].sent[0] as string) as {
    data: Record<string, unknown>;
  };
  assert.strictEqual(first.data[".clientdata_mcp_restore"], "_inputs_&n=200");

  const second = JSON.parse(instances[0].sent[1] as string) as {
    data: Record<string, unknown>;
  };
  assert.strictEqual(second.data[".clientdata_mcp_restore"], undefined);
});

void test("does not inject when pendingRestore is null", async () => {
  const { ctor, instances } = makeFakeWs();
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
  });
  // pendingRestore left as null (default)

  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  const initFrame = JSON.stringify({ method: "init", data: { a: 1 } });
  sock.send(initFrame);

  assert.strictEqual(instances[0].sent.length, 1);
  assert.strictEqual(instances[0].sent[0], initFrame);
});
