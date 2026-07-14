import assert from "node:assert";
import test from "node:test";
import { McpHybridWebSocket } from "../hybridSocket";
import type { McpTunnelWebSocket, ToolCaller } from "../tunnelSocket";
import { McpTunnelWebSocket as Tunnel } from "../tunnelSocket";

// A controllable fake WebSocket constructor
function makeFakeWs(behavior: "open" | "error" | "hang") {
  const instances: FakeWs[] = [];
  class FakeWs {
    url: string;
    binaryType = "";
    onopen: (() => void) | null = null;
    onerror: (() => void) | null = null;
    onclose: (() => void) | null = null;
    onmessage: ((e: { data: unknown }) => void) | null = null;
    sent: unknown[] = [];
    closed = false;
    constructor(url: string) {
      this.url = url;
      instances.push(this);
      setTimeout(() => {
        if (behavior === "open") this.onopen?.();
        else if (behavior === "error") this.onerror?.();
      }, 5);
    }
    send(data: unknown): void {
      this.sent.push(data);
    }
    close(): void {
      this.closed = true;
      this.onclose?.();
    }
  }
  return { ctor: FakeWs as unknown as typeof WebSocket, instances };
}

function makeTunnelFactory() {
  const caller: ToolCaller = async (name) => {
    if (name === "_shiny_connect") return { connectionId: "tid" };
    if (name === "_shiny_receive") {
      return await new Promise(() => {
        // hang
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
  };
}

void test("adopts the direct websocket when it opens", async () => {
  const { ctor, instances } = makeFakeWs("open");
  const tunnels = makeTunnelFactory();
  const kinds: string[] = [];
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
    onTransport: (k) => kinds.push(k),
  });
  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  assert.deepEqual(kinds, ["direct"]);
  assert.equal(sock.readyState, 1);
  assert.equal(tunnels.created, null);

  sock.send("hello");
  assert.deepEqual(instances[0].sent, ["hello"]);

  // messages flow through the facade
  const got: unknown[] = [];
  sock.onmessage = (e) => got.push(e.data);
  instances[0].onmessage?.({ data: "frame" });
  assert.deepEqual(got, ["frame"]);
});

void test("falls back to the tunnel when the direct attempt errors", async () => {
  const { ctor } = makeFakeWs("error");
  const tunnels = makeTunnelFactory();
  const kinds: string[] = [];
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 200,
    onTransport: (k) => kinds.push(k),
  });
  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  assert.deepEqual(kinds, ["tunnel"]);
  assert.equal(sock.readyState, 1);
  assert.notEqual(tunnels.created, null);
});

void test("falls back when the direct attempt hangs past the timeout", async () => {
  const { ctor, instances } = makeFakeWs("hang");
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: "ws://127.0.0.1:1/websocket/?mcp=1",
    makeTunnel: tunnels.make,
    wsConstructor: ctor,
    timeoutMs: 30,
  });
  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;

  assert.equal(sock.transportKind, "tunnel");
  assert.equal(instances[0].closed, true);
});

void test("goes straight to the tunnel without a direct URL", async () => {
  const tunnels = makeTunnelFactory();
  const sock = new McpHybridWebSocket({
    directWsUrl: null,
    makeTunnel: tunnels.make,
  });
  const opened = new Promise<void>((r) => (sock.onopen = () => r()));
  void sock.start();
  await opened;
  assert.equal(sock.transportKind, "tunnel");
});
