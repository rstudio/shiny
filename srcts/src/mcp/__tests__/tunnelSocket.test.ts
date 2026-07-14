import assert from "node:assert";
import test from "node:test";
import type { ToolCaller } from "../tunnelSocket";
import { McpTunnelWebSocket } from "../tunnelSocket";

function makeHost(
  receiveBatches: Array<{ frames: unknown[]; closed: boolean }>,
) {
  const sent: Array<{ [key: string]: unknown }> = [];
  const closed: Array<{ [key: string]: unknown }> = [];
  const caller: ToolCaller = async (name, args) => {
    if (name === "_shiny_connect") return { connectionId: "abc" };
    if (name === "_shiny_send") {
      sent.push(args);
      return {};
    }
    if (name === "_shiny_receive") {
      const batch = receiveBatches.shift();
      if (batch) return batch as unknown as { [key: string]: unknown };
      return await new Promise(() => {
        // hang: no more data (simulates a long-poll that never resolves)
      });
    }
    if (name === "_shiny_close") {
      closed.push(args);
      return {};
    }
    throw new Error("unknown tool " + name);
  };
  return { caller, sent, closed };
}

async function tick(ms = 20): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

void test("opens, delivers frames in order, sends, and closes", async () => {
  const { caller, sent } = makeHost([
    { frames: [{ data: '{"config":{}}', binary: false }], closed: false },
    { frames: [], closed: true },
  ]);
  const sock = new McpTunnelWebSocket(caller);
  const events: string[] = [];
  sock.onopen = () => events.push("open");
  sock.onmessage = (e) => events.push("msg:" + String(e.data));
  sock.onclose = () => events.push("close");

  await sock.start();
  sock.send('{"method":"init","data":{}}');
  await tick();

  assert.equal(events[0], "open");
  assert.ok(events.includes('msg:{"config":{}}'));
  assert.ok(events.includes("close"));
  assert.equal(sock.readyState, 3);
  assert.equal(sent.length, 1);
  const frames = sent[0].frames as Array<{ data: string }>;
  assert.equal(frames[0].data, '{"method":"init","data":{}}');
});

void test("binary frames round-trip as ArrayBuffer/base64", async () => {
  const { caller, sent } = makeHost([
    { frames: [{ data: "AQICAg==", binary: true }], closed: false },
  ]);
  const sock = new McpTunnelWebSocket(caller);
  const received: ArrayBuffer[] = [];
  sock.onmessage = (e) => received.push(e.data as ArrayBuffer);

  await sock.start();
  await tick();

  assert.equal(received.length, 1);
  assert.deepEqual(
    Array.from(new Uint8Array(received[0])),
    [0x01, 0x02, 0x02, 0x02],
  );

  sock.send(new Uint8Array([0x01, 0x02, 0x02, 0x02]).buffer);
  await tick();
  const frames = sent[0].frames as Array<{ data: string; binary: boolean }>;
  assert.equal(frames[0].binary, true);
  assert.equal(frames[0].data, "AQICAg==");
});

void test("connect failure surfaces as close with code 1006", async () => {
  const caller: ToolCaller = async () => {
    throw new Error("no host");
  };
  const sock = new McpTunnelWebSocket(caller);
  const events: string[] = [];
  sock.onclose = (e) => events.push("close:" + e.code);
  sock.onerror = () => events.push("error");

  await sock.start();
  assert.deepEqual(events, ["error", "close:1006"]);
  assert.equal(sock.readyState, 3);
});

void test("client-initiated close calls _shiny_close once", async () => {
  const { caller, closed } = makeHost([]);
  const sock = new McpTunnelWebSocket(caller);
  const events: string[] = [];
  sock.onclose = (e) => events.push("close:" + e.code);

  await sock.start();
  sock.close();
  sock.close();
  await tick();

  assert.deepEqual(events, ["close:1000"]);
  assert.equal(closed.length, 1);
  assert.deepEqual(closed[0], { connectionId: "abc" });
});
