import assert from "node:assert/strict";
import test from "node:test";

import { peekAnswered, ReconnectState } from "../reconnectState";

const tokenA = "0123456789abcdef0123456789abcdef";
const tokenB = "fedcba9876543210fedcba9876543210";

void test("a fresh page sends init and adopts the config token at once", () => {
  const s = new ReconnectState();
  assert.deepEqual(s.firstMessage({ a: 1 }, { a: 1 }), {
    method: "init",
    data: { a: 1 },
  });
  s.onConfig(tokenA);
  assert.equal(s.resumeToken, tokenA);
});

void test("a reconnect sends the inputs as of the disconnect plus the current ones", () => {
  const s = new ReconnectState();
  s.onConfig(tokenA);
  s.onSocketClosed(true, { a: 1 });
  s.beginReconnect();
  assert.deepEqual(s.firstMessage({ a: 2 }, { a: 2 }), {
    method: "resume",
    data: { token: tokenA, inputs: { a: 1 }, current: { a: 2 } },
  });
});

void test("retries that never opened keep the snapshot", () => {
  const s = new ReconnectState();
  s.onSocketClosed(true, { a: 1 });
  s.beginReconnect();
  s.onSocketClosed(false, { a: 3 }); // a failed attempt never opened
  assert.deepEqual(s.firstMessage({ a: 3 }, { a: 3 }).data, {
    inputs: { a: 1 },
    current: { a: 3 },
  });
});

void test("an attempt that opened and died re-takes the snapshot", () => {
  const s = new ReconnectState();
  s.onSocketClosed(true, { x: 1 });
  s.beginReconnect();
  s.onSocketClosed(true, { x: 2 });
  s.beginReconnect();
  assert.deepEqual(s.firstMessage({ x: 2 }, { x: 2 }).data, {
    inputs: { x: 2 },
    current: { x: 2 },
  });
});

void test("init sends the initial inputs; resume sends the live ones as current", () => {
  const s = new ReconnectState();
  assert.deepEqual(s.firstMessage({ a: 1 }, { a: 2 }), {
    method: "init",
    data: { a: 1 },
  });
  s.onSocketClosed(true, { a: 1 });
  s.beginReconnect();
  assert.deepEqual(s.firstMessage({ a: 1 }, { a: 2 }).data, {
    inputs: { a: 1 },
    current: { a: 2 },
  });
});

void test("while reconnecting the config token waits for resumed; a drop in between keeps the old token", () => {
  const s = new ReconnectState();
  s.onConfig(tokenA);
  s.onSocketClosed(true, {});
  s.beginReconnect();
  s.onConfig(tokenB);
  assert.equal(s.resumeToken, tokenA);
  s.onSocketClosed(true, {});
  assert.equal(s.resumeToken, tokenA);
  assert.deepEqual(s.firstMessage({}, {}).data, {
    token: tokenA,
    inputs: {},
    current: {},
  });
  s.onConfig(tokenB);
  s.onResumed();
  assert.equal(s.resumeToken, tokenB);
});

void test("with resume off there is never a token", () => {
  const s = new ReconnectState();
  s.onConfig(null);
  s.onSocketClosed(true, { a: 1 });
  s.beginReconnect();
  assert.deepEqual(s.firstMessage({ a: 1 }, { a: 1 }).data, {
    inputs: { a: 1 },
    current: { a: 1 },
  });
});

void test("answered tracks whether the server replied since the reconnect began", () => {
  const s = new ReconnectState();
  assert.equal(s.answered, false);
  s.onSocketClosed(true, {});
  s.beginReconnect();
  assert.equal(s.answered, false);
  s.onAnswered();
  assert.equal(s.answered, true);
  s.onSocketClosed(true, {});
  s.beginReconnect();
  assert.equal(s.answered, false);
});

void test("after a successful reconnect the next drop takes a fresh snapshot", () => {
  const s = new ReconnectState();
  s.onSocketClosed(true, { a: 1 });
  s.beginReconnect();
  s.onAnswered();
  s.onSocketClosed(true, { a: 5 });
  assert.deepEqual(s.firstMessage({ a: 6 }, { a: 6 }).data, {
    inputs: { a: 5 },
    current: { a: 6 },
  });
});

void test("peekAnswered reads a top-level resumed or values from text messages", () => {
  assert.equal(peekAnswered('{"resumed":"snapshot"}'), true);
  assert.equal(peekAnswered('{"resumed":"inputs"}'), true);
  assert.equal(peekAnswered('{"values":{"x":1}}'), true);
  assert.equal(
    peekAnswered('{"config":{"workerId":"","sessionId":"s"}}'),
    false,
  );
  assert.equal(peekAnswered('{"allowReconnect":false}'), false);
  assert.equal(
    peekAnswered('{"custom":{"txt":"\\"resumed\\":\\"inputs\\""}}'),
    false,
  );
  assert.equal(peekAnswered("not json, resumed values"), false);
  assert.equal(peekAnswered(new ArrayBuffer(4)), false);
});
