import assert from "node:assert/strict";
import test from "node:test";

import { peekAllowReconnect, ReconnectControl } from "../reconnectControl";

// Timers the test fires by hand. `clearWorks = false` models a callback that
// was already due when it was cancelled.
function fakeTimers(clearWorks = true) {
  let next = 1;
  const pending = new Map<number, () => void>();
  return {
    set: (fn: () => void): number => {
      const id = next++;
      pending.set(id, fn);
      return id;
    },
    clear: (id: unknown): void => {
      if (clearWorks) pending.delete(id as number);
    },
    runAll: (): void => {
      const fns = [...pending.values()];
      pending.clear();
      fns.forEach((fn) => fn());
    },
  };
}

void test("a stop cancels a reconnect that is already scheduled", () => {
  const timers = fakeTimers();
  const control = new ReconnectControl(timers.set, timers.clear);
  let calls = 0;
  control.setAllow(true);
  control.schedule(1500, () => calls++);
  control.setAllow(false);
  timers.runAll();
  assert.equal(calls, 0);
});

void test("a callback already due when cancelled does nothing", () => {
  const timers = fakeTimers(false);
  const control = new ReconnectControl(timers.set, timers.clear);
  const fired: string[] = [];
  control.setAllow(true);
  control.schedule(1000, () => fired.push("first"));
  control.schedule(1000, () => fired.push("second"));
  timers.runAll();
  assert.deepEqual(fired, ["second"]);
});

void test("turning retries off and on again in a live session lets the next drop retry", () => {
  const timers = fakeTimers();
  const control = new ReconnectControl(timers.set, timers.clear);
  let calls = 0;
  control.setAllow(false);
  control.setAllow(true);
  assert.equal(control.shouldRetry(true), true);
  control.schedule(1000, () => calls++);
  timers.runAll();
  assert.equal(calls, 1);
});

void test("the retry rule needs the connection layer for true, not for force", () => {
  const control = new ReconnectControl();
  assert.equal(control.shouldRetry(true), false); // default: off
  control.setAllow(true);
  assert.equal(control.shouldRetry(true), true);
  assert.equal(control.shouldRetry(false), false);
  control.setAllow("force");
  assert.equal(control.shouldRetry(false), true);
  control.setAllow(false);
  assert.equal(control.shouldRetry(true), false);
  assert.equal(control.permitsManualRetry(), false);
  control.setAllow(true);
  assert.equal(control.permitsManualRetry(), true);
});

void test("a manual retry is refused only after an explicit stop", () => {
  const control = new ReconnectControl();
  assert.equal(control.allow, false);
  assert.equal(control.permitsManualRetry(), true);
  control.setAllow(false);
  assert.equal(control.permitsManualRetry(), false);
  control.setAllow(true);
  assert.equal(control.permitsManualRetry(), true);
  control.setAllow(false);
  control.setAllow("force");
  assert.equal(control.permitsManualRetry(), true);
});

void test("peekAllowReconnect reads only a top-level allowReconnect from text messages", () => {
  assert.equal(peekAllowReconnect('{"allowReconnect":false}'), false);
  assert.equal(peekAllowReconnect('{"allowReconnect":true}'), true);
  assert.equal(peekAllowReconnect('{"allowReconnect":"force"}'), "force");
  assert.equal(peekAllowReconnect('{"allowReconnect":"maybe"}'), undefined);
  assert.equal(
    peekAllowReconnect('{"values":{"txt":"\\"allowReconnect\\":false"}}'),
    undefined,
  );
  assert.equal(peekAllowReconnect('{"values":{"x":1}}'), undefined);
  assert.equal(peekAllowReconnect("not json, allowReconnect"), undefined);
  assert.equal(peekAllowReconnect(new ArrayBuffer(4)), undefined);
});

void test("peekAllowReconnect reads a config's resumeToken as allowReconnect(TRUE)", () => {
  const token = "0123456789abcdef0123456789abcdef";
  const config = (extra: string) =>
    `{"config":{"workerId":"","sessionId":"s"${extra}}}`;
  assert.equal(peekAllowReconnect(config(`,"resumeToken":"${token}"`)), true);
  assert.equal(peekAllowReconnect(config(',"resumeToken":null')), undefined);
  assert.equal(peekAllowReconnect(config("")), undefined);
  assert.equal(
    peekAllowReconnect(`{"values":{"txt":"\\"resumeToken\\":\\"${token}\\""}}`),
    undefined,
  );
  assert.equal(
    peekAllowReconnect(
      `{"allowReconnect":false,"config":{"resumeToken":"${token}"}}`,
    ),
    false,
  );
});
