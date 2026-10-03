import assert from "node:assert/strict";
import test from "node:test";

import { maxAttempts, ReconnectDelay } from "../reconnectDelay";

void test("delays grow and then repeat the last one", () => {
  const d = new ReconnectDelay();
  const seen = Array.from({ length: 9 }, () => d.next());
  assert.deepEqual(
    seen,
    [1500, 1500, 2500, 2500, 5500, 5500, 10500, 10500, 10500],
  );
});

void test("the schedule is exhausted after ten attempts and reset() starts over", () => {
  const d = new ReconnectDelay();
  for (let i = 0; i < maxAttempts - 1; i++) {
    d.next();
    assert.equal(d.exhausted(), false, `attempt ${i + 1}`);
  }
  d.next();
  assert.equal(d.exhausted(), true);
  d.reset();
  assert.equal(d.exhausted(), false);
  assert.equal(d.next(), 1500);
});

void test("opening a socket does not reset the count: only reset() does", () => {
  // The class exposes no other way to lower the count; this pins the shape so
  // a future "onopen" hook cannot quietly reset it (spec §6.1).
  assert.deepEqual(
    Object.getOwnPropertyNames(ReconnectDelay.prototype).sort(),
    ["constructor", "exhausted", "next", "reset"],
  );
});
