import assert from "node:assert/strict";
import test from "node:test";

import { ReconnectDelay } from "../reconnectDelay";

void test("delays grow to a 30 s ceiling after about a minute and then repeat", () => {
  const d = new ReconnectDelay();
  const seen = Array.from({ length: 12 }, () => d.next());
  assert.deepEqual(
    seen,
    [
      1500, 1500, 2500, 2500, 5500, 5500, 10500, 10500, 10500, 30500, 30500,
      30500,
    ],
  );
});

void test("reset() starts the schedule over; nothing else does", () => {
  const d = new ReconnectDelay();
  for (let i = 0; i < 11; i++) d.next();
  d.reset();
  assert.equal(d.next(), 1500);
  assert.deepEqual(
    Object.getOwnPropertyNames(ReconnectDelay.prototype).sort(),
    ["constructor", "next", "reset"],
  );
});
