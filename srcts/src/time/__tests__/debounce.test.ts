import assert from "node:assert/strict";
import test from "node:test";

import { debounce } from "../debounce";

void test("debounce can cancel a pending callback before it fires", async () => {
  let calls = 0;
  const debounced = debounce(10, () => {
    calls += 1;
  });

  debounced();
  debounced.cancel();

  await new Promise((resolve) => setTimeout(resolve, 30));

  assert.equal(calls, 0);
});
