import test from "node:test";
import assert from "node:assert/strict";

import { debounce } from "../debounce";

test("debounce can cancel a pending callback before it fires", async () => {
  let calls = 0;
  const debounced = debounce(10, () => {
    calls += 1;
  }) as ReturnType<typeof debounce> & { cancel?: () => void };

  debounced();
  debounced.cancel?.();

  await new Promise((resolve) => setTimeout(resolve, 30));

  assert.equal(calls, 0);
});
