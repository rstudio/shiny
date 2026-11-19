import assert from "node:assert/strict";
import test from "node:test";

import { InputBatchSender } from "../inputBatchSender";

void test("deferred inputs are sent in one queued batch", () => {
  const tasks: Array<() => void> = [];
  const sentInputs: Array<{ [key: string]: unknown }> = [];
  const shinyapp = {
    taskQueue: {
      enqueue: (task: () => void) => {
        tasks.push(task);
      },
    },
    sendInput: (values: { [key: string]: unknown }) => sentInputs.push(values),
  };
  const inputBatchSender = new InputBatchSender(shinyapp as never);

  for (let i = 1; i <= 5; i++) {
    inputBatchSender.setInput(`x${i}`, i, { priority: "deferred" });
  }

  assert.equal(tasks.length, 1);
  tasks[0]();
  assert.deepEqual(sentInputs, [{ x1: 1, x2: 2, x3: 3, x4: 4, x5: 5 }]);
});
