import assert from "node:assert/strict";
import test from "node:test";

import { InputBatchSender } from "../inputBatchSender";

void test("InputBatchSender enqueues once per batch and skips empty flushes", () => {
  const queuedTasks: Array<() => void> = [];
  const sentInputs: Array<{ [key: string]: unknown }> = [];
  const shinyapp = {
    taskQueue: {
      enqueue: (fn: () => void) => {
        queuedTasks.push(fn);
      },
    },
    sendInput: (values: { [key: string]: unknown }) => {
      sentInputs.push(values);
    },
  };

  const sender = new InputBatchSender(shinyapp as never);

  sender.setInput("a", 1, { priority: "immediate" });
  sender.setInput("b", 2, { priority: "immediate" });
  sender.setInput("c", 3, { priority: "deferred" });

  assert.equal(queuedTasks.length, 1);

  queuedTasks.shift()!();

  assert.equal(sentInputs.length, 1);
  assert.deepEqual(sentInputs[0], { a: 1, b: 2, c: 3 });

  // If a deferred input is queued and then an event-priority input flushes
  // immediately, the queued task should not send an empty batch afterwards.
  sender.setInput("d", 4, { priority: "immediate" });
  assert.equal(queuedTasks.length, 1);

  sender.setInput("e", 5, { priority: "event" });
  assert.equal(sentInputs.length, 2);
  assert.deepEqual(sentInputs[1], { d: 4, e: 5 });

  queuedTasks.shift()!();
  assert.equal(sentInputs.length, 2);
});
