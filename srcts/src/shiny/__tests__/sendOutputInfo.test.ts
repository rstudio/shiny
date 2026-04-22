import assert from "node:assert/strict";
import test from "node:test";

import { InputBatchSender } from "../../inputPolicies";
import { SendOutputInfo } from "../sendOutputInfo";

void test("pending observer output info is flushed before the next input batch send", () => {
  const sentInputs: Array<{ [key: string]: unknown }> = [];
  const shinyapp = {
    taskQueue: {
      enqueue: () => {
        throw new Error("task queue should not be used in this test");
      },
    },
    sendInput: (values: { [key: string]: unknown }) => {
      sentInputs.push(values);
    },
  };
  const inputBatchSender = new InputBatchSender(shinyapp as never);
  const sendOutputInfo = new SendOutputInfo();

  sendOutputInfo.setSendMethod(inputBatchSender, () => {
    /* no-op */
  });

  const observerCallback = sendOutputInfo.createObserverCallback(100, () => {
    inputBatchSender.setInput(".clientdata_output_plot_width", 400, {
      priority: "immediate",
    });
  });

  observerCallback();

  inputBatchSender.setInput("user", 1, { priority: "event" });

  assert.equal(sentInputs.length, 1);
  const expected: { [key: string]: unknown } = { user: 1 };

  expected[".clientdata_output_plot_width"] = 400;

  assert.deepEqual(sentInputs[0], expected);
});
