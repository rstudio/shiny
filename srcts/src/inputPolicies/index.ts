import { InputBatchSender } from "./inputBatchSender";
import { InputNoResendDecorator } from "./inputNoResendDecorator";
import { InputEventDecorator } from "./inputEventDecorator";
import { InputRateDecorator } from "./inputRateDecorator";
import { InputDeferDecorator } from "./inputDeferDecorator";
import { InputValidateDecorator } from "./inputValidateDecorator";

type priorityType = "immediate" | "deferred" | "event";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputPolicy {
  target: InputPolicy;

  setInput(
    name: string,
    value: unknown,
    opts: { priority: priorityType }
  ): void {
    throw "not implemented";
    name;
    value;
    opts;
  }
}

export {
  InputBatchSender,
  InputEventDecorator,
  InputNoResendDecorator,
  InputRateDecorator,
  InputDeferDecorator,
  InputValidateDecorator,
  InputPolicy,
};

export type { priorityType };
