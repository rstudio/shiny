import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
import { hasOwnProperty } from "../utils";

class InputDeferDecorator extends InputPolicy {
  pendingInput: {
    [key: string]: { value: unknown; opts: { priority: EventPriority } };
  } = {};
  constructor(target: InputPolicy) {
    super();
    this.target = target;
  }

  setInput(
    nameType: string,
    value: unknown,
    opts: { priority: EventPriority }
  ): void {
    if (/^\./.test(nameType)) this.target.setInput(nameType, value, opts);
    else this.pendingInput[nameType] = { value, opts };
  }
  submit(): void {
    for (const nameType in this.pendingInput) {
      if (hasOwnProperty(this.pendingInput, nameType)) {
        const { value, opts } = this.pendingInput[nameType];

        this.target.setInput(nameType, value, opts);
      }
    }
  }
}

export { InputDeferDecorator };
