import { hasDefinedProperty } from "../utils";
import type {
  EventPriority,
  InputPolicy,
  InputPolicyOpts,
} from "./inputPolicy";

class InputDeferDecorator implements InputPolicy {
  pendingInput: {
    [key: string]: { value: unknown; opts: { priority: EventPriority } };
  } = {};
  target: InputPolicy;

  constructor(target: InputPolicy) {
    this.target = target;
  }

  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    if (/^\./.test(nameType)) this.target.setInput(nameType, value, opts);
    else this.pendingInput[nameType] = { value, opts };
  }
  submit(): void {
    for (const nameType in this.pendingInput) {
      if (hasDefinedProperty(this.pendingInput, nameType)) {
        const { value, opts } = this.pendingInput[nameType];

        this.target.setInput(nameType, value, opts);
      }
    }
  }
}

export { InputDeferDecorator };
