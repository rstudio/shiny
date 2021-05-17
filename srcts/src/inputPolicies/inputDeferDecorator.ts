import { hasOwnProperty } from "../utils";

class InputDeferDecorator {
  target;
  pendingInput = {};
  constructor(target) {
    this.target = target;
  }

  setInput(nameType, value, opts): void {
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
