import { hasDefinedProperty } from "../utils";
import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import { splitInputNameType } from "./splitInputNameType";

type LastSentValues = { [key: string]: { [key: string]: string } };

class InputNoResendDecorator implements InputPolicy {
  target: InputPolicy;
  lastSentValues: LastSentValues = {};

  constructor(target: InputPolicy, initialValues: LastSentValues = {}) {
    this.target = target;
    this.reset(initialValues);
  }

  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    const { name: inputName, inputType: inputType } =
      splitInputNameType(nameType);
    const jsonValue = JSON.stringify(value);

    if (
      opts.priority !== "event" &&
      this.lastSentValues[inputName] &&
      this.lastSentValues[inputName].jsonValue === jsonValue &&
      this.lastSentValues[inputName].inputType === inputType
    ) {
      return;
    }
    this.lastSentValues[inputName] = { jsonValue, inputType };
    this.target.setInput(nameType, value, opts);
  }
  reset(values: LastSentValues = {}): void {
    // Given an object with flat name-value format:
    //   { x: "abc", "y.shiny.number": 123 }
    // Create an object in cache format and save it:
    //   { x: { jsonValue: '"abc"', inputType: "" },
    //     y: { jsonValue: "123", inputType: "shiny.number" } }
    const cacheValues: {
      [key: string]: { jsonValue: string; inputType: string };
    } = {};

    for (const inputName in values) {
      if (hasDefinedProperty(values, inputName)) {
        const { name, inputType } = splitInputNameType(inputName);

        cacheValues[name] = {
          jsonValue: JSON.stringify(values[inputName]),
          inputType: inputType,
        };
      }
    }

    this.lastSentValues = cacheValues;
  }
  forget(name: string): void {
    delete this.lastSentValues[name];
  }
}

export { InputNoResendDecorator };
