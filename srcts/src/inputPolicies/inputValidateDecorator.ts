import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";

// Merge opts with defaults, and return a new object.
function addDefaultInputOpts(opts: Partial<InputPolicyOpts>): InputPolicyOpts {
  const newOpts: InputPolicyOpts = {
    priority: "immediate",
    ...opts,
  };

  switch (newOpts.priority) {
    case "deferred":
    case "immediate":
    case "event":
      break;
    default:
      throw new Error(
        "Unexpected input value mode: '" + newOpts.priority + "'",
      );
  }

  return newOpts;
}

class InputValidateDecorator implements InputPolicy {
  target: InputPolicy;
  constructor(target: InputPolicy) {
    this.target = target;
  }

  setInput(
    nameType: string,
    value: unknown,
    opts: Partial<InputPolicyOpts> = {},
  ): void {
    if (!nameType) throw "Can't set input with empty name.";

    const newOpts = addDefaultInputOpts(opts);

    this.target.setInput(nameType, value, newOpts);
  }
}

export { addDefaultInputOpts, InputValidateDecorator };
