import { Debouncer, Invoker, Throttler } from "../time";
import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import type { InputRatePolicy } from "./inputRatePolicy";
import { splitInputNameType } from "./splitInputNameType";

type RatePolicyModes = "debounce" | "direct" | "throttle";

class InputRateDecorator implements InputPolicy {
  target: InputPolicy;
  inputRatePolicies: {
    [key: string]: InputRatePolicy<InputRateDecorator["_doSetInput"]>;
  } = {};

  constructor(target: InputPolicy) {
    this.target = target;
  }

  // Note that the first argument of setInput() and setRatePolicy()
  // are passed both the input name (i.e., inputId) and type.
  // https://github.com/rstudio/shiny/blob/67d3a/srcjs/init_shiny.js#L111-L126
  // However, $ensureInit() and $doSetInput() are meant to be passed just
  // the input name (i.e., inputId), which is why we distinguish between
  // nameType and name.
  setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void {
    const { name: inputName } = splitInputNameType(nameType);

    this._ensureInit(inputName);

    if (opts.priority !== "deferred")
      this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
    else this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
  }
  setRatePolicy(
    nameType: string,
    mode: RatePolicyModes,
    millis?: number,
  ): void {
    const { name: inputName } = splitInputNameType(nameType);

    if (mode === "direct") {
      this.inputRatePolicies[inputName] = new Invoker(this, this._doSetInput);
    } else if (mode === "debounce") {
      this.inputRatePolicies[inputName] = new Debouncer(
        this,
        this._doSetInput,
        millis,
      );
    } else if (mode === "throttle") {
      this.inputRatePolicies[inputName] = new Throttler(
        this,
        this._doSetInput,
        millis,
      );
    }
  }
  private _ensureInit(name: string): void {
    if (!(name in this.inputRatePolicies)) this.setRatePolicy(name, "direct");
  }
  private _doSetInput(
    nameType: string,
    value: unknown,
    opts: InputPolicyOpts,
  ): void {
    this.target.setInput(nameType, value, opts);
  }
}

export { InputRateDecorator };
export type { RatePolicyModes };
