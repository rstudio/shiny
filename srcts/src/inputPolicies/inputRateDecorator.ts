import { InputPolicy, priorityType } from ".";
import { Debouncer, Invoker, Throttler } from "../time";
import { splitInputNameType } from "./splitInputNameType";

class InputRateDecorator extends InputPolicy {
  inputRatePolicies = {};

  constructor(target: InputPolicy) {
    super();
    this.target = target;
  }

  // Note that the first argument of setInput() and setRatePolicy()
  // are passed both the input name (i.e., inputId) and type.
  // https://github.com/rstudio/shiny/blob/67d3a/srcjs/init_shiny.js#L111-L126
  // However, $ensureInit() and $doSetInput() are meant to be passed just
  // the input name (i.e., inputId), which is why we distinguish between
  // nameType and name.
  setInput(
    nameType: string,
    value: unknown,
    opts: { priority: priorityType }
  ): void {
    const { name: inputName } = splitInputNameType(nameType);

    this.$ensureInit(inputName);

    if (opts.priority !== "deferred")
      this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
    else this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
  }
  setRatePolicy(
    nameType: string,
    mode: "direct" | "debounce" | "throttle",
    millis?: number
  ): void {
    const { name: inputName } = splitInputNameType(nameType);

    if (mode === "direct") {
      this.inputRatePolicies[inputName] = new Invoker(this, this.$doSetInput);
    } else if (mode === "debounce") {
      this.inputRatePolicies[inputName] = new Debouncer(
        this,
        this.$doSetInput,
        millis
      );
    } else if (mode === "throttle") {
      this.inputRatePolicies[inputName] = new Throttler(
        this,
        this.$doSetInput,
        millis
      );
    }
  }
  private $ensureInit(name: string): void {
    if (!(name in this.inputRatePolicies)) this.setRatePolicy(name, "direct");
  }
  private $doSetInput(
    nameType: string,
    value: unknown,
    opts: { priority: priorityType }
  ): void {
    this.target.setInput(nameType, value, opts);
  }
}

export { InputRateDecorator };
