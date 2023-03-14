import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";

class Invoker<X extends AnyVoidFunction> implements InputRatePolicy<X> {
  target: InputPolicy | null;
  func: X;

  constructor(target: InputPolicy | null, func: X) {
    this.target = target;
    this.func = func;
  }

  // TODO-barret - Don't know how to define the method twice and still have access to "this"
  normalCall(...args: Parameters<X>): void {
    this.func.apply(this.target, args);
  }
  immediateCall(...args: Parameters<X>): void {
    this.func.apply(this.target, args);
  }
}

export { Invoker };
