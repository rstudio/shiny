import type { InputPolicy } from "../inputPolicies";

class Invoker<T> {
  target: InputPolicy;
  func: () => void;

  constructor(target: InputPolicy, func: (...args: T[]) => void) {
    this.target = target;
    this.func = func;
  }

  // TODO-barret - Don't know how to define the method twice and still have access to "this"
  normalCall(...args: T[]): void {
    this.func.apply(this.target, args);
  }
  immediateCall(...args: T[]): void {
    this.func.apply(this.target, args);
  }
}

export { Invoker };
