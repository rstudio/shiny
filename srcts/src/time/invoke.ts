import { InputPolicy } from "../inputPolicies";

class Invoker<T> {
  target: InputPolicy;
  func: () => void;

  constructor(target: InputPolicy, func: (...args: Array<T>) => void) {
    this.target = target;
    this.func = func;
  }

  // TODO-barret - Don't know how to define the method twice and still have access to "this"
  normalCall(...args: Array<T>): void {
    this.func.apply(this.target, args);
  }
  immediateCall(...args: Array<T>): void {
    this.func.apply(this.target, args);
  }
}

export { Invoker };
