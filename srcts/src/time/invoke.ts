class Invoker {
  target: HTMLElement;
  func: () => void;

  constructor(target: HTMLElement, func: () => void) {
    this.target = target;
    this.func = func;
  }

  // TODO - Don't know how to define the method twice
  normalCall(...args: Array<any>): void {
    this.func.apply(this.target, args);
  }
  immediateCall(...args: Array<any>): void {
    this.func.apply(this.target, args);
  }
}

export { Invoker };
