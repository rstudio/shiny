class Debouncer {
  target: any;
  func: (...args: Array<any>) => void;
  delayMs: number;
  timerId: NodeJS.Timeout;
  args: Array<any>;

  constructor(
    target: any,
    func: (...args: Array<any>) => void,
    delayMs: number
  ) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  }

  normalCall(...args: Array<any>): void {
    this.$clearTimer();
    this.args = args;

    this.timerId = setTimeout(() => {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (this.timerId === null) return;
      this.$clearTimer();
      this.$invoke();
    }, this.delayMs);
  }
  immediateCall(...args: Array<any>): void {
    this.$clearTimer();
    this.args = args;
    this.$invoke();
  }
  isPending(): boolean {
    return this.timerId !== null;
  }
  $clearTimer(): void {
    if (this.timerId !== null) {
      clearTimeout(this.timerId);
      this.timerId = null;
    }
  }
  $invoke(): void {
    this.func.apply(this.target, this.args);
    this.args = null;
  }
}

// Returns a debounced version of the given function.
// Debouncing means that when the function is invoked,
// there is a delay of `threshold` milliseconds before
// it is actually executed, and if the function is
// invoked again before that threshold has elapsed then
// the clock starts over.
//
// For example, if a function is debounced with a
// threshold of 1000ms, then calling it 17 times at
// 900ms intervals will result in a single execution
// of the underlying function, 1000ms after the 17th
// call.
function debounce(
  threshold: number,
  func: (...args: Array<any>) => void
): (...args: Array<any>) => void {
  let timerId = null;

  return function (...args) {
    if (timerId !== null) {
      clearTimeout(timerId);
      timerId = null;
    }
    timerId = setTimeout(() => {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (timerId === null) return;
      timerId = null;
      func.apply(this, args);
    }, threshold);
  };
}

export { Debouncer, debounce };
