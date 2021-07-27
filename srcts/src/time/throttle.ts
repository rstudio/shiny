class Throttler<T = unknown> {
  target: unknown;
  func: (...args: T[]) => void;
  delayMs: number;
  timerId: NodeJS.Timeout;
  args: T[];

  constructor(target: unknown, func: (...args: T[]) => void, delayMs: number) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  }

  normalCall(...args: T[]): void {
    this.args = args;
    if (this.timerId === null) {
      this.$invoke();
      this.timerId = setTimeout(() => {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (this.timerId === null) return;
        this.$clearTimer();
        if (args.length > 0) this.normalCall.apply(this, ...args);
      }, this.delayMs);
    }
  }
  immediateCall(...args: T[]): void {
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

// Returns a throttled version of the given function.
// Throttling means that the underlying function will
// be executed no more than once every `threshold`
// milliseconds.
//
// For example, if a function is throttled with a
// threshold of 1000ms, then calling it 17 times at
// 900ms intervals will result in something like 15
// or 16 executions of the underlying function.
// eslint-disable-next-line no-unused-vars
function throttle<T>(
  threshold: number,
  func: (...args: T[]) => void
): (...args: T[]) => void {
  let executionPending = false;
  let timerId = null;
  let self, args: T[];

  function throttled(...argumentVals: T[]) {
    self = null;
    args = null;
    if (timerId === null) {
      // Haven't seen a call recently. Execute now and
      // start a timer to buffer any subsequent calls.
      timerId = setTimeout(function () {
        // When time expires, clear the timer; and if
        // there has been a call in the meantime, repeat.
        timerId = null;
        if (executionPending) {
          executionPending = false;
          throttled.apply(self, args);
        }
      }, threshold);
      func.apply(this, argumentVals);
    } else {
      // Something executed recently. Don't do anything
      // except set up target/arguments to be called later
      executionPending = true;
      self = this;
      args = argumentVals;
    }
  }
  return throttled;
}

export { Throttler, throttle };
