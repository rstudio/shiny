import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";

class Debouncer<X extends AnyVoidFunction> implements InputRatePolicy<X> {
  target: InputPolicy;
  func: X;
  delayMs: number | undefined;
  timerId: number | null;
  args: Parameters<X> | null;

  constructor(target: InputPolicy, func: X, delayMs: number | undefined) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  }

  normalCall(...args: Parameters<X>): void {
    this.$clearTimer();
    this.args = args;

    this.timerId = window.setTimeout(() => {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (this.timerId === null) return;
      this.$clearTimer();
      this.$invoke();
    }, this.delayMs);
  }
  immediateCall(...args: Parameters<X>): void {
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
    // this.func.apply(this.target, this.args);
    if (this.args && this.args.length > 0) {
      this.func.apply(this.target, this.args);
    } else {
      this.func.apply(this.target);
    }
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
function debounce<T extends (...args: unknown[]) => void>(
  threshold: number | undefined,
  func: T
): (...args: Parameters<T>) => void {
  let timerId: number | null = null;

  // Do not alter `function()` into an arrow function.
  // The `this` context needs to be kept
  return function (...args: Parameters<T>) {
    if (timerId !== null) {
      clearTimeout(timerId);
      timerId = null;
    }
    timerId = window.setTimeout(() => {
      // IE8 doesn't reliably clear timeout, so this additional
      // check is needed
      if (timerId === null) return;
      timerId = null;
      // eslint-disable-next-line @typescript-eslint/ban-ts-comment
      // @ts-ignore; The `this` scope is correct. Do not change it!
      // Possible typing: https://www.typescriptlang.org/docs/handbook/utility-types.html#thistypetype
      func.apply(this, args);
    }, threshold);
  };
}

export { Debouncer, debounce };
