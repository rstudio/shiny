import type { InputPolicy } from "../inputPolicies";
import type { InputRatePolicy } from "../inputPolicies/inputRatePolicy";
import type { AnyVoidFunction } from "../utils/extraTypes";

class Throttler<X extends AnyVoidFunction> implements InputRatePolicy<X> {
  target: InputPolicy | null;
  func: X;
  delayMs: number | undefined;
  timerId: ReturnType<typeof setTimeout> | null;
  args: Parameters<X> | null;

  constructor(
    target: InputPolicy | null,
    func: X,
    delayMs: number | undefined,
  ) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  }

  // If no timer is currently running, immediately call the function and set the
  // timer; if a timer is running out, just queue up the args for the call when
  // the timer runs out. Later calls during the same timeout will overwrite
  // earlier ones.
  normalCall(...args: Parameters<X>): void {
    // This will be an empty array (not null) if called without arguments, and
    // `[null]` if called with `null`.
    this.args = args;

    // Only invoke immediately if there isn't a timer running.
    if (this.timerId === null) {
      this.$invoke();
    }
  }

  // Reset the timer if active and call immediately
  immediateCall(...args: Parameters<X>): void {
    this.$clearTimer();
    this.args = args;
    this.$invoke();
  }

  // Is there a call waiting to send?
  isPending(): boolean {
    return this.args !== null;
  }

  $clearTimer(): void {
    if (this.timerId !== null) {
      clearTimeout(this.timerId);
      this.timerId = null;
    }
  }

  // Invoke the throttled function with the currently-stored args and start the
  // timer.
  $invoke(): void {
    if (this.args === null) {
      // Shouldn't get here, because $invoke should only be called right after
      // setting this.args. But just in case.
      return;
    }

    this.func.apply(this.target, this.args);

    // Clear the stored args. This is used to track if a call is pending.
    this.args = null;

    // Set this.timerId to a newly-created timer, which will invoke a call with
    // the most recently called args (if any) when it expires.
    this.timerId = setTimeout(() => {
      // IE8 doesn't reliably clear timeout, so this additional check is needed
      if (this.timerId === null) return;

      this.$clearTimer();
      // Do we have a call queued up?
      if (this.isPending()) {
        // If so, invoke the call with queued args and reset timer.
        this.$invoke();
      }
    }, this.delayMs);
  }
}

// // Returns a throttled version of the given function.
// // Throttling means that the underlying function will
// // be executed no more than once every `threshold`
// // milliseconds.
// //
// // For example, if a function is throttled with a
// // threshold of 1000ms, then calling it 17 times at
// // 900ms intervals will result in something like 15
// // or 16 executions of the underlying function.
// // eslint-disable-next-line no-unused-vars
// function throttle<T>(
//   threshold: number,
//   func: (...args: T[]) => void
// ): (...args: T[]) => void {
//   let executionPending = false;
//   let timerId: number | null = null;
//   let self: unknown, args: T[] | null;

//   function throttled(...argumentVals: T[]) {
//     self = null;
//     args = null;
//     if (timerId === null) {
//       // Haven't seen a call recently. Execute now and
//       // start a timer to buffer any subsequent calls.
//       timerId = setTimeout(function () {
//         // When time expires, clear the timer; and if
//         // there has been a call in the meantime, repeat.
//         timerId = null;
//         if (executionPending) {
//           executionPending = false;
//           throttled.apply(self, args || []);
//         }
//       }, threshold);
//       func.apply(this, argumentVals);
//     } else {
//       // Something executed recently. Don't do anything
//       // except set up target/arguments to be called later
//       executionPending = true;
//       self = this as unknown;
//       args = argumentVals;
//     }
//   }
//   return throttled;
// }

export { Throttler };
