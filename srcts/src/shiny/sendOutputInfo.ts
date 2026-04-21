import type { InputBatchSender } from "../inputPolicies";
import { debounce, Debouncer } from "../time";

type FlushableObserverCallback = (() => void) & {
  cancel: () => void;
  flush: () => void;
  isPending: () => boolean;
};

class SendOutputInfo {
  regular!: () => void;
  transitioned!: () => void;
  #pendingObserverCallbacks = new Set<FlushableObserverCallback>();

  setSendMethod(
    inputBatchSender: InputBatchSender,
    doSendOutputInfo: () => void,
  ): Debouncer<typeof doSendOutputInfo> {
    const sendOutputInfoDebouncer = new Debouncer(null, doSendOutputInfo, 0);

    this.regular = function () {
      sendOutputInfoDebouncer.normalCall();
    };

    inputBatchSender.lastChanceCallback.push(() => {
      this.#pendingObserverCallbacks.forEach((callback) => callback.flush());

      if (sendOutputInfoDebouncer.isPending())
        sendOutputInfoDebouncer.immediateCall();
    });

    this.transitioned = debounce(200, this.regular);

    return sendOutputInfoDebouncer;
  }

  createObserverCallback(
    delayMs: number,
    callback: () => void,
  ): FlushableObserverCallback {
    let observerCallback!: FlushableObserverCallback;
    const debouncer = new Debouncer(
      null,
      () => {
        this.#pendingObserverCallbacks.delete(observerCallback);
        callback();
      },
      delayMs,
    );

    observerCallback = Object.assign(
      () => {
        this.#pendingObserverCallbacks.add(observerCallback);
        debouncer.normalCall();
      },
      {
        cancel: () => {
          this.#pendingObserverCallbacks.delete(observerCallback);
          debouncer.cancel();
        },
        flush: () => {
          this.#pendingObserverCallbacks.delete(observerCallback);
          if (debouncer.isPending()) {
            debouncer.immediateCall();
          }
        },
        isPending: () => debouncer.isPending(),
      },
    );

    return observerCallback;
  }
}

const sendOutputInfoFns = new SendOutputInfo();

export { sendOutputInfoFns, SendOutputInfo };
export type { FlushableObserverCallback };
