import type { InputBatchSender } from "../inputPolicies";
import { debounce, Debouncer } from "../time";

class SendWindowSize {
  regular!: () => void;
  transitioned!: () => void;

  setWindowSizeSend(
    inputBatchSender: InputBatchSender,
    doSendWindowSize: () => void
  ): Debouncer<typeof doSendWindowSize> {
    const sendWindowSizeDebouncer = new Debouncer(null, doSendWindowSize, 0);

    this.regular = function () {
      sendWindowSizeDebouncer.normalCall();
    };

    inputBatchSender.lastChanceCallback.push(function () {
      if (sendWindowSizeDebouncer.isPending()) {
        sendWindowSizeDebouncer.immediateCall();
      }
    });

    this.transitioned = debounce(200, this.regular);

    return sendWindowSizeDebouncer;
  }
}

const sendWindowSizeFns = new SendWindowSize();

export { sendWindowSizeFns };
