import type { InputBatchSender } from "../inputPolicies";
import { debounce, Debouncer } from "../time";

class SendOutputInfo {
  regular!: () => void;
  transitioned!: () => void;

  setSendMethod(
    inputBatchSender: InputBatchSender,
    doSendOutputInfo: () => void,
  ): Debouncer<typeof doSendOutputInfo> {
    const sendOutputInfoDebouncer = new Debouncer(null, doSendOutputInfo, 0);

    this.regular = function () {
      sendOutputInfoDebouncer.normalCall();
    };

    inputBatchSender.lastChanceCallback.push(function () {
      if (sendOutputInfoDebouncer.isPending())
        sendOutputInfoDebouncer.immediateCall();
    });

    this.transitioned = debounce(200, this.regular);

    return sendOutputInfoDebouncer;
  }
}

const sendOutputInfoFns = new SendOutputInfo();

export { sendOutputInfoFns };
