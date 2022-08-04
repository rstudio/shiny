import type { InputBatchSender } from "../inputPolicies";
import { debounce, Debouncer } from "../time";

class SendOutputInfo {
  // This function gets defined in initShiny() and 'hoisted' so it can be reused
  // (to send CSS info) inside of Shiny.renderDependencies()
  regular: () => void;
  transitioned: () => void;

  setSendMethod(
    inputBatchSender: InputBatchSender,
    doSendOutputInfo: () => void
  ): Debouncer<typeof doSendOutputInfo> {
    const sendOutputInfoDebouncer = new Debouncer(null, doSendOutputInfo, 0);

    this.regular = function () {
      sendOutputInfoDebouncer.normalCall();
    };

    // Make sure sendOutputInfo actually gets called before the inputBatchSender
    // sends data to the server.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendOutputInfoDebouncer.isPending())
        sendOutputInfoDebouncer.immediateCall();
    });

    // A version of sendOutputInfo which debounces for longer.
    this.transitioned = debounce(200, this.regular);

    return sendOutputInfoDebouncer;
  }
}

const sendOutputInfoFns = new SendOutputInfo();

export { sendOutputInfoFns };
