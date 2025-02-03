import type { InputBatchSender } from "../inputPolicies";
import { debounce, Debouncer } from "../time";

class SendImageSize {
  // This function gets defined in initShiny() and 'hoisted' so it can be reused
  // (to send CSS info) inside of Shiny.renderDependencies()
  regular!: () => void;
  transitioned!: () => void;

  setImageSend(
    inputBatchSender: InputBatchSender,
    doSendImageSize: () => void,
  ): Debouncer<typeof doSendImageSize> {
    const sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);

    this.regular = function () {
      sendImageSizeDebouncer.normalCall();
    };

    // Make sure sendImageSize actually gets called before the inputBatchSender
    // sends data to the server.
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendImageSizeDebouncer.isPending())
        sendImageSizeDebouncer.immediateCall();
    });

    // A version of sendImageSize which debounces for longer.
    this.transitioned = debounce(200, this.regular);

    return sendImageSizeDebouncer;
  }
}

const sendImageSizeFns = new SendImageSize();

export { sendImageSizeFns };
