import type { InputBatchSender } from "../inputPolicies";
import { Debouncer } from "../time";
declare class SendWindowSize {
    regular: () => void;
    transitioned: () => void;
    setWindowSizeSend(inputBatchSender: InputBatchSender, doSendWindowSize: () => void): Debouncer<typeof doSendWindowSize>;
}
declare const sendWindowSizeFns: SendWindowSize;
export { sendWindowSizeFns };
