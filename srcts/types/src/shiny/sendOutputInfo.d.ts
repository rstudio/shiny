import type { InputBatchSender } from "../inputPolicies";
import { Debouncer } from "../time";
declare class SendOutputInfo {
    regular: () => void;
    transitioned: () => void;
    setSendMethod(inputBatchSender: InputBatchSender, doSendOutputInfo: () => void): Debouncer<typeof doSendOutputInfo>;
}
declare const sendOutputInfoFns: SendOutputInfo;
export { sendOutputInfoFns };
