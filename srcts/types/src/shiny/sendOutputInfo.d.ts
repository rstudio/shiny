import type { InputBatchSender } from "../inputPolicies";
import { Debouncer } from "../time";
type FlushableObserverCallback = (() => void) & {
    cancel: () => void;
    flush: () => void;
    isPending: () => boolean;
};
declare class SendOutputInfo {
    #private;
    regular: () => void;
    transitioned: () => void;
    setSendMethod(inputBatchSender: InputBatchSender, doSendOutputInfo: () => void): Debouncer<typeof doSendOutputInfo>;
    createObserverCallback(delayMs: number, callback: () => void): FlushableObserverCallback;
}
declare const sendOutputInfoFns: SendOutputInfo;
export { sendOutputInfoFns, SendOutputInfo };
export type { FlushableObserverCallback };
