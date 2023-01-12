import type { InputPolicy, InputPolicyOpts } from "./inputPolicy";
import type { ShinyApp } from "../shiny/shinyapp";
declare class InputBatchSender implements InputPolicy {
    target: InputPolicy;
    shinyapp: ShinyApp;
    pendingData: {
        [key: string]: unknown;
    };
    reentrant: boolean;
    sendIsEnqueued: boolean;
    lastChanceCallback: Array<() => void>;
    constructor(shinyapp: ShinyApp);
    setInput(nameType: string, value: unknown, opts: InputPolicyOpts): void;
    private _sendNow;
}
export { InputBatchSender };
