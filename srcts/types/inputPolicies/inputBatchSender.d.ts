/// <reference types="node" />
import type { EventPriority } from "./inputPolicy";
import { InputPolicy } from "./inputPolicy";
import type { ShinyApp } from "../shiny/shinyapp";
declare class InputBatchSender extends InputPolicy {
    shinyapp: ShinyApp;
    timerId: NodeJS.Timeout;
    pendingData: Record<string, unknown>;
    reentrant: boolean;
    lastChanceCallback: Array<() => void>;
    constructor(shinyapp: ShinyApp);
    setInput(nameType: string, value: unknown, opts: {
        priority: EventPriority;
    }): void;
    private _sendNow;
}
export { InputBatchSender };
