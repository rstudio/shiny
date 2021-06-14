/// <reference types="node" />
import { priorityType, InputPolicy } from "./InputPolicy";
import { ShinyApp } from "../shiny/shinyapp";
declare class InputBatchSender extends InputPolicy {
    shinyapp: ShinyApp;
    timerId: NodeJS.Timeout;
    pendingData: Record<string, unknown>;
    reentrant: boolean;
    lastChanceCallback: Array<() => void>;
    constructor(shinyapp: ShinyApp);
    setInput(nameType: string, value: unknown, opts: {
        priority: priorityType;
    }): void;
    private $sendNow;
}
export { InputBatchSender };
