import { RatePolicyModes } from "../../inputPolicies/inputRateDecorator";
import { bindScope } from "../../shiny/bind";
import type { NumberReceiveMessageData } from "./number";
import type { RadioReceiveMessageData } from "./radio";
import type { SelectInputReceiveMessageData } from "./selectInput";
import type { SliderReceiveMessageData } from "./slider";
import type { TabInputReceiveMessageData } from "./tabinput";
import type { TextReceiveMessageData } from "./text";
import type { ActionButtonReceiveMessageData } from "./actionbutton";
import type { CheckboxGroupReceiveMessageData } from "./checkboxgroup";
import type { DateReceiveMessageData } from "./date";
import type { CheckboxReceiveMessageData } from "./checkbox";
import type { DateRangeReceiveMessageData } from "./daterange";
declare type receiveMessageDataType = NumberReceiveMessageData | RadioReceiveMessageData | SelectInputReceiveMessageData | SliderReceiveMessageData | TabInputReceiveMessageData | TextReceiveMessageData | ActionButtonReceiveMessageData | CheckboxReceiveMessageData | CheckboxGroupReceiveMessageData | DateReceiveMessageData | DateRangeReceiveMessageData;
declare class InputBinding {
    name: string;
    find(scope: bindScope): JQuery<HTMLElement>;
    getId(el: HTMLElement): string;
    getType(el: HTMLElement): string | false;
    getValue(el: HTMLElement): any;
    subscribe(el: HTMLElement, callback: (value: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    receiveMessage(el: HTMLElement, data: receiveMessageDataType): void;
    getState(el: HTMLElement): any;
    getRatePolicy(el: HTMLElement): {
        policy: RatePolicyModes;
        delay: number;
    } | null;
    initialize(el: HTMLElement): void;
    dispose(el: HTMLElement): void;
}
export { InputBinding };
export type { receiveMessageDataType };
