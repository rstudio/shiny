import { InputBinding } from "./inputBinding";
import type { CheckedHTMLElement } from "./checkbox";
declare type CheckboxGroupHTMLElement = CheckedHTMLElement;
declare type ValueLabelObject = {
    value: HTMLInputElement["value"];
    label: string;
};
declare type CheckboxGroupReceiveMessageData = {
    options?: string;
    value?: Parameters<CheckboxGroupInputBinding["setValue"]>[1];
    label: string;
};
declare type CheckboxGroupValue = CheckboxGroupHTMLElement["value"];
declare class CheckboxGroupInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: CheckboxGroupHTMLElement): CheckboxGroupValue[];
    setValue(el: HTMLElement, value: string[] | string): void;
    getState(el: CheckboxGroupHTMLElement): {
        label: string;
        value: ReturnType<CheckboxGroupInputBinding["getValue"]>;
        options: ValueLabelObject[];
    };
    receiveMessage(el: CheckboxGroupHTMLElement, data: CheckboxGroupReceiveMessageData): void;
    subscribe(el: CheckboxGroupHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: CheckboxGroupHTMLElement): void;
}
export { CheckboxGroupInputBinding };
export type { CheckboxGroupReceiveMessageData };
