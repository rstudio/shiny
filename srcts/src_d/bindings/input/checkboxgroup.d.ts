import { InputBinding } from "./InputBinding";
import { CheckedHTMLElement } from "./checkbox";
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
    getValue(el: CheckboxGroupHTMLElement): Array<CheckboxGroupValue>;
    setValue(el: HTMLElement, value: Array<string> | string): void;
    getState(el: CheckboxGroupHTMLElement): {
        label: string;
        value: ReturnType<CheckboxGroupInputBinding["getValue"]>;
        options: Array<ValueLabelObject>;
    };
    receiveMessage(el: CheckboxGroupHTMLElement, data: CheckboxGroupReceiveMessageData): void;
    subscribe(el: CheckboxGroupHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: CheckboxGroupHTMLElement): void;
    _getLabelNode(el: CheckboxGroupHTMLElement): JQuery<HTMLElement>;
    _getLabel(obj: HTMLElement): string | null;
    _setLabel(obj: HTMLElement, value: string): null;
}
export { CheckboxGroupInputBinding };
export type { CheckboxGroupReceiveMessageData };
