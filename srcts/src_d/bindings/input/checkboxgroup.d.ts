import { InputBinding } from "./InputBinding";
import { CheckedHTMLElement } from "./checkbox";
declare type CheckboxGroupHTMLElement = CheckedHTMLElement;
declare type ValueLabelObject = {
    value: any;
    label: string;
};
declare type CheckboxGroupReceiveMessageData = {
    options?: any;
    value?: any;
    label: string;
};
declare class CheckboxGroupInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: CheckboxGroupHTMLElement): Array<CheckboxGroupHTMLElement>;
    setValue(el: HTMLElement, value: Array<string> | string): void;
    getState(el: CheckboxGroupHTMLElement): {
        label: string;
        value: any;
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
