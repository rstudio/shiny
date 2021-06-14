import { InputBinding } from "./InputBinding";
declare type CheckedHTMLElement = HTMLInputElement;
declare type CheckboxChecked = CheckedHTMLElement["checked"];
declare type CheckboxReceiveMessageData = {
    value?: CheckboxChecked;
    label?: string;
};
declare class CheckboxInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: CheckedHTMLElement): CheckboxChecked;
    setValue(el: CheckedHTMLElement, value: CheckboxChecked): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    getState(el: CheckedHTMLElement): {
        label: string;
        value: CheckboxChecked;
    };
    receiveMessage(el: CheckedHTMLElement, data: CheckboxReceiveMessageData): void;
}
export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
