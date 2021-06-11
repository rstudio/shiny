import { InputBinding } from "./InputBinding";
declare type CheckedHTMLElement = HTMLInputElement;
declare type CheckboxReceiveMessageData = {
    value?: any;
    label?: string;
};
declare class CheckboxInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: CheckedHTMLElement): any;
    setValue(el: CheckedHTMLElement, value: boolean): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    getState(el: CheckedHTMLElement): {
        label: string;
        value: any;
    };
    receiveMessage(el: CheckedHTMLElement, data: CheckboxReceiveMessageData): void;
}
export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
