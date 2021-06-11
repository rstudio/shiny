import { TextInputBinding } from "./text";
declare type NumberHTMLElement = HTMLInputElement;
declare type NumberReceiveMessageData = {
    label: string;
    value?: any;
    min?: any;
    max?: any;
    step?: any;
};
declare class NumberInputBinding extends TextInputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: NumberHTMLElement): string | number | string[];
    setValue(el: NumberHTMLElement, value: number): void;
    getType(el: NumberHTMLElement): string;
    receiveMessage(el: NumberHTMLElement, data: NumberReceiveMessageData): void;
    getState(el: NumberHTMLElement): any;
    _getLabelNode(el: NumberHTMLElement): JQuery<HTMLElement>;
}
export { NumberInputBinding };
export type { NumberReceiveMessageData };
