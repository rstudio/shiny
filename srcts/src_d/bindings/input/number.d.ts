import { TextInputBindingBase } from "./text";
declare type NumberHTMLElement = HTMLInputElement;
declare type NumberReceiveMessageData = {
    label: string;
    value?: string | null;
    min?: string | null;
    max?: string | null;
    step?: string | null;
};
declare class NumberInputBinding extends TextInputBindingBase {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: NumberHTMLElement): string | number | string[];
    setValue(el: NumberHTMLElement, value: number): void;
    getType(el: NumberHTMLElement): string;
    receiveMessage(el: NumberHTMLElement, data: NumberReceiveMessageData): void;
    getState(el: NumberHTMLElement): {
        label: string;
        value: ReturnType<NumberInputBinding["getValue"]>;
        min: number;
        max: number;
        step: number;
    };
    _getLabelNode(el: NumberHTMLElement): JQuery<HTMLElement>;
}
export { NumberInputBinding };
export type { NumberReceiveMessageData };
