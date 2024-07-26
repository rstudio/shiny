import { TextInputBindingBase } from "./text";
type NumberHTMLElement = HTMLInputElement;
type NumberReceiveMessageData = {
    label: string;
    value?: string | null;
    min?: string | null;
    max?: string | null;
    step?: string | null;
};
declare class NumberInputBinding extends TextInputBindingBase {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: NumberHTMLElement): string[] | number | string | null | undefined;
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
}
export { NumberInputBinding };
export type { NumberReceiveMessageData };
