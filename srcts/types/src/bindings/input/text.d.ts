import { InputBinding } from "./inputBinding";
declare type TextHTMLElement = HTMLInputElement;
declare type TextReceiveMessageData = {
    label: string;
    value?: TextHTMLElement["value"];
    placeholder?: TextHTMLElement["placeholder"];
};
declare class TextInputBindingBase extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getId(el: TextHTMLElement): string;
    getValue(el: TextHTMLElement): unknown;
    setValue(el: TextHTMLElement, value: unknown): void;
    subscribe(el: TextHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: TextHTMLElement): void;
    receiveMessage(el: TextHTMLElement, data: unknown): void;
    getState(el: TextHTMLElement): unknown;
    getRatePolicy(el: HTMLElement): {
        policy: "debounce";
        delay: 250;
    };
}
declare class TextInputBinding extends TextInputBindingBase {
    setValue(el: TextHTMLElement, value: string | undefined): void;
    getValue(el: TextHTMLElement): TextHTMLElement["value"];
    getState(el: TextHTMLElement): {
        label: string;
        value: string;
        placeholder: string;
    };
    receiveMessage(el: TextHTMLElement, data: TextReceiveMessageData): void;
}
export { TextInputBinding, TextInputBindingBase };
export type { TextHTMLElement, TextReceiveMessageData };
