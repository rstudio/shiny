import { InputBinding } from "./InputBinding";
declare type TextHTMLElement = HTMLInputElement;
declare type TextReceiveMessageData = {
    label: string;
    value?: any;
    placeholder?: any;
};
declare class TextInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getId(el: TextHTMLElement): string;
    getValue(el: TextHTMLElement): any;
    setValue(el: TextHTMLElement, value: any): void;
    subscribe(el: TextHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: TextHTMLElement): void;
    receiveMessage(el: TextHTMLElement, data: TextReceiveMessageData): void;
    getState(el: TextHTMLElement): {
        label: string;
        value: any;
        placeholder: any;
    };
    getRatePolicy(el: HTMLElement): {
        policy: "debounce";
        delay: 250;
    };
    _getLabelNode(el: HTMLElement): JQuery<HTMLElement>;
}
export { TextInputBinding };
export type { TextHTMLElement, TextReceiveMessageData };
