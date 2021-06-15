import { InputBinding } from "./InputBinding";
declare type RadioHTMLElement = HTMLInputElement;
declare type ValueLabelObject = {
    value: HTMLInputElement["value"];
    label: string;
};
declare type RadioReceiveMessageData = {
    value?: string;
    options?: Array<ValueLabelObject>;
    label: string;
};
declare class RadioInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: RadioHTMLElement): string | number | string[] | null;
    setValue(el: RadioHTMLElement, value: string): void;
    getState(el: RadioHTMLElement): {
        label: string;
        value: string | number | string[];
        options: Array<ValueLabelObject>;
    };
    receiveMessage(el: RadioHTMLElement, data: RadioReceiveMessageData): void;
    subscribe(el: RadioHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: RadioHTMLElement): void;
    _getLabelNode(el: RadioHTMLElement): JQuery<HTMLElement>;
    _getLabel(obj: HTMLElement): string | null;
    _setLabel(obj: HTMLElement, value: string): null;
}
export { RadioInputBinding };
export type { RadioReceiveMessageData };
