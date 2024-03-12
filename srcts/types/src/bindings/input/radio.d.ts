import { InputBinding } from "./inputBinding";
type RadioHTMLElement = HTMLInputElement;
type ValueLabelObject = {
    value: HTMLInputElement["value"];
    label: string;
};
type RadioReceiveMessageData = {
    value?: string | [];
    options?: ValueLabelObject[];
    label: string;
};
declare class RadioInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: RadioHTMLElement): string[] | number | string | null | undefined;
    setValue(el: RadioHTMLElement, value: string | []): void;
    getState(el: RadioHTMLElement): {
        label: string;
        value: ReturnType<RadioInputBinding["getValue"]>;
        options: ValueLabelObject[];
    };
    receiveMessage(el: RadioHTMLElement, data: RadioReceiveMessageData): Promise<void>;
    subscribe(el: RadioHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: RadioHTMLElement): void;
}
export { RadioInputBinding };
export type { RadioReceiveMessageData };
