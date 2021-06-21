/// <reference types="selectize" />
import { InputBinding } from "./inputBinding";
declare type SelectHTMLElement = HTMLSelectElement & {
    nonempty: boolean;
};
declare type SelectInputReceiveMessageData = {
    label: string;
    options?: string;
    config?: string;
    url?: string;
    value?: string;
};
declare type SelectizeInfo = Selectize.IApi<string, unknown> & {
    settings: Selectize.IOptions<string, unknown>;
};
declare class SelectInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string;
    getId(el: SelectHTMLElement): string;
    getValue(el: HTMLElement): string[] | number | string;
    setValue(el: SelectHTMLElement, value: string): void;
    getState(el: SelectHTMLElement): {
        label: JQuery<HTMLElement>;
        value: string[] | number | string;
        options: Array<{
            value: string;
            label: string;
        }>;
    };
    receiveMessage(el: SelectHTMLElement, data: SelectInputReceiveMessageData): void;
    subscribe(el: SelectHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    initialize(el: SelectHTMLElement): void;
    protected _selectize(el: SelectHTMLElement, update?: boolean): SelectizeInfo;
}
export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
