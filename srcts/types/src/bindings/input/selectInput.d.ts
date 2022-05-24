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
declare type SelectizeOptions = Selectize.IOptions<string, unknown>;
declare type SelectizeInfo = Selectize.IApi<string, unknown> & {
    settings: SelectizeOptions;
};
declare class SelectInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string | false;
    getId(el: SelectHTMLElement): string;
    getValue(el: HTMLElement): string[] | number | string | undefined;
    setValue(el: SelectHTMLElement, value: string): void;
    getState(el: SelectHTMLElement): {
        label: JQuery<HTMLElement>;
        value: ReturnType<SelectInputBinding["getValue"]>;
        options: Array<{
            value: string;
            label: string;
        }>;
    };
    receiveMessage(el: SelectHTMLElement, data: SelectInputReceiveMessageData): void;
    subscribe(el: SelectHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    initialize(el: SelectHTMLElement): void;
    protected _selectize(el: SelectHTMLElement, update?: boolean): SelectizeInfo | undefined;
}
export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
