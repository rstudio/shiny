/// <reference types="selectize" />
import { InputBinding } from "./InputBinding";
declare type SelectHTMLElement = HTMLSelectElement & {
    nonempty: boolean;
};
declare type SelectInputReceiveMessageData = {
    label: string;
    options?: any;
    config?: any;
    url?: string;
    value?: string;
};
declare class SelectInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string;
    getId(el: SelectHTMLElement): string;
    getValue(el: HTMLElement): string | number | string[];
    setValue(el: SelectHTMLElement, value: string): void;
    getState(el: SelectHTMLElement): {
        label: JQuery<HTMLElement>;
        value: string | number | string[];
        options: Array<{
            value: string;
            label: string;
        }>;
    };
    receiveMessage(el: SelectHTMLElement, data: SelectInputReceiveMessageData): void;
    subscribe(el: SelectHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    initialize(el: SelectHTMLElement): void;
    _getLabelNode(el: SelectHTMLElement): JQuery<HTMLElement>;
    _is_selectize(el: HTMLElement): boolean;
    _selectize(el: SelectHTMLElement, update?: boolean): Selectize.IApi<any, any>;
}
export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
