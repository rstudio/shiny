/// <reference types="selectize" />
import { InputBinding } from "./inputBinding";
import type { NotUndefined } from "../../utils/extraTypes";
type SelectHTMLElement = HTMLSelectElement & {
    nonempty: boolean;
};
type SelectInputReceiveMessageData = {
    label: string;
    options?: string;
    config?: string;
    url?: string;
    value?: string;
};
type SelectizeOptions = Selectize.IOptions<string, unknown>;
type SelectizeInfo = Selectize.IApi<string, unknown> & {
    settings: SelectizeOptions;
};
declare class SelectInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string | null;
    getId(el: SelectHTMLElement): string;
    getValue(el: HTMLElement): NotUndefined<ReturnType<JQuery<HTMLElement>["val"]>>;
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
<<<<<<< HEAD
    protected _newSelectize($el: JQuery<HTMLSelectElement>, options: SelectizeOptions): SelectizeInfo;
=======
    protected _isMultipleSelect($el: JQuery<HTMLElement>): boolean;
>>>>>>> parent of eddc3047d (Fix regression in `updateSelectizeInput()` (#3890))
}
export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
