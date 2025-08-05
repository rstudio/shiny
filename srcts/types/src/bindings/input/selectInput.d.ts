import { InputBinding } from "./inputBinding";
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
    getValue(el: SelectHTMLElement): any;
    setValue(el: SelectHTMLElement, value: string): void;
    getState(el: SelectHTMLElement): {
        label: JQuery<HTMLElement>;
        value: ReturnType<SelectInputBinding["getValue"]>;
        options: Array<{
            value: string;
            label: string;
        }>;
    };
    receiveMessage(el: SelectHTMLElement, data: SelectInputReceiveMessageData): Promise<void>;
    subscribe(el: SelectHTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    initialize(el: SelectHTMLElement): void;
    protected _selectize(el: SelectHTMLElement, update?: boolean): SelectizeInfo | undefined;
}
export { SelectInputBinding };
export type { SelectInputReceiveMessageData };
