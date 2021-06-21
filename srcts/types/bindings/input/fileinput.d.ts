import { InputBinding } from "./InputBinding";
declare class FileInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getId(el: HTMLInputElement): string;
    getValue(el: HTMLElement): {
        name?: string;
    } | null;
    setValue(el: HTMLElement, value: void): void;
    getType(el: HTMLElement): string;
    subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
}
export { FileInputBinding };
