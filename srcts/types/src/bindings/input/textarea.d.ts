import { TextInputBinding } from "./text";
declare class TextareaInputBinding extends TextInputBinding {
    #private;
    find(scope: HTMLElement): JQuery<HTMLElement>;
    initialize(el: HTMLInputElement): void;
    setValue(el: HTMLInputElement, value: string): void;
    subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLInputElement): void;
}
export { TextareaInputBinding };
