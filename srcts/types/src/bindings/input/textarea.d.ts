import { TextInputBinding } from "./text";
export declare class TextareaInputBinding extends TextInputBinding {
    #private;
    find(scope: HTMLElement): JQuery<HTMLElement>;
    initialize(el: HTMLInputElement): void;
    subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLInputElement): void;
}
