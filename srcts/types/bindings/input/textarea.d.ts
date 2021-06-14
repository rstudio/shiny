import { TextInputBinding } from "./text";
declare class TextareaInputBinding extends TextInputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
}
export { TextareaInputBinding };
