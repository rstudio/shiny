import { TextInputBinding } from "./text";
declare class PasswordInputBinding extends TextInputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string;
}
export { PasswordInputBinding };
