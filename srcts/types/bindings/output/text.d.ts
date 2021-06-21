import { OutputBinding } from "./outputBinding";
declare class TextOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: string | number | boolean): void;
}
export { TextOutputBinding };
