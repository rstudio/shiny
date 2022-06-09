import { OutputBinding } from "./outputBinding";
declare class TextOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: boolean | number | string): void;
}
export { TextOutputBinding };
