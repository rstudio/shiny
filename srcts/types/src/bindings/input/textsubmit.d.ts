import type { EventPriority } from "../../inputPolicies/inputPolicy";
import { InputBinding } from "./inputBinding";
type TextHTMLElement = HTMLInputElement;
declare class TextSubmitInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: TextHTMLElement): string;
    setValue(el: TextHTMLElement, value: string): void;
    subscribe(el: TextHTMLElement, callback: (x: EventPriority | boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
}
export { TextSubmitInputBinding };
