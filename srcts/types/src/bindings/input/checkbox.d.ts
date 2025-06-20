import type { HtmlDep } from "../../shiny/render";
import { InputBinding } from "./inputBinding";
type CheckedHTMLElement = HTMLInputElement;
type CheckboxChecked = CheckedHTMLElement["checked"];
type CheckboxReceiveMessageData = {
    value?: CheckboxChecked;
    label?: {
        html: string;
        deps: HtmlDep[];
    };
};
declare class CheckboxInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: CheckedHTMLElement): CheckboxChecked;
    setValue(el: CheckedHTMLElement, value: CheckboxChecked): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    getState(el: CheckedHTMLElement): {
        label: string;
        value: CheckboxChecked;
    };
    receiveMessage(el: CheckedHTMLElement, data: CheckboxReceiveMessageData): Promise<void>;
}
export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
