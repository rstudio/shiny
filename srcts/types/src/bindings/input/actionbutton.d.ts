import type { HtmlDep } from "../../shiny/render";
import { InputBinding } from "./inputBinding";
type ActionButtonReceiveMessageData = {
    label?: {
        html: string;
        deps: HtmlDep[];
    };
    icon?: {
        html: string;
        deps: HtmlDep[];
    };
    disabled?: boolean;
};
declare class ActionButtonInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: HTMLElement): number;
    setValue(el: HTMLElement, value: number): void;
    getType(el: HTMLElement): string;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    getState(el: HTMLElement): {
        value: number;
    };
    receiveMessage(el: HTMLElement, data: ActionButtonReceiveMessageData): Promise<void>;
    unsubscribe(el: HTMLElement): void;
    private _getIconLabel;
}
export { ActionButtonInputBinding };
export type { ActionButtonReceiveMessageData };
