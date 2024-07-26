import { InputBinding } from "./inputBinding";
type ActionButtonReceiveMessageData = {
    label?: string;
    icon?: string | [];
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
    receiveMessage(el: HTMLElement, data: ActionButtonReceiveMessageData): void;
    unsubscribe(el: HTMLElement): void;
}
export { ActionButtonInputBinding };
export type { ActionButtonReceiveMessageData };
