import { InputBinding } from "./InputBinding";
declare type TabInputReceiveMessageData = {
    value?: string;
};
declare class BootstrapTabInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: HTMLElement): string | null;
    setValue(el: HTMLElement, value: string): void;
    getState(el: HTMLElement): {
        value: string | null;
    };
    receiveMessage(el: HTMLElement, data: TabInputReceiveMessageData): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    _getTabName(anchor: JQuery<HTMLElement>): string;
}
export { BootstrapTabInputBinding };
export type { TabInputReceiveMessageData };
