import type { TextHTMLElement } from "./text";
import { TextInputBindingBase } from "./text";
declare type TimeFormatter = (fmt: string, dt: Date) => string;
declare type SliderReceiveMessageData = {
    label: string;
    value?: Array<string | number> | string | number;
    min?: number;
    max?: number;
    step?: number;
};
declare global {
    interface Window {
        strftime: {
            utc: () => TimeFormatter;
            timezone: (timezone: string) => TimeFormatter;
        } & TimeFormatter;
    }
}
declare class SliderInputBinding extends TextInputBindingBase {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string | false;
    getValue(el: TextHTMLElement): number | string | [number | string, number | string];
    setValue(el: HTMLElement, value: number | string | [number | string, number | string]): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    receiveMessage(el: HTMLElement, data: SliderReceiveMessageData): void;
    getRatePolicy(el: HTMLElement): {
        policy: "debounce";
        delay: 250;
    };
    getState(el: HTMLInputElement): void;
    initialize(el: HTMLElement): void;
}
export { SliderInputBinding };
export type { SliderReceiveMessageData };
