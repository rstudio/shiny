import type { TextHTMLElement } from "./text";
import { TextInputBindingBase } from "./text";
type TimeFormatter = (fmt: string, dt: Date) => string;
type SliderReceiveMessageData = {
    label: string;
    value?: Array<number | string> | number | string;
    min?: number;
    max?: number;
    step?: number;
    "data-type"?: string;
    "time-format"?: string;
    timezone?: string;
};
declare global {
    interface Window {
        strftime: TimeFormatter & {
            utc: () => TimeFormatter;
            timezone: (timezone: string) => TimeFormatter;
        };
    }
}
declare class SliderInputBinding extends TextInputBindingBase {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string | null;
    getValue(el: TextHTMLElement): number | string | [number | string, number | string];
    setValue(el: HTMLElement, value: number | string | [number | string, number | string]): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    receiveMessage(el: HTMLElement, data: SliderReceiveMessageData): Promise<void>;
    getRatePolicy(el: HTMLElement): {
        policy: "debounce";
        delay: 250;
    };
    getState(el: HTMLInputElement): void;
    initialize(el: HTMLElement): void;
}
export { SliderInputBinding };
export type { SliderReceiveMessageData };
