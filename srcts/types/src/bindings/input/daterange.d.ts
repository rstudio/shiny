import { formatDateUTC } from "../../utils";
import { DateInputBindingBase } from "./date";
type DateRangeReceiveMessageData = {
    label: string;
    min?: Date;
    max?: Date;
    value?: {
        start?: Date;
        end?: Date;
    };
};
declare class DateRangeInputBinding extends DateInputBindingBase {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getValue(el: HTMLElement): [string, string];
    setValue(el: HTMLElement, value: {
        start?: Date;
        end?: Date;
    }): void;
    getState(el: HTMLElement): {
        label: string;
        value: [string, string];
        valueString: [string, string];
        min: ReturnType<typeof formatDateUTC> | null;
        max: ReturnType<typeof formatDateUTC> | null;
        weekstart: string;
        format: string;
        language: string;
        startview: string;
    };
    receiveMessage(el: HTMLElement, data: DateRangeReceiveMessageData): Promise<void>;
    initialize(el: HTMLElement): void;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
}
export { DateRangeInputBinding };
export type { DateRangeReceiveMessageData };
