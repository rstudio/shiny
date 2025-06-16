/// <reference types="bootstrap-datepicker" />
import { InputBinding } from "./inputBinding";
declare global {
    interface JQuery {
        bsDatepicker(methodName: "getUTCDate"): Date;
        bsDatepicker(methodName: "getStartDate"): Date | -1e9999;
        bsDatepicker(methodName: "getEndDate"): Date | 1e9999;
        bsDatepicker(methodName: string): void;
        bsDatepicker(methodName: string, params: Date | null): void;
    }
}
type DateReceiveMessageData = {
    label: string;
    min?: Date | null;
    max?: Date | null;
    value?: Date | null;
};
declare class DateInputBindingBase extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getType(el: HTMLElement): string;
    subscribe(el: HTMLElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    getRatePolicy(): {
        policy: "debounce";
        delay: 250;
    };
    setValue(el: HTMLElement, data: unknown): void;
    initialize(el: HTMLElement): void;
    protected _getLabelNode(el: HTMLElement): JQuery<HTMLElement>;
    protected _formatToString(format: {
        parts: string[];
        separators: string[];
    }): string;
    protected _setMin(el: HTMLElement, date: Date | null): void;
    protected _setMax(el: HTMLElement, date: Date | null): void;
    protected _newDate(date: Date | never | string): Date | null;
    protected _floorDateTime(date: Date): Date;
    protected _dateAsUTC(date: Date): Date;
    protected _utcDateAsLocal(date: Date): Date;
}
declare class DateInputBinding extends DateInputBindingBase {
    getValue(el: HTMLElement): string;
    setValue(el: HTMLElement, value: Date | null): void;
    getState(el: HTMLElement): {
        label: string;
        value: string | null;
        valueString: string[] | number | string;
        min: string | null;
        max: string | null;
        language: string | null;
        weekstart: number;
        format: string;
        startview: DatepickerViewModes;
    };
    receiveMessage(el: HTMLElement, data: DateReceiveMessageData): Promise<void>;
}
export { DateInputBinding, DateInputBindingBase };
export type { DateReceiveMessageData };
