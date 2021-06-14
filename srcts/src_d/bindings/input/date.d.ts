/// <reference types="bootstrap-datepicker" />
import { InputBinding } from "./InputBinding";
declare global {
    interface JQuery {
        bsDatepicker(methodName: string): any;
        bsDatepicker(methodName: string, params: any): any;
    }
}
declare type DateReceiveMessageData = {
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
    _getLabelNode(el: HTMLElement): JQuery<HTMLElement>;
    _formatToString(format: {
        parts: Array<string>;
        separators: Array<string>;
    }): string;
    _setMin(el: HTMLElement, date: Date | undefined | null): void;
    _setMax(el: HTMLElement, date: Date): void;
    _newDate(date: Date | string | never): Date | void;
    _floorDateTime(date: Date): Date;
    _dateAsUTC(date: Date): Date;
    _UTCDateAsLocal(date: Date): Date;
}
declare class DateInputBinding extends DateInputBindingBase {
    getValue(el: HTMLElement): string;
    setValue(el: HTMLElement, value: Date): void;
    getState(el: HTMLElement): {
        label: string;
        value: string | null;
        valueString: string | number | string[];
        min: string | null;
        max: string | null;
        language: string | null;
        weekstart: number;
        format: string;
        startview: DatepickerViewModes;
    };
    receiveMessage(el: HTMLElement, data: DateReceiveMessageData): void;
}
export { DateInputBinding, DateInputBindingBase };
export type { DateReceiveMessageData };
