import $ from "jquery";
import {
  $escape,
  formatDateUTC,
  hasDefinedProperty,
  parseDate,
  updateLabel,
} from "../../utils";
import type { NotUndefined } from "../../utils/extraTypes";
import { InputBinding } from "./inputBinding";

declare global {
  interface JQuery {
    // Adjustment of https://github.com/DefinitelyTyped/DefinitelyTyped/blob/1626e0bac175121ec2e9f766a770e03a91843c31/types/bootstrap-datepicker/index.d.ts#L113-L114
    bsDatepicker(methodName: "getUTCDate"): Date;
    // Infinity is not allowed as a literal return type. Using `1e9999` as a placeholder that resolves to Infinity
    // https://github.com/microsoft/TypeScript/issues/32277
    // eslint-disable-next-line @typescript-eslint/no-loss-of-precision
    bsDatepicker(methodName: "getStartDate"): Date | -1e9999;
    // eslint-disable-next-line @typescript-eslint/no-loss-of-precision
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

class DateInputBindingBase extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-date-input");
  }
  getType(el: HTMLElement): string {
    return "shiny.date";
    el;
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    // Don't update when in the middle of typing; listening on keyup or input
    // tends to send spurious values to the server, based on unpredictable
    // browser-dependant interpretation of partially-typed date strings.
    $(el).on(
      "changeDate.dateInputBinding change.dateInputBinding",
      // event: Event
      function () {
        // Send immediately when clicked
        // Or if typing, when enter pressed or focus lost
        callback(false);
      }
    );
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".dateInputBinding");
  }

  getRatePolicy(): { policy: "debounce"; delay: 250 } {
    return {
      policy: "debounce",
      delay: 250,
    };
  }

  setValue(el: HTMLElement, data: unknown): void {
    throw "not implemented";
    el;
    data;
  }
  initialize(el: HTMLElement): void {
    const $input = $(el).find("input");

    // The challenge with dates is that we want them to be at 00:00 in UTC so
    // that we can do comparisons with them. However, the Date object itself
    // does not carry timezone information, so we should call _floorDateTime()
    // on Dates as soon as possible so that we know we're always working with
    // consistent objects.

    let date = $input.data("initial-date");
    // If initial_date is null, set to current date

    if (date === undefined || date === null) {
      // Get local date, but normalized to beginning of day in UTC.
      date = this._floorDateTime(this._dateAsUTC(new Date()));
    }

    this.setValue(el, date);

    // Set the start and end dates, from min-date and max-date. These always
    // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
    // support for date-startdate and data-enddate, which use the current
    // date format.
    if ($input.data("min-date") !== undefined) {
      this._setMin($input[0], $input.data("min-date"));
    }
    if ($input.data("max-date") !== undefined) {
      this._setMax($input[0], $input.data("max-date"));
    }
  }
  protected _getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
    return $(el).find('label[for="' + $escape(el.id) + '"]');
  }
  // Given a format object from a date picker, return a string
  protected _formatToString(format: {
    parts: string[];
    separators: string[];
  }): string {
    // Format object has structure like:
    // { parts: ['mm', 'dd', 'yy'], separators: ['', '/', '/' ,''] }
    let str = "";

    let i;

    for (i = 0; i < format.parts.length; i++) {
      str += format.separators[i] + format.parts[i];
    }
    str += format.separators[i];
    return str;
  }
  // Given an unambiguous date string or a Date object, set the min (start) date.
  // null will unset. undefined will result in no change,
  protected _setMin(el: HTMLElement, date: Date | null): void {
    if (date === null) {
      $(el).bsDatepicker("setStartDate", null);
      return;
    }

    const parsedDate = this._newDate(date);

    // If date parsing fails, do nothing
    if (parsedDate === null) return;

    // (Assign back to date as a Date object)
    date = parsedDate as Date;

    if (isNaN(date.valueOf())) return;
    // Workarounds for
    // https://github.com/rstudio/shiny/issues/2335
    const curValue = $(el).bsDatepicker("getUTCDate");

    // Note that there's no `setUTCStartDate`, so we need to convert this Date.
    // It starts at 00:00 UTC, and we convert it to 00:00 in local time, which
    // is what's needed for `setStartDate`.
    $(el).bsDatepicker("setStartDate", this._utcDateAsLocal(date));

    // If the new min is greater than the current date, unset the current date.
    if (date && curValue && date.getTime() > curValue.getTime()) {
      $(el).bsDatepicker("clearDates");
    } else {
      // Setting the date needs to be done AFTER `setStartDate`, because the
      // datepicker has a bug where calling `setStartDate` will clear the date
      // internally (even though it will still be visible in the UI) when a
      // 2-digit year format is used.
      // https://github.com/eternicode/bootstrap-datepicker/issues/2010
      $(el).bsDatepicker("setUTCDate", curValue);
    }
  }
  // Given an unambiguous date string or a Date object, set the max (end) date
  // null will unset.
  protected _setMax(el: HTMLElement, date: Date | null): void {
    if (date === null) {
      $(el).bsDatepicker("setEndDate", null);
      return;
    }

    const parsedDate = this._newDate(date);

    // If date parsing fails, do nothing
    if (parsedDate === null) return;

    date = parsedDate as Date;

    if (isNaN(date.valueOf())) return;

    // Workaround for same issue as in _setMin.
    const curValue = $(el).bsDatepicker("getUTCDate");

    $(el).bsDatepicker("setEndDate", this._utcDateAsLocal(date));

    // If the new min is greater than the current date, unset the current date.
    if (date && curValue && date.getTime() < curValue.getTime()) {
      $(el).bsDatepicker("clearDates");
    } else {
      $(el).bsDatepicker("setUTCDate", curValue);
    }
  }
  // Given a date string of format yyyy-mm-dd, return a Date object with
  // that date at 12AM UTC.
  // If date is a Date object, return it unchanged.
  protected _newDate(date: Date | never | string): Date | null {
    if (date instanceof Date) return date;
    if (!date) return null;

    // Get Date object - this will be at 12AM in UTC, but may print
    // differently at the Javascript console.
    const d = parseDate(date);

    // If invalid date, return null
    if (isNaN(d.valueOf())) return null;

    return d;
  }
  // A Date can have any time during a day; this will return a new Date object
  // set to 00:00 in UTC.
  protected _floorDateTime(date: Date): Date {
    date = new Date(date.getTime());
    date.setUTCHours(0, 0, 0, 0);
    return date;
  }
  // Given a Date object, return a Date object which has the same "clock time"
  // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
  // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
  // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
  protected _dateAsUTC(date: Date): Date {
    return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
  }
  // The inverse of _dateAsUTC. This is needed to adjust time zones because
  // some bootstrap-datepicker methods only take local dates as input, and not
  // UTC.
  protected _utcDateAsLocal(date: Date): Date {
    return new Date(date.getTime() + date.getTimezoneOffset() * 60000);
  }
}

class DateInputBinding extends DateInputBindingBase {
  // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
  // format like mm/dd/yyyy)
  getValue(el: HTMLElement): string {
    const date = $(el).find("input").bsDatepicker("getUTCDate");

    return formatDateUTC(date);
  }
  // value must be an unambiguous string like '2001-01-01', or a Date object.
  setValue(el: HTMLElement, value: Date | null): void {
    // R's NA, which is null here will remove current value
    if (value === null) {
      $(el).find("input").val("").bsDatepicker("update");
      return;
    }

    const date = this._newDate(value);

    if (date === null) {
      return;
    }

    // If date is invalid, do nothing
    if (isNaN((date as Date).valueOf())) return;

    $(el).find("input").bsDatepicker("setUTCDate", date);
  }
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
  } {
    const $el = $(el);
    const $input = $el.find("input");

    let min = $input.data("datepicker").startDate;
    let max = $input.data("datepicker").endDate;

    // Stringify min and max. If min and max aren't set, they will be
    // -Infinity and Infinity; replace these with null.
    min = min === -Infinity ? null : formatDateUTC(min);
    max = max === Infinity ? null : formatDateUTC(max);

    // startViewMode is stored as a number; convert to string
    let startview = $input.data("datepicker").startViewMode;

    if (startview === 2) startview = "decade";
    else if (startview === 1) startview = "year";
    else if (startview === 0) startview = "month";

    return {
      label: this._getLabelNode(el).text(),
      value: this.getValue(el),
      valueString: $input.val() as NotUndefined<ReturnType<typeof $input.val>>,
      min: min,
      max: max,
      language: $input.data("datepicker").language,
      weekstart: $input.data("datepicker").weekStart,
      format: this._formatToString($input.data("datepicker").format),
      startview: startview,
    };
  }
  receiveMessage(el: HTMLElement, data: DateReceiveMessageData): void {
    const $input = $(el).find("input");

    updateLabel(data.label, this._getLabelNode(el));

    if (hasDefinedProperty(data, "min")) this._setMin($input[0], data.min);

    if (hasDefinedProperty(data, "max")) this._setMax($input[0], data.max);

    // Must set value only after min and max have been set. If new value is
    // outside the bounds of the previous min/max, then the result will be a
    // blank input.
    if (hasDefinedProperty(data, "value")) this.setValue(el, data.value);

    $(el).trigger("change");
  }
}

export { DateInputBinding, DateInputBindingBase };
export type { DateReceiveMessageData };
