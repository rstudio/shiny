import $ from "jquery";

import {
  $escape,
  formatDateUTC,
  hasDefinedProperty,
  updateLabel,
} from "../../utils";
import { DateInputBindingBase } from "./date";

type DateRangeReceiveMessageData = {
  label: string;
  min?: Date;
  max?: Date;
  value?: { start?: Date; end?: Date };
};

function getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
  return $(el).find('label[for="' + $escape(el.id) + '"]');
}
class DateRangeInputBinding extends DateInputBindingBase {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-date-range-input");
  }
  // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
  // format like mm/dd/yyyy)
  getValue(el: HTMLElement): [string, string] {
    const $inputs = $(el).find("input");
    const start = $inputs.eq(0).bsDatepicker("getUTCDate");
    const end = $inputs.eq(1).bsDatepicker("getUTCDate");

    return [formatDateUTC(start), formatDateUTC(end)];
  }
  // value must be an object, with optional fields `start` and `end`. These
  // should be unambiguous strings like '2001-01-01', or Date objects.
  setValue(el: HTMLElement, value: { start?: Date; end?: Date }): void {
    if (!(value instanceof Object)) {
      return;
    }

    // Get the start and end input objects
    const $inputs = $(el).find("input");

    // If value is undefined, don't try to set
    // null will remove the current value
    if (value.start !== undefined) {
      if (value.start === null) {
        $inputs.eq(0).val("").bsDatepicker("update");
      } else {
        const start = this._newDate(value.start);

        $inputs.eq(0).bsDatepicker("setUTCDate", start);
      }
    }
    if (value.end !== undefined) {
      if (value.end === null) {
        $inputs.eq(1).val("").bsDatepicker("update");
      } else {
        const end = this._newDate(value.end);

        $inputs.eq(1).bsDatepicker("setUTCDate", end);
      }
    }
  }
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
  } {
    const $el = $(el);
    const $inputs = $el.find("input");
    const $startinput = $inputs.eq(0);
    const $endinput = $inputs.eq(1);

    // For many of the properties, assume start and end have the same values
    const min = $startinput.bsDatepicker("getStartDate");
    const max = $startinput.bsDatepicker("getEndDate");

    // Stringify min and max. If min and max aren't set, they will be
    // -Infinity and Infinity; replace these with null.
    const minStr = min === -Infinity ? null : formatDateUTC(min as Date);
    const maxStr = max === Infinity ? null : formatDateUTC(max as Date);

    // startViewMode is stored as a number; convert to string
    let startview = $startinput.data("datepicker").startView;

    if (startview === 2) startview = "decade";
    else if (startview === 1) startview = "year";
    else if (startview === 0) startview = "month";

    return {
      label: getLabelNode(el).text(),
      value: this.getValue(el),
      valueString: [$startinput.val() as string, $endinput.val() as string],
      min: minStr,
      max: maxStr,
      weekstart: $startinput.data("datepicker").weekStart,
      format: this._formatToString($startinput.data("datepicker").format),
      language: $startinput.data("datepicker").language,
      startview: startview,
    };
  }
  async receiveMessage(
    el: HTMLElement,
    data: DateRangeReceiveMessageData,
  ): Promise<void> {
    const $el = $(el);
    const $inputs = $el.find("input");
    const $startinput = $inputs.eq(0);
    const $endinput = $inputs.eq(1);

    await updateLabel(data.label, getLabelNode(el));

    if (hasDefinedProperty(data, "min")) {
      this._setMin($startinput[0], data.min!);
      this._setMin($endinput[0], data.min!);
    }

    if (hasDefinedProperty(data, "max")) {
      this._setMax($startinput[0], data.max!);
      this._setMax($endinput[0], data.max!);
    }

    // Must set value only after min and max have been set. If new value is
    // outside the bounds of the previous min/max, then the result will be a
    // blank input.
    if (hasDefinedProperty(data, "value")) {
      this.setValue(el, data.value!);
    }

    $el.trigger("change");
  }

  initialize(el: HTMLElement): void {
    const $el = $(el);
    const $inputs = $el.find("input");
    const $startinput = $inputs.eq(0);
    const $endinput = $inputs.eq(1);

    let start = $startinput.data("initial-date");
    let end = $endinput.data("initial-date");

    // If empty/null, use local date, but as UTC
    if (start === undefined || start === null)
      start = this._dateAsUTC(new Date());

    if (end === undefined || end === null) end = this._dateAsUTC(new Date());

    this.setValue(el, { start: start, end: end });

    // // Set the start and end dates, from min-date and max-date. These always
    // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
    // // support for date-startdate and data-enddate, which use the current
    // // date format.
    this._setMin($startinput[0], $startinput.data("min-date"));
    this._setMin($endinput[0], $startinput.data("min-date"));
    this._setMax($startinput[0], $endinput.data("max-date"));
    this._setMax($endinput[0], $endinput.data("max-date"));
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    // Don't update when in the middle of typing; listening on keyup or input
    // tends to send spurious values to the server, based on unpredictable
    // browser-dependant interpretation of partially-typed date strings.
    $(el).on(
      "changeDate.dateRangeInputBinding change.dateRangeInputBinding",
      // event: Event
      function () {
        // Send immediately when clicked
        // Or if typing, when enter pressed or focus lost
        callback(false);
      },
    );
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".dateRangeInputBinding");
  }
}

export { DateRangeInputBinding };
export type { DateRangeReceiveMessageData };
