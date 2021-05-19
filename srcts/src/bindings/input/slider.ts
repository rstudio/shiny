import $ from "jquery";
import strftime from "strftime";
// import { NameValueHTMLElement } from ".";
import {
  formatDateUTC,
  updateLabel,
  $escape,
  hasOwnProperty,
} from "../../utils";

import { TextInputBinding } from "./text";

// interface SliderHTMLElement extends NameValueHTMLElement {
//   checked?: any;
// }

type legacySliderType = {
  canStepNext: () => boolean;
  stepNext: () => void;
  resetToStart: () => void;
};
declare global {
  interface JQuery {
    // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
    slider: () => legacySliderType;
  }
}

// Necessary to get hidden sliders to send their updated values
function forceIonSliderUpdate(slider) {
  if (slider.$cache && slider.$cache.input)
    slider.$cache.input.trigger("change");
  else console.log("Couldn't force ion slider to update");
}

type prettifyType = (num: number) => string;
function getTypePrettifyer(
  dataType: string,
  timeFormat: string,
  timezone: string
) {
  let timeFormatter: (fmt: string, dt: Date) => string;
  let prettify: prettifyType;

  if (dataType === "date") {
    timeFormatter = strftime.utc();
    prettify = function (num) {
      return timeFormatter(timeFormat, new Date(num));
    };
  } else if (dataType === "datetime") {
    if (timezone) timeFormatter = strftime.timezone(timezone);
    else timeFormatter = strftime;

    prettify = function (num) {
      return timeFormatter(timeFormat, new Date(num));
    };
  } else {
    // The default prettify function for ion.rangeSlider adds thousands
    // separators after the decimal mark, so we have our own version here.
    // (#1958)
    prettify = function (num) {
      // When executed, `this` will refer to the `IonRangeSlider.options`
      // object.
      return formatNumber(num, this.prettify_separator);
    };
  }
  return prettify;
}

class SliderInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> | Array<HTMLElement> {
    // Check if ionRangeSlider plugin is loaded
    if (!$.fn.ionRangeSlider) return [];

    return $(scope).find("input.js-range-slider");
  }

  getType(el: HTMLElement): string | boolean {
    const dataType = $(el).data("data-type");

    if (dataType === "date") return "shiny.date";
    else if (dataType === "datetime") return "shiny.datetime";
    else return false;
  }
  getValue(el): number | string | [number | string, number | string] {
    const $el = $(el);
    const result = $(el).data("ionRangeSlider").result;

    // Function for converting numeric value from slider to appropriate type.
    let convert: (val: any) => number | string;
    const dataType = $el.data("data-type");

    if (dataType === "date") {
      convert = function (val: any) {
        return formatDateUTC(new Date(Number(val)));
      };
    } else if (dataType === "datetime") {
      convert = function (val: any) {
        // Convert ms to s
        return Number(val) / 1000;
      };
    } else {
      convert = function (val: any) {
        return Number(val);
      };
    }

    if (this._numValues(el) === 2) {
      return [convert(result.from), convert(result.to)];
    } else {
      return convert(result.from);
    }
  }
  setValue(
    el: HTMLElement,
    value: number | string | [number | string, number | string]
  ): void {
    const $el = $(el);
    const slider = $el.data("ionRangeSlider");

    $el.data("immediate", true);
    try {
      if (this._numValues(el) === 2 && value instanceof Array) {
        slider.update({ from: value[0], to: value[1] });
      } else {
        slider.update({ from: value });
      }

      forceIonSliderUpdate(slider);
    } finally {
      $el.data("immediate", false);
    }
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    $(el).on("change.sliderInputBinding", function () {
      callback(!$(el).data("immediate") && !$(el).data("animating"));
    });
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".sliderInputBinding");
  }
  receiveMessage(
    el: HTMLElement,
    data: {
      label: string;
      value?: Array<string | number> | string | number;
      min?: number;
      max?: number;
      step?: number;
    }
  ): void {
    const $el = $(el);
    const slider = $el.data("ionRangeSlider");
    const msg: {
      from?: string | number;
      to?: string | number;
      min?: number;
      max?: number;
      step?: number;
      prettify?: prettifyType;
    } = {};

    if (hasOwnProperty(data, "value")) {
      if (this._numValues(el) === 2 && data.value instanceof Array) {
        msg.from = data.value[0];
        msg.to = data.value[1];
      } else {
        msg.from = data.value as string | number;
      }
    }

    const sliderFeatures = ["min", "max", "step"];

    for (let i = 0; i < sliderFeatures.length; i++) {
      const feats = sliderFeatures[i];

      if (hasOwnProperty(data, feats)) {
        msg[feats] = data[feats];
      }
    }

    updateLabel(data.label, this._getLabelNode(el));

    // (maybe) update data elements
    const domElements = ["data-type", "time-format", "timezone"];

    for (let i = 0; i < domElements.length; i++) {
      const elem = domElements[i];

      if (hasOwnProperty(data, elem)) {
        $el.data(elem, data[elem]);
      }
    }

    // retrieve latest data values
    const dataType = $el.data("data-type");
    const timeFormat = $el.data("time-format");
    const timezone = $el.data("timezone");

    msg.prettify = getTypePrettifyer(dataType, timeFormat, timezone);

    $el.data("immediate", true);
    try {
      slider.update(msg);
      forceIonSliderUpdate(slider);
    } finally {
      $el.data("immediate", false);
    }
  }
  getRatePolicy(): { policy: string; delay: number } {
    return {
      policy: "debounce",
      delay: 250,
    };
  }
  // TODO-barret Why not implemented?
  getState(el: any): void {
    // empty
    el;
  }

  initialize(el: HTMLElement): void {
    const $el = $(el);
    const dataType = $el.data("data-type");
    const timeFormat = $el.data("time-format");
    const timezone = $el.data("timezone");

    const opts = {
      prettify: getTypePrettifyer(dataType, timeFormat, timezone),
    };

    $el.ionRangeSlider(opts);
  }
  _getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
    return $(el)
      .parent()
      .find('label[for="' + $escape(el.id) + '"]');
  }
  // Number of values; 1 for single slider, 2 for range slider
  _numValues(el: HTMLElement): 1 | 2 {
    if ($(el).data("ionRangeSlider").options.type === "double") return 2;
    else return 1;
  }
}

// Format numbers for nicer output.
// formatNumber(1234567.12345)           === "1,234,567.12345"
// formatNumber(1234567.12345, ".", ",") === "1.234.567,12345"
// formatNumber(1000, " ")               === "1 000"
// formatNumber(20)                      === "20"
// formatNumber(1.2345e24)               === "1.2345e+24"
function formatNumber(
  num: number,
  thousandSep = ",",
  decimalSep = "."
): string {
  const parts = num.toString().split(".");

  // Add separators to portion before decimal mark.
  parts[0] = parts[0].replace(
    /(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g,
    "$1" + thousandSep
  );

  if (parts.length === 1) return parts[0];
  else if (parts.length === 2) return parts[0] + decimalSep + parts[1];
  else return "";
}

// TODO-barret ; this should be put in the "init" areas, correct?
$(document).on("click", ".slider-animate-button", function (evt: Event) {
  evt.preventDefault();
  const self = $(this);
  const target = $("#" + $escape(self.attr("data-target-id")));
  const startLabel = "Play";
  const stopLabel = "Pause";
  const loop =
    self.attr("data-loop") !== undefined &&
    !/^\s*false\s*$/i.test(self.attr("data-loop"));
  let animInterval = self.attr("data-interval") as number | string;

  if (isNaN(animInterval as number)) animInterval = 1500;
  else animInterval = Number(animInterval);

  if (!target.data("animTimer")) {
    let timer;

    // Separate code paths:
    // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
    // and new-style ionsliders.
    if (target.hasClass("jslider")) {
      const slider = target.slider();

      // If we're currently at the end, restart
      if (!slider.canStepNext()) slider.resetToStart();

      timer = setInterval(function () {
        if (loop && !slider.canStepNext()) {
          slider.resetToStart();
        } else {
          slider.stepNext();
          if (!loop && !slider.canStepNext()) {
            // TODO-barret replace with self.trigger("click")
            self.click(); // stop the animation
          }
        }
      }, animInterval);
    } else {
      const slider = target.data("ionRangeSlider");
      // Single sliders have slider.options.type == "single", and only the
      // `from` value is used. Double sliders have type == "double", and also
      // use the `to` value for the right handle.
      const sliderCanStep = function () {
        if (slider.options.type === "double")
          return slider.result.to < slider.result.max;
        else return slider.result.from < slider.result.max;
      };
      const sliderReset = function () {
        const val: { from: number; to?: number } = { from: slider.result.min };
        // Preserve the current spacing for double sliders

        if (slider.options.type === "double")
          val.to = val.from + (slider.result.to - slider.result.from);

        slider.update(val);
        forceIonSliderUpdate(slider);
      };
      const sliderStep = function () {
        // Don't overshoot the end
        const val: { from: number; to?: number } = {
          from: Math.min(
            slider.result.max,
            slider.result.from + slider.options.step
          ),
        };

        if (slider.options.type === "double")
          val.to = Math.min(
            slider.result.max,
            slider.result.to + slider.options.step
          );

        slider.update(val);
        forceIonSliderUpdate(slider);
      };

      // If we're currently at the end, restart
      if (!sliderCanStep()) sliderReset();

      timer = setInterval(function () {
        if (loop && !sliderCanStep()) {
          sliderReset();
        } else {
          sliderStep();
          if (!loop && !sliderCanStep()) {
            self.click(); // stop the animation
          }
        }
      }, animInterval);
    }

    target.data("animTimer", timer);
    self.attr("title", stopLabel);
    self.addClass("playing");
    target.data("animating", true);
  } else {
    clearTimeout(target.data("animTimer"));
    target.removeData("animTimer");
    self.attr("title", startLabel);
    self.removeClass("playing");
    target.removeData("animating");
  }
});

export { SliderInputBinding };
