import $ from "jquery";
import { NameValueHTMLElement } from ".";
import { $escape, hasOwnProperty, updateLabel } from "../../utils";
import { TextInputBinding } from "./text";

interface NumberHTMLElement extends NameValueHTMLElement {
  min?: any;
  max?: any;
  step?: any;
}

class NumberInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find('input[type="number"]');
  }

  getValue(el: HTMLElement): string | number | string[] {
    const numberVal = $(el).val();

    if (typeof numberVal == "string") {
      if (/^\s*$/.test(numberVal))
        // Return null if all whitespace
        return null;
    }

    if (typeof numberVal == "number") {
      if (!isNaN(numberVal))
        // If valid Javascript number string, coerce to number
        return +numberVal;
    }

    return numberVal; // If other string like "1e6", send it unchanged
  }
  setValue(el: NumberHTMLElement, value: any): void {
    el.value = value;
  }
  getType(el: NumberHTMLElement): string {
    return "shiny.number";
    el;
  }
  receiveMessage(
    el: NumberHTMLElement,
    data: { label: string; value?: any; min?: any; max?: any; step?: any }
  ): void {
    if (hasOwnProperty(data, "value")) el.value = data.value;
    if (hasOwnProperty(data, "min")) el.min = data.min;
    if (hasOwnProperty(data, "max")) el.max = data.max;
    if (hasOwnProperty(data, "step")) el.step = data.step;

    updateLabel(data.label, this._getLabelNode(el));

    $(el).trigger("change");
  }

  getState(el: NumberHTMLElement): any {
    return {
      label: this._getLabelNode(el).text(),
      value: this.getValue(el),
      min: Number(el.min),
      max: Number(el.max),
      step: Number(el.step),
    };
  }
  _getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
    return $(el)
      .parent()
      .find('label[for="' + $escape(el.id) + '"]');
  }
}

export { NumberInputBinding };
