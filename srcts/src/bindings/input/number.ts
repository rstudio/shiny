import $ from "jquery";
import { $escape, hasOwnProperty, updateLabel } from "../../utils";
import { TextInputBindingBase } from "./text";

type NumberHTMLElement = HTMLInputElement;

type NumberReceiveMessageData = {
  label: string;
  value?: string | null;
  min?: string | null;
  max?: string | null;
  step?: string | null;
};

function getLabelNode(el: NumberHTMLElement): JQuery<HTMLElement> {
  return $(el)
    .parent()
    .find('label[for="' + $escape(el.id) + '"]');
}

class NumberInputBinding extends TextInputBindingBase {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find('input[type="number"]');
  }

  getValue(el: NumberHTMLElement): string[] | number | string {
    const numberVal = $(el).val();

    if (typeof numberVal == "string") {
      if (/^\s*$/.test(numberVal))
        // Return null if all whitespace
        return null;
    }

    // If valid Javascript number string, coerce to number
    const numberValue = Number(numberVal);

    if (!isNaN(numberValue)) {
      return numberValue;
    }

    return numberVal; // If other string like "1e6", send it unchanged
  }
  setValue(el: NumberHTMLElement, value: number): void {
    el.value = "" + value;
  }
  getType(el: NumberHTMLElement): string {
    return "shiny.number";
    el;
  }
  receiveMessage(el: NumberHTMLElement, data: NumberReceiveMessageData): void {
    if (hasOwnProperty(data, "value")) el.value = data.value;
    if (hasOwnProperty(data, "min")) el.min = data.min;
    if (hasOwnProperty(data, "max")) el.max = data.max;
    if (hasOwnProperty(data, "step")) el.step = data.step;

    updateLabel(data.label, getLabelNode(el));

    $(el).trigger("change");
  }

  getState(el: NumberHTMLElement): {
    label: string;
    value: ReturnType<NumberInputBinding["getValue"]>;
    min: number;
    max: number;
    step: number;
  } {
    return {
      label: getLabelNode(el).text(),
      value: this.getValue(el),
      min: Number(el.min),
      max: Number(el.max),
      step: Number(el.step),
    };
  }
}

export { NumberInputBinding };
export type { NumberReceiveMessageData };
