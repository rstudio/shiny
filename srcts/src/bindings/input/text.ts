import $ from "jquery";
import { $escape, updateLabel, hasOwnProperty } from "../../utils";

import { InputBinding } from "../input";

// interface TextHTMLElement extends NameValueHTMLElement {
//   placeholder: any;
// }

type TextHTMLElement = HTMLInputElement;

class TextInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    const $inputs = $(scope).find(
      'input[type="text"], input[type="search"], input[type="url"], input[type="email"]'
    );
    // selectize.js 0.12.4 inserts a hidden text input with an
    // id that ends in '-selectized'. The .not() selector below
    // is to prevent textInputBinding from accidentally picking up
    // this hidden element as a shiny input (#2396)

    return $inputs.not('input[type="text"][id$="-selectized"]');
  }

  getId(el: TextHTMLElement): string {
    return super.getId(el) || el.name;
    // return InputBinding.prototype.getId.call(this, el) || el.name;
  }

  getValue(el: TextHTMLElement): any {
    return el.value;
  }
  setValue(el: TextHTMLElement, value): void {
    el.value = value;
  }

  subscribe(el: TextHTMLElement, callback: (x: boolean) => void): void {
    $(el).on(
      "keyup.textInputBinding input.textInputBinding",
      // event: Event
      function () {
        callback(true);
      }
    );
    $(el).on(
      "change.textInputBinding",
      // event: Event
      function () {
        callback(false);
      }
    );
  }
  unsubscribe(el: TextHTMLElement): void {
    $(el).off(".textInputBinding");
  }

  receiveMessage(
    el: TextHTMLElement,
    data: { label: string; value?: any; placeholder?: any }
  ): void {
    if (hasOwnProperty(data, "value")) this.setValue(el, data.value);

    updateLabel(data.label, this._getLabelNode(el));

    if (hasOwnProperty(data, "placeholder")) el.placeholder = data.placeholder;

    $(el).trigger("change");
  }

  getState(
    el: TextHTMLElement
  ): { label: string; value: any; placeholder: any } {
    return {
      label: this._getLabelNode(el).text(),
      value: el.value,
      placeholder: el.placeholder,
    };
  }

  getRatePolicy(el: HTMLElement): { policy: "debounce"; delay: 250 } {
    return {
      policy: "debounce",
      delay: 250,
    };
    el;
  }

  _getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
    return $(el)
      .parent()
      .find('label[for="' + $escape(el.id) + '"]');
  }
}

export { TextInputBinding };

export type { TextHTMLElement };
