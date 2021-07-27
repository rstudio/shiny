import $ from "jquery";
import { $escape, updateLabel, hasOwnProperty } from "../../utils";

import { InputBinding } from "./inputBinding";

// interface TextHTMLElement extends NameValueHTMLElement {
//   placeholder: any;
// }

type TextHTMLElement = HTMLInputElement;
type TextReceiveMessageData = {
  label: string;
  value?: TextHTMLElement["value"];
  placeholder?: TextHTMLElement["placeholder"];
};

function getLabelNode(el: HTMLElement): JQuery<HTMLElement> {
  return $(el)
    .parent()
    .find('label[for="' + $escape(el.id) + '"]');
}

class TextInputBindingBase extends InputBinding {
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

  getValue(el: TextHTMLElement): unknown {
    throw "not implemented";
    el;
  }
  setValue(el: TextHTMLElement, value: unknown): void {
    throw "not implemented";
    el;
    value;
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

  receiveMessage(el: TextHTMLElement, data: unknown): void {
    throw "not implemented";
    el;
    data;
  }

  getState(el: TextHTMLElement): unknown {
    throw "not implemented";
    el;
  }

  getRatePolicy(el: HTMLElement): { policy: "debounce"; delay: 250 } {
    return {
      policy: "debounce",
      delay: 250,
    };
    el;
  }
}

class TextInputBinding extends TextInputBindingBase {
  setValue(el: TextHTMLElement, value: string): void {
    el.value = value;
  }

  getValue(el: TextHTMLElement): TextHTMLElement["value"] {
    return el.value;
  }

  getState(el: TextHTMLElement): {
    label: string;
    value: string;
    placeholder: string;
  } {
    return {
      label: getLabelNode(el).text(),
      value: el.value,
      placeholder: el.placeholder,
    };
  }
  receiveMessage(el: TextHTMLElement, data: TextReceiveMessageData): void {
    if (hasOwnProperty(data, "value")) this.setValue(el, data.value);

    updateLabel(data.label, getLabelNode(el));

    if (hasOwnProperty(data, "placeholder")) el.placeholder = data.placeholder;

    $(el).trigger("change");
  }
}

export { TextInputBinding, TextInputBindingBase };

export type { TextHTMLElement, TextReceiveMessageData };
