import $ from "jquery";
import { $escape, hasDefinedProperty, updateLabel } from "../../utils";

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
      'input[type="text"], input[type="search"], input[type="url"], input[type="email"]',
    );
    // selectize.js 0.12.4 inserts a hidden text input with an
    // id that ends in '-selectized'. The .not() selector below
    // is to prevent textInputBinding from accidentally picking up
    // this hidden element as a shiny input (#2396)
    //
    // Inputs also now have .shiny-input-text class
    return $inputs.not('input[type="text"][id$="-selectized"]');
  }

  getId(el: TextHTMLElement): string {
    return super.getId(el) || el.name;
    // return InputBinding.prototype.getId.call(this, el) || el.name;
  }

  getValue(el: TextHTMLElement): unknown {
    throw "not implemented";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }
  setValue(el: TextHTMLElement, value: unknown): void {
    throw "not implemented";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
    value; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }

  subscribe(el: TextHTMLElement, callback: (x: boolean) => void): void {
    const $el = $(el);
    const updateOn = $el.data("update-on") || "change";

    if (updateOn === "change") {
      $el.on(
        "keyup.textInputBinding input.textInputBinding",
        // event: Event
        function () {
          callback(true);
        },
      );
    } else if (updateOn === "blur") {
      $el.on("blur.textInputBinding", function () {
        callback(false);
      });
      $el.on("keydown.textInputBinding", function (event: JQuery.Event) {
        if (event.key !== "Enter") return;
        if ($el.is("textarea")) {
          if (!(event.ctrlKey || event.metaKey)) return;
        }
        callback(false);
      });
    }

    $el.on("change.textInputBinding", function () {
      if (updateOn === "blur" && $el.is(":focus")) {
        return;
      }
      callback(false);
    });
  }

  unsubscribe(el: TextHTMLElement): void {
    $(el).off(".textInputBinding");
  }

  receiveMessage(el: TextHTMLElement, data: unknown): void {
    throw "not implemented";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
    data; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }

  getState(el: TextHTMLElement): unknown {
    throw "not implemented";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }

  getRatePolicy(el: HTMLElement): { policy: "debounce"; delay: 250 } {
    return {
      policy: "debounce",
      delay: 250,
    };
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
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

  async receiveMessage(
    el: TextHTMLElement,
    data: TextReceiveMessageData,
  ): Promise<void> {
    if (hasDefinedProperty(data, "value")) this.setValue(el, data.value);

    await updateLabel(data.label, getLabelNode(el));

    if (hasDefinedProperty(data, "placeholder")) {
      // @ts-expect-error; data.value is currently a never type
      el.placeholder = data.placeholder;
    }

    $(el).trigger("change");
  }
}

export { TextInputBinding, TextInputBindingBase };
export type { TextHTMLElement, TextReceiveMessageData };
