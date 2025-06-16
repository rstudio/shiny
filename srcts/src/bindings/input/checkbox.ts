import $ from "jquery";
import { hasDefinedProperty } from "../../utils";
import { InputBinding } from "./inputBinding";

type CheckedHTMLElement = HTMLInputElement;

type CheckboxChecked = CheckedHTMLElement["checked"];
type CheckboxReceiveMessageData = { value?: CheckboxChecked; label?: string };

class CheckboxInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs also have .shiny-input-checkbox class
    return $(scope).find('input[type="checkbox"]');
  }
  getValue(el: CheckedHTMLElement): CheckboxChecked {
    return el.checked;
  }
  setValue(el: CheckedHTMLElement, value: CheckboxChecked): void {
    el.checked = value;
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    $(el).on("change.checkboxInputBinding", function () {
      callback(true);
    });
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".checkboxInputBinding");
  }
  getState(el: CheckedHTMLElement): { label: string; value: CheckboxChecked } {
    return {
      label: $(el).parent().find("span").text(),
      value: el.checked,
    };
  }
  receiveMessage(
    el: CheckedHTMLElement,
    data: CheckboxReceiveMessageData
  ): void {
    if (hasDefinedProperty(data, "value")) {
      el.checked = data.value;
    }

    // checkboxInput()'s label works different from other
    // input labels...the label container should always exist
    if (hasDefinedProperty(data, "label")) {
      $(el).parent().find("span").text(data.label);
    }

    $(el).trigger("change");
  }
}

export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
