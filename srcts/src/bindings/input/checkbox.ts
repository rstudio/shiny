import $ from "jquery";
import { InputBinding } from "./InputBinding";
import { hasOwnProperty } from "../../utils";

type CheckedHTMLElement = HTMLInputElement;

type CheckboxReceiveMessageData = { value?: any; label?: string };

class CheckboxInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find('input[type="checkbox"]');
  }
  getValue(el: CheckedHTMLElement): any {
    return el.checked;
  }
  setValue(el: CheckedHTMLElement, value: boolean): void {
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
  getState(el: CheckedHTMLElement): { label: string; value: any } {
    return {
      label: $(el).parent().find("span").text(),
      value: el.checked,
    };
  }
  receiveMessage(
    el: CheckedHTMLElement,
    data: CheckboxReceiveMessageData
  ): void {
    if (hasOwnProperty(data, "value")) el.checked = data.value;

    // checkboxInput()'s label works different from other
    // input labels...the label container should always exist
    if (hasOwnProperty(data, "label"))
      $(el).parent().find("span").text(data.label);

    $(el).trigger("change");
  }
}

export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
