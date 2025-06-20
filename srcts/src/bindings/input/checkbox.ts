import $ from "jquery";
import type { HtmlDep } from "../../shiny/render";
import { renderContent } from "../../shiny/render";
import { hasDefinedProperty } from "../../utils";
import { InputBinding } from "./inputBinding";

type CheckedHTMLElement = HTMLInputElement;

type CheckboxChecked = CheckedHTMLElement["checked"];
type CheckboxReceiveMessageData = {
  value?: CheckboxChecked;
  label?: { html: string; deps: HtmlDep[] };
};

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
  async receiveMessage(
    el: CheckedHTMLElement,
    data: CheckboxReceiveMessageData
  ): Promise<void> {
    if (hasDefinedProperty(data, "value")) {
      el.checked = data.value;
    }

    // checkboxInput()'s label works different from other
    // input labels...the label container should always exist
    if (hasDefinedProperty(data, "label")) {
      const labelSpan = $(el).parent().find("span");
      await renderContent(labelSpan, data.label);
    }

    $(el).trigger("change");
  }
}

export { CheckboxInputBinding };
export type { CheckedHTMLElement, CheckboxReceiveMessageData };
