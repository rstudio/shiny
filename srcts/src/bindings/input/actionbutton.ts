import $ from "jquery";
import { hasDefinedProperty } from "../../utils";
import { InputBinding } from "./inputBinding";

type ActionButtonReceiveMessageData = {
  label?: string;
  icon?: string | [];
  disabled?: boolean;
};

class ActionButtonInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".action-button");
  }
  getValue(el: HTMLElement): number {
    return $(el).data("val") || 0;
  }
  setValue(el: HTMLElement, value: number): void {
    $(el).data("val", value);
  }
  getType(el: HTMLElement): string {
    return "shiny.action";
    el;
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    $(el).on(
      "click.actionButtonInputBinding",
      // e: Event
      function () {
        const $el = $(this);
        const val = $el.data("val") || 0;

        $el.data("val", val + 1);

        callback(false);
      }
    );
  }
  getState(el: HTMLElement): { value: number } {
    return { value: this.getValue(el) };
  }
  receiveMessage(el: HTMLElement, data: ActionButtonReceiveMessageData): void {
    const $el = $(el);

    if (hasDefinedProperty(data, "label") || hasDefinedProperty(data, "icon")) {
      // retrieve current label and icon
      let label: string = $el.text();
      let icon = "";

      // to check (and store) the previous icon, we look for a $el child
      // object that has an i tag, and some (any) class (this prevents
      // italicized text - which has an i tag but, usually, no class -
      // from being mistakenly selected)
      if ($el.find("i[class]").length > 0) {
        const iconHtml = $el.find("i[class]")[0];

        if (iconHtml === $el.children()[0]) {
          // another check for robustness
          icon = $(iconHtml).prop("outerHTML");
        }
      }

      // update the requested properties
      if (hasDefinedProperty(data, "label")) {
        label = data.label;
      }
      if (hasDefinedProperty(data, "icon")) {
        // `data.icon` can be an [] if user gave `character(0)`.
        icon = Array.isArray(data.icon) ? "" : data.icon ?? "";
      }

      // produce new html
      $el.html(icon + " " + label);
    }

    if (hasDefinedProperty(data, "disabled")) {
      if (data.disabled) {
        $el.attr("disabled", "");
      } else {
        $el.attr("disabled", null);
      }
    }
  }

  unsubscribe(el: HTMLElement): void {
    $(el).off(".actionButtonInputBinding");
  }
}

// TODO-barret should this be put in the init methods?
$(document).on("click", "a.action-button", function (e) {
  e.preventDefault();
});

export { ActionButtonInputBinding };
export type { ActionButtonReceiveMessageData };
