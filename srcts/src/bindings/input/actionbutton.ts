import $ from "jquery";
import { InputBinding } from ".";

import { hasOwnProperty } from "../../utils";

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
  receiveMessage(
    el: HTMLElement,
    data: { label?: string; icon?: string }
  ): void {
    const $el = $(el);

    // retrieve current label and icon
    let label = $el.text();
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
    if (hasOwnProperty(data, "label")) label = data.label;
    if (hasOwnProperty(data, "icon")) {
      icon = data.icon;
      // if the user entered icon=character(0), remove the icon
      if (icon.length === 0) icon = "";
    }

    // produce new html
    $el.html(icon + " " + label);
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
