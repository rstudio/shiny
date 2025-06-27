import $ from "jquery";
import type { HtmlDep } from "../../shiny/render";
import { renderContent } from "../../shiny/render";
import { hasDefinedProperty } from "../../utils";
import { InputBinding } from "./inputBinding";

type ActionButtonReceiveMessageData = {
  label?: { html: string; deps: HtmlDep[] };
  icon?: { html: string; deps: HtmlDep[] };
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
  async receiveMessage(
    el: HTMLElement,
    data: ActionButtonReceiveMessageData
  ): Promise<void> {
    const $el = $(el);

    if (hasDefinedProperty(data, "label")) {
      const labelContainer = el.querySelector(".action-label") as HTMLElement;
      await renderContent(labelContainer, data.label);
    }

    if (hasDefinedProperty(data, "icon")) {
      const iconContainer = el.querySelector(".action-icon") as HTMLElement;
      await renderContent(iconContainer, data.icon);
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
