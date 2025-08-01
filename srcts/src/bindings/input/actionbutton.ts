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
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
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
      },
    );
  }
  getState(el: HTMLElement): { value: number } {
    return { value: this.getValue(el) };
  }
  async receiveMessage(
    el: HTMLElement,
    data: ActionButtonReceiveMessageData
  ): Promise<void> {
    if (hasDefinedProperty(data, "icon")) {
      let iconContainer = el.querySelector<HTMLElement>(
        ":scope > .action-icon"
      );
      // If no container exists yet, create one
      if (!iconContainer) {
        iconContainer = document.createElement("span");
        iconContainer.className = "action-icon";
        el.prepend(iconContainer);
      }
      await renderContent(iconContainer, data.icon);
    }

    if (hasDefinedProperty(data, "label")) {
      let labelContainer = el.querySelector<HTMLElement>(
        ":scope > .action-label"
      );
      if (!labelContainer) {
        labelContainer = document.createElement("span");
        labelContainer.className = "action-label";
        el.appendChild(labelContainer);
      }
      await renderContent(labelContainer, data.label);
    }

    if (hasDefinedProperty(data, "disabled")) {
      if (data.disabled) {
        el.setAttribute("disabled", "");
      } else {
        el.removeAttribute("disabled");
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
