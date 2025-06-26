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

// Mirror the classes in the CSS/R markup generation for action buttons.
const iconSeparatorClass = "shiny-icon-separator";
const iconSpacingClass = "shiny-icon-spacer";

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

    if (hasDefinedProperty(data, "label") || hasDefinedProperty(data, "icon")) {
      let { label, icon } = this._getIconLabel(el);
      const deps: HtmlDep[] = [];

      if (hasDefinedProperty(data, "label")) {
        label = data.label.html;
        deps.push(...data.label.deps);
      }

      if (hasDefinedProperty(data, "icon")) {
        icon = data.icon.html;
        deps.push(...data.icon.deps);
      }

      // Always add the separator when icon is present, but spacing is only needed
      // when both icon and label are present.
      if (icon.trim()) {
        const cssClass = label.trim()
          ? iconSeparatorClass + " " + iconSpacingClass
          : iconSeparatorClass;
        icon += `<span class='${cssClass}'></span>`;
      }

      await renderContent(el, { html: icon + label, deps });
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

  // Split the contents of the element into the icon and label.
  private _getIconLabel(el: HTMLElement): { icon: string; label: string } {
    const nodes = Array.from(el.childNodes);
    const nodeContents = nodes.map((node) =>
      node instanceof Element ? node.outerHTML : node.textContent
    );

    // Query the separator element
    const separator = el.querySelector(`.${iconSeparatorClass}`);

    // No separator found, so the entire contents are the label.
    if (!separator) {
      return { icon: "", label: nodeContents.join("") };
    }

    // Find the index of the separator element in the child nodes.
    const idx = nodes.indexOf(separator);
    return {
      icon: nodeContents.slice(0, idx).join(""),
      label: nodeContents.slice(idx + 1).join(""),
    };
  }
}

// TODO-barret should this be put in the init methods?
$(document).on("click", "a.action-button", function (e) {
  e.preventDefault();
});

export { ActionButtonInputBinding };
export type { ActionButtonReceiveMessageData };
