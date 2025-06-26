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

// Needs to mirror shiny:::icon_separator()'s markup.
const separatorClass = "shiny-icon-separator";
const separatorHTML = `<span class='${separatorClass}'></span>`;

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
      let label = this._getLabel(el);
      let icon = this._getIcon(el);
      const deps: HtmlDep[] = [];

      if (hasDefinedProperty(data, "label")) {
        label = data.label.html;
        deps.push(...data.label.deps);
      }

      if (hasDefinedProperty(data, "icon")) {
        icon = data.icon.html;
        deps.push(...data.icon.deps);
      }

      if (icon.trim()) {
        icon = icon + separatorHTML;
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

  // All the contents *after* the icon/label separator are considered the label.
  // If no separator is found, the entire contents are considered the label.
  private _getLabel(el: HTMLElement): string {
    const idx = this._findSeparatorIndex(el);

    return Array.from(el.childNodes)
      .slice(idx + 1)
      .map((node) =>
        node instanceof Element ? node.outerHTML : node.textContent
      )
      .join("");
  }

  // All the contents *before* the icon/label separator are considered the icon.
  // If no separator is found, an icon isn't present (i.e., empty string).
  private _getIcon(el: HTMLElement): string {
    const idx = this._findSeparatorIndex(el);
    if (idx === -1) {
      return "";
    }

    // Don't include the separator in this result (we'll add it later)
    return Array.from(el.childNodes)
      .slice(0, idx)
      .map((node) =>
        node instanceof Element ? node.outerHTML : node.textContent
      )
      .join("");
  }

  // Find the index of the separator element in the child nodes.
  private _findSeparatorIndex(el: HTMLElement): number {
    const separator = el.querySelector(`.${separatorClass}`);
    return separator ? Array.from(el.childNodes).indexOf(separator) : -1;
  }
}

// TODO-barret should this be put in the init methods?
$(document).on("click", "a.action-button", function (e) {
  e.preventDefault();
});

export { ActionButtonInputBinding };
export type { ActionButtonReceiveMessageData };
