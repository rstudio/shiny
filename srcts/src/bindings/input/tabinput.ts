import $ from "jquery";
import { hasDefinedProperty, isBS3 } from "../../utils";
import { InputBinding } from "./inputBinding";

type TabInputReceiveMessageData = { value?: string };

function getTabName(anchor: JQuery<HTMLElement>): string {
  return anchor.attr("data-value") || anchor.text();
}

class BootstrapTabInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find("ul.nav.shiny-tab-input");
  }
  getValue(el: HTMLElement): string | null {
    // prettier-ignore
    // The BS4+ selectors may not work as is for dropdowns within dropdowns, but BS3+ dropped support for those anyway
    const anchor = isBS3()
      ? $(el).find("li:not(.dropdown).active > a")
      : $(el).find(
        ".nav-link:not(.dropdown-toggle).active, .dropdown-menu .dropdown-item.active"
      );

    if (anchor.length === 1) return getTabName(anchor);

    return null;
  }
  setValue(el: HTMLElement, value: string | undefined): void {
    let success = false;

    if (value) {
      // prettier-ignore
      // The BS4+ selectors may not work as is for dropdowns within dropdowns, but BS3+ dropped support for those anyway
      const anchors = isBS3()
        ? $(el).find("li:not(.dropdown) > a")
        : $(el).find(
          ".nav-link:not(.dropdown-toggle), .dropdown-menu .dropdown-item"
        );

      anchors.each(function () {
        if (getTabName($(this)) === value) {
          $(this).tab("show");
          success = true;
          return false; // Break out of each()
        }
        return;
      });
    }
    if (!success) {
      // This is to handle the case where nothing is selected, e.g. the last tab
      // was removed using removeTab.
      $(el).trigger("change");
    }
  }
  getState(el: HTMLElement): { value: string | null } {
    return { value: this.getValue(el) };
  }
  receiveMessage(el: HTMLElement, data: TabInputReceiveMessageData): void {
    if (hasDefinedProperty(data, "value")) this.setValue(el, data.value);
    $(el).trigger("change");
  }
  subscribe(el: HTMLElement, callback: (x: boolean) => void): void {
    $(el).on(
      "change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding",
      // event: Event
      function () {
        callback(false);
      },
    );
  }
  unsubscribe(el: HTMLElement): void {
    $(el).off(".bootstrapTabInputBinding");
  }
}

export { BootstrapTabInputBinding };
export type { TabInputReceiveMessageData };
