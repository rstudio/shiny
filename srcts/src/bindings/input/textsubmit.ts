import $ from "jquery";

import type { EventPriority } from "../../inputPolicies/inputPolicy";
import { InputBinding } from "./inputBinding";

type TextHTMLElement = HTMLInputElement;

class TextSubmitInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-input-textsubmit > textarea");
  }

  // Don't read the input value directly since we intentionally don't
  // want the input value until it's submitted.
  getValue(el: TextHTMLElement): string {
    return $(el).data("val");
  }

  setValue(el: TextHTMLElement, value: string): void {
    el.value = value;
  }

  subscribe(
    el: TextHTMLElement,
    callback: (x: EventPriority | boolean) => void
  ): void {
    const btn = el.nextElementSibling;

    // This gets verified server-side, but for sanity...
    if (!(btn instanceof HTMLButtonElement)) {
      throw new Error("No submit button found");
    }

    function doSendValue() {
      $(el).data("val", el.value);
      el.value = "";
      callback("event");
    }

    if (btn.classList.contains("shiny-bound-input")) {
      // If the button is a task/action button, make sure this value gets
      // sent along with the button value on _the same_ update tick.
      //
      // This is important for a task button since its input handler updates
      // state from 'busy' to 'ready' on the next flush, but if the button
      // gets first, then the text value, the flush will likely happen right
      // away, and thus the button will lose its 'busy' state.
      $(btn).on("shiny:inputchanged", doSendValue);
    } else {
      // If this is just a regular button, send the value on click
      $(btn).on("click.textSubmitInputBinding", doSendValue);
    }

    $(el).on(
      "keydown.textSubmitInputBinding",
      // event: JQuery.KeyboardEventObject
      function (event) {
        // If this isn't an enter key, do nothing
        if (event.key !== "Enter") {
          return;
        }

        const needsModifier = el.hasAttribute("data-needs-modifier");
        const hasModifier = event.ctrlKey || event.metaKey;

        if (needsModifier && hasModifier) {
          event.preventDefault();
          btn.click();
          return;
        }

        // If the input doesn't need a modifier, submit as long as the
        // shift key isn't pressed.
        if (!needsModifier && !event.shiftKey) {
          event.preventDefault();
          btn.click();
        }
      }
    );
  }

  unsubscribe(el: HTMLElement): void {
    $(el).off(".textSubmitInputBinding");
    const btn = el.nextElementSibling as HTMLElement;
    $(btn).off("shiny:inputchanged");
  }
}

export { TextSubmitInputBinding };
