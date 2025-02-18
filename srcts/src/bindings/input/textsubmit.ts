import $ from "jquery";
import { hasDefinedProperty, updateLabel } from "../../utils";

import type { EventPriority } from "../../inputPolicies/inputPolicy";
import { InputBinding } from "./inputBinding";

type TextHTMLElement = HTMLInputElement;
type TextSubmitReceiveMessageData = {
  value?: string;
  placeholder?: string;
  label?: string;
  submit?: boolean;
  focus?: boolean;
};

class TextSubmitInputBinding extends InputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-input-textsubmit > textarea");
  }

  // Don't read a 'proxy' input value instead of the actual value since we
  // intentionally don't want the input value until it's submitted.
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
    // Before notifying Shiny of a change, update the proxy value,
    // clear the input, and trigger an input event (for disabled state).
    function doSendValue() {
      $(el).data("val", el.value);
      el.value = "";
      el.dispatchEvent(new Event("input", { bubbles: true }));
      callback("event");
    }

    // This assumption is forced server-side
    const btn = el.nextElementSibling;
    if (!(btn instanceof HTMLButtonElement)) {
      throw new Error("No submit button found");
    }

    if (btn.classList.contains("shiny-bound-input")) {
      // If the button is a task/action button, make sure this text input value
      // is sent along with the button input value _on the same update message
      // tick_.
      //
      // This is important for a task button since its input handler updates
      // state from 'busy' to 'ready' on the next flush, but if the button value
      // gets sent before the text value, the next flush will happen before the
      // new text value gets handled. In other words, you'll get a "flash" of
      // busy state, then a pre-mature ready state.
      $(btn).on("shiny:inputchanged", doSendValue);
    } else {
      // If this is just a regular button, send the value on click
      $(btn).on("click.textSubmitInputBinding", doSendValue);
    }

    // When new input is received, update disabled state
    $(el).on("input.textSubmitInputBinding", function () {
      btn.classList.toggle("disabled", !el.value);
    });

    $(el).on(
      "keydown.textSubmitInputBinding",
      // event: JQuery.KeyboardEventObject
      function (event) {
        // If this isn't an enter key, do nothing
        if (event.key !== "Enter") {
          return;
        }

        // If the enter key is pressed with a modifier, do nothing
        if (btn.classList.contains("disabled")) {
          event.preventDefault();
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

  receiveMessage(
    el: TextHTMLElement,
    data: TextSubmitReceiveMessageData
  ): void {
    const oldValue = el.value;

    if (hasDefinedProperty(data, "value")) {
      el.value = data.value;
      el.dispatchEvent(new Event("input", { bubbles: true }));
    }

    if (hasDefinedProperty(data, "placeholder")) {
      el.placeholder = data.placeholder;
    }

    if (hasDefinedProperty(data, "label")) {
      const labEl = $(el).closest(".shiny-input-container").find("label");
      updateLabel(data.label, labEl);
    }

    if (hasDefinedProperty(data, "submit") && data.submit) {
      const btn = el.nextElementSibling;
      if (btn instanceof HTMLButtonElement) {
        btn.click();
        el.value = oldValue;
      }
    }

    if (hasDefinedProperty(data, "focus") && data.focus) {
      el.focus();
    }
  }
}

export { TextSubmitInputBinding };
