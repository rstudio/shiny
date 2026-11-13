import $ from "jquery";

import { TextInputBinding } from "./text";

// When a textarea becomes visible, update the height
const intersectObserver = new IntersectionObserver((entries) => {
  entries.forEach((entry) => {
    if (entry.isIntersecting) {
      updateHeight(entry.target as HTMLInputElement);
    }
  });
});

class TextareaInputBinding extends TextInputBinding {
  #inputHandler: EventListener = (e: Event) =>
    updateHeight(e.target as HTMLInputElement);

  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs now also have the .shiny-input-textarea class
    return $(scope).find("textarea");
  }

  initialize(el: HTMLInputElement): void {
    super.initialize(el);
    updateHeight(el);
  }

  setValue(el: HTMLInputElement, value: string): void {
    super.setValue(el, value);
    updateHeight(el);
  }

  subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void {
    super.subscribe(el, callback);

    el.addEventListener("input", this.#inputHandler);
    intersectObserver.observe(el);
  }

  unsubscribe(el: HTMLInputElement): void {
    super.unsubscribe(el);

    el.removeEventListener("input", this.#inputHandler);
    intersectObserver.unobserve(el);
  }
}

function updateHeight(el: HTMLInputElement) {
  if (!el.classList.contains("textarea-autoresize")) {
    return;
  }
  if (el.scrollHeight == 0) {
    return;
  }
  el.style.height = "auto";
  el.style.height = el.scrollHeight + "px";
}

export { TextareaInputBinding };
