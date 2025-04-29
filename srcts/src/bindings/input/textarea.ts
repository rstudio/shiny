import $ from "jquery";

import { TextInputBinding } from "./text";

// When a textarea becomes visible, update the textarea height
const intersectObserver = new IntersectionObserver((entries) => {
  entries.forEach((entry) => {
    if (entry.isIntersecting) {
      updateHeight(entry.target as HTMLInputElement);
    }
  });
});

export class TextareaInputBinding extends TextInputBinding {
  #inputHandler: EventListener | null = null;

  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs now also have the .shiny-input-textarea class
    return $(scope).find("textarea");
  }

  initialize(el: HTMLInputElement): void {
    super.initialize(el);
    updateHeight(el);
  }

  subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void {
    super.subscribe(el, callback);

    this.#inputHandler = (e) => updateHeight(e.target as HTMLInputElement);
    el.addEventListener("input", this.#inputHandler);
    intersectObserver.observe(el);
  }

  unsubscribe(el: HTMLInputElement): void {
    super.unsubscribe(el);

    if (this.#inputHandler) el.removeEventListener("input", this.#inputHandler);
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
