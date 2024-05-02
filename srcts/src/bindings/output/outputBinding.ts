import $ from "jquery";
import { asArray } from "../../utils";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";

class OutputBinding {
  name!: string;

  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  find(scope: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement> {
    throw "Not implemented";
    scope;
  }
  renderValue(el: HTMLElement, data: unknown): Promise<void> | void {
    throw "Not implemented";
    el;
    data;
  }

  getId(el: HTMLElement): string {
    return el.getAttribute("data-input-id") || el.id;
  }

  async onValueChange(el: HTMLElement, data: unknown): Promise<void> {
    this.clearError(el);
    await this.renderValue(el, data);
  }
  onValueError(el: HTMLElement, err: ErrorsMessageValue): void {
    this.renderError(el, err);
  }
  renderError(el: HTMLElement, err: ErrorsMessageValue): void {
    this.clearError(el);
    if (err.message === "") {
      // not really error, but we just need to wait (e.g. action buttons)
      $(el).empty();
      return;
    }
    let errClass = "shiny-output-error";

    if (err.type !== null) {
      // use the classes of the error condition as CSS class names
      errClass =
        errClass +
        " " +
        $.map(asArray(err.type), function (type) {
          return errClass + "-" + type;
        }).join(" ");
    }
    $(el).addClass(errClass).text(err.message);
  }
  clearError(el: HTMLElement): void {
    $(el).attr("class", function (i, c) {
      return c.replace(/(^|\s)shiny-output-error\S*/g, "");
    });
  }
  showProgress(el: HTMLElement, show: boolean): void {
    const recalcClass = "recalculating";

    if (show) {
      el.classList.add(recalcClass);
    } else {
      el.classList.remove(recalcClass);
    }

    // Ideally the code below wouldn't be necessary, but Chromium has an odd
    // bug where animations sometimes don't work on pseudo-elements (i.e., ::after).
    // https://issues.chromium.org/issues/40932064
    // To work around this, we add/remove a spinner element directly to the output
    // container if necessary.
    const spinners = document.documentElement.hasAttribute(
      "data-shiny-busy-spinners"
    );
    if (!spinners) {
      return;
    }

    if (show) {
      const spinnerEl = document.createElement("div");
      spinnerEl.classList.add("shiny-output-spinner");
      el.appendChild(spinnerEl);
    } else {
      const spinnerEl = el.querySelector(":scope > .shiny-output-spinner");
      if (spinnerEl) {
        el.removeChild(spinnerEl);
      }
    }
  }
}

export { OutputBinding };
