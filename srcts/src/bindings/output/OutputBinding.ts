import $ from "jquery";
import { asArray } from "../../utils";
import type { errorsMessageValue } from "../../shiny/shinyapp";

class OutputBinding {
  name: string;

  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  find(scope: JQuery<HTMLElement> | HTMLElement): JQuery<HTMLElement> {
    throw "Not implemented";
    scope;
  }
  renderValue(el: HTMLElement, data: unknown): void {
    throw "Not implemented";
    el;
    data;
  }

  getId(el: HTMLElement): string {
    return el["data-input-id"] || el.id;
  }

  onValueChange(el: HTMLElement, data: unknown): void {
    this.clearError(el);
    this.renderValue(el, data);
  }
  onValueError(el: HTMLElement, err: errorsMessageValue): void {
    this.renderError(el, err);
  }
  renderError(el: HTMLElement, err: errorsMessageValue): void {
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
    const RECALC_CLASS = "recalculating";

    if (show) $(el).addClass(RECALC_CLASS);
    else $(el).removeClass(RECALC_CLASS);
  }
}

export { OutputBinding };
