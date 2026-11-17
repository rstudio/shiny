import $ from "jquery";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { asArray } from "../../utils";

// What an agent should see of an output; see OutputBinding.getSnapshot()
type OutputSnapshot = {
  // Images, as `data:` (or other) URLs, in order. Replaces the <img> and
  // <canvas> elements `Shiny.drive.getOutput()` would read from the page.
  images?: string[];
  // Any JSON value, e.g. a table's rows or a chart's data
  data?: unknown;
};

class OutputBinding {
  name!: string;

  // Returns a jQuery object or element array that contains the
  // descendants of scope that match this binding
  find(scope: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement> {
    throw "Not implemented";
    scope; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }
  renderValue(el: HTMLElement, data: unknown): Promise<void> | void {
    throw "Not implemented";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
    data; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }

  /**
   * Optional: what `Shiny.drive.getOutput()` (and the WebMCP `get_output`
   * tool) should return for this output beyond its text, when reading the
   * page isn't enough, e.g. an image of a chart drawn in SVG or WebGL, or a
   * table's rows. Return `undefined` (the default) to read the page.
   */
  getSnapshot(
    el: HTMLElement,
  ): OutputSnapshot | Promise<OutputSnapshot | undefined> | undefined {
    return undefined;
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
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

    if (show) $(el).addClass(recalcClass);
    else $(el).removeClass(recalcClass);
  }
}

export { OutputBinding };
export type { OutputSnapshot };
