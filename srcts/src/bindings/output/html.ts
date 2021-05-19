import $ from "jquery";

import { OutputBinding } from ".";

class HtmlOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-html-output");
  }
  onValueError(el: HTMLElement, err): void {
    Shiny.unbindAll(el);
    this.renderError(el, err);
  }
  renderValue(el: HTMLElement, data): void {
    Shiny.renderContent(el, data);
  }
}

export { HtmlOutputBinding };
