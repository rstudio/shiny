import $ from "jquery";

import { OutputBinding } from "./outputBinding";
import { shinyUnbindAll } from "../../shiny/initedMethods";
import { renderContent } from "../../shiny/render";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";

class HtmlOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-html-output");
  }
  onValueError(el: HTMLElement, err: ErrorsMessageValue): void {
    shinyUnbindAll(el);
    this.renderError(el, err);
  }
  override async renderValue(
    el: HTMLElement,
    data: Parameters<typeof renderContent>[1]
  ): Promise<void> {
    await renderContent(el, data);
  }
}

export { HtmlOutputBinding };
