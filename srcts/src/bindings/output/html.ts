import $ from "jquery";

import { shinyUnbindAll } from "../../shiny/initedMethods";
import { renderContentAsync } from "../../shiny/render";
import type { ErrorsMessageValue } from "../../shiny/shinyapp";
import { OutputBinding } from "./outputBinding";

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
    data: Parameters<typeof renderContentAsync>[1],
  ): Promise<void> {
    await renderContentAsync(el, data);
  }
}

export { HtmlOutputBinding };
