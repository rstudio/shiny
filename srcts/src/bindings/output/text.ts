import $ from "jquery";
import { OutputBinding } from "./outputBinding";

class TextOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-text-output");
  }
  renderValue(el: HTMLElement, data: boolean | number | string): void {
    $(el).text(data);
  }
}

export { TextOutputBinding };
