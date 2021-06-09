import $ from "jquery";
import { OutputBinding } from "./OutputBinding";

class TextOutputBinding extends OutputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find(".shiny-text-output");
  }
  renderValue(el: HTMLElement, data: string | number | boolean): void {
    $(el).text(data);
  }
}

export { TextOutputBinding };
