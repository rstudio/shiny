import $ from "jquery";

import { TextInputBinding } from "./text";

class TextareaInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs now also have the .shiny-input-textarea class
    return $(scope).find("textarea");
  }
}

export { TextareaInputBinding };
