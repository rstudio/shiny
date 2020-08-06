import $ from "jquery";

import { TextInputBinding } from "./text";

class TextareaInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    if (exports.bindGenericInputs) {
      return $(scope).find('textarea');
    } else {
      return $(scope).find('textarea.shiny-input-textarea');
    }
  }
}

export { TextareaInputBinding };
