import $ from "jquery";

import { TextInputBinding } from "./text";

class TextareaInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find("textarea");
  }
}

export { TextareaInputBinding };
