import $ from "jquery";

import { TextInputBinding } from "./text";

class PasswordInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find('input[type="password"]');
  }

  getType(el: HTMLElement): string {
    return "shiny.password";
    el;
  }
}

export { PasswordInputBinding };
