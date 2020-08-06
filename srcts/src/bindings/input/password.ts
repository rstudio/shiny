import $ from "jquery";

import { TextInputBinding } from "./text";

class PasswordInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    if (exports.bindGenericInputs) {
      return $(scope).find('input[type="password"]');
    } else {
      return $(scope).find('input[type="password"].shiny-input-password');
    }
  }

  getType(el: HTMLElement): string {
    return "shiny.password";
    el;
  }
}

export { PasswordInputBinding };
