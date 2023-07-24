import $ from "jquery";

import { TextInputBinding } from "./text";
import { filterBindingMatchesIfStrict } from "./_filterBindingMatches";

class PasswordInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    const matches = $(scope).find('input[type="password"]');
    return filterBindingMatchesIfStrict(matches, "shiny-input-password");
  }

  getType(el: HTMLElement): string {
    return "shiny.password";
    el;
  }
}

export { PasswordInputBinding };
