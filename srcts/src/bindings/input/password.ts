// shiny:jquery-allowed -- awaiting dom/* migration (see .claude/specs/2026-06-03-jquery-wrapper-design.md)
import $ from "jquery";

import { TextInputBinding } from "./text";

class PasswordInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs also have .shiny-input-password class
    return $(scope).find('input[type="password"]');
  }

  getType(el: HTMLElement): string {
    return "shiny.password";
    el; // eslint-disable-line @typescript-eslint/no-unused-expressions
  }
}

export { PasswordInputBinding };
