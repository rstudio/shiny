import $ from "jquery";

import { TextInputBinding } from "./text";
import { filterBindingMatchesIfStrict } from "./_filterBindingMatches";

class TextareaInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    const matches = $(scope).find("textarea");
    return filterBindingMatchesIfStrict(matches, "shiny-input-textarea");
  }
}

export { TextareaInputBinding };
