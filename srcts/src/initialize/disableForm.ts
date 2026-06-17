// shiny:jquery-allowed -- awaiting dom/* migration (see .claude/specs/2026-06-03-jquery-wrapper-design.md)
import $ from "jquery";

function disableFormSubmission(): void {
  // disable form submissions
  $(document).on("submit", "form:not([action])", function (e) {
    e.preventDefault();
  });
}

export { disableFormSubmission };
