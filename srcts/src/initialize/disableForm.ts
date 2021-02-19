import { $ } from "../jquery";

function disable_form(): void {
  // disable form submissions
  $(document).on("submit", "form:not([action])", function (e) {
    e.preventDefault();
  });
}

export { disable_form };
