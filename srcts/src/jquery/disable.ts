import { $ } from "../external/globals";

function main(): void {
  // disable form submissions
  $(document).on("submit", "form:not([action])", function (e) {
    e.preventDefault();
  });
}

export { main };
