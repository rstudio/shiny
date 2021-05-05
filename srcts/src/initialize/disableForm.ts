import $ from "jquery";

function disableFormSubmission(): void {
  // disable form submissions
  $(document).on("submit", "form:not([action])", function (e) {
    e.preventDefault();
  });
}

export { disableFormSubmission };
