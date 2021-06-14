import $ from "jquery";
import { shinyShinyApp } from "./initedMethods";
import { showNotification } from "./notifications";

function initReactlog(): void {
  $(document).on("keydown", function (e) {
    if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || e.shiftKey || e.altKey)
      return;
    const url =
      "reactlog?w=" +
      window.escape(shinyShinyApp().config.workerId) +
      "&s=" +
      window.escape(shinyShinyApp().config.sessionId);

    window.open(url);
    e.preventDefault();
  });

  $(document).on("keydown", function (e) {
    if (
      // if not one of the key combos below
      !(
        // cmd/ctrl + fn + f4
        (
          (e.which === 115 &&
            (e.ctrlKey || e.metaKey) &&
            !e.shiftKey &&
            !e.altKey) ||
          // cmd/ctrl + shift + fn + f3
          (e.which === 114 &&
            (e.ctrlKey || e.metaKey) &&
            e.shiftKey &&
            !e.altKey)
        )
      )
    ) {
      return;
    }

    const url =
      "reactlog/mark?w=" +
      window.escape(shinyShinyApp().config.workerId) +
      "&s=" +
      window.escape(shinyShinyApp().config.sessionId);

    // send notification
    $.get(url, function (result) {
      if (result !== "marked") return;

      const html =
        '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';

      showNotification({
        html: html,
        closeButton: true,
      });
    }).fail(function () {
      // found returned error while marking, should open webpage
      window.open(url);
    });

    e.preventDefault();
  });
}

export { initReactlog };
