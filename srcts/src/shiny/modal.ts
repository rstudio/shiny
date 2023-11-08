import $ from "jquery";
import { shinyUnbindAll } from "./initedMethods";
import type { HtmlDep } from "./render";
import { renderContentAsync, renderDependenciesAsync } from "./render";

// Show a modal dialog. This is meant to handle two types of cases: one is
// that the content is a Bootstrap modal dialog, and the other is that the
// content is non-Bootstrap. Bootstrap modals require some special handling,
// which is coded in here.
async function show({
  html = "",
  deps = [],
}: {
  html?: string;
  deps?: HtmlDep[];
} = {}): Promise<void> {
  // Normally we'd first create the modal's DOM elements, then call
  // `renderContentAsync(x, {html: html, deps: deps})`, but that has a potential
  // problem with async rendering. If we did that, then an empty modal (from
  // this function) could show up and then sit there empty while the
  // dependencies load (asynchronously), and only after all that get filled with
  // content for the modal. So instead we'll render the deps here, then render
  // the modal, then render the content in the modal.
  await renderDependenciesAsync(deps);

  // If there was an existing Bootstrap modal, then there will be a modal-
  // backdrop div that was added outside of the modal wrapper, and it must be
  // removed; otherwise there can be multiple of these divs.
  $(".modal-backdrop").remove();

  // Get existing wrapper DOM element, or create if needed.
  let $modal = $("#shiny-modal-wrapper");

  if ($modal.length === 0) {
    $modal = $('<div id="shiny-modal-wrapper"></div>');
    $(document.body).append($modal);

    // If the wrapper's content is a Bootstrap modal, then when the inner
    // modal is hidden, remove the entire thing, including wrapper.
    $modal.on("hidden.bs.modal", function (e) {
      if (e.target === $("#shiny-modal")[0]) {
        shinyUnbindAll($modal);
        $modal.remove();
      }
    });
  }

  $modal.on("keydown.shinymodal", function (e) {
    // If we're listening for Esc, don't let the event propagate. See
    // https://github.com/rstudio/shiny/issues/1453. The value of
    // data("keyboard") needs to be checked inside the handler, because at
    // the time that $modal.on() is called, the $("#shiny-modal") div doesn't
    // yet exist.
    if ($("#shiny-modal").data("keyboard") === false) return;

    if (e.keyCode === 27) {
      e.stopPropagation();
      e.preventDefault();
    }
  });

  // Set/replace contents of wrapper with html.
  await renderContentAsync($modal, { html: html });
}

function remove(): void {
  const $modal = $("#shiny-modal-wrapper");

  $modal.off("keydown.shinymodal");

  // Look for a Bootstrap modal and if present, trigger hide event. This will
  // trigger the hidden.bs.modal callback that we set in show(), which unbinds
  // and removes the element.
  if ($modal.find(".modal").length > 0) {
    $modal.find(".modal").modal("hide");
  } else {
    // If not a Bootstrap modal dialog, simply unbind and remove it.
    shinyUnbindAll($modal);
    $modal.remove();
  }
}

export { show as showModal, remove as removeModal };
