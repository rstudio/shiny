import { OutputBinding } from "rstudio-shiny/srcts/types/src/bindings";

interface BoundEvent extends JQuery.TriggeredEvent {
  binding: OutputBinding;
  bindingType: string;
}

interface InvalidatedEvent extends JQuery.TriggeredEvent {
  name: string;
  binding: OutputBinding;
}

// This of this like the .shiny-busy class that shiny.js puts on the root element,
// except it's added before shiny.js is initialized, connected, etc.
// TODO: maybe shiny.js should be doing something like this?
document.documentElement.classList.add("shiny-not-yet-idle");
$(document).one("shiny:idle", function () {
  document.documentElement.classList.remove("shiny-not-yet-idle");
});

// Think of this like the .recalculating class that shiny.js when recalculating,
// except it also gets added to output bindings that haven't yet received their
// 1st value.
const BUSY_CLASS = "shiny-output-busy";

// Downloads are implemented via an output binding, but we don't want to
// show a busy indicator on the trigger.
function isDownloadBinding(binding: OutputBinding): boolean {
  return binding.name === "shiny.downloadLink";
}

// Leverage shiny.js events to add BUSY_CLASS when output bindings are either
// recalculating or calculating for the first time.
// https://shiny.posit.co/r/articles/build/js-events/
$(document).on("shiny:bound", function (x) {
  const e = x as BoundEvent;

  if (e.bindingType !== "output") return;
  if (isDownloadBinding(e.binding)) return;

  // shiny:bound is always triggered on the output element
  // https://github.com/rstudio/shiny/blob/950c63/srcts/src/shiny/bind.ts#L325
  e.target.classList.add(BUSY_CLASS);
});

$(document).on("shiny:outputinvalidated", function (x) {
  const e = x as InvalidatedEvent;

  if (isDownloadBinding(e.binding)) return;

  // shiny:outputinvalidated is always triggered on the output element
  // https://github.com/rstudio/shiny/blob/950c63/srcts/src/shiny/shinyapp.ts#L1422
  e.target.classList.add(BUSY_CLASS);
});

// shiny:value & shiny:error are usually triggered on the output element, but
// can in theory, be triggered on the document. That's probably fine since we
// probably don't need an output spinner in that case, and the UI will still get
// a page-level pulse when shiny is busy.
// https://github.com/rstudio/shiny/blob/950c63/srcts/src/shiny/shinyapp.ts#L471-L512
$(document).on("shiny:value", function (e: JQuery.TriggeredEvent) {
  e.target.classList.remove(BUSY_CLASS);
});

$(document).on("shiny:error", function (e: JQuery.TriggeredEvent) {
  e.target.classList.remove(BUSY_CLASS);
});

// TODO: remove all on shiny:disconnected?
