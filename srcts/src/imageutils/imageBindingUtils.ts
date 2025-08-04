import $ from "jquery";

export function findImageOutputs(
  scope: HTMLElement = document.documentElement,
): JQuery<HTMLElement> {
  return $(scope).find(".shiny-image-output, .shiny-plot-output");
}
