// shiny:jquery-allowed -- awaiting dom/* migration (see .claude/specs/2026-06-03-jquery-wrapper-design.md)
import $ from "jquery";

export function findImageOutputs(
  scope: HTMLElement = document.documentElement,
): JQuery<HTMLElement> {
  return $(scope).find(".shiny-image-output, .shiny-plot-output");
}
