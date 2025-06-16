import { BindingRegistry } from "../registry";
import { DatatableOutputBinding } from "./datatable";
import { DownloadLinkOutputBinding } from "./downloadlink";
import { HtmlOutputBinding } from "./html";
import { imageOutputBinding } from "./image";
import { TextOutputBinding } from "./text";

import { OutputBinding } from "./outputBinding";

type InitOutputBindings = {
  outputBindings: BindingRegistry<OutputBinding>;
};
function initOutputBindings(): InitOutputBindings {
  const outputBindings = new BindingRegistry<OutputBinding>();

  outputBindings.register(new TextOutputBinding(), "shiny.textOutput");
  outputBindings.register(
    new DownloadLinkOutputBinding(),
    "shiny.downloadLink"
  );
  outputBindings.register(
    new DatatableOutputBinding(),
    "shiny.datatableOutput"
  );
  outputBindings.register(new HtmlOutputBinding(), "shiny.htmlOutput");
  outputBindings.register(imageOutputBinding, "shiny.imageOutput");

  return { outputBindings };
}

export { OutputBinding, initOutputBindings };
