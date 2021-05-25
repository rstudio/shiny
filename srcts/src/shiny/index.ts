import $ from "jquery";

import {
  inputBindings,
  InputBinding,
  outputBindings,
  OutputBinding,
} from "../bindings";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, compareVersion } from "../utils";
import {
  show as showNotification,
  remove as removeNotification,
} from "./notifications";
import { show as showModal, remove as removeModal } from "./modal";
import { showReconnectDialog, hideReconnectDialog } from "./reconnectDialog";
import { renderContent, renderDependencies, renderHtml } from "./render";
import {
  initShiny,
  shinyBindAll,
  shinyForgetLastInputValue,
  shinySetInputValue,
  shinyInitializeInputs,
  shinyUnbindAll,
} from "./init";
import { HandlerType, ShinyApp } from "./shinyapp";

interface ShinyType {
  version: string;
  $escape: typeof $escape;
  compareVersion: typeof compareVersion;
  inputBindings: typeof inputBindings;
  InputBinding: typeof InputBinding;
  outputBindings: typeof outputBindings;
  OutputBinding: typeof OutputBinding;
  resetBrush: typeof resetBrush;
  notifications: {
    show: typeof showNotification;
    remove: typeof removeNotification;
  };
  modal: { show: typeof showModal; remove: typeof removeModal };
  createSocket?: () => WebSocket;
  showReconnectDialog: typeof showReconnectDialog;
  hideReconnectDialog: typeof hideReconnectDialog;
  renderDependencies: typeof renderDependencies;
  renderContent: typeof renderContent;
  renderHtml: typeof renderHtml;
  user: string;
  shinyapp?: ShinyApp;
  progressHandlers?: ShinyApp["progressHandlers"];
  addCustomMessageHandler?: ShinyApp["addCustomMessageHandler"];
  setInputValue?: typeof shinySetInputValue;
  onInputChange?: typeof shinySetInputValue;
  forgetLastInputValue?: typeof shinyForgetLastInputValue;
  bindAll?: typeof shinyBindAll;
  unbindAll?: typeof shinyUnbindAll;
  initializeInputs?: typeof shinyInitializeInputs;

  // Eventually deprecate
  // For old-style custom messages - should deprecate and migrate to new
  oncustommessage?: HandlerType;
}

let Shiny: ShinyType;

function setShiny(Shiny_: ShinyType): void {
  Shiny = Shiny_;

  // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
  // During testing, the `Shiny.version` will be `"development"`
  Shiny.version = process.env.SHINY_VERSION || "development";

  Shiny.$escape = $escape;
  Shiny.compareVersion = compareVersion;
  Shiny.inputBindings = inputBindings;
  Shiny.outputBindings = outputBindings;
  Shiny.resetBrush = resetBrush;
  Shiny.notifications = { show: showNotification, remove: removeNotification };
  Shiny.modal = { show: showModal, remove: removeModal };

  // Shiny.addCustomMessageHandler;
  Shiny.showReconnectDialog = showReconnectDialog;
  Shiny.hideReconnectDialog = hideReconnectDialog;
  Shiny.renderDependencies = renderDependencies;
  Shiny.renderContent = renderContent;
  Shiny.renderHtml = renderHtml;

  $(function () {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(function () {
      initShiny(Shiny);
    }, 1);
  });
}

export { Shiny, setShiny };

export type { ShinyType };
