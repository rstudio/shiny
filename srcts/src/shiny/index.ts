import $ from "jquery";

import { InputBinding, OutputBinding } from "../bindings";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, compareVersion } from "../utils";
import { showNotification, removeNotification } from "./notifications";
import { showModal, removeModal } from "./modal";
import { showReconnectDialog, hideReconnectDialog } from "./reconnectDialog";
import {
  renderContentAsync,
  renderContent,
  renderDependenciesAsync,
  renderDependencies,
  renderHtmlAsync,
  renderHtml,
} from "./render";
import { initShiny } from "./init";
import type {
  shinyBindAll,
  shinyForgetLastInputValue,
  shinySetInputValue,
  shinyInitializeInputs,
  shinyUnbindAll,
} from "./initedMethods";
import { setFileInputBinding } from "./initedMethods";
import type { Handler, ShinyApp } from "./shinyapp";
import { addCustomMessageHandler } from "./shinyapp";
import { initInputBindings } from "../bindings/input";
import { initOutputBindings } from "../bindings/output";
import { showErrorInClientConsole } from "../components/errorConsole";

interface Shiny {
  version: string;
  $escape: typeof $escape;
  compareVersion: typeof compareVersion;
  inputBindings: ReturnType<typeof initInputBindings>["inputBindings"];
  // eslint-disable-next-line @typescript-eslint/naming-convention
  InputBinding: typeof InputBinding;
  outputBindings: ReturnType<typeof initOutputBindings>["outputBindings"];
  // eslint-disable-next-line @typescript-eslint/naming-convention
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
  renderDependenciesAsync: typeof renderDependenciesAsync;
  renderDependencies: typeof renderDependencies;
  renderContentAsync: typeof renderContentAsync;
  renderContent: typeof renderContent;
  renderHtmlAsync: typeof renderHtmlAsync;
  renderHtml: typeof renderHtml;
  user: string;
  progressHandlers?: ShinyApp["progressHandlers"];
  addCustomMessageHandler: typeof addCustomMessageHandler;
  shinyapp?: ShinyApp;
  setInputValue?: typeof shinySetInputValue;
  onInputChange?: typeof shinySetInputValue;
  forgetLastInputValue?: typeof shinyForgetLastInputValue;
  bindAll?: typeof shinyBindAll;
  unbindAll?: typeof shinyUnbindAll;
  initializeInputs?: typeof shinyInitializeInputs;

  // Eventually deprecate
  // For old-style custom messages - should deprecate and migrate to new
  oncustommessage?: Handler;

  /**
   * Method to check if Shiny is running in development mode. By packaging as a
   * method, we can we can avoid needing to look for the `__SHINY_DEV_MODE__`
   * variable in the global scope.
   * @returns `true` if Shiny is running in development mode, `false` otherwise.
   */
  inDevMode: () => boolean;
}

let windowShiny: Shiny;

function setShiny(windowShiny_: Shiny): void {
  windowShiny = windowShiny_;

  // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
  // During testing, the `Shiny.version` will be `"development"`
  windowShiny.version = process.env.SHINY_VERSION || "development";

  const { inputBindings, fileInputBinding } = initInputBindings();
  const { outputBindings } = initOutputBindings();

  // set variable to be retrieved later
  setFileInputBinding(fileInputBinding);

  windowShiny.$escape = $escape;
  windowShiny.compareVersion = compareVersion;
  windowShiny.inputBindings = inputBindings;
  windowShiny.InputBinding = InputBinding;
  windowShiny.outputBindings = outputBindings;
  windowShiny.OutputBinding = OutputBinding;
  windowShiny.resetBrush = resetBrush;
  windowShiny.notifications = {
    show: showNotification,
    remove: removeNotification,
  };
  windowShiny.modal = { show: showModal, remove: removeModal };

  windowShiny.addCustomMessageHandler = addCustomMessageHandler;
  windowShiny.showReconnectDialog = showReconnectDialog;
  windowShiny.hideReconnectDialog = hideReconnectDialog;
  windowShiny.renderDependenciesAsync = renderDependenciesAsync;
  windowShiny.renderDependencies = renderDependencies;
  windowShiny.renderContentAsync = renderContentAsync;
  windowShiny.renderContent = renderContent;
  windowShiny.renderHtmlAsync = renderHtmlAsync;
  windowShiny.renderHtml = renderHtml;

  windowShiny.inDevMode = () => {
    if ("__SHINY_DEV_MODE__" in window)
      return Boolean(window.__SHINY_DEV_MODE__);

    return false;
  };

  $(function () {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(async function () {
      try {
        await initShiny(windowShiny);
      } catch (e) {
        showErrorInClientConsole(e);
        throw e;
      }
    }, 1);
  });
}

export { windowShiny, setShiny };

export type { Shiny };
