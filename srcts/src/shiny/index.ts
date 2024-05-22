import $ from "jquery";

import { InputBinding, OutputBinding } from "../bindings";
import { initInputBindings } from "../bindings/input";
import { initOutputBindings } from "../bindings/output";
import { showErrorInClientConsole } from "../components/errorConsole";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, compareVersion } from "../utils";
import { initShiny } from "./init";
import type {
  shinyBindAll,
  shinyForgetLastInputValue,
  shinyInitializeInputs,
  shinySetInputValue,
  shinyUnbindAll,
} from "./initedMethods";
import { setFileInputBinding } from "./initedMethods";
import { removeModal, showModal } from "./modal";
import { removeNotification, showNotification } from "./notifications";
import { hideReconnectDialog, showReconnectDialog } from "./reconnectDialog";
import {
  renderContent,
  renderContentAsync,
  renderDependencies,
  renderDependenciesAsync,
  renderHtml,
  renderHtmlAsync,
} from "./render";
import type { Handler, ShinyApp } from "./shinyapp";
import { addCustomMessageHandler } from "./shinyapp";

class Shiny {
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
  user?: string;
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

  constructor() {
    // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
    // During testing, the `Shiny.version` will be `"development"`
    this.version = process.env.SHINY_VERSION || "development";

    const { inputBindings, fileInputBinding } = initInputBindings();
    const { outputBindings } = initOutputBindings();

    setFileInputBinding(fileInputBinding);

    this.$escape = $escape;
    this.compareVersion = compareVersion;
    this.inputBindings = inputBindings;
    this.InputBinding = InputBinding;
    this.outputBindings = outputBindings;
    this.OutputBinding = OutputBinding;
    this.resetBrush = resetBrush;
    this.notifications = {
      show: showNotification,
      remove: removeNotification,
    };
    this.modal = { show: showModal, remove: removeModal };

    this.addCustomMessageHandler = addCustomMessageHandler;
    this.showReconnectDialog = showReconnectDialog;
    this.hideReconnectDialog = hideReconnectDialog;
    this.renderDependenciesAsync = renderDependenciesAsync;
    this.renderDependencies = renderDependencies;
    this.renderContentAsync = renderContentAsync;
    this.renderContent = renderContent;
    this.renderHtmlAsync = renderHtmlAsync;
    this.renderHtml = renderHtml;

    $(() => {
      // Init Shiny a little later than document ready, so user code can
      // run first (i.e. to register bindings)
      setTimeout(async () => {
        try {
          await initShiny(this);
        } catch (e) {
          showErrorInClientConsole(e);
          throw e;
        }
      }, 1);
    });
  }

  /**
   * Method to check if Shiny is running in development mode. By packaging as a
   * method, we can we can avoid needing to look for the `__SHINY_DEV_MODE__`
   * variable in the global scope.
   * @returns `true` if Shiny is running in development mode, `false` otherwise.
   */
  inDevMode(): boolean {
    if ("__SHINY_DEV_MODE__" in window)
      return Boolean(window.__SHINY_DEV_MODE__);

    return false;
  }
}

export { Shiny };
