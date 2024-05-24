import { InputBinding, OutputBinding } from "../bindings";
import type { BindingRegistry } from "../bindings/registry";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, compareVersion } from "../utils";
import type { shinyBindAll, shinyForgetLastInputValue, shinyInitializeInputs, shinySetInputValue, shinyUnbindAll } from "./initedMethods";
import { removeModal, showModal } from "./modal";
import { removeNotification, showNotification } from "./notifications";
import { hideReconnectDialog, showReconnectDialog } from "./reconnectDialog";
import { renderContent, renderContentAsync, renderDependencies, renderDependenciesAsync, renderHtml, renderHtmlAsync } from "./render";
import type { Handler, ShinyApp } from "./shinyapp";
import { addCustomMessageHandler } from "./shinyapp";
declare class ShinyClass {
    version: string;
    $escape: typeof $escape;
    compareVersion: typeof compareVersion;
    inputBindings: BindingRegistry<InputBinding>;
    InputBinding: typeof InputBinding;
    outputBindings: BindingRegistry<OutputBinding>;
    OutputBinding: typeof OutputBinding;
    resetBrush: typeof resetBrush;
    notifications: {
        show: typeof showNotification;
        remove: typeof removeNotification;
    };
    modal: {
        show: typeof showModal;
        remove: typeof removeModal;
    };
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
    oncustommessage?: Handler;
    constructor();
    /**
     * Method to check if Shiny is running in development mode. By packaging as a
     * method, we can we can avoid needing to look for the `__SHINY_DEV_MODE__`
     * variable in the global scope.
     * @returns `true` if Shiny is running in development mode, `false` otherwise.
     */
    inDevMode(): boolean;
}
export { ShinyClass };
