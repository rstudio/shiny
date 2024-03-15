import { InputBinding, OutputBinding } from "../bindings";
import { resetBrush } from "../imageutils/resetBrush";
import { $escape, compareVersion } from "../utils";
import { showNotification, removeNotification } from "./notifications";
import { showModal, removeModal } from "./modal";
import { showReconnectDialog, hideReconnectDialog } from "./reconnectDialog";
import { renderContentAsync, renderContent, renderDependenciesAsync, renderDependencies, renderHtmlAsync, renderHtml } from "./render";
import type { shinyBindAll, shinyForgetLastInputValue, shinySetInputValue, shinyInitializeInputs, shinyUnbindAll } from "./initedMethods";
import type { Handler, ShinyApp } from "./shinyapp";
import { addCustomMessageHandler } from "./shinyapp";
import { initInputBindings } from "../bindings/input";
import { initOutputBindings } from "../bindings/output";
interface Shiny {
    version: string;
    $escape: typeof $escape;
    compareVersion: typeof compareVersion;
    inputBindings: ReturnType<typeof initInputBindings>["inputBindings"];
    InputBinding: typeof InputBinding;
    outputBindings: ReturnType<typeof initOutputBindings>["outputBindings"];
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
    oncustommessage?: Handler;
    /**
     * Method to check if Shiny is running in development mode. By packaging as a
     * method, we can we can avoid needing to look for the `__SHINY_DEV_MODE__`
     * variable in the global scope.
     * @returns `true` if Shiny is running in development mode, `false` otherwise.
     */
    inDevMode: () => boolean;
}
declare let windowShiny: Shiny;
declare function setShiny(windowShiny_: Shiny): void;
export { windowShiny, setShiny };
export type { Shiny };
