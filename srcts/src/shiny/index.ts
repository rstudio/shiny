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

interface ShinyType {
  version: string;
  $escape: any;
  compareVersion: any;
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
  createSocket: null | (() => WebSocket);
  addCustomMessageHandler;
  progressHandlers;
  showReconnectDialog;
  hideReconnectDialog;
  user: string;
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

  Shiny.addCustomMessageHandler;
  Shiny.progressHandlers = shinyapp.progressHandlers;
  Shiny.showReconnectDialog = showReconnectDialog;
  Shiny.hideReconnectDialog = hideReconnectDialog;
}

function getCreateSocket(): ShinyType["createSocket"] {
  return Shiny.createSocket;
}

export { Shiny, setShiny, getCreateSocket };

export type { ShinyType };
