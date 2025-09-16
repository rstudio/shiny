import type { ShinyClass } from ".";
import type { FileInputBinding } from "../bindings/input/fileinput";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies";
import type { BindScope } from "./bind";
import type { Handler, ShinyApp } from "./shinyapp";

let fullShinyObj: FullShinyDef;

// TODO-future; It would be nice to have a way to export this type value instead of / in addition to `Shiny`
type FullShinyDef = Required<
  Pick<
    ShinyClass,
    | "bindAll"
    | "forgetLastInputValue"
    | "initializeInputs"
    | "oncustommessage"
    | "setInputValue"
    | "shinyapp"
    | "unbindAll"
    | "user"
  >
> &
  ShinyClass;

function setShinyObj(shiny: ShinyClass): void {
  fullShinyObj = shiny as FullShinyDef;
}

function validateShinyHasBeenSet(): FullShinyDef {
  if (typeof fullShinyObj === "undefined") {
    throw "Shiny has not finish initialization yet. Please wait for the 'shiny-initialized' event.";
  }
  return fullShinyObj;
}

//// 2021/03: TypeScript Conversion note
// These methods are here due to the delayed initialization of `Shiny.shinyapp`. I
// In theory, there could be multiple instances of `shinyapp`. In practice (and implementation), this is not possible and is a 1:1 coupling with `window.Shiny`.
// To avoid calls to a large Shiny object, helper methods are created to wrap around calling the fully instantiated window.Shiny value.
// TODO-barret; Why is `initShiny()` delayed? Is this to allow users to shim in some code? Why can't it be defined in the init method (maybe w/ an extra trigger call?)
function shinySetInputValue(
  name: string,
  value: unknown,
  opts?: { priority?: EventPriority },
): void {
  validateShinyHasBeenSet().setInputValue(name, value, opts);
}
function shinyShinyApp(): ShinyApp {
  return validateShinyHasBeenSet().shinyapp;
}
function setShinyUser(user: string): void {
  validateShinyHasBeenSet().user = user;
}
function shinyForgetLastInputValue(name: string): void {
  validateShinyHasBeenSet().forgetLastInputValue(name);
}
async function shinyBindAll(scope: BindScope): Promise<void> {
  await validateShinyHasBeenSet().bindAll(scope);
}
function shinyUnbindAll(scope: BindScope, includeSelf = false): void {
  validateShinyHasBeenSet().unbindAll(scope, includeSelf);
}
function shinyInitializeInputs(scope: BindScope): void {
  validateShinyHasBeenSet().initializeInputs(scope);
}

async function shinyAppBindOutput(
  id: string,
  binding: OutputBindingAdapter,
): Promise<void> {
  await shinyShinyApp().bindOutput(id, binding);
}

function shinyAppUnbindOutput(
  id: string,
  binding: OutputBindingAdapter,
): boolean {
  return shinyShinyApp().unbindOutput(id, binding);
}

function getShinyOnCustomMessage(): Handler | null {
  return validateShinyHasBeenSet().oncustommessage;
}

let fileInputBinding: FileInputBinding;

function getFileInputBinding(): FileInputBinding {
  return fileInputBinding;
}
function setFileInputBinding(fileInputBinding_: FileInputBinding): void {
  fileInputBinding = fileInputBinding_;
}

function getShinyCreateWebsocket(): (() => WebSocket) | void {
  return validateShinyHasBeenSet().createSocket;
}

export {
  getFileInputBinding,
  getShinyCreateWebsocket,
  getShinyOnCustomMessage,
  setFileInputBinding,
  setShinyObj,
  setShinyUser,
  shinyAppBindOutput,
  shinyAppUnbindOutput,
  shinyBindAll,
  shinyForgetLastInputValue,
  shinyInitializeInputs,
  shinySetInputValue,
  shinyShinyApp,
  shinyUnbindAll,
};
