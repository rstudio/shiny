import type { Shiny } from ".";
import type { FileInputBinding } from "../bindings/input/fileinput";
import type { OutputBindingAdapter } from "../bindings/output_adapter";
import type { EventPriority } from "../inputPolicies";
import type { BindScope } from "./bind";
import type { Handler, ShinyApp } from "./shinyapp";

let fullShinyObj: Shiny = null;

function setShinyObj(shiny: Shiny): void {
  fullShinyObj = shiny;
}

//// 2021/03: TypeScript Conversion note
// These methods are here due to the delayed initialization of `Shiny.shinyapp`. I
// In theory, there could be multiple instances of `shinyapp`. In practice (and implementation), this is not possible and is a 1:1 coupling with `window.Shiny`.
// To avoid calls to a large Shiny object, helper methods are created to wrap around calling the fully instantiated window.Shiny value.
// TODO-barret; Why is `initShiny()` delayed? Is this to allow users to shim in some code? Why can't it be defined in the init method (maybe w/ an extra trigger call?)
function shinySetInputValue(
  name: string,
  value: unknown,
  opts?: { priority?: EventPriority }
): void {
  fullShinyObj.setInputValue(name, value, opts);
}
function shinyShinyApp(): ShinyApp {
  return fullShinyObj.shinyapp;
}
function setShinyUser(user: string): void {
  fullShinyObj.user = user;
}
function shinyForgetLastInputValue(name: string): void {
  fullShinyObj.forgetLastInputValue(name);
}
function shinyBindAll(scope: BindScope): void {
  fullShinyObj.bindAll(scope);
}
function shinyUnbindAll(scope: BindScope, includeSelf = false): void {
  fullShinyObj.unbindAll(scope, includeSelf);
}
function shinyInitializeInputs(scope: BindScope): void {
  fullShinyObj.initializeInputs(scope);
}

function shinyAppBindOutput(id: string, binding: OutputBindingAdapter): void {
  fullShinyObj.shinyapp.bindOutput(id, binding);
}

function shinyAppUnbindOutput(
  id: string,
  binding: OutputBindingAdapter
): boolean {
  return fullShinyObj.shinyapp.unbindOutput(id, binding);
}

function getShinyOnCustomMessage(): null | Handler {
  return fullShinyObj.oncustommessage;
}

let fileInputBinding: FileInputBinding;

function getFileInputBinding(): FileInputBinding {
  return fileInputBinding;
}
function setFileInputBinding(fileInputBinding_: FileInputBinding): void {
  fileInputBinding = fileInputBinding_;
}

function getShinyCreateWebsocket(): (() => WebSocket) | void {
  return fullShinyObj.createSocket;
}

export {
  setShinyObj,
  shinySetInputValue,
  shinyShinyApp,
  setShinyUser,
  shinyForgetLastInputValue,
  shinyBindAll,
  shinyUnbindAll,
  shinyInitializeInputs,
  shinyAppBindOutput,
  shinyAppUnbindOutput,
  getShinyOnCustomMessage,
  getFileInputBinding,
  setFileInputBinding,
  getShinyCreateWebsocket,
};
