import type { ShinyType } from ".";
import type { FileInputBinding } from "../bindings/input/fileinput";
import type { OutputBindingAdapter } from "../bindings/output_adapter";
import type { priorityType } from "../inputPolicies";
import type { bindScope } from "./bind";
import type { HandlerType, ShinyApp } from "./shinyapp";

let fullShinyObj_: ShinyType = null;

function setShinyObj(shiny: ShinyType): void {
  fullShinyObj_ = shiny;
}

//// 2021/03: TypeScript Conversion note
// These methods are here due to the delayed initialization of `Shiny.shinyapp`. I
// In theory, there could be multiple instances of `shinyapp`. In practice (and implementation), this is not possible and is a 1:1 coupling with `window.Shiny`.
// To avoid calls to a large Shiny object, helper methods are created to wrap around calling the fully instantiated window.Shiny value.
// TODO-barret; Why is `initShiny()` delayed? Is this to allow users to shim in some code? Why can't it be defined in the init method (maybe w/ an extra trigger call?)
function shinySetInputValue(
  name: string,
  value: unknown,
  opts?: { priority?: priorityType }
): void {
  fullShinyObj_.setInputValue(name, value, opts);
}
function shinyShinyApp(): ShinyApp {
  return fullShinyObj_.shinyapp;
}
function setShinyUser(user: string): void {
  fullShinyObj_.user = user;
}
function shinyForgetLastInputValue(name: string): void {
  fullShinyObj_.forgetLastInputValue(name);
}
function shinyBindAll(scope: bindScope): void {
  fullShinyObj_.bindAll(scope);
}
function shinyUnbindAll(scope: bindScope, includeSelf = false): void {
  fullShinyObj_.unbindAll(scope, includeSelf);
}
function shinyInitializeInputs(scope: bindScope): void {
  fullShinyObj_.initializeInputs(scope);
}

function shinyAppBindOutput(id: string, binding: OutputBindingAdapter): void {
  fullShinyObj_.shinyapp.bindOutput(id, binding);
}

function shinyAppUnbindOutput(
  id: string,
  binding: OutputBindingAdapter
): boolean {
  return fullShinyObj_.shinyapp.unbindOutput(id, binding);
}

function getShinyOnCustomMessage(): null | HandlerType {
  return fullShinyObj_.oncustommessage;
}

let fileInputBinding_: FileInputBinding;

function getFileInputBinding(): FileInputBinding {
  return fileInputBinding_;
}
function setFileInputBinding(fileInputBinding: FileInputBinding): void {
  fileInputBinding_ = fileInputBinding;
}

function getShinyCreateWebsocket(): (() => WebSocket) | void {
  return fullShinyObj_.createSocket;
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
