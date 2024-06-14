import { BindingRegistry } from "../registry";

import { InputBinding } from "./inputBinding";

import { ActionButtonInputBinding } from "./actionbutton";
import { CheckboxInputBinding } from "./checkbox";
import { CheckboxGroupInputBinding } from "./checkboxgroup";
import { DateInputBinding } from "./date";
import { DateRangeInputBinding } from "./daterange";
import { FileInputBinding } from "./fileinput";
import { NumberInputBinding } from "./number";
import { PasswordInputBinding } from "./password";
import { RadioInputBinding } from "./radio";
import { SelectInputBinding } from "./selectInput";
import { SliderInputBinding } from "./slider";
import { BootstrapTabInputBinding } from "./tabinput";
import { TextInputBinding } from "./text";
import { TextareaInputBinding } from "./textarea";

// TODO-barret make this an init method
function initInputBindings(): {
  inputBindings: BindingRegistry<InputBinding>;
  fileInputBinding: FileInputBinding;
} {
  const inputBindings = new BindingRegistry<InputBinding>();

  inputBindings.register(new TextInputBinding(), "shiny.textInput");
  inputBindings.register(new TextareaInputBinding(), "shiny.textareaInput");
  inputBindings.register(new PasswordInputBinding(), "shiny.passwordInput");
  inputBindings.register(new NumberInputBinding(), "shiny.numberInput");
  inputBindings.register(new CheckboxInputBinding(), "shiny.checkboxInput");
  inputBindings.register(
    new CheckboxGroupInputBinding(),
    "shiny.checkboxGroupInput"
  );
  inputBindings.register(new RadioInputBinding(), "shiny.radioInput");
  inputBindings.register(new SliderInputBinding(), "shiny.sliderInput");
  inputBindings.register(new DateInputBinding(), "shiny.dateInput");
  inputBindings.register(new DateRangeInputBinding(), "shiny.dateRangeInput");
  inputBindings.register(new SelectInputBinding(), "shiny.selectInput");
  inputBindings.register(
    new ActionButtonInputBinding(),
    "shiny.actionButtonInput"
  );
  inputBindings.register(
    new BootstrapTabInputBinding(),
    "shiny.bootstrapTabInput"
  );
  const fileInputBinding = new FileInputBinding();

  inputBindings.register(fileInputBinding, "shiny.fileInputBinding");

  return { inputBindings, fileInputBinding };
}

export { initInputBindings, InputBinding };
