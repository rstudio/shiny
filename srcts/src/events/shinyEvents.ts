import type { InputBinding } from "../bindings/input/InputBinding";
import type { OutputBindingAdapter } from "../bindings/output_adapter";
import type { priorityType } from "../inputPolicies/InputPolicy";
import type { errorsMessageValue } from "../shiny/shinyapp";

interface ShinyEventCommon extends JQuery.Event {
  name: string;
  value: unknown;
  el: HTMLElement;
}
interface ShinyEventInputChanged extends ShinyEventCommon {
  value: unknown;
  binding: InputBinding;
  inputType: string;
  priority: priorityType;
}
interface ShinyEventUpdateInput extends ShinyEventCommon {
  message: unknown;
  binding: InputBinding;
}
interface ShinyEventValue extends ShinyEventCommon {
  value: unknown;
  binding: OutputBindingAdapter;
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: errorsMessageValue;
}
interface ShinyEventMessage extends JQuery.Event {
  message: any;
}

export type {
  ShinyEventInputChanged,
  ShinyEventUpdateInput,
  ShinyEventValue,
  ShinyEventError,
  ShinyEventMessage,
};
