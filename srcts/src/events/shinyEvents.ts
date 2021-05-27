import { InputBinding } from "../bindings/input/InputBinding";
import { OutputBindingAdapter } from "../bindings/output_adapter";
import { priorityType } from "../inputPolicies/InputPolicy";

interface ShinyEventCommon extends JQuery.Event {
  name: string;
  value;
  el: HTMLElement;
}
interface ShinyEventInputChanged extends ShinyEventCommon {
  value;
  binding: InputBinding;
  inputType: string;
  priority: priorityType;
}
interface ShinyEventUpdateInput extends ShinyEventCommon {
  message: any;
  binding: InputBinding;
}
interface ShinyEventValue extends ShinyEventCommon {
  value;
  binding: OutputBindingAdapter;
}

interface ShinyEventError extends ShinyEventCommon {
  binding: OutputBindingAdapter;
  error: string;
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
