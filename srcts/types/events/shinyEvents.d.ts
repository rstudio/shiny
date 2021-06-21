/// <reference types="jquery" />
import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
interface ShinyEventCommon extends JQuery.Event {
    name: string;
    value: unknown;
    el: HTMLElement;
}
interface ShinyEventInputChanged extends ShinyEventCommon {
    value: unknown;
    binding: InputBinding;
    inputType: string;
    priority: EventPriority;
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
    error: ErrorsMessageValue;
}
interface ShinyEventMessage extends JQuery.Event {
    message: Record<string, unknown>;
}
export type { ShinyEventInputChanged, ShinyEventUpdateInput, ShinyEventValue, ShinyEventError, ShinyEventMessage, };
