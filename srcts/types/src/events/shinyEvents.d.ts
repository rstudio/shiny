import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
interface ShinyEventCommon extends JQuery.Event {
    name: string;
    value: unknown;
    el: HTMLElement | null;
}
interface ShinyEventInputChanged extends ShinyEventCommon {
    value: unknown;
    binding: InputBinding | null;
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
    message: {
        [key: string]: unknown;
    };
}
export type { ShinyEventError, ShinyEventInputChanged, ShinyEventMessage, ShinyEventUpdateInput, ShinyEventValue, };
