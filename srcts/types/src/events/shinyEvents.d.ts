/// <reference types="jquery" />
import type { InputBinding } from "../bindings/input/inputBinding";
import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { EventPriority } from "../inputPolicies/inputPolicy";
import type { ErrorsMessageValue } from "../shiny/shinyapp";
import type { EvtFn } from "./jQueryEvents";
declare class EventBase {
    event: JQuery.Event;
    constructor(type: string);
    triggerOn(el: HTMLElement | JQuery<HTMLElement> | typeof document | null): void;
    isDefaultPrevented(): boolean;
}
interface ShinyEventCommon extends JQuery.Event {
    name: string;
    value: any;
}
declare class EventCommon extends EventBase {
    event: ShinyEventCommon;
    constructor(type: ShinyEventCommon["type"], name: ShinyEventCommon["name"], value: ShinyEventCommon["value"]);
    get name(): ShinyEventCommon["name"];
    get value(): ShinyEventCommon["value"];
}
interface ShinyEventInputChanged extends ShinyEventCommon {
    el: HTMLElement | null;
    binding: InputBinding | null;
    inputType: string;
    priority?: EventPriority;
}
declare class EventInputChanged extends EventCommon {
    event: ShinyEventInputChanged;
    constructor({ name, value, el, binding, inputType, priority, }: {
        name: ShinyEventInputChanged["name"];
        value: ShinyEventInputChanged["value"];
        el: ShinyEventInputChanged["el"];
        binding: ShinyEventInputChanged["binding"];
        inputType: ShinyEventInputChanged["inputType"];
        priority?: ShinyEventInputChanged["priority"];
    });
    get el(): ShinyEventInputChanged["el"];
    get binding(): ShinyEventInputChanged["binding"];
    get inputType(): ShinyEventInputChanged["inputType"];
    get priority(): ShinyEventInputChanged["priority"];
}
interface ShinyEventUpdateInput extends ShinyEventCommon {
    message?: any;
    binding: InputBinding;
}
declare class EventUpdateInput extends EventBase {
    event: ShinyEventUpdateInput;
    constructor({ message, binding, }: {
        message?: ShinyEventUpdateInput["message"];
        binding: ShinyEventUpdateInput["binding"];
    });
    get message(): ShinyEventUpdateInput["message"];
    get binding(): ShinyEventUpdateInput["binding"];
}
interface ShinyEventValue extends ShinyEventCommon {
    binding: OutputBindingAdapter;
}
declare class EventValue extends EventCommon {
    event: ShinyEventValue;
    constructor({ name, value, binding, }: {
        name: ShinyEventValue["name"];
        value: ShinyEventValue["value"];
        binding: ShinyEventValue["binding"];
    });
    get binding(): ShinyEventValue["binding"];
}
interface ShinyEventError extends ShinyEventCommon {
    binding: OutputBindingAdapter;
    error: ErrorsMessageValue;
}
declare class EventError extends EventCommon {
    event: ShinyEventError;
    constructor({ name, binding, error, }: {
        name: ShinyEventError["name"];
        binding: ShinyEventError["binding"];
        error: ShinyEventError["error"];
    });
    get binding(): ShinyEventError["binding"];
    get error(): ShinyEventError["error"];
}
interface ShinyEventMessage extends JQuery.Event {
    message: {
        [key: string]: unknown;
    };
}
declare class EventMessage extends EventBase {
    event: ShinyEventMessage;
    constructor(message: ShinyEventMessage["message"]);
    get message(): ShinyEventMessage["message"];
}
declare global {
    interface JQuery {
        on(events: "shiny:inputchanged", handler: EvtFn<ShinyEventInputChanged>): this;
        on(events: "shiny:updateinput", handler: EvtFn<ShinyEventUpdateInput>): this;
        on(events: "shiny:value", handler: EvtFn<ShinyEventValue>): this;
        on(events: "shiny:error", handler: EvtFn<ShinyEventError>): this;
        on(events: "shiny:message", handler: EvtFn<ShinyEventMessage>): this;
    }
}
export { EventCommon, EventInputChanged, EventUpdateInput, EventValue, EventError, EventMessage, };
export type { ShinyEventInputChanged, ShinyEventUpdateInput, ShinyEventValue, ShinyEventError, ShinyEventMessage, };
