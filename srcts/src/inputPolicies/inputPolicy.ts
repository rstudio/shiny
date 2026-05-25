import type { InputBinding } from "../bindings";

type EventPriority = "deferred" | "event" | "immediate";

type InputPolicyOpts = {
  priority: EventPriority;
  el?: HTMLElement;
  binding?: InputBinding;
};

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
interface InputPolicy {
  target: InputPolicy;

  setInput(name: string, value: unknown, opts: InputPolicyOpts): void;
}

export type { EventPriority, InputPolicy, InputPolicyOpts };
