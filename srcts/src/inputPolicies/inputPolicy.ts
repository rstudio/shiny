type EventPriority = "deferred" | "event" | "immediate";

// Schedules data to be sent to shinyapp at the next setTimeout(0).
// Batches multiple input calls into one websocket message.
class InputPolicy {
  target: InputPolicy;

  setInput(
    name: string,
    value: unknown,
    opts: { priority: EventPriority }
  ): void {
    throw "not implemented";
    name;
    value;
    opts;
  }
}

export { InputPolicy };
export type { EventPriority };
