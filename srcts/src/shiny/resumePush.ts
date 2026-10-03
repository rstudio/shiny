import type { InputBinding } from "../bindings";

// The server pushes the snapshot values of
// the bound inputs whose widget shows something else. Each goes through the
// input's binding; then every bound input is re-sent, and the no-resend
// filter (taught the pushed values) lets through only the ones whose widget
// did not take its value. The server takes those as ordinary updates.

type PushedInputs = { [id: string]: unknown };

type BoundInput = { binding: InputBinding; el: HTMLElement; dataType: unknown };

type PushDeps = {
  lookup: (id: string) => BoundInput | null;
  remember: (nameType: string, value: unknown) => void;
  resendAll: () => void;
  log: (message: string, error: unknown) => void;
};

// setValue() is not part of the documented InputBinding contract; every
// built-in binding defines it.
type SettableBinding = InputBinding & {
  setValue?: (el: HTMLElement, value: unknown) => void;
};

async function applyPushedInputs(
  values: PushedInputs,
  deps: PushDeps,
): Promise<void> {
  for (const id of Object.keys(values)) {
    try {
      const bound = deps.lookup(id);
      if (bound === null) continue;
      const binding = bound.binding as SettableBinding;
      const type = binding.getType(bound.el);
      // First: some bindings report a change synchronously from setValue().
      deps.remember(type ? id + ":" + type : id, values[id]);
      if (typeof binding.setValue === "function") {
        binding.setValue(
          bound.el,
          adaptPushedValue(binding.name, bound.dataType, values[id]),
        );
      } else {
        await binding.receiveMessage(bound.el, { value: values[id] });
      }
    } catch (error) {
      deps.log(
        "Could not apply the restored value of input '" + id + "'",
        error,
      );
    }
  }
  try {
    deps.resendAll();
  } catch (error) {
    deps.log("Could not re-send the inputs after restoring them", error);
  }
}

// The two built-in bindings whose setValue() does not take the JSON of
// the R value as is.
function adaptPushedValue(
  bindingName: string,
  dataType: unknown,
  value: unknown,
): unknown {
  if (bindingName === "shiny.dateRangeInput" && Array.isArray(value)) {
    return { start: value[0], end: value[1] };
  }
  if (
    bindingName === "shiny.sliderInput" &&
    (dataType === "date" || dataType === "datetime")
  ) {
    const toMs = (v: unknown): unknown =>
      typeof v === "string" ? Date.parse(v) : v;
    return Array.isArray(value) ? value.map(toMs) : toMs(value);
  }
  return value;
}

export { adaptPushedValue, applyPushedInputs };
export type { BoundInput, PushDeps, PushedInputs };
