import $ from "jquery";
import type { InputBinding, OutputBinding } from "../bindings";
import { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { BindingRegistry } from "../bindings/registry";
import type {
  InputRateDecorator,
  InputValidateDecorator,
} from "../inputPolicies";
import { shinyAppBindOutput, shinyAppUnbindOutput } from "./initedMethods";
import { sendImageSizeFns } from "./sendImageSize";
import { ShinyClientError } from "./error";

type BindScope = HTMLElement | JQuery<HTMLElement>;

// todo make sure allowDeferred can NOT be supplied and still work
function valueChangeCallback(
  inputs: InputValidateDecorator,
  binding: InputBinding,
  el: HTMLElement,
  allowDeferred: boolean
) {
  let id = binding.getId(el);

  if (id) {
    const value = binding.getValue(el);
    const type = binding.getType(el);

    if (type) id = id + ":" + type;

    const opts: {
      priority: "deferred" | "immediate";
      binding: typeof binding;
      el: typeof el;
    } = {
      priority: allowDeferred ? "deferred" : "immediate",
      binding: binding,
      el: el,
    };

    inputs.setInput(id, value, opts);
  }
}

/**
 * Registry for input and output binding IDs. Used to check for duplicate IDs
 * and to keep track of which IDs have already been added to the app. Use an
 * immediately invoked function to keep the sets private and not clutter the
 * scope.
 */
const bindingsRegistry = (() => {
  type BindingTypes = "input" | "output";

  /**
   * Keyed by binding IDs to the array of each type of binding that ID is associated for in current app state.
   *
   * Ideally the
   * value would be a length 1 array but in some (invalid) cases there could be
   * multiple types for a single ID.
   */
  type IdToBindingTypes = Map<string, BindingTypes[]>;

  // Main store of bindings.
  const bindings: IdToBindingTypes = new Map();

  /**
   * Checks if the bindings registry is valid. Currently this just checks if IDs
   * are duplicated within a binding typ but in the future could be expanded to
   * check more conditions.
   *
   * @description IDs are allowed to be duplicated across binding types, but
   * when duplicated within a binding type we report all uses of the ID.
   * Currently the IDs are typically stored in the bound element's `id`
   * attribute, in which case they really *should* be globally unique for
   * accessibility and other reasons. However, in practice our bindings still
   * work as long as inputs the IDs within a binding type don't overlap.
   *
   * @returns ShinyClientError if current ID bindings are invalid, otherwise
   * returns an ok status.
   */
  function checkValidity():
    | { status: "error"; error: ShinyClientError }
    | { status: "ok" } {
    type BindingCounts = { [T in BindingTypes]: number };
    const duplicateIds = new Map<string, BindingCounts>();

    // count duplicate IDs of each binding type
    bindings.forEach((idTypes, id) => {
      const counts: { [T in BindingTypes]: number } = { input: 0, output: 0 };

      idTypes.forEach((type) => (counts[type] += 1));

      // If there's a single duplication of ids across both binding types, then
      // when we're not in devmode, we allow this to pass because a good amount of
      // existing applications use this pattern even though its invalid. Eventually
      // this behavior should be removed.
      if (counts.input === 1 && counts.output === 1 && !Shiny.inDevMode()) {
        return;
      }

      // If we have duplicated IDs, then add them to the set of duplicated IDs
      // to be reported to the user.
      if (counts.input + counts.output > 1) {
        duplicateIds.set(id, counts);
      }
    });

    if (duplicateIds.size === 0) return { status: "ok" };

    const duplicateIdMsg = Array.from(duplicateIds.entries())
      .map(([id, counts]) => {
        const messages = [
          pluralize(counts.input, "input"),
          pluralize(counts.output, "output"),
        ]
          .filter((msg) => msg !== "")
          .join(" and ");

        return `- "${id}": ${messages}`;
      })
      .join("\n");

    return {
      status: "error",
      error: new ShinyClientError({
        headline: "Duplicate input/output IDs found",
        message: `The following ${
          duplicateIds.size === 1 ? "ID was" : "IDs were"
        } repeated:\n${duplicateIdMsg}`,
      }),
    };
  }

  /**
   * Add a binding id to the binding ids registry
   * @param id Id to add
   * @param bindingType Binding type, either "input" or "output"
   */
  function addBinding(id: string, bindingType: BindingTypes): void {
    if (id === "") {
      throw new ShinyClientError({
        headline: `Empty ${bindingType} ID found`,
        message: "Binding IDs must not be empty.",
      });
    }

    const existingBinding = bindings.get(id);

    if (existingBinding) {
      existingBinding.push(bindingType);
    } else {
      bindings.set(id, [bindingType]);
    }
  }

  /**
   * Remove a binding id from the binding ids registry
   * @param id Id to remove
   * @param bindingType Binding type, either "input" or "output"
   */
  function removeBinding(id: string, bindingType: BindingTypes): void {
    const existingBinding = bindings.get(id);

    if (existingBinding) {
      const index = existingBinding.indexOf(bindingType);
      if (index > -1) {
        existingBinding.splice(index, 1);
      }
    }

    if (existingBinding?.length === 0) {
      bindings.delete(id);
    }
  }

  return {
    addBinding,
    removeBinding,
    checkValidity,
  };
})();

function pluralize(num: number, word: string): string {
  if (num === 0) return "";
  if (num === 1) return `${num} ${word}`;
  return `${num} ${word}s`;
}

type BindInputsCtx = {
  inputs: InputValidateDecorator;
  inputsRate: InputRateDecorator;
  inputBindings: BindingRegistry<InputBinding>;
  outputBindings: BindingRegistry<OutputBinding>;
  sendOutputHiddenState: () => void;
  maybeAddThemeObserver: (el: HTMLElement) => void;
  initDeferredIframes: () => void;
};
function bindInputs(
  shinyCtx: BindInputsCtx,
  scope: BindScope = document.documentElement
): {
  [key: string]: {
    value: ReturnType<InputBinding["getValue"]>;
    opts: { immediate: boolean; binding: InputBinding; el: HTMLElement };
  };
} {
  const { inputs, inputsRate, inputBindings } = shinyCtx;
  const bindings = inputBindings.getBindings();

  const inputItems: {
    [key: string]: {
      value: any;
      opts: {
        immediate: true;
        binding: InputBinding;
        el: HTMLElement;
      };
    };
  } = {};

  for (let i = 0; i < bindings.length; i++) {
    const binding = bindings[i].binding;
    const matches = binding.find(scope) || [];

    for (let j = 0; j < matches.length; j++) {
      const el = matches[j];
      if (el.hasAttribute("data-shiny-no-bind-input")) continue;
      const id = binding.getId(el);

      // Don't bind if ID is falsy or is currently bound
      if (!id || $(el).hasClass("shiny-bound-input")) continue;

      const type = binding.getType(el);
      const effectiveId = type ? id + ":" + type : id;

      inputItems[effectiveId] = {
        value: binding.getValue(el),
        opts: {
          immediate: true,
          binding: binding,
          el: el,
        },
      };

      /*jshint loopfunc:true*/
      const thisCallback = (function () {
        const thisBinding = binding;
        const thisEl = el;

        return function (allowDeferred: boolean) {
          valueChangeCallback(inputs, thisBinding, thisEl, allowDeferred);
        };
      })();

      binding.subscribe(el, thisCallback);
      $(el).data("shiny-input-binding", binding);
      $(el).addClass("shiny-bound-input");
      const ratePolicy = binding.getRatePolicy(el);

      if (ratePolicy !== null) {
        inputsRate.setRatePolicy(
          effectiveId,
          ratePolicy.policy,
          ratePolicy.delay
        );
      }

      bindingsRegistry.addBinding(id, "input");
      $(el).trigger({
        type: "shiny:bound",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        binding: binding,
        bindingType: "input",
      });
    }
  }

  return inputItems;
}

async function bindOutputs(
  {
    sendOutputHiddenState,
    maybeAddThemeObserver,
    outputBindings,
  }: BindInputsCtx,
  scope: BindScope = document.documentElement
): Promise<void> {
  const $scope = $(scope);

  const bindings = outputBindings.getBindings();

  for (let i = 0; i < bindings.length; i++) {
    const binding = bindings[i].binding;
    const matches = binding.find($scope) || [];

    // First loop over the matches and assemble map of id->element
    for (let j = 0; j < matches.length; j++) {
      const el = matches[j];
      const id = binding.getId(el);

      // Check if ID is falsy
      if (!id) continue;

      // In some uncommon cases, elements that are later in the
      // matches array can be removed from the document by earlier
      // iterations. See https://github.com/rstudio/shiny/issues/1399
      if (!$.contains(document.documentElement, el)) continue;

      const $el = $(el);

      if ($el.hasClass("shiny-bound-output")) {
        // Already bound; can happen with nested uiOutput (bindAll
        // gets called on two ancestors)
        continue;
      }

      // If this element reports its CSS styles to getCurrentOutputInfo()
      // then it should have a MutationObserver() to resend CSS if its
      // style/class attributes change. This observer should already exist
      // for _static_ UI, but not yet for _dynamic_ UI
      maybeAddThemeObserver(el);

      const bindingAdapter = new OutputBindingAdapter(el, binding);

      await shinyAppBindOutput(id, bindingAdapter);
      $el.data("shiny-output-binding", bindingAdapter);
      $el.addClass("shiny-bound-output");
      if (!$el.attr("aria-live")) $el.attr("aria-live", "polite");

      bindingsRegistry.addBinding(id, "output");
      $el.trigger({
        type: "shiny:bound",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        binding: binding,
        bindingType: "output",
      });
    }
  }

  // Send later in case DOM layout isn't final yet.
  setTimeout(sendImageSizeFns.regular, 0);
  setTimeout(sendOutputHiddenState, 0);
}

function unbindInputs(
  scope: BindScope = document.documentElement,
  includeSelf = false
) {
  const inputs: Array<HTMLElement | JQuery<HTMLElement>> = $(scope)
    .find(".shiny-bound-input")
    .toArray();

  if (includeSelf && $(scope).hasClass("shiny-bound-input")) {
    inputs.push(scope);
  }

  for (let i = 0; i < inputs.length; i++) {
    const el = inputs[i];
    const binding = $(el).data("shiny-input-binding");

    if (!binding) continue;
    const id = binding.getId(el);

    $(el).removeClass("shiny-bound-input");

    bindingsRegistry.removeBinding(id, "input");
    binding.unsubscribe(el);
    $(el).trigger({
      type: "shiny:unbound",
      // @ts-expect-error; Can not remove info on a established, malformed Event object
      binding: binding,
      bindingType: "input",
    });
  }
}
function unbindOutputs(
  { sendOutputHiddenState }: BindInputsCtx,
  scope: BindScope = document.documentElement,
  includeSelf = false
) {
  const outputs: Array<HTMLElement | JQuery<HTMLElement>> = $(scope)
    .find(".shiny-bound-output")
    .toArray();

  if (includeSelf && $(scope).hasClass("shiny-bound-output")) {
    outputs.push(scope);
  }

  for (let i = 0; i < outputs.length; i++) {
    const $el = $(outputs[i]);
    const bindingAdapter = $el.data("shiny-output-binding");

    if (!bindingAdapter) continue;
    const id = bindingAdapter.binding.getId(outputs[i]);

    shinyAppUnbindOutput(id, bindingAdapter);

    bindingsRegistry.removeBinding(id, "output");
    $el.removeClass("shiny-bound-output");
    $el.removeData("shiny-output-binding");
    $el.trigger({
      type: "shiny:unbound",
      // @ts-expect-error; Can not remove info on a established, malformed Event object
      binding: bindingAdapter.binding,
      bindingType: "output",
    });
  }

  // Send later in case DOM layout isn't final yet.
  setTimeout(sendImageSizeFns.regular, 0);
  setTimeout(sendOutputHiddenState, 0);
}

// (Named used before TS conversion)
// eslint-disable-next-line @typescript-eslint/naming-convention
async function _bindAll(
  shinyCtx: BindInputsCtx,
  scope: BindScope
): Promise<ReturnType<typeof bindInputs>> {
  await bindOutputs(shinyCtx, scope);
  const currentInputs = bindInputs(shinyCtx, scope);

  // Check to make sure the bindings setup is valid. By checking the validity
  // _after_ we've attempted all the bindings we can give the user a more
  // complete error message that contains everything they will need to fix. If
  // we threw as we saw collisions then the user would fix the first collision,
  // re-run, and then see the next collision, etc.
  const bindingValidity = bindingsRegistry.checkValidity();
  if (bindingValidity.status === "error") {
    throw bindingValidity.error;
  }

  return currentInputs;
}
function unbindAll(
  shinyCtx: BindInputsCtx,
  scope: BindScope,
  includeSelf = false
): void {
  unbindInputs(scope, includeSelf);
  unbindOutputs(shinyCtx, scope, includeSelf);
}
async function bindAll(
  shinyCtx: BindInputsCtx,
  scope: BindScope
): Promise<void> {
  // _bindAll returns input values; it doesn't send them to the server.
  // Shiny.bindAll needs to send the values to the server.
  const currentInputItems = await _bindAll(shinyCtx, scope);

  const inputs = shinyCtx.inputs;

  $.each(currentInputItems, function (name: string, item) {
    inputs.setInput(name, item.value, item.opts);
  });

  // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
  // is a convenient place to hang it. bindAll will be called anytime new HTML
  // appears that might contain inputs/outputs; it's reasonable to assume that
  // any such HTML may contain iframes as well.
  shinyCtx.initDeferredIframes();
}

export { unbindAll, bindAll, _bindAll };

export type { BindScope, BindInputsCtx };
