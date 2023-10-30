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
import {
  ShinyClientError,
  showErrorInClientConsole,
} from "../components/errorConsole";

const boundInputs: {
  [key: string]: { binding: InputBinding; node: HTMLElement };
} = {};

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
 * Sets for input and outputs to keep track of what IDs are bound so we can
 * detect duplicates and warn users. This is done outside of the
 * bindOutputs()/bindInputs() functions so that we can check for duplicate IDs
 * across all bindings and dynamically generated outputs.
 */
const bindingIds = {
  /**
   * Set of IDs for output bindings
   */
  outputs: new Set<string>(),
  /**
   * Set of IDs for input bindings
   */
  inputs: new Set<string>(),
};

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

  // Keep track of duplicate IDs for input bindings so we can warn users
  const inputDuplicateIds = new Set<string>();

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

      // Check for duplicates in bindingIds array and keep track of them
      const duplicateId = id && bindingIds.inputs.has(id);
      if (duplicateId) {
        inputDuplicateIds.add(id);
      }
      // Check if ID is falsy, or if already bound, or has the same ID as
      // another input binding and if it is, skip
      if (!id || boundInputs[id] || duplicateId) continue;

      bindingIds.inputs.add(id);
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

      boundInputs[id] = {
        binding: binding,
        node: el,
      };

      $(el).trigger({
        type: "shiny:bound",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        binding: binding,
        bindingType: "input",
      });
    }
  }

  // Send error message to the user if duplicate IDs are found
  if (inputDuplicateIds.size > 0) {
    throw new ShinyClientError({
      headline: "Duplicate input IDs found",
      message: `The following ${
        inputDuplicateIds.size === 1 ? "ID was" : "IDs were"
      } repeated: ${Array.from(inputDuplicateIds)
        .map((id) => `"${id}"`)
        .join(", ")}.`,
    });
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
  // Keep track of duplicate IDs for output bindings so we can warn users
  const outputDuplicateIds = new Set<string>();

  const $scope = $(scope);

  const bindings = outputBindings.getBindings();

  for (let i = 0; i < bindings.length; i++) {
    const binding = bindings[i].binding;
    const matches = binding.find($scope) || [];

    // First loop over the matches and assemble map of id->element and also note
    // any duplicates
    for (let j = 0; j < matches.length; j++) {
      const el = matches[j];
      const id = binding.getId(el);

      // Check if ID is falsy
      if (!id) continue;

      // Check for duplicates in bindingIds array and keep track of them
      if (id && bindingIds.outputs.has(id)) {
        outputDuplicateIds.add(id);
      }

      bindingIds.outputs.add(id);

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
      $el.trigger({
        type: "shiny:bound",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        binding: binding,
        bindingType: "output",
      });
    }
  }

  // Send error message to the user if duplicate IDs are found
  if (outputDuplicateIds.size > 0) {
    throw new ShinyClientError({
      headline: "Duplicate output IDs found",
      message: `The following ${
        outputDuplicateIds.size === 1 ? "ID was" : "IDs were"
      } repeated: ${Array.from(outputDuplicateIds)
        .map((id) => `"${id}"`)
        .join(", ")}.`,
    });
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
    delete boundInputs[id];

    bindingIds.inputs.delete(id);
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

    bindingIds.outputs.delete(id);
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
  return bindInputs(shinyCtx, scope);
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
  try {
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
  } catch (e) {
    showErrorInClientConsole(e);
  }
}

export { unbindAll, bindAll, _bindAll };

export type { BindScope, BindInputsCtx };
