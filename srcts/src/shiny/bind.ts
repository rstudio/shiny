import $ from "jquery";
import { InputBinding, inputBindings, outputBindings } from "../bindings";
import { OutputBindingAdapter } from "../bindings/output_adapter";
import { InputRateDecorator, InputValidateDecorator } from "../inputPolicies";
import {
  initDeferredIframes,
  maybeAddThemeObserver,
  shinyAppBindOutput,
  shinyAppUnbindOutput,
} from "./init";
import { sendImageSizeFns } from "./sendImageSize";

const boundInputs = {};

type bindScope = HTMLElement | JQuery<HTMLElement>;

// todo make sure allowDeferred can NOT be supplied and still work
function valueChangeCallback(inputs, binding, el, allowDeferred) {
  let id = binding.getId(el);

  if (id) {
    const value = binding.getValue(el);
    const type = binding.getType(el);

    if (type) id = id + ":" + type;

    const opts = {
      priority: allowDeferred ? "deferred" : "immediate",
      binding: binding,
      el: el,
    };

    inputs.setInput(id, value, opts);
  }
}

type bindInputsCtx = {
  inputs: InputValidateDecorator;
  inputsRate: InputRateDecorator;
};
function bindInputs(
  shinyCtx: bindInputsCtx,
  scope: bindScope = document.documentElement
): Record<
  string,
  {
    value: unknown;
    opts: { immediate: boolean; binding: InputBinding; el: HTMLElement };
  }
> {
  const { inputs, inputsRate } = shinyCtx;
  const bindings = inputBindings.getBindings();

  const inputItems = {};

  for (let i = 0; i < bindings.length; i++) {
    const binding = bindings[i].binding;
    const matches = binding.find(scope) || [];

    for (let j = 0; j < matches.length; j++) {
      const el = matches[j];
      const id = binding.getId(el);

      // Check if ID is falsy, or if already bound
      if (!id || boundInputs[id]) continue;

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

        return function (allowDeferred) {
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
        binding: binding,
        bindingType: "input",
      });
    }
  }

  return inputItems;
}

function bindOutputs(scope: bindScope = document.documentElement): void {
  const $scope = $(scope);

  const bindings = outputBindings.getBindings();

  for (let i = 0; i < bindings.length; i++) {
    const binding = bindings[i].binding;
    const matches = binding.find($scope) || [];

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

      shinyAppBindOutput(id, bindingAdapter);
      $el.data("shiny-output-binding", bindingAdapter);
      $el.addClass("shiny-bound-output");
      if (!$el.attr("aria-live")) $el.attr("aria-live", "polite");
      $el.trigger({
        type: "shiny:bound",
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
  scope: bindScope = document.documentElement,
  includeSelf = false
) {
  const inputs = $(scope).find(".shiny-bound-input");

  if (includeSelf && $(scope).hasClass("shiny-bound-input")) {
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    inputs.push(scope);
  }

  for (let i = 0; i < inputs.length; i++) {
    const el = inputs[i];
    const binding = $(el).data("shiny-input-binding");

    if (!binding) continue;
    const id = binding.getId(el);

    $(el).removeClass("shiny-bound-input");
    delete boundInputs[id];
    binding.unsubscribe(el);
    $(el).trigger({
      type: "shiny:unbound",
      binding: binding,
      bindingType: "input",
    });
  }
}
function unbindOutputs(
  scope: bindScope = document.documentElement,
  includeSelf = false
) {
  const outputs = $(scope).find(".shiny-bound-output");

  if (includeSelf && $(scope).hasClass("shiny-bound-output")) {
    outputs.push(scope);
  }

  for (let i = 0; i < outputs.length; i++) {
    const $el = $(outputs[i]);
    const bindingAdapter = $el.data("shiny-output-binding");

    if (!bindingAdapter) continue;
    const id = bindingAdapter.binding.getId(outputs[i]);

    shinyAppUnbindOutput(id, bindingAdapter);
    $el.removeClass("shiny-bound-output");
    $el.removeData("shiny-output-binding");
    $el.trigger({
      type: "shiny:unbound",
      binding: bindingAdapter.binding,
      bindingType: "output",
    });
  }

  // Send later in case DOM layout isn't final yet.
  setTimeout(sendImageSizeFns.regular, 0);
  setTimeout(sendOutputHiddenState, 0);
}

function _bindAll(
  shinyCtx: bindInputsCtx,
  scope: bindScope
): ReturnType<typeof bindInputs> {
  bindOutputs(scope);
  return bindInputs(shinyCtx, scope);
}
function unbindAll(scope: bindScope, includeSelf = false): void {
  unbindInputs(scope, includeSelf);
  unbindOutputs(scope, includeSelf);
}
function bindAll(shinyCtx: bindInputsCtx, scope: bindScope): void {
  // _bindAll returns input values; it doesn't send them to the server.
  // Shiny.bindAll needs to send the values to the server.
  const currentInputItems = _bindAll(shinyCtx, scope);

  const inputs = shinyCtx.inputs;

  $.each(currentInputItems, function (name, item) {
    inputs.setInput(name, item.value, item.opts);
  });

  // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
  // is a convenient place to hang it. bindAll will be called anytime new HTML
  // appears that might contain inputs/outputs; it's reasonable to assume that
  // any such HTML may contain iframes as well.
  initDeferredIframes();
}

export { unbindAll, bindAll, _bindAll };

export type { bindScope };
