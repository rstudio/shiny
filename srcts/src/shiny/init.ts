import $ from "jquery";
import type { Shiny } from ".";
import {
  InputBatchSender,
  InputDeferDecorator,
  InputEventDecorator,
  InputNoResendDecorator,
  InputRateDecorator,
  InputValidateDecorator,
} from "../inputPolicies";
import type { InputPolicy } from "../inputPolicies";
import { addDefaultInputOpts } from "../inputPolicies/inputValidateDecorator";
import { debounce } from "../time";
import {
  getComputedLinkColor,
  getStyle,
  isHidden,
  mapValues,
  pixelRatio,
} from "../utils";
import { bindAll, unbindAll, _bindAll } from "./bind";
import type { BindInputsCtx, BindScope } from "./bind";
import { setShinyObj } from "./initedMethods";
import { registerDependency } from "./render";
import { sendOutputInfoFns } from "./sendOutputInfo";
import { ShinyApp } from "./shinyapp";
import { registerNames as singletonsRegisterNames } from "./singletons";
import type { InputPolicyOpts } from "../inputPolicies/inputPolicy";

// "init_shiny.js"
function initShiny(windowShiny: Shiny): void {
  setShinyObj(windowShiny);
  const shinyapp = (windowShiny.shinyapp = new ShinyApp());

  windowShiny.progressHandlers = shinyapp.progressHandlers;

  const inputBatchSender = new InputBatchSender(shinyapp);
  const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
  const inputsEvent = new InputEventDecorator(inputsNoResend);
  const inputsRate = new InputRateDecorator(inputsEvent);
  const inputsDefer = new InputDeferDecorator(inputsEvent);

  let target: InputPolicy;

  if ($('input[type="submit"], button[type="submit"]').length > 0) {
    // If there is a submit button on the page, use defer decorator
    target = inputsDefer;

    $('input[type="submit"], button[type="submit"]').each(function () {
      $(this).click(function (event) {
        event.preventDefault();
        inputsDefer.submit();
      });
    });
  } else {
    // By default, use rate decorator
    target = inputsRate;
  }

  const inputs = new InputValidateDecorator(target);

  windowShiny.setInputValue = windowShiny.onInputChange = function (
    name: string,
    value: unknown,
    opts: Partial<InputPolicyOpts> = {}
  ): void {
    const newOpts = addDefaultInputOpts(opts);

    inputs.setInput(name, value, newOpts);
  };

  // By default, Shiny deduplicates input value changes; that is, if
  // `setInputValue` is called with the same value as the input already
  // has, the call is ignored (unless opts.priority = "event"). Calling
  // `forgetLastInputValue` tells Shiny that the very next call to
  // `setInputValue` for this input id shouldn't be ignored, even if it
  // is a dupe of the existing value.
  windowShiny.forgetLastInputValue = function (name) {
    inputsNoResend.forget(name);
  };

  // MUST be called after `setShiny()`
  const inputBindings = windowShiny.inputBindings;
  const outputBindings = windowShiny.outputBindings;

  function shinyBindCtx(): BindInputsCtx {
    return {
      inputs,
      inputsRate,
      inputBindings,
      outputBindings,
      initDeferredIframes,
    };
  }

  windowShiny.bindAll = function (scope: BindScope) {
    bindAll(shinyBindCtx(), scope);
  };
  windowShiny.unbindAll = function (scope: BindScope, includeSelf = false) {
    unbindAll(scope, includeSelf);
  };

  // Calls .initialize() for all of the input objects in all input bindings,
  // in the given scope.
  function initializeInputs(scope: BindScope = document.documentElement) {
    const bindings = inputBindings.getBindings();

    // Iterate over all bindings
    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i].binding;
      const inputObjects = binding.find(scope);

      if (inputObjects) {
        // Iterate over all input objects for this binding
        for (let j = 0; j < inputObjects.length; j++) {
          const $inputObjectJ = $(inputObjects[j]);

          if (!$inputObjectJ.data("_shiny_initialized")) {
            $inputObjectJ.data("_shiny_initialized", true);
            binding.initialize(inputObjects[j]);
          }
        }
      }
    }
  }
  windowShiny.initializeInputs = initializeInputs;

  function getIdFromEl(el: HTMLElement): string | null {
    const $el = $(el);
    const bindingAdapter = $el.data("shiny-output-binding");

    return bindingAdapter ? bindingAdapter.getId() : null;
  }

  // Initialize all input objects in the document, before binding
  initializeInputs(document.documentElement);

  // The input values returned by _bindAll() each have a structure like this:
  //   { value: 123, opts: { ... } }
  // We want to only keep the value. This is because when the initialValues is
  // passed to ShinyApp.connect(), the ShinyApp object stores the
  // initialValues object for the duration of the session, and the opts may
  // have a reference to the DOM element, which would prevent it from being
  // GC'd.
  const initialValues = mapValues(
    _bindAll(shinyBindCtx(), document.documentElement),
    (x) => x.value
  );

  // Helper function for both initializing and updating input values
  function setInput(name: string, value: unknown, initial = false): void {
    if (initial) {
      initialValues[name] = value;
    } else {
      inputs.setInput(name, value);
    }
  }

  function doSendSize(el: HTMLElement, initial = false): void {
    const id = getIdFromEl(el);

    if (el.offsetWidth !== 0 || el.offsetHeight !== 0) {
      setInput(".clientdata_output_" + id + "_width", el.offsetWidth, initial);
      setInput(
        ".clientdata_output_" + id + "_height",
        el.offsetHeight,
        initial
      );
    }
  }

  function doTriggerResize(el: HTMLElement): void {
    const $el = $(el),
      binding = $el.data("shiny-output-binding");

    $el.trigger({
      type: "shiny:visualchange",
      // @ts-expect-error; Can not remove info on a established, malformed Event object
      visible: !isHidden(el),
      binding: binding,
    });
    binding.onResize();
  }

  function doSendTheme(el: HTMLElement, initial = false): void {
    // Sending theme info on error isn't necessary (it'd add an unnecessary additional round-trip)
    if (el.classList.contains("shiny-output-error")) {
      return;
    }

    function getComputedBgColor(el: HTMLElement): string {
      if (!el) {
        // Top of document, can't recurse further
        return null;
      }

      const bgColor = getStyle(el, "background-color");
      const m = bgColor.match(
        /^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/
      );

      if (bgColor === "transparent" || (m && parseFloat(m[4]) === 0)) {
        // No background color on this element. See if it has a background image.
        const bgImage = getStyle(el, "background-image");

        if (bgImage && bgImage !== "none") {
          // Failed to detect background color, since it has a background image
          return null;
        } else {
          // Recurse
          return getComputedBgColor(el.parentElement);
        }
      }
      return bgColor;
    }

    function getComputedFont(el: HTMLElement): {
      families: string[];
      size: string;
    } {
      const fontFamily = getStyle(el, "font-family");
      const fontSize = getStyle(el, "font-size");

      return {
        families: fontFamily.replace(/"/g, "").split(", "),
        size: fontSize,
      };
    }

    const id = getIdFromEl(el);

    setInput(
      ".clientdata_output_" + id + "_bg",
      getComputedBgColor(el),
      initial
    );
    setInput(
      ".clientdata_output_" + id + "_fg",
      getStyle(el, "color"),
      initial
    );
    setInput(
      ".clientdata_output_" + id + "_accent",
      getComputedLinkColor(el),
      initial
    );
    setInput(
      ".clientdata_output_" + id + "_font",
      getComputedFont(el),
      initial
    );
  }

  // eslint-disable-next-line prefer-const
  let visibleOutputs = new Set();

  function doSendHiddenState(el: HTMLElement, initial = false): void {
    const id = getIdFromEl(el);
    const hidden = isHidden(el);

    if (!hidden) visibleOutputs.add(id);
    setInput(".clientdata_output_" + id + "_hidden", hidden, initial);
  }

  function doSendOutputInfo(initial = false) {
    const outputIds = new Set();

    // TODO: can we always rely on this class existing when we're calling this (especially initially)?
    $(".shiny-bound-output").each(function () {
      // eslint-disable-next-line @typescript-eslint/no-this-alias
      const el = this,
        id = getIdFromEl(el),
        isPlot =
          el.classList.contains("shiny-image-output") ||
          el.classList.contains("shiny-plot-output");

      outputIds.add(id);

      function handleResize(initial = false) {
        doTriggerResize(el);
        doSendHiddenState(el, initial);
        if (isPlot || el.classList.contains("shiny-report-size")) {
          doSendSize(el, initial);
        }
      }

      // TODO: do we need a polyfill for ResizeObserver?
      if (!$(el).data("shiny-resize-observer")) {
        const onResize = debounce(100, handleResize);
        const ro = new ResizeObserver(() => onResize(false));

        ro.observe(el);
        $(el).data("shiny-resize-observer", ro);
      }

      function handleIntersect(entries) {
        entries.forEach(() => {
          handleResize(false);
        });
      }

      // TODO: do we need a polyfill for IntersectionObserver?
      if (!$(el).data("shiny-intersection-observer")) {
        const onIntersect = debounce(100, handleIntersect);
        const io = new IntersectionObserver(onIntersect);

        io.observe(el);
        $(el).data("shiny-intersection-observer", io);
      }

      function handleMutate(initial = false) {
        if (isPlot || el.classList.contains("shiny-report-theme")) {
          doSendTheme(el, initial);
        }
      }

      // TODO: do we need a polyfill for MutationObserver?
      if (!$(el).data("shiny-mutate-observer")) {
        const onMutate = debounce(100, handleMutate);
        const mo = new MutationObserver(() => onMutate(false));

        mo.observe(el, {
          attributes: true,
          attributeFilter: ["style", "class"],
        });

        $(el).data("shiny-mutate-observer", mo);
      }

      handleResize(initial);
      handleMutate(initial);
    });

    // It could be that previously visible outputs have been removed from the DOM,
    // in that case, consider them hidden.
    visibleOutputs.forEach((id) => {
      if (!outputIds.has(id)) {
        visibleOutputs.delete(id);
        setInput(".clientdata_output_" + id + "_hidden", true, initial);
      }
    });
  }

  // Send initial input values to the server and also register a callback
  // to send updated input values whenever we receive new UI, etc.
  doSendOutputInfo(true);
  sendOutputInfoFns.setSendMethod(inputBatchSender, doSendOutputInfo);

  // Send initial pixel ratio, and update it if it changes
  initialValues[".clientdata_pixelratio"] = pixelRatio();
  $(window).resize(function () {
    inputs.setInput(".clientdata_pixelratio", pixelRatio());
  });

  // Send initial URL
  initialValues[".clientdata_url_protocol"] = window.location.protocol;
  initialValues[".clientdata_url_hostname"] = window.location.hostname;
  initialValues[".clientdata_url_port"] = window.location.port;
  initialValues[".clientdata_url_pathname"] = window.location.pathname;

  // Send initial URL search (query string) and update it if it changes
  initialValues[".clientdata_url_search"] = window.location.search;

  $(window).on("pushstate", function (e) {
    inputs.setInput(".clientdata_url_search", window.location.search);
    return;
    e;
  });

  $(window).on("popstate", function (e) {
    inputs.setInput(".clientdata_url_search", window.location.search);
    return;
    e;
  });

  // This is only the initial value of the hash. The hash can change, but
  // a reactive version of this isn't sent because watching for changes can
  // require polling on some browsers. The JQuery hashchange plugin can be
  // used if this capability is important.
  initialValues[".clientdata_url_hash_initial"] = window.location.hash;
  initialValues[".clientdata_url_hash"] = window.location.hash;

  $(window).on("hashchange", function (e) {
    inputs.setInput(".clientdata_url_hash", window.location.hash);
    return;
    e;
  });

  // The server needs to know what singletons were rendered as part of
  // the page loading
  const singletonText = (initialValues[".clientdata_singletons"] = $(
    'script[type="application/shiny-singletons"]'
  ).text());

  singletonsRegisterNames(singletonText.split(/,/));

  const dependencyText = $(
    'script[type="application/html-dependencies"]'
  ).text();

  $.each(dependencyText.split(/;/), function (i, depStr) {
    const match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);

    if (match) {
      registerDependency(match[1], match[2]);
    }
  });

  // We've collected all the initial values--start the server process!
  inputsNoResend.reset(initialValues);
  shinyapp.connect(initialValues);
  $(document).one("shiny:connected", function () {
    initDeferredIframes();
  });

  // window.console.log("Shiny version: ", windowShiny.version);
} // function initShiny()

// Give any deferred iframes a chance to load.
function initDeferredIframes(): void {
  // TODO-barret; This method uses `window.Shiny`. Could be replaced with `fullShinyObj_.shinyapp?.isConnected()`,
  // but that would not use `window.Shiny`. Is it a problem???
  if (
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore; Do not want to define `window.Shiny` as a type to discourage usage of `window.Shiny`;
    // Can not expect error when combining with window available Shiny definition
    !window.Shiny ||
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore; Do not want to define `window.Shiny` as a type to discourage usage of `window.Shiny`;
    // Can not expect error when combining with window available Shiny definition
    !window.Shiny.shinyapp ||
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore; Do not want to define `window.Shiny` as a type to discourage usage of `window.Shiny`;
    // Can not expect error when combining with window available Shiny definition
    !window.Shiny.shinyapp.isConnected()
  ) {
    // If somehow we accidentally call this before the server connection is
    // established, just ignore the call. At the time of this writing it
    // doesn't happen, but it's easy to imagine a later refactoring putting
    // us in this situation and it'd be hard to notice with either manual
    // testing or automated tests, because the only effect is on HTTP request
    // timing. (Update: Actually Aron saw this being called without even
    // window.Shiny being defined, but it was hard to repro.)
    return;
  }

  $(".shiny-frame-deferred").each(function (i, el) {
    const $el = $(el);

    $el.removeClass("shiny-frame-deferred");
    $el.attr("src", $el.attr("data-deferred-src"));
    $el.attr("data-deferred-src", null);
  });
}

export { initShiny };
