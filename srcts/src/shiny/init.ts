import $ from "jquery";
import { bindAll } from "lodash";
import { ShinyType } from ".";
import { inputBindings, outputBindings } from "../bindings";
import { OutputBindingAdapter } from "../bindings/output_adapter";
import {
  InputBatchSender,
  InputDeferDecorator,
  InputEventDecorator,
  InputNoResendDecorator,
  InputRateDecorator,
  InputValidateDecorator,
} from "../inputPolicies";
import { addDefaultInputOpts } from "../inputPolicies/inputValidateDecorator";
import { debounce, Debouncer } from "../time";
import { getComputedLinkColor, pixelRatio } from "../utils";
import { registerDependency } from "./render";
import { ShinyApp } from "./shinyapp";
import { registerNames as singletonsRegisterNames } from "./singletons";

// This function gets defined in initShiny() and 'hoisted' so it can be reused
// (to send CSS info) inside of Shiny.renderDependencies()
let sendImageSize;
let sendImageSize2;

// "init_shiny.js"
function initShiny(Shiny: ShinyType): void {
  const shinyapp = (Shiny.shinyapp = new ShinyApp());

  Shiny.progressHandlers = shinyapp.progressHandlers;

  Shiny.addCustomMessageHandler = shinyapp.addCustomMessageHandler;

  const inputBatchSender = new InputBatchSender(shinyapp);
  const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
  const inputsEvent = new InputEventDecorator(inputsNoResend);
  const inputsRate = new InputRateDecorator(inputsEvent);
  const inputsDefer = new InputDeferDecorator(inputsEvent);

  let inputs;

  if ($('input[type="submit"], button[type="submit"]').length > 0) {
    // If there is a submit button on the page, use defer decorator
    inputs = inputsDefer;

    $('input[type="submit"], button[type="submit"]').each(function () {
      $(this).click(function (event) {
        event.preventDefault();
        inputsDefer.submit();
      });
    });
  } else {
    // By default, use rate decorator
    inputs = inputsRate;
  }

  inputs = new InputValidateDecorator(inputs);

  Shiny.setInputValue = Shiny.onInputChange = function (name, value, opts) {
    opts = addDefaultInputOpts(opts);
    inputs.setInput(name, value, opts);
  };

  // By default, Shiny deduplicates input value changes; that is, if
  // `setInputValue` is called with the same value as the input already
  // has, the call is ignored (unless opts.priority = "event"). Calling
  // `forgetLastInputValue` tells Shiny that the very next call to
  // `setInputValue` for this input id shouldn't be ignored, even if it
  // is a dupe of the existing value.
  Shiny.forgetLastInputValue = function (name) {
    inputsNoResend.forget(name);
  };

  Shiny.bindAll = function (scope) {
    bindAll(inputs, scope);
  };

  // Calls .initialize() for all of the input objects in all input bindings,
  // in the given scope.
  function initializeInputs(scope = document) {
    const bindings = inputBindings.getBindings();

    // Iterate over all bindings
    for (let i = 0; i < bindings.length; i++) {
      const binding = bindings[i].binding;
      const inputObjects = binding.find(scope) || [];

      // Iterate over all input objects for this binding
      for (let j = 0; j < inputObjects.length; j++) {
        if (!inputObjects[j]._shiny_initialized) {
          inputObjects[j]._shiny_initialized = true;
          binding.initialize(inputObjects[j]);
        }
      }
    }
  }
  Shiny.initializeInputs = initializeInputs;

  function getIdFromEl(el) {
    const $el = $(el);
    const bindingAdapter = $el.data("shiny-output-binding");

    if (!bindingAdapter) return null;
    else return bindingAdapter.getId();
  }

  // Initialize all input objects in the document, before binding
  initializeInputs(document);

  // The input values returned by _bindAll() each have a structure like this:
  //   { value: 123, opts: { ... } }
  // We want to only keep the value. This is because when the initialValues is
  // passed to ShinyApp.connect(), the ShinyApp object stores the
  // initialValues object for the duration of the session, and the opts may
  // have a reference to the DOM element, which would prevent it from being
  // GC'd.
  const initialValues = mapValues(_bindAll(document), (x) => x.value);

  // The server needs to know the size of each image and plot output element,
  // in case it is auto-sizing
  $(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
    function () {
      const id = getIdFromEl(this);

      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        initialValues[".clientdata_output_" + id + "_width"] = this.offsetWidth;
        initialValues[
          ".clientdata_output_" + id + "_height"
        ] = this.offsetHeight;
      }
    }
  );

  function getComputedBgColor(el) {
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

  function getComputedFont(el) {
    const fontFamily = getStyle(el, "font-family");
    const fontSize = getStyle(el, "font-size");

    return {
      families: fontFamily.replace(/"/g, "").split(", "),
      size: fontSize,
    };
  }

  $(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
    function () {
      const el = this,
        id = getIdFromEl(el);

      initialValues[".clientdata_output_" + id + "_bg"] = getComputedBgColor(
        el
      );
      initialValues[".clientdata_output_" + id + "_fg"] = getStyle(el, "color");
      initialValues[
        ".clientdata_output_" + id + "_accent"
      ] = getComputedLinkColor(el);
      initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(el);
      maybeAddThemeObserver(el);
    }
  );

  // Resend computed styles if *an output element's* class or style attribute changes.
  // This gives us some level of confidence that getCurrentOutputInfo() will be
  // properly invalidated if output container is mutated; but unfortunately,
  // we don't have a reasonable way to detect change in *inherited* styles
  // (other than session$setCurrentTheme())
  // https://github.com/rstudio/shiny/issues/3196
  // https://github.com/rstudio/shiny/issues/2998
  function maybeAddThemeObserver(el) {
    if (!window.MutationObserver) {
      return; // IE10 and lower
    }

    const cl = el.classList;
    const reportTheme =
      cl.contains("shiny-image-output") ||
      cl.contains("shiny-plot-output") ||
      cl.contains("shiny-report-theme");

    if (!reportTheme) {
      return;
    }

    const $el = $(el);

    if ($el.data("shiny-theme-observer")) {
      return; // i.e., observer is already observing
    }

    const observerCallback = new Debouncer(null, () => doSendTheme(el), 100);
    const observer = new MutationObserver(() => observerCallback.normalCall());
    const config = { attributes: true, attributeFilter: ["style", "class"] };

    observer.observe(el, config);
    $el.data("shiny-theme-observer", observer);
  }

  function doSendTheme(el) {
    // Sending theme info on error isn't necessary (it'd add an unnecessary additional round-trip)
    if (el.classList.contains("shiny-output-error")) {
      return;
    }
    const id = getIdFromEl(el);

    inputs.setInput(".clientdata_output_" + id + "_bg", getComputedBgColor(el));
    inputs.setInput(".clientdata_output_" + id + "_fg", getStyle(el, "color"));
    inputs.setInput(
      ".clientdata_output_" + id + "_accent",
      getComputedLinkColor(el)
    );
    inputs.setInput(".clientdata_output_" + id + "_font", getComputedFont(el));
  }

  function doSendImageSize() {
    $(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
      function () {
        const id = getIdFromEl(this);

        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          inputs.setInput(
            ".clientdata_output_" + id + "_width",
            this.offsetWidth
          );
          inputs.setInput(
            ".clientdata_output_" + id + "_height",
            this.offsetHeight
          );
        }
      }
    );

    $(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
      function () {
        doSendTheme(this);
      }
    );

    $(".shiny-bound-output").each(function () {
      const $this = $(this),
        binding = $this.data("shiny-output-binding");

      $this.trigger({
        type: "shiny:visualchange",
        visible: !isHidden(this),
        binding: binding,
      });
      binding.onResize();
    });
  }
  const sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);

  sendImageSize = function () {
    sendImageSizeDebouncer.normalCall();
  };
  // Make sure sendImageSize actually gets called before the inputBatchSender
  // sends data to the server.
  inputBatchSender.lastChanceCallback.push(function () {
    if (sendImageSizeDebouncer.isPending())
      sendImageSizeDebouncer.immediateCall();
  });

  // A version of sendImageSize which debounces for longer.
  sendImageSize2 = debounce(200, sendImageSize);

  // Return true if the object or one of its ancestors in the DOM tree has
  // style='display:none'; otherwise return false.
  function isHidden(obj) {
    // null means we've hit the top of the tree. If width or height is
    // non-zero, then we know that no ancestor has display:none.
    if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
      return false;
    } else if (getStyle(obj, "display") === "none") {
      return true;
    } else {
      return isHidden(obj.parentNode);
    }
  }
  let lastKnownVisibleOutputs = {};
  // Set initial state of outputs to hidden, if needed

  $(".shiny-bound-output").each(function () {
    const id = getIdFromEl(this);

    if (isHidden(this)) {
      initialValues[".clientdata_output_" + id + "_hidden"] = true;
    } else {
      lastKnownVisibleOutputs[id] = true;
      initialValues[".clientdata_output_" + id + "_hidden"] = false;
    }
  });
  // Send update when hidden state changes
  function doSendOutputHiddenState() {
    const visibleOutputs = {};

    $(".shiny-bound-output").each(function () {
      const id = getIdFromEl(this);

      delete lastKnownVisibleOutputs[id];
      // Assume that the object is hidden when width and height are 0
      const hidden = isHidden(this),
        evt = {
          type: "shiny:visualchange",
          visible: !hidden,
        };

      if (hidden) {
        inputs.setInput(".clientdata_output_" + id + "_hidden", true);
      } else {
        visibleOutputs[id] = true;
        inputs.setInput(".clientdata_output_" + id + "_hidden", false);
      }
      const $this = $(this);

      evt.binding = $this.data("shiny-output-binding");
      $this.trigger(evt);
    });
    // Anything left in lastKnownVisibleOutputs is orphaned
    for (const name in lastKnownVisibleOutputs) {
      if (lastKnownVisibleOutputs.hasOwnProperty(name))
        inputs.setInput(".clientdata_output_" + name + "_hidden", true);
    }
    // Update the visible outputs for next time
    lastKnownVisibleOutputs = visibleOutputs;
  }
  // sendOutputHiddenState gets called each time DOM elements are shown or
  // hidden. This can be in the hundreds or thousands of times at startup.
  // We'll debounce it, so that we do the actual work once per tick.
  const sendOutputHiddenStateDebouncer = new Debouncer(
    null,
    doSendOutputHiddenState,
    0
  );

  function sendOutputHiddenState() {
    sendOutputHiddenStateDebouncer.normalCall();
  }
  // We need to make sure doSendOutputHiddenState actually gets called before
  // the inputBatchSender sends data to the server. The lastChanceCallback
  // here does that - if the debouncer has a pending call, flush it.
  inputBatchSender.lastChanceCallback.push(function () {
    if (sendOutputHiddenStateDebouncer.isPending())
      sendOutputHiddenStateDebouncer.immediateCall();
  });

  // Given a namespace and a handler function, return a function that invokes
  // the handler only when e's namespace matches. For example, if the
  // namespace is "bs", it would match when e.namespace is "bs" or "bs.tab".
  // If the namespace is "bs.tab", it would match for "bs.tab", but not "bs".
  function filterEventsByNamespace(namespace, handler, ...args) {
    namespace = namespace.split(".");

    return function (e) {
      const eventNamespace = e.namespace.split(".");

      // If any of the namespace strings aren't present in this event, quit.
      for (let i = 0; i < namespace.length; i++) {
        if (eventNamespace.indexOf(namespace[i]) === -1) return;
      }

      handler.apply(this, [namespace, handler, ...args]);
    };
  }

  // The size of each image may change either because the browser window was
  // resized, or because a tab was shown/hidden (hidden elements report size
  // of 0x0). It's OK to over-report sizes because the input pipeline will
  // filter out values that haven't changed.
  $(window).resize(debounce(500, sendImageSize));
  // Need to register callbacks for each Bootstrap 3 class.
  const bs3classes = [
    "modal",
    "dropdown",
    "tab",
    "tooltip",
    "popover",
    "collapse",
  ];

  $.each(bs3classes, function (idx, classname) {
    $(document.body).on(
      "shown.bs." + classname + ".sendImageSize",
      "*",
      filterEventsByNamespace("bs", sendImageSize)
    );
    $(document.body).on(
      "shown.bs." +
        classname +
        ".sendOutputHiddenState " +
        "hidden.bs." +
        classname +
        ".sendOutputHiddenState",
      "*",
      filterEventsByNamespace("bs", sendOutputHiddenState)
    );
  });

  // This is needed for Bootstrap 2 compatibility and for non-Bootstrap
  // related shown/hidden events (like conditionalPanel)
  $(document.body).on("shown.sendImageSize", "*", sendImageSize);
  $(document.body).on(
    "shown.sendOutputHiddenState hidden.sendOutputHiddenState",
    "*",
    sendOutputHiddenState
  );

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
  });

  $(window).on("popstate", function (e) {
    inputs.setInput(".clientdata_url_search", window.location.search);
  });

  // This is only the initial value of the hash. The hash can change, but
  // a reactive version of this isn't sent because watching for changes can
  // require polling on some browsers. The JQuery hashchange plugin can be
  // used if this capability is important.
  initialValues[".clientdata_url_hash_initial"] = window.location.hash;
  initialValues[".clientdata_url_hash"] = window.location.hash;

  $(window).on("hashchange", function (e) {
    inputs.setInput(".clientdata_url_hash", window.location.hash);
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

  window.console.log("Shiny version: ", Shiny.version);
} // function initShiny()

// Give any deferred iframes a chance to load.
function initDeferredIframes() {
  if (
    !window.Shiny ||
    !window.Shiny.shinyapp ||
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

export { sendImageSize, sendImageSize2, initShiny, initDeferredIframes };
