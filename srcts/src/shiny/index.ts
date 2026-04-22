import $ from "jquery";

import { InputBinding, OutputBinding } from "../bindings";
import { initInputBindings } from "../bindings/input";
import { initOutputBindings } from "../bindings/output";
import type { BindingRegistry } from "../bindings/registry";
import { showErrorInClientConsole } from "../components/errorConsole";
import { resetBrush } from "../imageutils/resetBrush";
import type { InputPolicy } from "../inputPolicies";
import {
  InputBatchSender,
  InputDeferDecorator,
  InputEventDecorator,
  InputNoResendDecorator,
  InputRateDecorator,
  InputValidateDecorator,
} from "../inputPolicies";
import type { InputPolicyOpts } from "../inputPolicies/inputPolicy";
import { addDefaultInputOpts } from "../inputPolicies/inputValidateDecorator";
import {
  $escape,
  compareVersion,
  getBoundingClientSizeBeforeZoom,
  getComputedLinkColor,
  getStyle,
  isHidden,
  isShinyInDevMode,
  mapValues,
  pixelRatio,
} from "../utils";
import { createInitStatus, type InitStatusPromise } from "../utils/promise";
import type { BindInputsCtx, BindScope } from "./bind";
import { _bindAll, bindAll, unbindAll } from "./bind";
import type {
  shinyBindAll,
  shinyForgetLastInputValue,
  shinyInitializeInputs,
  shinySetInputValue,
  shinyUnbindAll,
} from "./initedMethods";
import { setFileInputBinding, setShinyObj } from "./initedMethods";
import { removeModal, showModal } from "./modal";
import { removeNotification, showNotification } from "./notifications";
import {
  handleVisualChange,
  shouldObserveThemeMutations,
} from "./outputInfoObserver";
import { hideReconnectDialog, showReconnectDialog } from "./reconnectDialog";
import {
  registerDependency,
  renderContent,
  renderContentAsync,
  renderDependencies,
  renderDependenciesAsync,
  renderHtml,
  renderHtmlAsync,
} from "./render";
import { sendOutputInfoFns } from "./sendOutputInfo";
import { addCustomMessageHandler, ShinyApp, type Handler } from "./shinyapp";
import { registerNames as singletonsRegisterNames } from "./singletons";

class ShinyClass {
  version: string;
  $escape: typeof $escape;
  compareVersion: typeof compareVersion;
  inputBindings: BindingRegistry<InputBinding>;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  InputBinding: typeof InputBinding;
  outputBindings: BindingRegistry<OutputBinding>;
  // eslint-disable-next-line @typescript-eslint/naming-convention
  OutputBinding: typeof OutputBinding;
  resetBrush: typeof resetBrush;
  notifications: {
    show: typeof showNotification;
    remove: typeof removeNotification;
  };
  modal: { show: typeof showModal; remove: typeof removeModal };
  showReconnectDialog: typeof showReconnectDialog;
  hideReconnectDialog: typeof hideReconnectDialog;
  renderDependenciesAsync: typeof renderDependenciesAsync;
  renderDependencies: typeof renderDependencies;
  renderContentAsync: typeof renderContentAsync;
  renderContent: typeof renderContent;
  renderHtmlAsync: typeof renderHtmlAsync;
  renderHtml: typeof renderHtml;
  addCustomMessageHandler: typeof addCustomMessageHandler;

  // The following are added in the initialization, by initShiny()
  createSocket?: () => WebSocket;
  user?: string;
  progressHandlers?: ShinyApp["progressHandlers"];
  shinyapp?: ShinyApp;
  setInputValue?: typeof shinySetInputValue;
  onInputChange?: typeof shinySetInputValue;
  forgetLastInputValue?: typeof shinyForgetLastInputValue;
  bindAll?: typeof shinyBindAll;
  unbindAll?: typeof shinyUnbindAll;
  initializeInputs?: typeof shinyInitializeInputs;

  // Promise-like object that is resolved after initialization.
  initializedPromise: InitStatusPromise<void>;

  // Eventually deprecate
  // For old-style custom messages - should deprecate and migrate to new
  oncustommessage?: Handler;

  constructor() {
    // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
    // During testing, the `Shiny.version` will be `"development"`
    this.version = process.env.SHINY_VERSION || "development";

    const { inputBindings, fileInputBinding } = initInputBindings();
    const { outputBindings } = initOutputBindings();

    setFileInputBinding(fileInputBinding);

    this.$escape = $escape;
    this.compareVersion = compareVersion;
    this.inputBindings = inputBindings;
    this.InputBinding = InputBinding;
    this.outputBindings = outputBindings;
    this.OutputBinding = OutputBinding;
    this.resetBrush = resetBrush;
    this.notifications = {
      show: showNotification,
      remove: removeNotification,
    };
    this.modal = { show: showModal, remove: removeModal };

    this.addCustomMessageHandler = addCustomMessageHandler;
    this.showReconnectDialog = showReconnectDialog;
    this.hideReconnectDialog = hideReconnectDialog;
    this.renderDependenciesAsync = renderDependenciesAsync;
    this.renderDependencies = renderDependencies;
    this.renderContentAsync = renderContentAsync;
    this.renderContent = renderContent;
    this.renderHtmlAsync = renderHtmlAsync;
    this.renderHtml = renderHtml;

    this.initializedPromise = createInitStatus<void>();

    $(() => {
      // Init Shiny a little later than document ready, so user code can
      // run first (i.e. to register bindings)
      setTimeout(async () => {
        try {
          await this.initialize();
        } catch (e) {
          showErrorInClientConsole(e);
          throw e;
        }
      }, 1);
    });
  }

  /**
   * Method to check if Shiny is running in development mode. By packaging as a
   * method, we can we can avoid needing to look for the `__SHINY_DEV_MODE__`
   * variable in the global scope.
   * @returns `true` if Shiny is running in development mode, `false` otherwise.
   */
  inDevMode(): boolean {
    return isShinyInDevMode();
  }

  async initialize(): Promise<void> {
    setShinyObj(this);
    this.shinyapp = new ShinyApp();
    const shinyapp = this.shinyapp;

    this.progressHandlers = shinyapp.progressHandlers;

    const inputBatchSender = new InputBatchSender(shinyapp);
    const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
    const inputsEvent = new InputEventDecorator(inputsNoResend);
    const inputsRate = new InputRateDecorator(inputsEvent);
    const inputsDefer = new InputDeferDecorator(inputsEvent);

    let target: InputPolicy;

    if (document.querySelector(".shiny-submit-button")) {
      // If there is a submit button on the page, use defer decorator
      target = inputsDefer;

      document.querySelectorAll(".shiny-submit-button").forEach(function (x) {
        x.addEventListener("click", function (event) {
          event.preventDefault();
          inputsDefer.submit();
        });
      });
    } else {
      // By default, use rate decorator
      target = inputsRate;
    }

    const inputs = new InputValidateDecorator(target);

    this.setInputValue = this.onInputChange = function (
      name: string,
      value: unknown,
      opts: Partial<InputPolicyOpts> = {},
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
    this.forgetLastInputValue = function (name) {
      inputsNoResend.forget(name);
    };

    // MUST be called after `setShiny()`
    const inputBindings = this.inputBindings;
    const outputBindings = this.outputBindings;

    const shinyBindCtx = (): BindInputsCtx => {
      return {
        inputs,
        inputsRate,
        inputBindings,
        outputBindings,
        initDeferredIframes,
        outputIsRecalculating: (id: string) =>
          this.shinyapp?.$outputProgress.isRecalculating(id) ?? false,
      };
    };

    this.bindAll = async function (scope: BindScope) {
      await bindAll(shinyBindCtx(), scope);
    };
    this.unbindAll = function (scope: BindScope, includeSelf = false) {
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
    this.initializeInputs = initializeInputs;

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
      await _bindAll(shinyBindCtx(), document.documentElement),
      (x) => x.value,
    );

    function setInput(name: string, value: unknown, initial = false): void {
      if (initial) {
        initialValues[name] = value;
      } else {
        inputs.setInput(name, value);
      }
    }

    function doSendSize(el: HTMLElement, initial = false): void {
      const id = getIdFromEl(el);
      const rect = getBoundingClientSizeBeforeZoom(el);

      if (rect.width !== 0 || rect.height !== 0) {
        setInput(".clientdata_output_" + id + "_width", rect.width, initial);
        setInput(".clientdata_output_" + id + "_height", rect.height, initial);
      }
    }

    function doTriggerResize(el: HTMLElement): void {
      const $el = $(el),
        binding = $el.data("shiny-output-binding");

      if (!binding) return;

      $el.trigger({
        type: "shiny:visualchange",
        // @ts-expect-error; Can not remove info on a established, malformed Event object
        visible: !isHidden(el),
        binding: binding,
      });
      binding.onResize();
    }

    function doSendTheme(el: HTMLElement, initial = false): void {
      if (el.classList.contains("shiny-output-error")) {
        return;
      }

      function getComputedBgColor(
        el: HTMLElement | null,
      ): string | null | undefined {
        if (!el) {
          return null;
        }

        const bgColor = getStyle(el, "background-color");

        if (!bgColor) return bgColor;
        const m = bgColor.match(
          /^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/,
        );

        if (bgColor === "transparent" || (m && parseFloat(m[4]) === 0)) {
          const bgImage = getStyle(el, "background-image");

          if (bgImage && bgImage !== "none") {
            return null;
          } else {
            return getComputedBgColor(el.parentElement);
          }
        }
        return bgColor;
      }

      function getComputedFont(el: HTMLElement): {
        families: string[] | undefined;
        size: string | undefined;
      } {
        const fontFamily = getStyle(el, "font-family");
        const fontSize = getStyle(el, "font-size");

        return {
          families: fontFamily?.replace(/"/g, "").split(", "),
          size: fontSize,
        };
      }

      const id = getIdFromEl(el);

      setInput(
        ".clientdata_output_" + id + "_bg",
        getComputedBgColor(el),
        initial,
      );
      setInput(
        ".clientdata_output_" + id + "_fg",
        getStyle(el, "color"),
        initial,
      );
      setInput(
        ".clientdata_output_" + id + "_accent",
        getComputedLinkColor(el),
        initial,
      );
      setInput(
        ".clientdata_output_" + id + "_font",
        getComputedFont(el),
        initial,
      );
    }

    const visibleOutputs = new Set<string | null>();

    function doSendHiddenState(el: HTMLElement, initial = false): void {
      const id = getIdFromEl(el);
      const hidden = isHidden(el);

      if (hidden) {
        visibleOutputs.delete(id);
      } else {
        visibleOutputs.add(id);
      }
      setInput(".clientdata_output_" + id + "_hidden", hidden, initial);
    }

    function reportsSize(el: HTMLElement): boolean {
      return (
        el.classList.contains("shiny-image-output") ||
        el.classList.contains("shiny-plot-output") ||
        el.classList.contains("shiny-report-size")
      );
    }

    function reportsTheme(el: HTMLElement): boolean {
      return (
        el.classList.contains("shiny-image-output") ||
        el.classList.contains("shiny-plot-output") ||
        el.classList.contains("shiny-report-theme")
      );
    }

    function ensureObservers(el: HTMLElement): void {
      if (!$(el).data("shiny-resize-observer")) {
        const onResize = sendOutputInfoFns.createObserverCallback(100, () => {
          handleVisualChange(el, {
            doTriggerResize,
            doSendHiddenState,
            doSendSize,
            doSendTheme,
            reportsSize,
            reportsTheme,
          });
        });
        const ro = new ResizeObserver(() => onResize());

        ro.observe(el);
        $(el).data("shiny-resize-observer-callback", onResize);
        $(el).data("shiny-resize-observer", ro);
      }

      if (!$(el).data("shiny-intersection-observer")) {
        const onIntersect = sendOutputInfoFns.createObserverCallback(
          100,
          () => {
            handleVisualChange(el, {
              doTriggerResize,
              doSendHiddenState,
              doSendSize,
              doSendTheme,
              reportsSize,
              reportsTheme,
            });
          },
        );
        const io = new IntersectionObserver(() => onIntersect());

        io.observe(el);
        $(el).data("shiny-intersection-observer-callback", onIntersect);
        $(el).data("shiny-intersection-observer", io);
      }

      if (
        shouldObserveThemeMutations(reportsTheme(el)) &&
        !$(el).data("shiny-mutate-observer")
      ) {
        const onMutate = sendOutputInfoFns.createObserverCallback(100, () => {
          if (reportsTheme(el)) {
            doSendTheme(el);
          }
        });
        const mo = new MutationObserver(() => onMutate());

        mo.observe(el, {
          attributes: true,
          attributeFilter: ["style", "class"],
        });

        $(el).data("shiny-mutate-observer", mo);
        $(el).data("shiny-mutate-observer-callback", onMutate);
      }
    }

    function doSendOutputInfo(initial = false) {
      const outputIds = new Set<string | null>();

      $(".shiny-bound-output").each(function () {
        // eslint-disable-next-line @typescript-eslint/no-this-alias
        const el = this;

        outputIds.add(getIdFromEl(el));
        ensureObservers(el);

        doTriggerResize(el);
        doSendHiddenState(el, initial);
        if (reportsSize(el)) {
          doSendSize(el, initial);
        }
        if (reportsTheme(el)) {
          doSendTheme(el, initial);
        }
      });

      visibleOutputs.forEach((id) => {
        if (!outputIds.has(id)) {
          visibleOutputs.delete(id);
          setInput(".clientdata_output_" + id + "_hidden", true, initial);
        }
      });
    }

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
      e; // eslint-disable-line @typescript-eslint/no-unused-expressions
    });

    $(window).on("popstate", function (e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
      return;
      e; // eslint-disable-line @typescript-eslint/no-unused-expressions
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
      e; // eslint-disable-line @typescript-eslint/no-unused-expressions
    });

    // The server needs to know what singletons were rendered as part of
    // the page loading
    const singletonText = (initialValues[".clientdata_singletons"] = $(
      'script[type="application/shiny-singletons"]',
    ).text());

    singletonsRegisterNames(singletonText.split(/,/));

    const dependencyText = $(
      'script[type="application/html-dependencies"]',
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
    $(document).one("shiny:connected", () => {
      initDeferredIframes();
    });

    $(document).one("shiny:sessioninitialized", () => {
      this.initializedPromise.resolve();
    });
  }
}

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
    // @ts-expect-error; If it is undefined, set using the undefined value
    $el.attr("src", $el.attr("data-deferred-src"));
    $el.attr("data-deferred-src", null);
  });
}

export { ShinyClass };
