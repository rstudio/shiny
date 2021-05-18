/* eslint "@typescript-eslint/ban-ts-comment": 0 */
// @ts-nocheck
/* eslint "camelcase": 0 */
/* eslint "@typescript-eslint/no-unused-vars": 0 */
/* eslint "@typescript-eslint/no-this-alias": 0 */
/* eslint "@typescript-eslint/no-empty-function": 0 */
/* eslint "no-prototype-builtins": 0 */
/* eslint "prefer-const": 0 */
/* eslint "no-constant-condition": 0 */

import $ from "jquery";
jQuery = $;

import {
  escapeHTML,
  randomId,
  strToBool,
  getStyle,
  padZeros,
  roundSignif,
  parseDate,
  formatDateUTC,
  makeResizeFilter,
  pixelRatio,
  scopeExprToFunc,
  asArray,
  mergeSort,
  $escape,
  mapValues,
  isnan,
  _equal,
  equal,
  compareVersion,
  updateLabel,
  getComputedLinkColor,
  makeBlob,
  isBS3,
} from "./utils";

import { isQt, isIE, IEVersion } from "./utils/browser";

import { FileProcessor, FileUploader } from "./file/FileProcessor";

import { Shiny } from "./shiny";

import { inputBindings, outputBindings } from "./bindings";

function main(): void {
  // "_start.js"
  // √

  // "utils.js"
  // √

  // "browser.js"
  // √

  // "input_rate.js"
  // √; ./time/invoke.ts
  // √; ./time/debounce.ts
  // √; ./time/throttle.ts

  // √; ./inputPolicies/inputBatchSender.ts
  // √; ./inputPolicies/inputNoResendDecorator.ts
  // √; ./inputPolicies/inputEventDecorator.ts
  // √; ./inputPolicies/inputRateDecorator.ts
  // √; ./inputPolicies/inputDeferDecorator.ts
  // √; ./inputPolicies/inputValidateDecorator.ts

  // "shinyapp.js"
  // √; ./shiny/shinyapp.ts
  // √; ./shiny/reconnectDialog.ts

  // "notifications.js"
  // √; shiny/notifications.ts

  // "modal.js"
  // √; shiny/modal.ts

  // "file_processor.js"
  // √

  // "binding_registry.js"
  // √; ./bindings/registry.ts

  // √; ./bindings/input/index.ts
  // const inputBindings = (Shiny.inputBindings = new BindingRegistry());

  // √; ./bindings/output/index.ts
  // const outputBindings = (Shiny.outputBindings = new BindingRegistry());

  // "output_binding.js"
  // √; ./bindings/output/index.ts

  // "output_binding_text.js"
  // √; ./bindings/output/text.ts

  // "output_binding_image.js"
  // √; ./bindings/output/image.ts

  // √; ./imageutils/index.ts
  // √; ./imageutils/disableDrag.ts
  // √; ./imageutils/initPanelScales.ts
  // √; ./imageutils/initCoordmap.ts
  // √; ./imageutils/findbox.ts
  // √; ./imageutils/shiftToRange.ts
  // √; ./imageutils/createClickInfo.ts
  // √; ./imageutils/createHandlers.ts
  // √; ./imageutils/createBrush.ts
  // √; ./imageutils/resetBrush.ts

  // "output_binding_html.js"
  // √; ./bindings/output/html.ts

  const renderDependencies = (Shiny.renderDependencies = function (
    dependencies
  ) {
    if (dependencies) {
      $.each(dependencies, function (i, dep) {
        renderDependency(dep);
      });
    }
  });

  // Render HTML in a DOM element, add dependencies, and bind Shiny
  // inputs/outputs. `content` can be null, a string, or an object with
  // properties 'html' and 'deps'.
  Shiny.renderContent = function (el, content, where = "replace") {
    if (where === "replace") {
      Shiny.unbindAll(el);
    }

    let html;
    let dependencies = [];

    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (typeof content === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }

    Shiny.renderHtml(html, el, dependencies, where);

    let scope = el;

    if (where === "replace") {
      Shiny.initializeInputs(el);
      Shiny.bindAll(el);
    } else {
      const $parent = $(el).parent();

      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          const $grandparent = $parent.parent();

          if ($grandparent.length > 0) scope = $grandparent;
        }
      }
      Shiny.initializeInputs(scope);
      Shiny.bindAll(scope);
    }
  };

  // Render HTML in a DOM element, inserting singletons into head as needed
  Shiny.renderHtml = function (html, el, dependencies, where = "replace") {
    renderDependencies(dependencies);
    return singletons.renderHtml(html, el, where);
  };

  const htmlDependencies = {};

  function registerDependency(name, version) {
    htmlDependencies[name] = version;
  }

  // Re-render stylesheet(s) if the dependency has specificially requested it
  // and it matches an existing dependency (name and version)
  function needsRestyle(dep) {
    if (!dep.restyle) {
      return false;
    }
    const names = Object.keys(htmlDependencies);
    const idx = names.indexOf(dep.name);

    if (idx === -1) {
      return false;
    }
    return htmlDependencies[names[idx]] === dep.version;
  }

  // Client-side dependency resolution and rendering
  function renderDependency(dep) {
    const restyle = needsRestyle(dep);

    if (htmlDependencies.hasOwnProperty(dep.name) && !restyle) return false;

    registerDependency(dep.name, dep.version);

    const href = dep.src.href;

    const $head = $("head").first();

    if (dep.meta && !restyle) {
      const metas = $.map(asArray(dep.meta), function (obj, idx) {
        // only one named pair is expected in obj as it's already been decomposed
        const name = Object.keys(obj)[0];

        return $("<meta>").attr("name", name).attr("content", obj[name]);
      });

      $head.append(metas);
    }

    if (dep.stylesheet) {
      const links = $.map(asArray(dep.stylesheet), function (stylesheet) {
        return $("<link rel='stylesheet' type='text/css'>").attr(
          "href",
          href + "/" + encodeURI(stylesheet)
        );
      });

      if (!restyle) {
        $head.append(links);
      } else {
        // This inline <style> based approach works for IE11
        let refreshStyle = function (href, oldSheet) {
          const xhr = new XMLHttpRequest();

          xhr.open("GET", href);
          xhr.onload = function () {
            const id =
              "shiny_restyle_" + href.split("?restyle")[0].replace(/\W/g, "_");
            const oldStyle = $head.find("style#" + id);
            const newStyle = $("<style>").attr("id", id).html(xhr.responseText);

            $head.append(newStyle);

            // We can remove the old styles immediately because the new styles
            // should have been applied synchronously.
            oldStyle.remove();
            removeSheet(oldSheet);
            sendImageSize2();
          };
          xhr.send();
        };

        let findSheet = function (href) {
          for (let i = 0; i < document.styleSheets.length; i++) {
            const sheet = document.styleSheets[i];
            // The sheet's href is a full URL

            if (
              typeof sheet.href === "string" &&
              sheet.href.indexOf(href) > -1
            ) {
              return sheet;
            }
          }
          return null;
        };

        // Removes the stylesheet from document.styleSheets, and also removes
        // the owning <link> element, if present.
        let removeSheet = function (sheet) {
          if (!sheet) return;
          sheet.disabled = true;
          if (isIE()) sheet.cssText = "";
          $(sheet.ownerNode).remove();
        };

        $.map(links, function (link) {
          // Find any document.styleSheets that match this link's href
          // so we can remove it after bringing in the new stylesheet
          const oldSheet = findSheet(link.attr("href"));

          // Add a timestamp to the href to prevent caching
          const href = link.attr("href") + "?restyle=" + new Date().getTime();
          // Use inline <style> approach for IE, otherwise use the more elegant
          // <link> -based approach

          if (isIE()) {
            refreshStyle(href, oldSheet);
          } else {
            link.attr("href", href);

            // This part is a bit tricky. The link's onload callback will be
            // invoked after the file is loaded, but it can be _before_ the
            // styles are actually applied. The amount of time it takes for the
            // style to be applied is not predictable. We need to make sure the
            // styles are applied before we send updated size/style information
            // to the server.
            //
            // We do this by adding _another_ link, with CSS content
            // base64-encoded and inlined into the href. We also add a dummy DOM
            // element that the CSS applies to. The dummy CSS includes a
            // transition, and when the `transitionend` event happens, we call
            // sendImageSize2() and remove the old sheet. We also remove the
            // dummy DOM element and dummy CSS content.
            //
            // The reason this works is because (we assume) that if multiple
            // <link> tags are added, they will be applied in the same order
            // that they are loaded. This seems to be true in the browsers we
            // have tested.
            //
            // Because it is common for multiple stylesheets to arrive close
            // together, but not on exactly the same tick, we call
            // sendImageSize2(), which is debounced. Otherwise, it can result in
            // the same plot being redrawn multiple times with different
            // styling.
            link.attr("onload", () => {
              const dummy_id = "dummy-" + Math.floor(Math.random() * 999999999);
              const css_string =
                "#" +
                dummy_id +
                " { " +
                "color: #a7c920 !important; " + // An arbitrary color for the transition
                "transition: 0.1s all !important; " +
                "visibility: hidden !important; " +
                "position: absolute !important; " +
                "top: -1000px !important; " +
                "left: 0 !important; }";
              const base64_css_string =
                "data:text/css;base64," + btoa(css_string);

              let $dummy_link = $("<link rel='stylesheet' type='text/css' />");

              $dummy_link.attr("href", base64_css_string);

              let $dummy_el = $("<div id='" + dummy_id + "'></div>");

              $dummy_el.one("transitionend", () => {
                $dummy_el.remove();
                removeSheet(findSheet($dummy_link.attr("href")));
                removeSheet(oldSheet);
                sendImageSize2();
              });
              $(document.body).append($dummy_el);

              // Need to add the CSS with a setTimeout 0, to ensure that it
              // takes effect _after_ the DOM element has been added. This is
              // necessary to ensure that the transition actually occurs.
              setTimeout(() => $head.append($dummy_link), 0);
            });

            $head.append(link);
          }
        });
      }
    }

    if (dep.script && !restyle) {
      const scripts = $.map(asArray(dep.script), function (scriptName) {
        return $("<script>").attr("src", href + "/" + encodeURI(scriptName));
      });

      $head.append(scripts);
    }

    if (dep.attachment && !restyle) {
      // dep.attachment might be a single string, an array, or an object.
      let attachments = dep.attachment;

      if (typeof attachments === "string") attachments = [attachments];
      if ($.isArray(attachments)) {
        // The contract for attachments is that arrays of attachments are
        // addressed using 1-based indexes. Convert this array to an object.
        const tmp = {};

        $.each(attachments, function (index, attachment) {
          tmp[index + 1 + ""] = attachment;
        });
        attachments = tmp;
      }

      const attach = $.map(attachments, function (attachment, key) {
        return $("<link rel='attachment'>")
          .attr("id", dep.name + "-" + key + "-attachment")
          .attr("href", href + "/" + encodeURI(attachment));
      });

      $head.append(attach);
    }

    if (dep.head && !restyle) {
      const $newHead = $("<head></head>");

      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
    return true;
  }

  const singletons = {
    knownSingletons: {},
    renderHtml: function (html, el, where) {
      const processed = this._processHtml(html);

      this._addToHead(processed.head);
      this.register(processed.singletons);
      if (where === "replace") {
        $(el).html(processed.html);
      } else {
        el.insertAdjacentHTML(where, processed.html);
      }
      return processed;
    },
    // Take an object where keys are names of singletons, and merges it into
    // knownSingletons
    register: function (s) {
      $.extend(this.knownSingletons, s);
    },
    // Takes a string or array of strings and adds them to knownSingletons
    registerNames: function (s) {
      if (typeof s === "string") {
        this.knownSingletons[s] = true;
      } else if (s instanceof Array) {
        for (let i = 0; i < s.length; i++) {
          this.knownSingletons[s[i]] = true;
        }
      }
    },
    // Inserts new content into document head
    _addToHead: function (head) {
      if (head.length > 0) {
        const tempDiv = $("<div>" + head + "</div>")[0];
        const $head = $("head");

        while (tempDiv.hasChildNodes()) {
          $head.append(tempDiv.firstChild);
        }
      }
    },
    // Reads HTML and returns an object with info about singletons
    _processHtml: function (val) {
      const self = this;
      const newSingletons = {};
      let newVal;

      const findNewPayload = function (match, p1, sig, payload) {
        if (self.knownSingletons[sig] || newSingletons[sig]) return "";
        newSingletons[sig] = true;
        return payload;
      };

      while (true) {
        newVal = val.replace(self._reSingleton, findNewPayload);
        if (val.length === newVal.length) break;
        val = newVal;
      }

      const heads = [];
      const headAddPayload = function (match, payload) {
        heads.push(payload);
        return "";
      };

      while (true) {
        newVal = val.replace(self._reHead, headAddPayload);
        if (val.length === newVal.length) break;
        val = newVal;
      }

      return {
        html: val,
        head: heads.join("\n"),
        singletons: newSingletons,
      };
    },
    _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
    _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/,
  };

  // "output_binding_downloadlink.js"
  // √; ./bindings/output/downloadlink.ts

  // "output_binding_datatable.js"
  // √; ./bindings/output/datatable.ts

  // "output_binding_adapter.js"
  // √; ./bindings/output_adapter.ts

  // "input_binding.js"
  // √; ./bindings/input/index.ts

  // "input_binding_text.js"
  // √; ./bindings/input/text.ts

  // "input_binding_textarea.js"
  // √; ./bindings/input/textarea.ts

  // "input_binding_password.js"
  // √; ./bindings/input/password.ts

  // "input_binding_number.js"
  // √; ./bindings/input/number.ts

  // "input_binding_checkbox.js"
  // √; ./bindings/input/checkbox.ts

  // "input_binding_slider.js"
  // √; ./bindings/input/slider.ts

  // "input_binding_date.js"
  // √; ./bindings/input/date.ts

  // "input_binding_daterange.js"
  // √; ./bindings/input/daterange.ts

  // "input_binding_select.js"

  // "input_binding_radio.js"
  // √; ./bindings/input/radio.ts

  // "input_binding_checkboxgroup.js"
  // √; ./bindings/input/checkboxgroup.ts

  // "input_binding_actionbutton.js"
  // √; ./bindings/input/actionbutton.ts

  // "input_binding_tabinput.js"
  // √; ./bindings/input/tabinput.ts

  // "input_binding_fileinput.js"
  // √; ./bindings/input/fileinput.ts

  // This function gets defined in initShiny() and 'hoisted' so it can be reused
  // (to send CSS info) inside of Shiny.renderDependencies()
  let sendImageSize;
  let sendImageSize2;

  // "init_shiny.js"
  function initShiny() {
    const shinyapp = (Shiny.shinyapp = new ShinyApp());

    Shiny.addCustomMessageHandler = shinyapp.addCustomMessageHandler;

    function bindOutputs(scope = document) {
      scope = $(scope);

      const bindings = outputBindings.getBindings();

      for (let i = 0; i < bindings.length; i++) {
        const binding = bindings[i].binding;
        const matches = binding.find(scope) || [];

        for (let j = 0; j < matches.length; j++) {
          const el = matches[j];
          const id = binding.getId(el);

          // Check if ID is falsy
          if (!id) continue;

          // In some uncommon cases, elements that are later in the
          // matches array can be removed from the document by earlier
          // iterations. See https://github.com/rstudio/shiny/issues/1399
          if (!$.contains(document, el)) continue;

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

          shinyapp.bindOutput(id, bindingAdapter);
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
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

    function unbindOutputs(scope = document, includeSelf = false) {
      const outputs = $(scope).find(".shiny-bound-output");

      if (includeSelf && $(scope).hasClass("shiny-bound-output")) {
        outputs.push(scope);
      }

      for (let i = 0; i < outputs.length; i++) {
        const $el = $(outputs[i]);
        const bindingAdapter = $el.data("shiny-output-binding");

        if (!bindingAdapter) continue;
        const id = bindingAdapter.binding.getId(outputs[i]);

        shinyapp.unbindOutput(id, bindingAdapter);
        $el.removeClass("shiny-bound-output");
        $el.removeData("shiny-output-binding");
        $el.trigger({
          type: "shiny:unbound",
          binding: bindingAdapter.binding,
          bindingType: "output",
        });
      }

      // Send later in case DOM layout isn't final yet.
      setTimeout(sendImageSize, 0);
      setTimeout(sendOutputHiddenState, 0);
    }

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

    const boundInputs = {};

    // todo make sure allowDeferred can NOT be supplied and still work
    function valueChangeCallback(binding, el, allowDeferred) {
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

    function bindInputs(scope = document) {
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
              valueChangeCallback(thisBinding, thisEl, allowDeferred);
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

    function unbindInputs(scope = document, includeSelf = false) {
      const inputs = $(scope).find(".shiny-bound-input");

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
        binding.unsubscribe(el);
        $(el).trigger({
          type: "shiny:unbound",
          binding: binding,
          bindingType: "input",
        });
      }
    }

    function _bindAll(scope) {
      bindOutputs(scope);
      return bindInputs(scope);
    }
    function unbindAll(scope, includeSelf = false) {
      unbindInputs(scope, includeSelf);
      unbindOutputs(scope, includeSelf);
    }
    Shiny.bindAll = function (scope) {
      // _bindAll returns input values; it doesn't send them to the server.
      // export.bindAll needs to send the values to the server.
      const currentInputItems = _bindAll(scope);

      $.each(currentInputItems, function (name, item) {
        inputs.setInput(name, item.value, item.opts);
      });

      // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
      // is a convenient place to hang it. bindAll will be called anytime new HTML
      // appears that might contain inputs/outputs; it's reasonable to assume that
      // any such HTML may contain iframes as well.
      initDeferredIframes();
    };
    Shiny.unbindAll = unbindAll;

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
          initialValues[
            ".clientdata_output_" + id + "_width"
          ] = this.offsetWidth;
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
        initialValues[".clientdata_output_" + id + "_fg"] = getStyle(
          el,
          "color"
        );
        initialValues[
          ".clientdata_output_" + id + "_accent"
        ] = getComputedLinkColor(el);
        initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(
          el
        );
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
      const observer = new MutationObserver(() =>
        observerCallback.normalCall()
      );
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

      inputs.setInput(
        ".clientdata_output_" + id + "_bg",
        getComputedBgColor(el)
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_fg",
        getStyle(el, "color")
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_accent",
        getComputedLinkColor(el)
      );
      inputs.setInput(
        ".clientdata_output_" + id + "_font",
        getComputedFont(el)
      );
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

    singletons.registerNames(singletonText.split(/,/));

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

  $(function () {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(initShiny, 1);
  });

  // "reactlog.js"
  // √; ./shiny/reactlog.ts

  // "_end.js"
  // √
}

export { main };
