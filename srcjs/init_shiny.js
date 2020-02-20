function initShiny() {

  var shinyapp = exports.shinyapp = new ShinyApp();

  function bindOutputs(scope = document) {
    scope = $(scope);

    var bindings = outputBindings.getBindings();

    for (var i = 0; i < bindings.length; i++) {
      var binding = bindings[i].binding;
      var matches = binding.find(scope) || [];
      for (var j = 0; j < matches.length; j++) {
        var el = matches[j];
        var id = binding.getId(el);

        // Check if ID is falsy
        if (!id)
          continue;

        // In some uncommon cases, elements that are later in the
        // matches array can be removed from the document by earlier
        // iterations. See https://github.com/rstudio/shiny/issues/1399
        if (!$.contains(document, el))
          continue;

        var $el = $(el);
        if ($el.hasClass('shiny-bound-output')) {
          // Already bound; can happen with nested uiOutput (bindAll
          // gets called on two ancestors)
          continue;
        }

        var bindingAdapter = new OutputBindingAdapter(el, binding);
        shinyapp.bindOutput(id, bindingAdapter);
        $el.data('shiny-output-binding', bindingAdapter);
        $el.addClass('shiny-bound-output');
        $el.trigger({
          type: 'shiny:bound',
          binding: binding,
          bindingType: 'output'
        });
      }
    }

    // Send later in case DOM layout isn't final yet.
    setTimeout(sendImageSize, 0);
    setTimeout(sendOutputHiddenState, 0);
  }

  function unbindOutputs(scope = document, includeSelf = false) {
    var outputs = $(scope).find('.shiny-bound-output');

    if (includeSelf && $(scope).hasClass('shiny-bound-output')) {
      outputs.push(scope);
    }

    for (var i = 0; i < outputs.length; i++) {
      var $el = $(outputs[i]);
      var bindingAdapter = $el.data('shiny-output-binding');
      if (!bindingAdapter)
        continue;
      var id = bindingAdapter.binding.getId(outputs[i]);
      shinyapp.unbindOutput(id, bindingAdapter);
      $el.removeClass('shiny-bound-output');
      $el.removeData('shiny-output-binding');
      $el.trigger({
        type: 'shiny:unbound',
        binding: bindingAdapter.binding,
        bindingType: 'output'
      });
    }

    // Send later in case DOM layout isn't final yet.
    setTimeout(sendImageSize, 0);
    setTimeout(sendOutputHiddenState, 0);
  }

  var inputBatchSender = new InputBatchSender(shinyapp);
  var inputsNoResend = new InputNoResendDecorator(inputBatchSender);
  var inputsEvent = new InputEventDecorator(inputsNoResend);
  var inputsRate = new InputRateDecorator(inputsEvent);
  var inputsDefer = new InputDeferDecorator(inputsEvent);

  var inputs;
  if ($('input[type="submit"], button[type="submit"]').length > 0) {
    // If there is a submit button on the page, use defer decorator
    inputs = inputsDefer;

    $('input[type="submit"], button[type="submit"]').each(function() {
      $(this).click(function(event) {
        event.preventDefault();
        inputsDefer.submit();
      });
    });

  } else {
    // By default, use rate decorator
    inputs = inputsRate;
  }

  inputs = new InputValidateDecorator(inputs);

  exports.setInputValue = exports.onInputChange = function(name, value, opts) {
    opts = addDefaultInputOpts(opts);
    inputs.setInput(name, value, opts);
  };

  var boundInputs = {};

  function valueChangeCallback(binding, el, allowDeferred) {
    var id = binding.getId(el);
    if (id) {
      var value = binding.getValue(el);
      var type = binding.getType(el);
      if (type)
        id = id + ":" + type;

      let opts = {
        priority: allowDeferred ? "deferred" : "immediate",
        binding: binding,
        el: el
      };
      inputs.setInput(id, value, opts);
    }
  }

  function bindInputs(scope = document) {
    var bindings = inputBindings.getBindings();

    var inputItems = {};

    for (var i = 0; i < bindings.length; i++) {
      var binding = bindings[i].binding;
      var matches = binding.find(scope) || [];
      for (var j = 0; j < matches.length; j++) {
        var el = matches[j];
        var id = binding.getId(el);

        // Check if ID is falsy, or if already bound
        if (!id || boundInputs[id])
          continue;

        var type = binding.getType(el);
        var effectiveId = type ? id + ":" + type : id;
        inputItems[effectiveId] = {
          value: binding.getValue(el),
          opts: {
            immediate: true,
            binding: binding,
            el: el
          }
        };

        /*jshint loopfunc:true*/
        var thisCallback = (function() {
          var thisBinding = binding;
          var thisEl = el;
          return function(allowDeferred) {
            valueChangeCallback(thisBinding, thisEl, allowDeferred);
          };
        })();

        binding.subscribe(el, thisCallback);
        $(el).data('shiny-input-binding', binding);
        $(el).addClass('shiny-bound-input');
        var ratePolicy = binding.getRatePolicy(el);
        if (ratePolicy !== null) {
          inputsRate.setRatePolicy(
            effectiveId,
            ratePolicy.policy,
            ratePolicy.delay);
        }

        boundInputs[id] = {
          binding: binding,
          node: el
        };

        $(el).trigger({
          type: 'shiny:bound',
          binding: binding,
          bindingType: 'input'
        });
      }
    }

    return inputItems;
  }

  function unbindInputs(scope = document, includeSelf = false) {
    var inputs = $(scope).find('.shiny-bound-input');

    if (includeSelf && $(scope).hasClass('shiny-bound-input')) {
      inputs.push(scope);
    }

    for (var i = 0; i < inputs.length; i++) {
      var el = inputs[i];
      var binding = $(el).data('shiny-input-binding');
      if (!binding)
        continue;
      var id = binding.getId(el);
      $(el).removeClass('shiny-bound-input');
      delete boundInputs[id];
      binding.unsubscribe(el);
      $(el).trigger({
        type: 'shiny:unbound',
        binding: binding,
        bindingType: 'input'
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
  exports.bindAll = function(scope) {
    // _bindAll returns input values; it doesn't send them to the server.
    // export.bindAll needs to send the values to the server.
    var currentInputItems = _bindAll(scope);
    $.each(currentInputItems, function(name, item) {
      inputs.setInput(name, item.value, item.opts);
    });

    // Not sure if the iframe stuff is an intrinsic part of bindAll, but bindAll
    // is a convenient place to hang it. bindAll will be called anytime new HTML
    // appears that might contain inputs/outputs; it's reasonable to assume that
    // any such HTML may contain iframes as well.
    initDeferredIframes();
  };
  exports.unbindAll = unbindAll;

  // Calls .initialize() for all of the input objects in all input bindings,
  // in the given scope.
  function initializeInputs(scope = document) {
    var bindings = inputBindings.getBindings();

    // Iterate over all bindings
    for (var i = 0; i < bindings.length; i++) {
      var binding = bindings[i].binding;
      var inputObjects = binding.find(scope) || [];

      // Iterate over all input objects for this binding
      for (var j = 0; j < inputObjects.length; j++) {
        if (!inputObjects[j]._shiny_initialized) {
          inputObjects[j]._shiny_initialized = true;
          binding.initialize(inputObjects[j]);
        }
      }
    }
  }
  exports.initializeInputs = initializeInputs;

  function getIdFromEl(el) {
    var $el = $(el);
    var bindingAdapter = $el.data("shiny-output-binding");
    if (!bindingAdapter)
      return null;
    else
      return bindingAdapter.getId();
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
  var initialValues = mapValues(_bindAll(document), x => x.value);

  // The server needs to know the size of each image and plot output element,
  // in case it is auto-sizing
  $('.shiny-image-output, .shiny-plot-output, .shiny-report-size').each(function() {
    var id = getIdFromEl(this);
    if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
      initialValues['.clientdata_output_' + id + '_width'] = this.offsetWidth;
      initialValues['.clientdata_output_' + id + '_height'] = this.offsetHeight;
    }
  });

  function getComputedBgColor(el) {
    if (!el) {
      // Top of document, can't recurse further
      return null;
    }

    let props = window.getComputedStyle(el);
    let bgColor = props.getPropertyValue("background-color");
    let m = bgColor.match(/^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/);
    if (bgColor === "transparent" || (m && parseFloat(m[4]) === 0)) {
      // No background color on this element. See if it has a background image.
      let bgImage = props.getPropertyValue("background-image");
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

  // Compute the color property of an a tag, scoped within the element
  function getComputedLinkColor(el) {
    let a = document.createElement("a");
    a.href = "/";
    let div = document.createElement("div");
    div.style.position = "absolute";
    div.style.setProperty("position", "absolute", "important");
    div.style.setProperty("top", "-1000px", "important");
    div.style.setProperty("left", "0", "important");
    div.style.setProperty("width", "30px", "important");
    div.style.setProperty("height", "10px", "important");
    div.appendChild(a);
    el.appendChild(div);
    let linkColor = window.getComputedStyle(a).getPropertyValue("color");
    el.removeChild(div);
    return linkColor;
  }

  $('.shiny-image-output, .shiny-plot-output, .shiny-report-theme').each(function() {
    var id = getIdFromEl(this);
    initialValues['.clientdata_output_' + id + '_bg'] = getComputedBgColor(this);
    initialValues['.clientdata_output_' + id + '_fg'] = window.getComputedStyle(this).getPropertyValue("color");
    initialValues['.clientdata_output_' + id + '_font_family'] = window.getComputedStyle(this).getPropertyValue("font-family");
    initialValues['.clientdata_output_' + id + '_accent'] = getComputedLinkColor(this);
  });

  function doSendImageSize() {
    $('.shiny-image-output, .shiny-plot-output, .shiny-report-size').each(function() {
      var id = getIdFromEl(this);
      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        inputs.setInput('.clientdata_output_' + id + '_width', this.offsetWidth);
        inputs.setInput('.clientdata_output_' + id + '_height', this.offsetHeight);
      }
    });

    $('.shiny-image-output, .shiny-plot-output, .shiny-report-theme').each(function() {
      var id = getIdFromEl(this);
      inputs.setInput('.clientdata_output_' + id + '_bg', getComputedBgColor(this));
      inputs.setInput('.clientdata_output_' + id + '_fg', window.getComputedStyle(this).getPropertyValue("color"));
      inputs.setInput('.clientdata_output_' + id + '_font_family', window.getComputedStyle(this).getPropertyValue("font-family"));
      inputs.setInput('.clientdata_output_' + id + '_accent', getComputedLinkColor(this));
    });

    $('.shiny-bound-output').each(function() {
      var $this = $(this), binding = $this.data('shiny-output-binding');
      $this.trigger({
        type: 'shiny:visualchange',
        visible: !isHidden(this),
        binding: binding
      });
      binding.onResize();
    });
  }
  var sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
  function sendImageSize() {
    sendImageSizeDebouncer.normalCall();
  }
  // Make sure sendImageSize actually gets called before the inputBatchSender
  // sends data to the server.
  inputBatchSender.lastChanceCallback.push(function() {
    if (sendImageSizeDebouncer.isPending())
      sendImageSizeDebouncer.immediateCall();
  });

  // Return true if the object or one of its ancestors in the DOM tree has
  // style='display:none'; otherwise return false.
  function isHidden(obj) {
    // null means we've hit the top of the tree. If width or height is
    // non-zero, then we know that no ancestor has display:none.
    if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
      return false;
    } else if (getStyle(obj, 'display') === 'none') {
      return true;
    } else {
      return(isHidden(obj.parentNode));
    }
  }
  var lastKnownVisibleOutputs = {};
  // Set initial state of outputs to hidden, if needed
  $('.shiny-bound-output').each(function() {
    var id = getIdFromEl(this);
    if (isHidden(this)) {
      initialValues['.clientdata_output_' + id + '_hidden'] = true;
    } else {
      lastKnownVisibleOutputs[id] = true;
      initialValues['.clientdata_output_' + id + '_hidden'] = false;
    }
  });
  // Send update when hidden state changes
  function doSendOutputHiddenState() {
    var visibleOutputs = {};
    $('.shiny-bound-output').each(function() {
      var id = getIdFromEl(this);
      delete lastKnownVisibleOutputs[id];
      // Assume that the object is hidden when width and height are 0
      var hidden = isHidden(this), evt = {
        type: 'shiny:visualchange',
        visible: !hidden
      };
      if (hidden) {
        inputs.setInput('.clientdata_output_' + id + '_hidden', true);
      } else {
        visibleOutputs[id] = true;
        inputs.setInput('.clientdata_output_' + id + '_hidden', false);
      }
      var $this = $(this);
      evt.binding = $this.data('shiny-output-binding');
      $this.trigger(evt);
    });
    // Anything left in lastKnownVisibleOutputs is orphaned
    for (var name in lastKnownVisibleOutputs) {
      if (lastKnownVisibleOutputs.hasOwnProperty(name))
        inputs.setInput('.clientdata_output_' + name + '_hidden', true);
    }
    // Update the visible outputs for next time
    lastKnownVisibleOutputs = visibleOutputs;
  }
  // sendOutputHiddenState gets called each time DOM elements are shown or
  // hidden. This can be in the hundreds or thousands of times at startup.
  // We'll debounce it, so that we do the actual work once per tick.
  var sendOutputHiddenStateDebouncer = new Debouncer(null, doSendOutputHiddenState, 0);
  function sendOutputHiddenState() {
    sendOutputHiddenStateDebouncer.normalCall();
  }
  // We need to make sure doSendOutputHiddenState actually gets called before
  // the inputBatchSender sends data to the server. The lastChanceCallback
  // here does that - if the debouncer has a pending call, flush it.
  inputBatchSender.lastChanceCallback.push(function() {
    if (sendOutputHiddenStateDebouncer.isPending())
      sendOutputHiddenStateDebouncer.immediateCall();
  });

  // Given a namespace and a handler function, return a function that invokes
  // the handler only when e's namespace matches. For example, if the
  // namespace is "bs", it would match when e.namespace is "bs" or "bs.tab".
  // If the namespace is "bs.tab", it would match for "bs.tab", but not "bs".
  function filterEventsByNamespace(namespace, handler) {
    namespace = namespace.split(".");

    return function(e) {
      var eventNamespace = e.namespace.split(".");

      // If any of the namespace strings aren't present in this event, quit.
      for (var i=0; i<namespace.length; i++) {
        if (eventNamespace.indexOf(namespace[i]) === -1)
          return;
      }

      handler.apply(this, arguments);
    };
  }

  // The size of each image may change either because the browser window was
  // resized, or because a tab was shown/hidden (hidden elements report size
  // of 0x0). It's OK to over-report sizes because the input pipeline will
  // filter out values that haven't changed.
  $(window).resize(debounce(500, sendImageSize));
  // Need to register callbacks for each Bootstrap 3 class.
  var bs3classes = ['modal', 'dropdown', 'tab', 'tooltip', 'popover', 'collapse'];
  $.each(bs3classes, function(idx, classname) {
    $(document.body).on('shown.bs.' + classname + '.sendImageSize', '*',
      filterEventsByNamespace('bs', sendImageSize));
    $(document.body).on('shown.bs.' + classname + '.sendOutputHiddenState ' +
                 'hidden.bs.' + classname + '.sendOutputHiddenState',
                 '*', filterEventsByNamespace('bs', sendOutputHiddenState));
  });

  // This is needed for Bootstrap 2 compatibility and for non-Bootstrap
  // related shown/hidden events (like conditionalPanel)
  $(document.body).on('shown.sendImageSize', '*', sendImageSize);
  $(document.body).on('shown.sendOutputHiddenState hidden.sendOutputHiddenState', '*',
               sendOutputHiddenState);

  // Send initial pixel ratio, and update it if it changes
  initialValues['.clientdata_pixelratio'] = pixelRatio();
  $(window).resize(function() {
    inputs.setInput('.clientdata_pixelratio', pixelRatio());
  });

  // Send initial URL
  initialValues['.clientdata_url_protocol'] = window.location.protocol;
  initialValues['.clientdata_url_hostname'] = window.location.hostname;
  initialValues['.clientdata_url_port']     = window.location.port;
  initialValues['.clientdata_url_pathname'] = window.location.pathname;

  // Send initial URL search (query string) and update it if it changes
  initialValues['.clientdata_url_search']   = window.location.search;

  $(window).on('pushstate', function(e) {
    inputs.setInput('.clientdata_url_search', window.location.search);
  });

  $(window).on('popstate', function(e) {
    inputs.setInput('.clientdata_url_search', window.location.search);
  });

  // This is only the initial value of the hash. The hash can change, but
  // a reactive version of this isn't sent because watching for changes can
  // require polling on some browsers. The JQuery hashchange plugin can be
  // used if this capability is important.
  initialValues['.clientdata_url_hash_initial'] = window.location.hash;
  initialValues['.clientdata_url_hash'] = window.location.hash;

  $(window).on('hashchange', function(e) {
    inputs.setInput('.clientdata_url_hash', window.location.hash);
  });

  // The server needs to know what singletons were rendered as part of
  // the page loading
  var singletonText = initialValues['.clientdata_singletons'] =
      $('script[type="application/shiny-singletons"]').text();
  singletons.registerNames(singletonText.split(/,/));

  var dependencyText = $('script[type="application/html-dependencies"]').text();
  $.each(dependencyText.split(/;/), function(i, depStr) {
    var match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
    if (match) {
      registerDependency(match[1], match[2]);
    }
  });

  // IE8 and IE9 have some limitations with data URIs
  initialValues['.clientdata_allowDataUriScheme'] = typeof WebSocket !== 'undefined';

  // We've collected all the initial values--start the server process!
  inputsNoResend.reset(initialValues);
  shinyapp.connect(initialValues);
  $(document).one("shiny:connected", function() {
    initDeferredIframes();
  });

} // function initShiny()


// Give any deferred iframes a chance to load.
function initDeferredIframes() {
  if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
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
    var $el = $(el);
    $el.removeClass("shiny-frame-deferred");
    $el.attr("src", $el.attr("data-deferred-src"));
    $el.attr("data-deferred-src", null);
  });
}

$(function() {
  // Init Shiny a little later than document ready, so user code can
  // run first (i.e. to register bindings)
  setTimeout(initShiny, 1);
});
