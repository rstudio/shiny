/*! shiny 1.10.0.9000 | (c) 2012-2025 Posit Software, PBC. | License: GPL-3 | file LICENSE */
"use strict";
(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = (cb, mod) => function __require() {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  };
  var __copyProps = (to, from, except, desc) => {
    if (from && typeof from === "object" || typeof from === "function") {
      for (let key of __getOwnPropNames(from))
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
    }
    return to;
  };
  var __toESM = (mod, isNodeMode, target) => (target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
    isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
    mod
  ));
  var __accessCheck = (obj, member, msg) => {
    if (!member.has(obj))
      throw TypeError("Cannot " + msg);
  };
  var __privateAdd = (obj, member, value) => {
    if (member.has(obj))
      throw TypeError("Cannot add the same private member more than once");
    member instanceof WeakSet ? member.add(obj) : member.set(obj, value);
  };
  var __privateMethod = (obj, member, method) => {
    __accessCheck(obj, member, "access private method");
    return method;
  };

  // globals:jquery
  var require_jquery = __commonJS({
    "globals:jquery"(exports, module) {
      module.exports = window.jQuery;
    }
  });

  // srcts/src/initialize/browser.ts
  var import_jquery = __toESM(require_jquery());

  // srcts/src/utils/browser.ts
  var isQtVal = false;
  var isIEVal = false;
  var versionIE = -1;
  function setIsQt(isQt2) {
    isQtVal = isQt2;
  }
  function setIsIE(isIE2) {
    isIEVal = isIE2;
  }
  function setIEVersion(versionIE_) {
    versionIE = versionIE_;
  }
  function isQt() {
    return isQtVal;
  }
  function isIE() {
    return isIEVal;
  }
  function IEVersion() {
    return versionIE;
  }

  // srcts/src/utils/userAgent.ts
  var userAgent;
  function setUserAgent(userAgent_) {
    userAgent = userAgent_;
  }

  // srcts/src/initialize/browser.ts
  function getIEVersion() {
    const msie = userAgent.indexOf("MSIE ");
    if (isIE() && msie > 0) {
      return parseInt(
        userAgent.substring(msie + 5, userAgent.indexOf(".", msie)),
        10
      );
    }
    const trident = userAgent.indexOf("Trident/");
    if (trident > 0) {
      const rv = userAgent.indexOf("rv:");
      return parseInt(
        userAgent.substring(rv + 3, userAgent.indexOf(".", rv)),
        10
      );
    }
    return -1;
  }
  function determineBrowserInfo() {
    if (/\bQt\//.test(userAgent)) {
      (0, import_jquery.default)(document.documentElement).addClass("qt");
      setIsQt(true);
    } else {
      setIsQt(false);
    }
    if (/\bQt/.test(userAgent) && /\bMacintosh/.test(userAgent)) {
      (0, import_jquery.default)(document.documentElement).addClass("qtmac");
    }
    if (/\bQt\/5/.test(userAgent) && /Linux/.test(userAgent)) {
      (0, import_jquery.default)(document.documentElement).addClass("qt5");
    }
    setIsIE(/MSIE|Trident|Edge/.test(userAgent));
    setIEVersion(getIEVersion());
  }

  // srcts/src/initialize/disableForm.ts
  var import_jquery2 = __toESM(require_jquery());
  function disableFormSubmission() {
    (0, import_jquery2.default)(document).on("submit", "form:not([action])", function(e4) {
      e4.preventDefault();
    });
  }

  // srcts/src/initialize/history.ts
  var import_jquery3 = __toESM(require_jquery());
  function trackHistory() {
    const origPushState = window.history.pushState;
    window.history.pushState = function(...args) {
      const result = origPushState.apply(this, args);
      (0, import_jquery3.default)(document).trigger("pushstate");
      return result;
    };
  }

  // srcts/src/shiny/index.ts
  var import_jquery39 = __toESM(require_jquery());

  // srcts/src/utils/index.ts
  var import_jquery4 = __toESM(require_jquery());

  // srcts/src/window/pixelRatio.ts
  function windowDevicePixelRatio() {
    return window.devicePixelRatio;
  }

  // srcts/src/utils/object.ts
  function hasOwnProperty(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop);
  }
  function hasDefinedProperty(obj, prop) {
    return Object.prototype.hasOwnProperty.call(obj, prop) && obj[prop] !== void 0;
  }
  function ifUndefined(value, alternate) {
    if (value === void 0)
      return alternate;
    return value;
  }

  // srcts/src/utils/index.ts
  function escapeHTML(str) {
    const escaped = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#039;",
      "/": "&#x2F;"
    };
    return str.replace(/[&<>'"/]/g, function(m2) {
      return escaped[m2];
    });
  }
  function randomId() {
    return Math.floor(4294967296 + Math.random() * 64424509440).toString(16);
  }
  function strToBool(str) {
    if (!str || !str.toLowerCase)
      return void 0;
    switch (str.toLowerCase()) {
      case "true":
        return true;
      case "false":
        return false;
      default:
        return void 0;
    }
  }
  function getStyle(el, styleProp) {
    let x2 = void 0;
    if ("currentStyle" in el) {
      x2 = el.currentStyle[styleProp];
    } else {
      const style = document?.defaultView?.getComputedStyle(el, null);
      if (style)
        x2 = style.getPropertyValue(styleProp);
    }
    return x2;
  }
  function padZeros(n4, digits) {
    let str = n4.toString();
    while (str.length < digits)
      str = "0" + str;
    return str;
  }
  function roundSignif(x2, digits = 1) {
    if (digits < 1)
      throw "Significant digits must be at least 1.";
    return parseFloat(x2.toPrecision(digits));
  }
  function parseDate(dateString) {
    let date = new Date(dateString);
    if (date.toString() === "Invalid Date") {
      date = new Date(dateString.replace(/-/g, "/"));
    }
    return date;
  }
  function formatDateUTC(date) {
    if (date instanceof Date) {
      return date.getUTCFullYear() + "-" + padZeros(date.getUTCMonth() + 1, 2) + "-" + padZeros(date.getUTCDate(), 2);
    } else {
      return null;
    }
  }
  function makeResizeFilter(el, func) {
    let lastSize = {};
    return function() {
      const rect = el.getBoundingClientRect();
      const size = { w: rect.width, h: rect.height };
      if (size.w === 0 && size.h === 0)
        return;
      if (size.w === lastSize.w && size.h === lastSize.h)
        return;
      lastSize = size;
      func(size.w, size.h);
    };
  }
  function pixelRatio() {
    if (windowDevicePixelRatio()) {
      return Math.round(windowDevicePixelRatio() * 100) / 100;
    } else {
      return 1;
    }
  }
  function getBoundingClientSizeBeforeZoom(el) {
    const rect = el.getBoundingClientRect();
    const zoom = el.currentCSSZoom || 1;
    return {
      width: rect.width / zoom,
      height: rect.height / zoom
    };
  }
  function scopeExprToFunc(expr) {
    const exprEscaped = expr.replace(/[\\"']/g, "\\$&").replace(/\u0000/g, "\\0").replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/[\b]/g, "\\b");
    let func;
    try {
      func = new Function(
        `with (this) {
        try {
          return (${expr});
        } catch (e) {
          console.error('Error evaluating expression: ${exprEscaped}');
          throw e;
        }
      }`
      );
    } catch (e4) {
      console.error("Error parsing expression: " + expr);
      throw e4;
    }
    return function(scope) {
      return func.call(scope);
    };
  }
  function asArray(value) {
    if (value === null || value === void 0)
      return [];
    if (Array.isArray(value))
      return value;
    return [value];
  }
  function mergeSort(list, sortfunc) {
    function merge(a3, b3) {
      let ia = 0;
      let ib = 0;
      const sorted = [];
      while (ia < a3.length && ib < b3.length) {
        if (sortfunc(a3[ia], b3[ib]) <= 0) {
          sorted.push(a3[ia++]);
        } else {
          sorted.push(b3[ib++]);
        }
      }
      while (ia < a3.length)
        sorted.push(a3[ia++]);
      while (ib < b3.length)
        sorted.push(b3[ib++]);
      return sorted;
    }
    list = list.slice(0);
    for (let chunkSize = 1; chunkSize < list.length; chunkSize *= 2) {
      for (let i4 = 0; i4 < list.length; i4 += chunkSize * 2) {
        const listA = list.slice(i4, i4 + chunkSize);
        const listB = list.slice(i4 + chunkSize, i4 + chunkSize * 2);
        const merged = merge(listA, listB);
        const args = [i4, merged.length];
        Array.prototype.push.apply(args, merged);
        Array.prototype.splice.apply(list, args);
      }
    }
    return list;
  }
  function $escape(val) {
    if (typeof val === "undefined")
      return val;
    return val.replace(/([!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~])/g, "\\$1");
  }
  function mapValues(obj, f3) {
    const newObj = {};
    Object.keys(obj).forEach((key) => {
      newObj[key] = f3(obj[key], key, obj);
    });
    return newObj;
  }
  function isnan(x2) {
    return typeof x2 === "number" && isNaN(x2);
  }
  function _equal(x2, y3) {
    if (import_jquery4.default.type(x2) === "object" && import_jquery4.default.type(y3) === "object") {
      const xo = x2;
      const yo = y3;
      if (Object.keys(xo).length !== Object.keys(yo).length)
        return false;
      for (const prop in xo) {
        if (!hasOwnProperty(yo, prop) || !_equal(xo[prop], yo[prop]))
          return false;
      }
      return true;
    } else if (import_jquery4.default.type(x2) === "array" && import_jquery4.default.type(y3) === "array") {
      const xa = x2;
      const ya = y3;
      if (xa.length !== ya.length)
        return false;
      for (let i4 = 0; i4 < xa.length; i4++)
        if (!_equal(xa[i4], ya[i4]))
          return false;
      return true;
    } else {
      return x2 === y3;
    }
  }
  function equal(...args) {
    if (args.length < 2)
      throw new Error("equal requires at least two arguments.");
    for (let i4 = 0; i4 < args.length - 1; i4++) {
      if (!_equal(args[i4], args[i4 + 1]))
        return false;
    }
    return true;
  }
  var compareVersion = function(a3, op, b3) {
    function versionParts(ver) {
      return (ver + "").replace(/-/, ".").replace(/(\.0)+[^.]*$/, "").split(".");
    }
    function cmpVersion(a4, b4) {
      const aParts = versionParts(a4);
      const bParts = versionParts(b4);
      const len = Math.min(aParts.length, bParts.length);
      let cmp;
      for (let i4 = 0; i4 < len; i4++) {
        cmp = parseInt(aParts[i4], 10) - parseInt(bParts[i4], 10);
        if (cmp !== 0) {
          return cmp;
        }
      }
      return aParts.length - bParts.length;
    }
    const diff = cmpVersion(a3, b3);
    if (op === "==")
      return diff === 0;
    else if (op === ">=")
      return diff >= 0;
    else if (op === ">")
      return diff > 0;
    else if (op === "<=")
      return diff <= 0;
    else if (op === "<")
      return diff < 0;
    else
      throw `Unknown operator: ${op}`;
  };
  function updateLabel(labelTxt, labelNode) {
    if (typeof labelTxt === "undefined")
      return;
    if (labelNode.length !== 1) {
      throw new Error("labelNode must be of length 1");
    }
    const emptyLabel = Array.isArray(labelTxt) && labelTxt.length === 0;
    if (emptyLabel) {
      labelNode.addClass("shiny-label-null");
    } else {
      labelNode.text(labelTxt);
      labelNode.removeClass("shiny-label-null");
    }
  }
  function getComputedLinkColor(el) {
    const a3 = document.createElement("a");
    a3.href = "/";
    const div = document.createElement("div");
    div.style.setProperty("position", "absolute", "important");
    div.style.setProperty("top", "-1000px", "important");
    div.style.setProperty("left", "0", "important");
    div.style.setProperty("width", "30px", "important");
    div.style.setProperty("height", "10px", "important");
    div.appendChild(a3);
    el.appendChild(div);
    const linkColor = window.getComputedStyle(a3).getPropertyValue("color");
    el.removeChild(div);
    return linkColor;
  }
  function isBS3() {
    return !window.bootstrap;
  }

  // srcts/src/bindings/registry.ts
  var BindingRegistry = class {
    constructor() {
      this.bindings = [];
      this.bindingNames = {};
    }
    register(binding, bindingName, priority = 0) {
      const bindingObj = { binding, priority };
      this.bindings.unshift(bindingObj);
      if (bindingName) {
        this.bindingNames[bindingName] = bindingObj;
        binding.name = bindingName;
      }
    }
    setPriority(bindingName, priority) {
      const bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    }
    getPriority(bindingName) {
      const bindingObj = this.bindingNames[bindingName];
      if (!bindingObj)
        return false;
      return bindingObj.priority;
    }
    getBindings() {
      return mergeSort(this.bindings, function(a3, b3) {
        return b3.priority - a3.priority;
      });
    }
  };

  // srcts/src/bindings/input/inputBinding.ts
  var InputBinding = class {
    find(scope) {
      throw "Not implemented";
      scope;
    }
    getId(el) {
      return el.getAttribute("data-input-id") || el.id;
    }
    getType(el) {
      return null;
      el;
    }
    getValue(el) {
      throw "Not implemented";
      el;
    }
    subscribe(el, callback) {
      el;
      callback;
    }
    unsubscribe(el) {
      el;
    }
    receiveMessage(el, data) {
      throw "Not implemented";
      el;
      data;
    }
    getState(el) {
      throw "Not implemented";
      el;
    }
    getRatePolicy(el) {
      return null;
      el;
    }
    initialize(el) {
      el;
    }
    dispose(el) {
      el;
    }
  };

  // srcts/src/bindings/input/actionbutton.ts
  var import_jquery5 = __toESM(require_jquery());
  var ActionButtonInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery5.default)(scope).find(".action-button");
    }
    getValue(el) {
      return (0, import_jquery5.default)(el).data("val") || 0;
    }
    setValue(el, value) {
      (0, import_jquery5.default)(el).data("val", value);
    }
    getType(el) {
      return "shiny.action";
      el;
    }
    subscribe(el, callback) {
      (0, import_jquery5.default)(el).on(
        "click.actionButtonInputBinding",
        function() {
          const $el = (0, import_jquery5.default)(this);
          const val = $el.data("val") || 0;
          $el.data("val", val + 1);
          callback(false);
        }
      );
    }
    getState(el) {
      return { value: this.getValue(el) };
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery5.default)(el);
      if (hasDefinedProperty(data, "label") || hasDefinedProperty(data, "icon")) {
        let label = $el.text();
        let icon = "";
        if ($el.find("i[class]").length > 0) {
          const iconHtml = $el.find("i[class]")[0];
          if (iconHtml === $el.children()[0]) {
            icon = (0, import_jquery5.default)(iconHtml).prop("outerHTML");
          }
        }
        if (hasDefinedProperty(data, "label")) {
          label = data.label;
        }
        if (hasDefinedProperty(data, "icon")) {
          icon = Array.isArray(data.icon) ? "" : data.icon ?? "";
        }
        $el.html(icon + " " + label);
      }
      if (hasDefinedProperty(data, "disabled")) {
        if (data.disabled) {
          $el.attr("disabled", "");
        } else {
          $el.attr("disabled", null);
        }
      }
    }
    unsubscribe(el) {
      (0, import_jquery5.default)(el).off(".actionButtonInputBinding");
    }
  };
  (0, import_jquery5.default)(document).on("click", "a.action-button", function(e4) {
    e4.preventDefault();
  });

  // srcts/src/bindings/input/checkbox.ts
  var import_jquery6 = __toESM(require_jquery());
  var CheckboxInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery6.default)(scope).find('input[type="checkbox"]');
    }
    getValue(el) {
      return el.checked;
    }
    setValue(el, value) {
      el.checked = value;
    }
    subscribe(el, callback) {
      (0, import_jquery6.default)(el).on("change.checkboxInputBinding", function() {
        callback(true);
      });
    }
    unsubscribe(el) {
      (0, import_jquery6.default)(el).off(".checkboxInputBinding");
    }
    getState(el) {
      return {
        label: (0, import_jquery6.default)(el).parent().find("span").text(),
        value: el.checked
      };
    }
    receiveMessage(el, data) {
      if (hasDefinedProperty(data, "value")) {
        el.checked = data.value;
      }
      if (hasDefinedProperty(data, "label")) {
        (0, import_jquery6.default)(el).parent().find("span").text(data.label);
      }
      (0, import_jquery6.default)(el).trigger("change");
    }
  };

  // srcts/src/bindings/input/checkboxgroup.ts
  var import_jquery7 = __toESM(require_jquery());
  function getLabelNode(el) {
    return (0, import_jquery7.default)(el).find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel(obj) {
    const parentNode = obj.parentNode;
    if (parentNode.tagName === "LABEL") {
      return (0, import_jquery7.default)(parentNode).find("span").text().trim();
    }
    return null;
  }
  var CheckboxGroupInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery7.default)(scope).find(".shiny-input-checkboxgroup");
    }
    getValue(el) {
      const $objs = (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"]:checked');
      const values = new Array($objs.length);
      for (let i4 = 0; i4 < $objs.length; i4++) {
        values[i4] = $objs[i4].value;
      }
      return values;
    }
    setValue(el, value) {
      value = value ?? [];
      (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"]').prop("checked", false);
      if (value instanceof Array) {
        for (let i4 = 0; i4 < value.length; i4++) {
          (0, import_jquery7.default)(
            'input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i4]) + '"]'
          ).prop("checked", true);
        }
      } else {
        (0, import_jquery7.default)(
          'input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]'
        ).prop("checked", true);
      }
    }
    getState(el) {
      const $objs = (0, import_jquery7.default)(
        'input:checkbox[name="' + $escape(el.id) + '"]'
      );
      const options = new Array($objs.length);
      for (let i4 = 0; i4 < options.length; i4++) {
        options[i4] = { value: $objs[i4].value, label: getLabel($objs[i4]) };
      }
      return {
        label: getLabelNode(el).text(),
        value: this.getValue(el),
        options
      };
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery7.default)(el);
      if (hasDefinedProperty(data, "options")) {
        $el.find("div.shiny-options-group").remove();
        $el.find("label.checkbox").remove();
        $el.append(data.options);
      }
      if (hasDefinedProperty(data, "value")) {
        this.setValue(el, data.value);
      }
      updateLabel(data.label, getLabelNode(el));
      (0, import_jquery7.default)(el).trigger("change");
    }
    subscribe(el, callback) {
      (0, import_jquery7.default)(el).on("change.checkboxGroupInputBinding", function() {
        callback(false);
      });
    }
    unsubscribe(el) {
      (0, import_jquery7.default)(el).off(".checkboxGroupInputBinding");
    }
  };

  // srcts/src/bindings/input/date.ts
  var import_jquery8 = __toESM(require_jquery());
  var DateInputBindingBase = class extends InputBinding {
    find(scope) {
      return (0, import_jquery8.default)(scope).find(".shiny-date-input");
    }
    getType(el) {
      return "shiny.date";
      el;
    }
    subscribe(el, callback) {
      (0, import_jquery8.default)(el).on(
        "changeDate.dateInputBinding change.dateInputBinding",
        function() {
          callback(false);
        }
      );
    }
    unsubscribe(el) {
      (0, import_jquery8.default)(el).off(".dateInputBinding");
    }
    getRatePolicy() {
      return {
        policy: "debounce",
        delay: 250
      };
    }
    setValue(el, data) {
      throw "not implemented";
      el;
      data;
    }
    initialize(el) {
      const $input = (0, import_jquery8.default)(el).find("input");
      let date = $input.data("initial-date");
      if (date === void 0 || date === null) {
        date = this._floorDateTime(this._dateAsUTC(new Date()));
      }
      this.setValue(el, date);
      if ($input.data("min-date") !== void 0) {
        this._setMin($input[0], $input.data("min-date"));
      }
      if ($input.data("max-date") !== void 0) {
        this._setMax($input[0], $input.data("max-date"));
      }
    }
    _getLabelNode(el) {
      return (0, import_jquery8.default)(el).find('label[for="' + $escape(el.id) + '"]');
    }
    _formatToString(format) {
      let str = "";
      let i4;
      for (i4 = 0; i4 < format.parts.length; i4++) {
        str += format.separators[i4] + format.parts[i4];
      }
      str += format.separators[i4];
      return str;
    }
    _setMin(el, date) {
      if (date === null) {
        (0, import_jquery8.default)(el).bsDatepicker("setStartDate", null);
        return;
      }
      const parsedDate = this._newDate(date);
      if (parsedDate === null)
        return;
      date = parsedDate;
      if (isNaN(date.valueOf()))
        return;
      const curValue = (0, import_jquery8.default)(el).bsDatepicker("getUTCDate");
      (0, import_jquery8.default)(el).bsDatepicker("setStartDate", this._utcDateAsLocal(date));
      if (date && curValue && date.getTime() > curValue.getTime()) {
        (0, import_jquery8.default)(el).bsDatepicker("clearDates");
      } else {
        (0, import_jquery8.default)(el).bsDatepicker("setUTCDate", curValue);
      }
    }
    _setMax(el, date) {
      if (date === null) {
        (0, import_jquery8.default)(el).bsDatepicker("setEndDate", null);
        return;
      }
      const parsedDate = this._newDate(date);
      if (parsedDate === null)
        return;
      date = parsedDate;
      if (isNaN(date.valueOf()))
        return;
      const curValue = (0, import_jquery8.default)(el).bsDatepicker("getUTCDate");
      (0, import_jquery8.default)(el).bsDatepicker("setEndDate", this._utcDateAsLocal(date));
      if (date && curValue && date.getTime() < curValue.getTime()) {
        (0, import_jquery8.default)(el).bsDatepicker("clearDates");
      } else {
        (0, import_jquery8.default)(el).bsDatepicker("setUTCDate", curValue);
      }
    }
    _newDate(date) {
      if (date instanceof Date)
        return date;
      if (!date)
        return null;
      const d3 = parseDate(date);
      if (isNaN(d3.valueOf()))
        return null;
      return d3;
    }
    _floorDateTime(date) {
      date = new Date(date.getTime());
      date.setUTCHours(0, 0, 0, 0);
      return date;
    }
    _dateAsUTC(date) {
      return new Date(date.getTime() - date.getTimezoneOffset() * 6e4);
    }
    _utcDateAsLocal(date) {
      return new Date(date.getTime() + date.getTimezoneOffset() * 6e4);
    }
  };
  var DateInputBinding = class extends DateInputBindingBase {
    getValue(el) {
      const date = (0, import_jquery8.default)(el).find("input").bsDatepicker("getUTCDate");
      return formatDateUTC(date);
    }
    setValue(el, value) {
      if (value === null) {
        (0, import_jquery8.default)(el).find("input").val("").bsDatepicker("update");
        return;
      }
      const date = this._newDate(value);
      if (date === null) {
        return;
      }
      if (isNaN(date.valueOf()))
        return;
      (0, import_jquery8.default)(el).find("input").bsDatepicker("setUTCDate", date);
    }
    getState(el) {
      const $el = (0, import_jquery8.default)(el);
      const $input = $el.find("input");
      let min = $input.data("datepicker").startDate;
      let max = $input.data("datepicker").endDate;
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);
      let startview = $input.data("datepicker").startViewMode;
      if (startview === 2)
        startview = "decade";
      else if (startview === 1)
        startview = "year";
      else if (startview === 0)
        startview = "month";
      return {
        label: this._getLabelNode(el).text(),
        value: this.getValue(el),
        valueString: $input.val(),
        min,
        max,
        language: $input.data("datepicker").language,
        weekstart: $input.data("datepicker").weekStart,
        format: this._formatToString($input.data("datepicker").format),
        startview
      };
    }
    receiveMessage(el, data) {
      const $input = (0, import_jquery8.default)(el).find("input");
      updateLabel(data.label, this._getLabelNode(el));
      if (hasDefinedProperty(data, "min"))
        this._setMin($input[0], data.min);
      if (hasDefinedProperty(data, "max"))
        this._setMax($input[0], data.max);
      if (hasDefinedProperty(data, "value"))
        this.setValue(el, data.value);
      (0, import_jquery8.default)(el).trigger("change");
    }
  };

  // srcts/src/bindings/input/daterange.ts
  var import_jquery9 = __toESM(require_jquery());
  function getLabelNode2(el) {
    return (0, import_jquery9.default)(el).find('label[for="' + $escape(el.id) + '"]');
  }
  var DateRangeInputBinding = class extends DateInputBindingBase {
    find(scope) {
      return (0, import_jquery9.default)(scope).find(".shiny-date-range-input");
    }
    getValue(el) {
      const $inputs = (0, import_jquery9.default)(el).find("input");
      const start = $inputs.eq(0).bsDatepicker("getUTCDate");
      const end = $inputs.eq(1).bsDatepicker("getUTCDate");
      return [formatDateUTC(start), formatDateUTC(end)];
    }
    setValue(el, value) {
      if (!(value instanceof Object)) {
        return;
      }
      const $inputs = (0, import_jquery9.default)(el).find("input");
      if (value.start !== void 0) {
        if (value.start === null) {
          $inputs.eq(0).val("").bsDatepicker("update");
        } else {
          const start = this._newDate(value.start);
          $inputs.eq(0).bsDatepicker("setUTCDate", start);
        }
      }
      if (value.end !== void 0) {
        if (value.end === null) {
          $inputs.eq(1).val("").bsDatepicker("update");
        } else {
          const end = this._newDate(value.end);
          $inputs.eq(1).bsDatepicker("setUTCDate", end);
        }
      }
    }
    getState(el) {
      const $el = (0, import_jquery9.default)(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);
      const min = $startinput.bsDatepicker("getStartDate");
      const max = $startinput.bsDatepicker("getEndDate");
      const minStr = min === -Infinity ? null : formatDateUTC(min);
      const maxStr = max === Infinity ? null : formatDateUTC(max);
      let startview = $startinput.data("datepicker").startView;
      if (startview === 2)
        startview = "decade";
      else if (startview === 1)
        startview = "year";
      else if (startview === 0)
        startview = "month";
      return {
        label: getLabelNode2(el).text(),
        value: this.getValue(el),
        valueString: [$startinput.val(), $endinput.val()],
        min: minStr,
        max: maxStr,
        weekstart: $startinput.data("datepicker").weekStart,
        format: this._formatToString($startinput.data("datepicker").format),
        language: $startinput.data("datepicker").language,
        startview
      };
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery9.default)(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);
      updateLabel(data.label, getLabelNode2(el));
      if (hasDefinedProperty(data, "min")) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0], data.min);
      }
      if (hasDefinedProperty(data, "max")) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0], data.max);
      }
      if (hasDefinedProperty(data, "value")) {
        this.setValue(el, data.value);
      }
      $el.trigger("change");
    }
    initialize(el) {
      const $el = (0, import_jquery9.default)(el);
      const $inputs = $el.find("input");
      const $startinput = $inputs.eq(0);
      const $endinput = $inputs.eq(1);
      let start = $startinput.data("initial-date");
      let end = $endinput.data("initial-date");
      if (start === void 0 || start === null)
        start = this._dateAsUTC(new Date());
      if (end === void 0 || end === null)
        end = this._dateAsUTC(new Date());
      this.setValue(el, { start, end });
      this._setMin($startinput[0], $startinput.data("min-date"));
      this._setMin($endinput[0], $startinput.data("min-date"));
      this._setMax($startinput[0], $endinput.data("max-date"));
      this._setMax($endinput[0], $endinput.data("max-date"));
    }
    subscribe(el, callback) {
      (0, import_jquery9.default)(el).on(
        "changeDate.dateRangeInputBinding change.dateRangeInputBinding",
        function() {
          callback(false);
        }
      );
    }
    unsubscribe(el) {
      (0, import_jquery9.default)(el).off(".dateRangeInputBinding");
    }
  };

  // srcts/src/bindings/input/fileinput.ts
  var import_jquery12 = __toESM(require_jquery());

  // srcts/src/file/fileProcessor.ts
  var import_jquery11 = __toESM(require_jquery());

  // srcts/src/events/inputChanged.ts
  var import_jquery10 = __toESM(require_jquery());
  function triggerFileInputChanged(name, value, binding, el, inputType, onEl) {
    const evt = import_jquery10.default.Event("shiny:inputchanged");
    evt.name = name;
    evt.value = value;
    evt.binding = binding;
    evt.el = el;
    evt.inputType = inputType;
    (0, import_jquery10.default)(onEl).trigger(evt);
    return evt;
  }

  // srcts/src/shiny/initedMethods.ts
  var fullShinyObj;
  function setShinyObj(shiny) {
    fullShinyObj = shiny;
  }
  function validateShinyHasBeenSet() {
    if (typeof fullShinyObj === "undefined") {
      throw "Shiny has not finish initialization yet. Please wait for the 'shiny-initialized' event.";
    }
    return fullShinyObj;
  }
  function shinySetInputValue(name, value, opts) {
    validateShinyHasBeenSet().setInputValue(name, value, opts);
  }
  function shinyShinyApp() {
    return validateShinyHasBeenSet().shinyapp;
  }
  function setShinyUser(user) {
    validateShinyHasBeenSet().user = user;
  }
  function shinyForgetLastInputValue(name) {
    validateShinyHasBeenSet().forgetLastInputValue(name);
  }
  async function shinyBindAll(scope) {
    await validateShinyHasBeenSet().bindAll(scope);
  }
  function shinyUnbindAll(scope, includeSelf = false) {
    validateShinyHasBeenSet().unbindAll(scope, includeSelf);
  }
  function shinyInitializeInputs(scope) {
    validateShinyHasBeenSet().initializeInputs(scope);
  }
  async function shinyAppBindOutput(id, binding) {
    await shinyShinyApp().bindOutput(id, binding);
  }
  function shinyAppUnbindOutput(id, binding) {
    return shinyShinyApp().unbindOutput(id, binding);
  }
  function getShinyOnCustomMessage() {
    return validateShinyHasBeenSet().oncustommessage;
  }
  var fileInputBinding;
  function getFileInputBinding() {
    return fileInputBinding;
  }
  function setFileInputBinding(fileInputBinding_) {
    fileInputBinding = fileInputBinding_;
  }
  function getShinyCreateWebsocket() {
    return validateShinyHasBeenSet().createSocket;
  }

  // srcts/src/file/fileProcessor.ts
  var FileProcessor = class {
    constructor(files, exec$run = true) {
      this.fileIndex = -1;
      this.aborted = false;
      this.completed = false;
      this.files = Array.from(files);
      if (exec$run) {
        this.$run();
      }
    }
    onBegin(files, cont) {
      files;
      setTimeout(cont, 0);
    }
    onFile(file, cont) {
      file;
      setTimeout(cont, 0);
    }
    onComplete() {
      return;
    }
    onAbort() {
      return;
    }
    abort() {
      if (this.completed || this.aborted)
        return;
      this.aborted = true;
      this.onAbort();
    }
    $getRun() {
      let called = false;
      return () => {
        if (called)
          return;
        called = true;
        this.$run();
      };
    }
    $run() {
      if (this.aborted || this.completed)
        return;
      if (this.fileIndex < 0) {
        this.fileIndex = 0;
        this.onBegin(this.files, this.$getRun());
        return;
      }
      if (this.fileIndex === this.files.length) {
        this.completed = true;
        this.onComplete();
        return;
      }
      const file = this.files[this.fileIndex++];
      this.onFile(file, this.$getRun());
    }
  };
  var FileUploader = class extends FileProcessor {
    constructor(shinyapp, id, files, el) {
      super(files, false);
      this.shinyapp = shinyapp;
      this.id = id;
      this.el = el;
      this.$run();
    }
    makeRequest(method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    }
    onBegin(files, cont) {
      this.$setError(null);
      this.$setActive(true);
      this.$setVisible(true);
      this.onProgress(null, 0);
      this.totalBytes = 0;
      this.progressBytes = 0;
      import_jquery11.default.each(files, (i4, file) => {
        this.totalBytes += file.size;
      });
      const fileInfo = import_jquery11.default.map(files, function(file) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
      });
      this.makeRequest(
        "uploadInit",
        [fileInfo],
        (response) => {
          this.jobId = response.jobId;
          this.uploadUrl = response.uploadUrl;
          cont();
        },
        (error) => {
          this.onError(error);
        },
        void 0
      );
    }
    onFile(file, cont) {
      this.onProgress(file, 0);
      import_jquery11.default.ajax(this.uploadUrl, {
        type: "POST",
        cache: false,
        xhr: () => {
          if (typeof import_jquery11.default.ajaxSettings.xhr !== "function")
            throw "jQuery's XHR is not a function";
          const xhrVal = import_jquery11.default.ajaxSettings.xhr();
          if (xhrVal.upload) {
            xhrVal.upload.onprogress = (e4) => {
              if (e4.lengthComputable) {
                this.onProgress(
                  file,
                  (this.progressBytes + e4.loaded) / this.totalBytes
                );
              }
            };
          }
          return xhrVal;
        },
        data: file,
        contentType: "application/octet-stream",
        processData: false,
        success: () => {
          this.progressBytes += file.size;
          cont();
        },
        error: (jqXHR, textStatus, errorThrown) => {
          errorThrown;
          this.onError(jqXHR.responseText || textStatus);
        }
      });
    }
    onComplete() {
      const fileInfo = import_jquery11.default.map(this.files, function(file, i4) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
        i4;
      });
      const evt = triggerFileInputChanged(
        this.id,
        fileInfo,
        getFileInputBinding(),
        this.el,
        "shiny.fileupload",
        document
      );
      this.makeRequest(
        "uploadEnd",
        [this.jobId, this.id],
        () => {
          this.$setActive(false);
          this.onProgress(null, 1);
          this.$bar().text("Upload complete");
          (0, import_jquery11.default)(evt.el).val("");
        },
        (error) => {
          this.onError(error);
        },
        void 0
      );
      this.$bar().text("Finishing upload");
    }
    onError(message) {
      this.$setError(message || "");
      this.$setActive(false);
    }
    onAbort() {
      this.$setVisible(false);
    }
    onProgress(file, completed) {
      this.$bar().width(Math.round(completed * 100) + "%");
      this.$bar().text(file ? file.name : "");
    }
    $container() {
      return (0, import_jquery11.default)("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
    }
    $bar() {
      return (0, import_jquery11.default)(
        "#" + $escape(this.id) + "_progress.shiny-file-input-progress .progress-bar"
      );
    }
    $setVisible(visible) {
      this.$container().css("visibility", visible ? "visible" : "hidden");
    }
    $setError(error) {
      this.$bar().toggleClass("progress-bar-danger", error !== null);
      if (error !== null) {
        this.onProgress(null, 1);
        this.$bar().text(error);
      }
    }
    $setActive(active) {
      this.$container().toggleClass("active", !!active);
    }
  };

  // srcts/src/bindings/input/fileinput.ts
  var zoneActive = "shiny-file-input-active";
  var zoneOver = "shiny-file-input-over";
  function zoneOf(el) {
    return (0, import_jquery12.default)(el).closest("div.input-group");
  }
  function enableDraghover(el) {
    const $el = (0, import_jquery12.default)(el);
    let childCounter = 0;
    $el.on({
      "dragenter.draghover": (e4) => {
        if (childCounter++ === 0) {
          $el.trigger("draghover:enter", e4);
        }
      },
      "dragleave.draghover": (e4) => {
        if (--childCounter === 0) {
          $el.trigger("draghover:leave", e4);
        }
        if (childCounter < 0) {
          console.error("draghover childCounter is negative somehow");
        }
      },
      "dragover.draghover": (e4) => {
        e4.preventDefault();
      },
      "drop.draghover": (e4) => {
        childCounter = 0;
        $el.trigger("draghover:drop", e4);
        e4.preventDefault();
      }
    });
    return $el;
  }
  function disableDraghover(el) {
    return (0, import_jquery12.default)(el).off(".draghover");
  }
  function enableDocumentEvents() {
    const $doc = (0, import_jquery12.default)("html");
    enableDraghover($doc).on({
      "draghover:enter.draghover": () => {
        zoneOf($fileInputs).addClass(zoneActive);
      },
      "draghover:leave.draghover": () => {
        zoneOf($fileInputs).removeClass(zoneActive);
      },
      "draghover:drop.draghover": () => {
        zoneOf($fileInputs).removeClass(zoneOver).removeClass(zoneActive);
      }
    });
  }
  function disableDocumentEvents() {
    const $doc = (0, import_jquery12.default)("html");
    $doc.off(".draghover");
    disableDraghover($doc);
  }
  function canSetFiles(fileList) {
    const testEl = document.createElement("input");
    testEl.type = "file";
    try {
      testEl.files = fileList;
    } catch (e4) {
      return false;
    }
    return true;
  }
  function handleDrop(e4, el) {
    const files = e4.originalEvent?.dataTransfer?.files, $el = (0, import_jquery12.default)(el);
    if (files === void 0 || files === null) {
      console.log(
        "Dropping files is not supported on this browser. (no FileList)"
      );
    } else if (!canSetFiles(files)) {
      $el.val("");
      uploadDroppedFilesIE10Plus(el, files);
    } else {
      $el.val("");
      el.files = files;
      $el.trigger("change");
    }
  }
  function setFileText($el, files) {
    const $fileText = $el.closest("div.input-group").find("input[type=text]");
    if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + " files");
    }
  }
  function abortCurrentUpload($el) {
    const uploader = $el.data("currentUploader");
    if (uploader)
      uploader.abort();
    $el.removeAttr("data-restore");
  }
  function uploadDroppedFilesIE10Plus(el, files) {
    const $el = (0, import_jquery12.default)(el);
    abortCurrentUpload($el);
    setFileText($el, files);
    $el.data(
      "currentUploader",
      new FileUploader(shinyShinyApp(), fileInputBindingGetId(el), files, el)
    );
  }
  function uploadFiles(evt) {
    const $el = (0, import_jquery12.default)(evt.target);
    abortCurrentUpload($el);
    const files = evt.target.files;
    const id = fileInputBindingGetId(evt.target);
    if (files.length === 0)
      return;
    setFileText($el, files);
    $el.data(
      "currentUploader",
      new FileUploader(shinyShinyApp(), id, files, evt.target)
    );
  }
  var $fileInputs = (0, import_jquery12.default)();
  function fileInputBindingGetId(el) {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  }
  var FileInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery12.default)(scope).find('input[type="file"]');
    }
    getId(el) {
      return fileInputBindingGetId(el);
    }
    getValue(el) {
      const data = (0, import_jquery12.default)(el).attr("data-restore");
      if (data) {
        const dataParsed = JSON.parse(data);
        const $fileText = (0, import_jquery12.default)(el).closest("div.input-group").find("input[type=text]");
        if (dataParsed.name.length === 1) {
          $fileText.val(dataParsed.name[0]);
        } else {
          $fileText.val(dataParsed.name.length + " files");
        }
        const $progress = (0, import_jquery12.default)(el).closest("div.form-group").find(".progress");
        const $bar = $progress.find(".progress-bar");
        $progress.removeClass("active");
        $bar.width("100%");
        $bar.css("visibility", "visible");
        return dataParsed;
      } else {
        return null;
      }
    }
    setValue(el, value) {
      el;
      value;
    }
    getType(el) {
      return "shiny.file";
      el;
    }
    subscribe(el, callback) {
      callback;
      (0, import_jquery12.default)(el).on("change.fileInputBinding", uploadFiles);
      if ($fileInputs.length === 0)
        enableDocumentEvents();
      $fileInputs = $fileInputs.add(el);
      const $zone = zoneOf(el);
      enableDraghover($zone).on({
        "draghover:enter.draghover": (e4) => {
          e4;
          $zone.addClass(zoneOver);
        },
        "draghover:leave.draghover": (e4) => {
          $zone.removeClass(zoneOver);
          e4.stopPropagation();
        },
        "draghover:drop.draghover": (e4, dropEvent) => {
          e4;
          handleDrop(dropEvent, el);
        }
      });
    }
    unsubscribe(el) {
      const $el = (0, import_jquery12.default)(el), $zone = zoneOf(el);
      $zone.removeClass(zoneOver).removeClass(zoneActive);
      disableDraghover($zone);
      $el.off(".fileInputBinding");
      $zone.off(".draghover");
      $fileInputs = $fileInputs.not(el);
      if ($fileInputs.length === 0)
        disableDocumentEvents();
    }
  };

  // srcts/src/bindings/input/number.ts
  var import_jquery14 = __toESM(require_jquery());

  // srcts/src/bindings/input/text.ts
  var import_jquery13 = __toESM(require_jquery());
  function getLabelNode3(el) {
    return (0, import_jquery13.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  var TextInputBindingBase = class extends InputBinding {
    find(scope) {
      const $inputs = (0, import_jquery13.default)(scope).find(
        'input[type="text"], input[type="search"], input[type="url"], input[type="email"]'
      );
      return $inputs.not('input[type="text"][id$="-selectized"]');
    }
    getId(el) {
      return super.getId(el) || el.name;
    }
    getValue(el) {
      throw "not implemented";
      el;
    }
    setValue(el, value) {
      throw "not implemented";
      el;
      value;
    }
    subscribe(el, callback) {
      const $el = (0, import_jquery13.default)(el);
      const updateOn = $el.data("update-on") || "change";
      if (updateOn === "change") {
        $el.on(
          "keyup.textInputBinding input.textInputBinding",
          function() {
            callback(true);
          }
        );
      } else if (updateOn === "blur") {
        $el.on("blur.textInputBinding", function() {
          callback(false);
        });
        $el.on("keydown.textInputBinding", function(event) {
          if (event.key !== "Enter")
            return;
          if ($el.is("textarea")) {
            if (!(event.ctrlKey || event.metaKey))
              return;
          }
          callback(false);
        });
      }
      $el.on("change.textInputBinding", function() {
        if (updateOn === "blur" && $el.is(":focus")) {
          return;
        }
        callback(false);
      });
    }
    unsubscribe(el) {
      (0, import_jquery13.default)(el).off(".textInputBinding");
    }
    receiveMessage(el, data) {
      throw "not implemented";
      el;
      data;
    }
    getState(el) {
      throw "not implemented";
      el;
    }
    getRatePolicy(el) {
      return {
        policy: "debounce",
        delay: 250
      };
      el;
    }
  };
  var TextInputBinding = class extends TextInputBindingBase {
    setValue(el, value) {
      el.value = value;
    }
    getValue(el) {
      return el.value;
    }
    getState(el) {
      return {
        label: getLabelNode3(el).text(),
        value: el.value,
        placeholder: el.placeholder
      };
    }
    receiveMessage(el, data) {
      if (hasDefinedProperty(data, "value"))
        this.setValue(el, data.value);
      updateLabel(data.label, getLabelNode3(el));
      if (hasDefinedProperty(data, "placeholder"))
        el.placeholder = data.placeholder;
      (0, import_jquery13.default)(el).trigger("change");
    }
  };

  // srcts/src/bindings/input/number.ts
  function getLabelNode4(el) {
    return (0, import_jquery14.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  var NumberInputBinding = class extends TextInputBindingBase {
    find(scope) {
      return (0, import_jquery14.default)(scope).find('input[type="number"]');
    }
    getValue(el) {
      const numberVal = (0, import_jquery14.default)(el).val();
      if (typeof numberVal == "string") {
        if (/^\s*$/.test(numberVal))
          return null;
      }
      const numberValue = Number(numberVal);
      if (!isNaN(numberValue)) {
        return numberValue;
      }
      return numberVal;
    }
    setValue(el, value) {
      el.value = "" + value;
    }
    getType(el) {
      return "shiny.number";
      el;
    }
    receiveMessage(el, data) {
      if (hasDefinedProperty(data, "value"))
        el.value = data.value ?? "";
      if (hasDefinedProperty(data, "min"))
        el.min = data.min ?? "";
      if (hasDefinedProperty(data, "max"))
        el.max = data.max ?? "";
      if (hasDefinedProperty(data, "step"))
        el.step = data.step ?? "";
      updateLabel(data.label, getLabelNode4(el));
      (0, import_jquery14.default)(el).trigger("change");
    }
    getState(el) {
      return {
        label: getLabelNode4(el).text(),
        value: this.getValue(el),
        min: Number(el.min),
        max: Number(el.max),
        step: Number(el.step)
      };
    }
  };

  // srcts/src/bindings/input/password.ts
  var import_jquery15 = __toESM(require_jquery());
  var PasswordInputBinding = class extends TextInputBinding {
    find(scope) {
      return (0, import_jquery15.default)(scope).find('input[type="password"]');
    }
    getType(el) {
      return "shiny.password";
      el;
    }
  };

  // srcts/src/bindings/input/radio.ts
  var import_jquery16 = __toESM(require_jquery());
  function getLabelNode5(el) {
    return (0, import_jquery16.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel2(obj) {
    const parentNode = obj.parentNode;
    if (parentNode.tagName === "LABEL") {
      return (0, import_jquery16.default)(parentNode).find("span").text().trim();
    }
    return null;
  }
  var RadioInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery16.default)(scope).find(".shiny-input-radiogroup");
    }
    getValue(el) {
      const checkedItems = (0, import_jquery16.default)(
        'input:radio[name="' + $escape(el.id) + '"]:checked'
      );
      if (checkedItems.length === 0) {
        return null;
      }
      return checkedItems.val();
    }
    setValue(el, value) {
      if (Array.isArray(value) && value.length === 0) {
        (0, import_jquery16.default)('input:radio[name="' + $escape(el.id) + '"]').prop("checked", false);
      } else {
        (0, import_jquery16.default)(
          'input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]'
        ).prop("checked", true);
      }
    }
    getState(el) {
      const $objs = (0, import_jquery16.default)(
        'input:radio[name="' + $escape(el.id) + '"]'
      );
      const options = new Array($objs.length);
      for (let i4 = 0; i4 < options.length; i4++) {
        options[i4] = { value: $objs[i4].value, label: getLabel2($objs[i4]) };
      }
      return {
        label: getLabelNode5(el).text(),
        value: this.getValue(el),
        options
      };
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery16.default)(el);
      if (hasDefinedProperty(data, "options")) {
        $el.find("div.shiny-options-group").remove();
        $el.find("label.radio").remove();
        $el.append(data.options);
      }
      if (hasDefinedProperty(data, "value")) {
        this.setValue(el, data.value);
      }
      updateLabel(data.label, getLabelNode5(el));
      (0, import_jquery16.default)(el).trigger("change");
    }
    subscribe(el, callback) {
      (0, import_jquery16.default)(el).on("change.radioInputBinding", function() {
        callback(false);
      });
    }
    unsubscribe(el) {
      (0, import_jquery16.default)(el).off(".radioInputBinding");
    }
  };

  // srcts/src/bindings/input/selectInput.ts
  var import_jquery17 = __toESM(require_jquery());

  // srcts/src/utils/eval.ts
  var indirectEval = eval;

  // srcts/src/bindings/input/selectInput.ts
  function getLabelNode6(el) {
    let escapedId = $escape(el.id);
    if (isSelectize(el)) {
      escapedId += "-selectized";
    }
    return (0, import_jquery17.default)(el).parent().parent().find('label[for="' + escapedId + '"]');
  }
  function isSelectize(el) {
    const config = (0, import_jquery17.default)(el).parent().find('script[data-for="' + $escape(el.id) + '"]');
    return config.length > 0;
  }
  var SelectInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery17.default)(scope).find("select");
    }
    getType(el) {
      const $el = (0, import_jquery17.default)(el);
      if (!$el.hasClass("symbol")) {
        return null;
      }
      if ($el.attr("multiple") === "multiple") {
        return "shiny.symbolList";
      } else {
        return "shiny.symbol";
      }
    }
    getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    }
    getValue(el) {
      if (!isSelectize(el)) {
        return (0, import_jquery17.default)(el).val();
      } else {
        const selectize = this._selectize(el);
        return selectize?.getValue();
      }
    }
    setValue(el, value) {
      if (!isSelectize(el)) {
        (0, import_jquery17.default)(el).val(value);
      } else {
        const selectize = this._selectize(el);
        selectize?.setValue(value);
      }
    }
    getState(el) {
      const options = new Array(
        el.length
      );
      for (let i4 = 0; i4 < el.length; i4++) {
        options[i4] = {
          value: el[i4].value,
          label: el[i4].label
        };
      }
      return {
        label: getLabelNode6(el),
        value: this.getValue(el),
        options
      };
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery17.default)(el);
      if (hasDefinedProperty(data, "options")) {
        const selectize = this._selectize(el);
        selectize?.destroy();
        $el.empty().append(data.options);
        this._selectize(el);
      }
      if (hasDefinedProperty(data, "config")) {
        $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
        this._selectize(el, true);
      }
      if (hasDefinedProperty(data, "url")) {
        const selectize = this._selectize(el);
        selectize.clear();
        selectize.clearOptions();
        let loaded = false;
        selectize.settings.load = function(query, callback) {
          const settings = selectize.settings;
          import_jquery17.default.ajax({
            url: data.url,
            data: {
              query,
              field: JSON.stringify([settings.searchField]),
              value: settings.valueField,
              conju: settings.searchConjunction,
              maxop: settings.maxOptions
            },
            type: "GET",
            error: function() {
              callback();
            },
            success: function(res) {
              import_jquery17.default.each(res, function(index, elem) {
                const optgroupId = elem[settings.optgroupField || "optgroup"];
                const optgroup = {};
                optgroup[settings.optgroupLabelField || "label"] = optgroupId;
                optgroup[settings.optgroupValueField || "value"] = optgroupId;
                selectize.addOptionGroup(optgroupId, optgroup);
              });
              callback(res);
              if (!loaded) {
                if (hasDefinedProperty(data, "value")) {
                  selectize.setValue(data.value);
                } else if (settings.maxItems === 1) {
                  selectize.setValue(res[0].value);
                }
              }
              loaded = true;
            }
          });
        };
        selectize.load(function(callback) {
          selectize.settings.load.apply(selectize, ["", callback]);
        });
      } else if (hasDefinedProperty(data, "value")) {
        this.setValue(el, data.value);
      }
      updateLabel(data.label, getLabelNode6(el));
      (0, import_jquery17.default)(el).trigger("change");
    }
    subscribe(el, callback) {
      (0, import_jquery17.default)(el).on(
        "change.selectInputBinding",
        () => {
          if (el.nonempty && this.getValue(el) === "") {
            return;
          }
          callback(false);
        }
      );
    }
    unsubscribe(el) {
      (0, import_jquery17.default)(el).off(".selectInputBinding");
    }
    initialize(el) {
      this._selectize(el);
    }
    _selectize(el, update = false) {
      if (!import_jquery17.default.fn.selectize)
        return void 0;
      const $el = (0, import_jquery17.default)(el);
      const config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
      if (config.length === 0)
        return void 0;
      let options = import_jquery17.default.extend(
        {
          labelField: "label",
          valueField: "value",
          searchField: ["label"]
        },
        JSON.parse(config.html())
      );
      if (typeof config.data("nonempty") !== "undefined") {
        el.nonempty = true;
        options = import_jquery17.default.extend(options, {
          onItemRemove: function(value) {
            if (this.getValue() === "")
              (0, import_jquery17.default)("select#" + $escape(el.id)).empty().append(
                (0, import_jquery17.default)("<option/>", {
                  value,
                  selected: true
                })
              ).trigger("change");
          },
          onDropdownClose: function() {
            if (this.getValue() === "") {
              this.setValue((0, import_jquery17.default)("select#" + $escape(el.id)).val());
            }
          }
        });
      } else {
        el.nonempty = false;
      }
      if (config.data("eval") instanceof Array)
        import_jquery17.default.each(config.data("eval"), function(i4, x2) {
          options[x2] = indirectEval("(" + options[x2] + ")");
        });
      let control = $el.selectize(options)[0].selectize;
      if (update) {
        const settings = import_jquery17.default.extend(control.settings, options);
        control.destroy();
        control = $el.selectize(settings)[0].selectize;
      }
      return control;
    }
  };

  // srcts/src/bindings/input/slider.ts
  var import_jquery18 = __toESM(require_jquery());
  function forceIonSliderUpdate(slider) {
    if (slider.$cache && slider.$cache.input)
      slider.$cache.input.trigger("change");
    else
      console.log("Couldn't force ion slider to update");
  }
  function getTypePrettifyer(dataType, timeFormat, timezone) {
    let timeFormatter;
    let prettify;
    if (dataType === "date") {
      timeFormatter = window.strftime.utc();
      prettify = function(num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else if (dataType === "datetime") {
      if (timezone)
        timeFormatter = window.strftime.timezone(timezone);
      else
        timeFormatter = window.strftime;
      prettify = function(num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else {
      prettify = function(num) {
        return formatNumber(num, this.prettify_separator);
      };
    }
    return prettify;
  }
  function getLabelNode7(el) {
    return (0, import_jquery18.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function numValues(el) {
    if ((0, import_jquery18.default)(el).data("ionRangeSlider").options.type === "double")
      return 2;
    else
      return 1;
  }
  var SliderInputBinding = class extends TextInputBindingBase {
    find(scope) {
      if (!import_jquery18.default.fn.ionRangeSlider) {
        return (0, import_jquery18.default)();
      }
      return (0, import_jquery18.default)(scope).find("input.js-range-slider");
    }
    getType(el) {
      const dataType = (0, import_jquery18.default)(el).data("data-type");
      if (dataType === "date")
        return "shiny.date";
      else if (dataType === "datetime")
        return "shiny.datetime";
      else
        return null;
    }
    getValue(el) {
      const $el = (0, import_jquery18.default)(el);
      const result = (0, import_jquery18.default)(el).data("ionRangeSlider").result;
      let convert;
      const dataType = $el.data("data-type");
      if (dataType === "date") {
        convert = function(val) {
          return formatDateUTC(new Date(Number(val)));
        };
      } else if (dataType === "datetime") {
        convert = function(val) {
          return Number(val) / 1e3;
        };
      } else {
        convert = function(val) {
          return Number(val);
        };
      }
      if (numValues(el) === 2) {
        return [convert(result.from), convert(result.to)];
      } else {
        return convert(result.from);
      }
    }
    setValue(el, value) {
      const $el = (0, import_jquery18.default)(el);
      const slider = $el.data("ionRangeSlider");
      $el.data("immediate", true);
      try {
        if (numValues(el) === 2 && value instanceof Array) {
          slider.update({ from: value[0], to: value[1] });
        } else {
          slider.update({ from: value });
        }
        forceIonSliderUpdate(slider);
      } finally {
        $el.data("immediate", false);
      }
    }
    subscribe(el, callback) {
      (0, import_jquery18.default)(el).on("change.sliderInputBinding", function() {
        callback(!(0, import_jquery18.default)(el).data("immediate") && !(0, import_jquery18.default)(el).data("animating"));
      });
    }
    unsubscribe(el) {
      (0, import_jquery18.default)(el).off(".sliderInputBinding");
    }
    receiveMessage(el, data) {
      const $el = (0, import_jquery18.default)(el);
      const slider = $el.data("ionRangeSlider");
      const msg = {};
      if (hasDefinedProperty(data, "value")) {
        if (numValues(el) === 2 && data.value instanceof Array) {
          msg.from = data.value[0];
          msg.to = data.value[1];
        } else {
          if (Array.isArray(data.value)) {
            const errorReason = [
              "an empty array.",
              "a single-value array.",
              "an array with more than two values."
            ];
            throw "Slider requires two values to update with an array, but message value was " + errorReason[Math.min(data.value.length, 2)];
          }
          msg.from = data.value;
        }
      }
      const sliderFeatures = [
        "min",
        "max",
        "step"
      ];
      for (let i4 = 0; i4 < sliderFeatures.length; i4++) {
        const feats = sliderFeatures[i4];
        if (hasDefinedProperty(data, feats)) {
          msg[feats] = data[feats];
        }
      }
      updateLabel(data.label, getLabelNode7(el));
      const domElements = [
        "data-type",
        "time-format",
        "timezone"
      ];
      for (let i4 = 0; i4 < domElements.length; i4++) {
        const elem = domElements[i4];
        if (hasDefinedProperty(data, elem)) {
          $el.data(elem, data[elem]);
        }
      }
      const dataType = $el.data("data-type");
      const timeFormat = $el.data("time-format");
      const timezone = $el.data("timezone");
      msg.prettify = getTypePrettifyer(dataType, timeFormat, timezone);
      $el.data("immediate", true);
      try {
        slider.update(msg);
        forceIonSliderUpdate(slider);
      } finally {
        $el.data("immediate", false);
      }
    }
    getRatePolicy(el) {
      return {
        policy: "debounce",
        delay: 250
      };
      el;
    }
    getState(el) {
      el;
    }
    initialize(el) {
      const $el = (0, import_jquery18.default)(el);
      const dataType = $el.data("data-type");
      const timeFormat = $el.data("time-format");
      const timezone = $el.data("timezone");
      const opts = {
        prettify: getTypePrettifyer(dataType, timeFormat, timezone)
      };
      $el.ionRangeSlider(opts);
    }
  };
  function formatNumber(num, thousandSep = ",", decimalSep = ".") {
    const parts = num.toString().split(".");
    parts[0] = parts[0].replace(
      /(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g,
      "$1" + thousandSep
    );
    if (parts.length === 1)
      return parts[0];
    else if (parts.length === 2)
      return parts[0] + decimalSep + parts[1];
    else
      return "";
  }
  (0, import_jquery18.default)(document).on("click", ".slider-animate-button", function(evt) {
    evt.preventDefault();
    const self = (0, import_jquery18.default)(this);
    const target = (0, import_jquery18.default)("#" + $escape(self.attr("data-target-id")));
    const startLabel = "Play";
    const stopLabel = "Pause";
    const loop = self.attr("data-loop") !== void 0 && !/^\s*false\s*$/i.test(self.attr("data-loop"));
    let animInterval = self.attr("data-interval");
    if (isNaN(animInterval))
      animInterval = 1500;
    else
      animInterval = Number(animInterval);
    if (!target.data("animTimer")) {
      let timer;
      if (target.hasClass("jslider")) {
        const slider = target.slider();
        if (!slider.canStepNext())
          slider.resetToStart();
        timer = setInterval(function() {
          if (loop && !slider.canStepNext()) {
            slider.resetToStart();
          } else {
            slider.stepNext();
            if (!loop && !slider.canStepNext()) {
              self.click();
            }
          }
        }, animInterval);
      } else {
        const slider = target.data("ionRangeSlider");
        const sliderCanStep = function() {
          if (slider.options.type === "double")
            return slider.result.to < slider.result.max;
          else
            return slider.result.from < slider.result.max;
        };
        const sliderReset = function() {
          const val = { from: slider.result.min };
          if (slider.options.type === "double")
            val.to = val.from + (slider.result.to - slider.result.from);
          slider.update(val);
          forceIonSliderUpdate(slider);
        };
        const sliderStep = function() {
          const val = {
            from: Math.min(
              slider.result.max,
              slider.result.from + slider.options.step
            )
          };
          if (slider.options.type === "double")
            val.to = Math.min(
              slider.result.max,
              slider.result.to + slider.options.step
            );
          slider.update(val);
          forceIonSliderUpdate(slider);
        };
        if (!sliderCanStep())
          sliderReset();
        timer = setInterval(function() {
          if (loop && !sliderCanStep()) {
            sliderReset();
          } else {
            sliderStep();
            if (!loop && !sliderCanStep()) {
              self.click();
            }
          }
        }, animInterval);
      }
      target.data("animTimer", timer);
      self.attr("title", stopLabel);
      self.addClass("playing");
      target.data("animating", true);
    } else {
      clearTimeout(target.data("animTimer"));
      target.removeData("animTimer");
      self.attr("title", startLabel);
      self.removeClass("playing");
      target.removeData("animating");
    }
  });

  // srcts/src/bindings/input/tabinput.ts
  var import_jquery19 = __toESM(require_jquery());
  function getTabName(anchor) {
    return anchor.attr("data-value") || anchor.text();
  }
  var BootstrapTabInputBinding = class extends InputBinding {
    find(scope) {
      return (0, import_jquery19.default)(scope).find("ul.nav.shiny-tab-input");
    }
    getValue(el) {
      const anchor = isBS3() ? (0, import_jquery19.default)(el).find("li:not(.dropdown).active > a") : (0, import_jquery19.default)(el).find(
        ".nav-link:not(.dropdown-toggle).active, .dropdown-menu .dropdown-item.active"
      );
      if (anchor.length === 1)
        return getTabName(anchor);
      return null;
    }
    setValue(el, value) {
      let success = false;
      if (value) {
        const anchors = isBS3() ? (0, import_jquery19.default)(el).find("li:not(.dropdown) > a") : (0, import_jquery19.default)(el).find(
          ".nav-link:not(.dropdown-toggle), .dropdown-menu .dropdown-item"
        );
        anchors.each(function() {
          if (getTabName((0, import_jquery19.default)(this)) === value) {
            (0, import_jquery19.default)(this).tab("show");
            success = true;
            return false;
          }
          return;
        });
      }
      if (!success) {
        (0, import_jquery19.default)(el).trigger("change");
      }
    }
    getState(el) {
      return { value: this.getValue(el) };
    }
    receiveMessage(el, data) {
      if (hasDefinedProperty(data, "value"))
        this.setValue(el, data.value);
      (0, import_jquery19.default)(el).trigger("change");
    }
    subscribe(el, callback) {
      (0, import_jquery19.default)(el).on(
        "change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding",
        function() {
          callback(false);
        }
      );
    }
    unsubscribe(el) {
      (0, import_jquery19.default)(el).off(".bootstrapTabInputBinding");
    }
  };

  // srcts/src/bindings/input/textarea.ts
  var import_jquery20 = __toESM(require_jquery());
  var TextareaInputBinding = class extends TextInputBinding {
    find(scope) {
      return (0, import_jquery20.default)(scope).find("textarea");
    }
  };

  // srcts/src/bindings/input/index.ts
  function initInputBindings() {
    const inputBindings = new BindingRegistry();
    inputBindings.register(new TextInputBinding(), "shiny.textInput");
    inputBindings.register(new TextareaInputBinding(), "shiny.textareaInput");
    inputBindings.register(new PasswordInputBinding(), "shiny.passwordInput");
    inputBindings.register(new NumberInputBinding(), "shiny.numberInput");
    inputBindings.register(new CheckboxInputBinding(), "shiny.checkboxInput");
    inputBindings.register(
      new CheckboxGroupInputBinding(),
      "shiny.checkboxGroupInput"
    );
    inputBindings.register(new RadioInputBinding(), "shiny.radioInput");
    inputBindings.register(new SliderInputBinding(), "shiny.sliderInput");
    inputBindings.register(new DateInputBinding(), "shiny.dateInput");
    inputBindings.register(new DateRangeInputBinding(), "shiny.dateRangeInput");
    inputBindings.register(new SelectInputBinding(), "shiny.selectInput");
    inputBindings.register(
      new ActionButtonInputBinding(),
      "shiny.actionButtonInput"
    );
    inputBindings.register(
      new BootstrapTabInputBinding(),
      "shiny.bootstrapTabInput"
    );
    const fileInputBinding2 = new FileInputBinding();
    inputBindings.register(fileInputBinding2, "shiny.fileInputBinding");
    return { inputBindings, fileInputBinding: fileInputBinding2 };
  }

  // srcts/src/bindings/output/datatable.ts
  var import_jquery22 = __toESM(require_jquery());

  // srcts/src/time/debounce.ts
  var Debouncer = class {
    constructor(target, func, delayMs) {
      this.target = target;
      this.func = func;
      this.delayMs = delayMs;
      this.timerId = null;
      this.args = null;
    }
    normalCall(...args) {
      this.$clearTimer();
      this.args = args;
      this.timerId = setTimeout(() => {
        if (this.timerId === null)
          return;
        this.$clearTimer();
        this.$invoke();
      }, this.delayMs);
    }
    immediateCall(...args) {
      this.$clearTimer();
      this.args = args;
      this.$invoke();
    }
    isPending() {
      return this.timerId !== null;
    }
    $clearTimer() {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    }
    $invoke() {
      if (this.args && this.args.length > 0) {
        this.func.apply(this.target, this.args);
      } else {
        this.func.apply(this.target);
      }
      this.args = null;
    }
  };
  function debounce(threshold, func) {
    let timerId = null;
    return function thisFunc(...args) {
      if (timerId !== null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(() => {
        if (timerId === null)
          return;
        timerId = null;
        func.apply(thisFunc, args);
      }, threshold);
    };
  }

  // srcts/src/time/invoke.ts
  var Invoker = class {
    constructor(target, func) {
      this.target = target;
      this.func = func;
    }
    normalCall(...args) {
      this.func.apply(this.target, args);
    }
    immediateCall(...args) {
      this.func.apply(this.target, args);
    }
  };

  // srcts/src/time/throttle.ts
  var Throttler = class {
    constructor(target, func, delayMs) {
      this.target = target;
      this.func = func;
      this.delayMs = delayMs;
      this.timerId = null;
      this.args = null;
    }
    normalCall(...args) {
      this.args = args;
      if (this.timerId === null) {
        this.$invoke();
      }
    }
    immediateCall(...args) {
      this.$clearTimer();
      this.args = args;
      this.$invoke();
    }
    isPending() {
      return this.args !== null;
    }
    $clearTimer() {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    }
    $invoke() {
      if (this.args === null) {
        return;
      }
      this.func.apply(this.target, this.args);
      this.args = null;
      this.timerId = setTimeout(() => {
        if (this.timerId === null)
          return;
        this.$clearTimer();
        if (this.isPending()) {
          this.$invoke();
        }
      }, this.delayMs);
    }
  };

  // srcts/src/bindings/output/outputBinding.ts
  var import_jquery21 = __toESM(require_jquery());
  var OutputBinding = class {
    find(scope) {
      throw "Not implemented";
      scope;
    }
    renderValue(el, data) {
      throw "Not implemented";
      el;
      data;
    }
    getId(el) {
      return el.getAttribute("data-input-id") || el.id;
    }
    async onValueChange(el, data) {
      this.clearError(el);
      await this.renderValue(el, data);
    }
    onValueError(el, err) {
      this.renderError(el, err);
    }
    renderError(el, err) {
      this.clearError(el);
      if (err.message === "") {
        (0, import_jquery21.default)(el).empty();
        return;
      }
      let errClass = "shiny-output-error";
      if (err.type !== null) {
        errClass = errClass + " " + import_jquery21.default.map(asArray(err.type), function(type) {
          return errClass + "-" + type;
        }).join(" ");
      }
      (0, import_jquery21.default)(el).addClass(errClass).text(err.message);
    }
    clearError(el) {
      (0, import_jquery21.default)(el).attr("class", function(i4, c4) {
        return c4.replace(/(^|\s)shiny-output-error\S*/g, "");
      });
    }
    showProgress(el, show3) {
      const recalcClass = "recalculating";
      if (show3)
        (0, import_jquery21.default)(el).addClass(recalcClass);
      else
        (0, import_jquery21.default)(el).removeClass(recalcClass);
    }
  };

  // srcts/src/bindings/output/datatable.ts
  var DatatableOutputBinding = class extends OutputBinding {
    find(scope) {
      return (0, import_jquery22.default)(scope).find(".shiny-datatable-output");
    }
    onValueError(el, err) {
      shinyUnbindAll(el);
      this.renderError(el, err);
    }
    renderValue(el, data) {
      const $el = (0, import_jquery22.default)(el).empty();
      if (!data || !data.colnames)
        return;
      const colnames = import_jquery22.default.makeArray(data.colnames);
      let header = import_jquery22.default.map(colnames, function(x2) {
        return "<th>" + x2 + "</th>";
      }).join("");
      header = "<thead><tr>" + header + "</tr></thead>";
      let footer = "";
      if (data.options?.searching ?? true) {
        footer = import_jquery22.default.map(colnames, function(x2) {
          return '<th><input type="text" placeholder="' + escapeHTML(x2.replace(/(<([^>]+)>)/gi, "")) + '" /></th>';
        }).join("");
        footer = "<tfoot>" + footer + "</tfoot>";
      }
      const content = '<table class="table table-striped table-hover">' + header + footer + "</table>";
      $el.append(content);
      if (data.evalOptions) {
        import_jquery22.default.each(data.evalOptions, function(i4, x2) {
          data.options[x2] = indirectEval("(" + data.options[x2] + ")");
        });
      }
      const searchCI = data.options?.search?.caseInsensitive !== false;
      const oTable = (0, import_jquery22.default)(el).children("table").DataTable(
        import_jquery22.default.extend(
          {
            processing: true,
            serverSide: true,
            order: [],
            orderClasses: false,
            pageLength: 25,
            ajax: {
              url: data.action,
              type: "POST",
              data: function(d3) {
                d3.search || (d3.search = {});
                d3.search.caseInsensitive = searchCI;
                d3.escape = data.escape;
              }
            }
          },
          data.options
        )
      );
      if (typeof data.callback === "string") {
        const callback = indirectEval("(" + data.callback + ")");
        if (typeof callback === "function")
          callback(oTable);
      }
      $el.find("label input").first().unbind("keyup").keyup(
        debounce(data.searchDelay, function() {
          oTable.search(this.value).draw();
        })
      );
      const searchInputs = $el.find("tfoot input");
      if (searchInputs.length > 0) {
        import_jquery22.default.each(oTable.settings()[0].aoColumns, function(i4, x2) {
          if (!x2.bSearchable)
            searchInputs.eq(i4).hide();
        });
        searchInputs.keyup(
          debounce(data.searchDelay, function() {
            oTable.column(searchInputs.index(this)).search(this.value).draw();
          })
        );
      }
      $el.parents(".tab-content").css("overflow", "visible");
    }
  };

  // srcts/src/bindings/output/downloadlink.ts
  var import_jquery23 = __toESM(require_jquery());
  var DownloadLinkOutputBinding = class extends OutputBinding {
    find(scope) {
      return (0, import_jquery23.default)(scope).find("a.shiny-download-link");
    }
    renderValue(el, data) {
      el.setAttribute("href", data);
      el.classList.remove("disabled");
      el.removeAttribute("aria-disabled");
      el.removeAttribute("tabindex");
    }
    showProgress(el, show3) {
      return;
      el;
      show3;
    }
  };
  (0, import_jquery23.default)(document).on(
    "click.shinyDownloadLink",
    "a.shiny-download-link",
    function(e4) {
      e4;
      const evt = import_jquery23.default.Event("shiny:filedownload");
      evt.name = this.id;
      evt.href = this.href;
      (0, import_jquery23.default)(document).trigger(evt);
    }
  );

  // srcts/src/bindings/output/html.ts
  var import_jquery26 = __toESM(require_jquery());

  // srcts/src/shiny/render.ts
  var import_jquery25 = __toESM(require_jquery());

  // srcts/src/shiny/sendImageSize.ts
  var SendImageSize = class {
    setImageSend(inputBatchSender, doSendImageSize) {
      const sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
      this.regular = function() {
        sendImageSizeDebouncer.normalCall();
      };
      inputBatchSender.lastChanceCallback.push(function() {
        if (sendImageSizeDebouncer.isPending())
          sendImageSizeDebouncer.immediateCall();
      });
      this.transitioned = debounce(200, this.regular);
      return sendImageSizeDebouncer;
    }
  };
  var sendImageSizeFns = new SendImageSize();

  // srcts/src/shiny/singletons.ts
  var import_jquery24 = __toESM(require_jquery());
  var reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
  var reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;
  var knownSingletons = {};
  function renderHtml(html, el, where) {
    const processed = processHtml(html);
    addToHead(processed.head);
    register(processed.singletons);
    switch (where.toLowerCase()) {
      case "replace":
        (0, import_jquery24.default)(el).html(processed.html);
        break;
      case "beforebegin":
        (0, import_jquery24.default)(el).before(processed.html);
        break;
      case "afterbegin":
        (0, import_jquery24.default)(el).prepend(processed.html);
        break;
      case "beforeend":
        (0, import_jquery24.default)(el).append(processed.html);
        break;
      case "afterend":
        (0, import_jquery24.default)(el).after(processed.html);
        break;
      default:
        throw new Error("Unknown where position: " + where);
    }
    return processed;
  }
  function register(s4) {
    import_jquery24.default.extend(knownSingletons, s4);
  }
  function registerNames(s4) {
    if (typeof s4 === "string") {
      knownSingletons[s4] = true;
    } else if (s4 instanceof Array) {
      for (let i4 = 0; i4 < s4.length; i4++) {
        knownSingletons[s4[i4]] = true;
      }
    }
  }
  function addToHead(head) {
    if (head.length > 0) {
      const tempDiv = (0, import_jquery24.default)("<div>" + head + "</div>").get(0);
      const $head = (0, import_jquery24.default)("head");
      while (tempDiv.hasChildNodes()) {
        $head.append(tempDiv.firstChild);
      }
    }
  }
  function processHtml(val) {
    const newSingletons = {};
    let newVal;
    const findNewPayload = function(match, p1, sig, payload) {
      if (knownSingletons[sig] || newSingletons[sig])
        return "";
      newSingletons[sig] = true;
      return payload;
    };
    while (true) {
      newVal = val.replace(reSingleton, findNewPayload);
      if (val.length === newVal.length)
        break;
      val = newVal;
    }
    const heads = [];
    const headAddPayload = function(match, payload) {
      heads.push(payload);
      return "";
    };
    while (true) {
      newVal = val.replace(reHead, headAddPayload);
      if (val.length === newVal.length)
        break;
      val = newVal;
    }
    return {
      html: val,
      head: heads.join("\n"),
      singletons: newSingletons
    };
  }

  // srcts/src/shiny/render.ts
  async function renderContentAsync(el, content, where = "replace") {
    if (where === "replace") {
      shinyUnbindAll(el);
    }
    let html = "";
    let dependencies = [];
    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (typeof content === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }
    await renderHtmlAsync(html, el, dependencies, where);
    let scope = el;
    if (where === "replace") {
      shinyInitializeInputs(el);
      await shinyBindAll(el);
    } else {
      const $parent = (0, import_jquery25.default)(el).parent();
      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          const $grandparent = $parent.parent();
          if ($grandparent.length > 0)
            scope = $grandparent;
        }
      }
      shinyInitializeInputs(scope);
      await shinyBindAll(scope);
    }
  }
  function renderContent(el, content, where = "replace") {
    if (where === "replace") {
      shinyUnbindAll(el);
    }
    let html = "";
    let dependencies = [];
    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (typeof content === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }
    renderHtml2(html, el, dependencies, where);
    let scope = el;
    if (where === "replace") {
      shinyInitializeInputs(el);
      return shinyBindAll(el);
    } else {
      const $parent = (0, import_jquery25.default)(el).parent();
      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          const $grandparent = $parent.parent();
          if ($grandparent.length > 0)
            scope = $grandparent;
        }
      }
      shinyInitializeInputs(scope);
      return shinyBindAll(scope);
    }
  }
  async function renderHtmlAsync(html, el, dependencies, where = "replace") {
    await renderDependenciesAsync(dependencies);
    return renderHtml(html, el, where);
  }
  function renderHtml2(html, el, dependencies, where = "replace") {
    renderDependencies(dependencies);
    return renderHtml(html, el, where);
  }
  async function renderDependenciesAsync(dependencies) {
    if (dependencies) {
      for (const dep of dependencies) {
        await renderDependencyAsync(dep);
      }
    }
  }
  function renderDependencies(dependencies) {
    if (dependencies) {
      for (const dep of dependencies) {
        renderDependency(dep);
      }
    }
  }
  var htmlDependencies = {};
  function registerDependency(name, version) {
    htmlDependencies[name] = version;
  }
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
  function addStylesheetsAndRestyle(links) {
    const $head = (0, import_jquery25.default)("head").first();
    const refreshStyle = function(href, oldSheet) {
      const xhr = new XMLHttpRequest();
      xhr.open("GET", href);
      xhr.onload = function() {
        const id = "shiny_restyle_" + href.split("?restyle")[0].replace(/\W/g, "_");
        const oldStyle = $head.find("style#" + id);
        const newStyle = (0, import_jquery25.default)("<style>").attr("id", id).html(xhr.responseText);
        $head.append(newStyle);
        oldStyle.remove();
        removeSheet(oldSheet);
        sendImageSizeFns.transitioned();
      };
      xhr.send();
    };
    const findSheet = function(href) {
      if (!href)
        return null;
      for (let i4 = 0; i4 < document.styleSheets.length; i4++) {
        const sheet = document.styleSheets[i4];
        if (typeof sheet.href === "string" && sheet.href.indexOf(href) > -1) {
          return sheet;
        }
      }
      return null;
    };
    const removeSheet = function(sheet) {
      if (!sheet)
        return;
      sheet.disabled = true;
      if (isIE())
        sheet.cssText = "";
      if (sheet.ownerNode instanceof Element) {
        (0, import_jquery25.default)(sheet.ownerNode).remove();
      }
    };
    links.map((link) => {
      const $link = (0, import_jquery25.default)(link);
      const oldSheet = findSheet($link.attr("href"));
      const href = $link.attr("href") + "?restyle=" + new Date().getTime();
      if (isIE()) {
        refreshStyle(href, oldSheet);
      } else {
        $link.attr("href", href);
        $link.attr("onload", () => {
          const $dummyEl = (0, import_jquery25.default)("<div>").css("transition", "0.1s all").css("position", "absolute").css("top", "-1000px").css("left", "0");
          $dummyEl.one("transitionend", () => {
            $dummyEl.remove();
            removeSheet(oldSheet);
            sendImageSizeFns.transitioned();
          });
          (0, import_jquery25.default)(document.body).append($dummyEl);
          const color = "#" + Math.floor(Math.random() * 16777215).toString(16);
          setTimeout(() => $dummyEl.css("color", color), 10);
        });
        $head.append(link);
      }
    });
  }
  function getStylesheetLinkTags(dep) {
    return dep.stylesheet.map((x2) => {
      if (!hasDefinedProperty(x2, "rel"))
        x2.rel = "stylesheet";
      if (!hasDefinedProperty(x2, "type"))
        x2.type = "text/css";
      const link = document.createElement("link");
      Object.entries(x2).forEach(function([attr, val]) {
        if (attr === "href") {
          val = encodeURI(val);
        }
        link.setAttribute(attr, val ? val : "");
      });
      return link;
    });
  }
  function appendStylesheetLinkTags(dep, $head) {
    const stylesheetLinks = getStylesheetLinkTags(dep);
    if (stylesheetLinks.length !== 0) {
      $head.append(stylesheetLinks);
    }
  }
  function appendScriptTags(dep, $head) {
    dep.script.forEach((x2) => {
      const script = document.createElement("script");
      Object.entries(x2).forEach(function([attr, val]) {
        if (attr === "src") {
          val = encodeURI(val);
        }
        script.setAttribute(attr, val ? val : "");
      });
      $head.append(script);
    });
  }
  async function appendScriptTagsAsync(dep) {
    const scriptPromises = [];
    dep.script.forEach((x2) => {
      const script = document.createElement("script");
      if (!hasDefinedProperty(x2, "async")) {
        script.async = false;
      }
      Object.entries(x2).forEach(function([attr, val]) {
        if (attr === "src") {
          val = encodeURI(val);
        }
        script.setAttribute(attr, val ? val : "");
      });
      const p3 = new Promise((resolve, reject) => {
        script.onload = (e4) => {
          resolve(null);
        };
        script.onerror = (e4) => {
          reject(e4);
        };
      });
      scriptPromises.push(p3);
      document.head.append(script);
    });
    await Promise.allSettled(scriptPromises);
  }
  function appendMetaTags(dep, $head) {
    dep.meta.forEach((x2) => {
      const meta = document.createElement("meta");
      for (const [attr, val] of Object.entries(x2)) {
        meta.setAttribute(attr, val);
      }
      $head.append(meta);
    });
  }
  function appendAttachmentLinkTags(dep, $head) {
    dep.attachment.forEach((x2) => {
      const link = (0, import_jquery25.default)("<link rel='attachment'>").attr("id", dep.name + "-" + x2.key + "-attachment").attr("href", encodeURI(x2.href));
      $head.append(link);
    });
  }
  function appendExtraHeadContent(dep, $head) {
    if (dep.head) {
      const $newHead = (0, import_jquery25.default)("<head></head>");
      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
  }
  async function renderDependencyAsync(dep_) {
    const dep = normalizeHtmlDependency(dep_);
    if (needsRestyle(dep)) {
      addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
      return true;
    }
    if (hasDefinedProperty(htmlDependencies, dep.name))
      return false;
    registerDependency(dep.name, dep.version);
    const $head = (0, import_jquery25.default)("head").first();
    appendMetaTags(dep, $head);
    appendStylesheetLinkTags(dep, $head);
    await appendScriptTagsAsync(dep);
    appendAttachmentLinkTags(dep, $head);
    appendExtraHeadContent(dep, $head);
    return true;
  }
  function renderDependency(dep_) {
    const dep = normalizeHtmlDependency(dep_);
    if (needsRestyle(dep)) {
      addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
      return true;
    }
    if (hasDefinedProperty(htmlDependencies, dep.name))
      return false;
    registerDependency(dep.name, dep.version);
    const $head = (0, import_jquery25.default)("head").first();
    appendMetaTags(dep, $head);
    appendStylesheetLinkTags(dep, $head);
    appendScriptTags(dep, $head);
    appendAttachmentLinkTags(dep, $head);
    appendExtraHeadContent(dep, $head);
    return true;
  }
  function normalizeHtmlDependency(dep) {
    const hrefPrefix = dep.src?.href;
    const result = {
      name: dep.name,
      version: dep.version,
      restyle: dep.restyle,
      meta: [],
      stylesheet: [],
      script: [],
      attachment: [],
      head: dep.head
    };
    if (dep.meta) {
      if (Array.isArray(dep.meta)) {
        result.meta = dep.meta;
      } else {
        result.meta = Object.entries(dep.meta).map(function([attr, val]) {
          return { name: attr, content: val };
        });
      }
    }
    result.stylesheet = asArray(dep.stylesheet).map((s4) => {
      if (typeof s4 === "string") {
        s4 = { href: s4 };
      }
      if (hrefPrefix) {
        s4.href = hrefPrefix + "/" + s4.href;
      }
      return s4;
    });
    result.script = asArray(dep.script).map((s4) => {
      if (typeof s4 === "string") {
        s4 = { src: s4 };
      }
      if (hrefPrefix) {
        s4.src = hrefPrefix + "/" + s4.src;
      }
      return s4;
    });
    let attachments = dep.attachment;
    if (!attachments)
      attachments = [];
    if (typeof attachments === "string")
      attachments = [attachments];
    if (Array.isArray(attachments)) {
      const tmp = attachments;
      attachments = tmp.map((attachment, index) => {
        if (typeof attachment === "string") {
          return {
            key: (index + 1).toString(),
            href: attachment
          };
        } else {
          return attachment;
        }
      });
    } else {
      attachments = Object.entries(attachments).map(function([attr, val]) {
        return { key: attr, href: val };
      });
    }
    result.attachment = attachments.map((s4) => {
      if (hrefPrefix) {
        s4.href = hrefPrefix + "/" + s4.href;
      }
      return s4;
    });
    return result;
  }

  // srcts/src/bindings/output/html.ts
  var HtmlOutputBinding = class extends OutputBinding {
    find(scope) {
      return (0, import_jquery26.default)(scope).find(".shiny-html-output");
    }
    onValueError(el, err) {
      shinyUnbindAll(el);
      this.renderError(el, err);
    }
    async renderValue(el, data) {
      await renderContentAsync(el, data);
    }
  };

  // srcts/src/bindings/output/image.ts
  var import_jquery31 = __toESM(require_jquery());

  // srcts/src/imageutils/createBrush.ts
  var import_jquery28 = __toESM(require_jquery());

  // srcts/src/imageutils/initCoordmap.ts
  var import_jquery27 = __toESM(require_jquery());

  // srcts/src/imageutils/initPanelScales.ts
  function mapLinear(x2, domainMin, domainMax, rangeMin, rangeMax, clip = true) {
    clip = clip || true;
    const factor = (rangeMax - rangeMin) / (domainMax - domainMin);
    const val = x2 - domainMin;
    let newval = val * factor + rangeMin;
    if (clip) {
      const max = Math.max(rangeMax, rangeMin);
      const min = Math.min(rangeMax, rangeMin);
      if (newval > max)
        newval = max;
      else if (newval < min)
        newval = min;
    }
    return newval;
  }
  function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
    return {
      scale: function(val, clip) {
        if (logbase)
          val = Math.log(val) / Math.log(logbase);
        return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
      },
      scaleInv: function(val, clip) {
        let res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);
        if (logbase)
          res = Math.pow(logbase, res);
        return res;
      }
    };
  }
  function addScaleFuns(panel_) {
    const panel = panel_;
    const d3 = panel.domain;
    const r5 = panel.range;
    const xlog = panel.log && panel.log.x ? panel.log.x : null;
    const ylog = panel.log && panel.log.y ? panel.log.y : null;
    const xscaler = scaler1D(d3.left, d3.right, r5.left, r5.right, xlog);
    const yscaler = scaler1D(d3.bottom, d3.top, r5.bottom, r5.top, ylog);
    function scaleDataToImg(val, clip) {
      return mapValues(val, (value, key) => {
        const prefix = key.substring(0, 1);
        if (prefix === "x") {
          return xscaler.scale(value, clip);
        } else if (prefix === "y") {
          return yscaler.scale(value, clip);
        }
        return null;
      });
    }
    panel.scaleDataToImg = scaleDataToImg;
    function scaleImgToData(val, clip) {
      return mapValues(val, (value, key) => {
        const prefix = key.substring(0, 1);
        if (prefix === "x") {
          return xscaler.scaleInv(value, clip);
        } else if (prefix === "y") {
          return yscaler.scaleInv(value, clip);
        }
        return null;
      });
    }
    panel.scaleImgToData = scaleImgToData;
    panel.clipImg = function(offsetImg) {
      const newOffset = {
        x: offsetImg.x,
        y: offsetImg.y
      };
      const bounds = panel.range;
      if (offsetImg.x > bounds.right)
        newOffset.x = bounds.right;
      else if (offsetImg.x < bounds.left)
        newOffset.x = bounds.left;
      if (offsetImg.y > bounds.bottom)
        newOffset.y = bounds.bottom;
      else if (offsetImg.y < bounds.top)
        newOffset.y = bounds.top;
      return newOffset;
    };
    return panel;
  }
  function initPanelScales(panels) {
    return panels.map((panel) => addScaleFuns(panel));
  }

  // srcts/src/imageutils/initCoordmap.ts
  function findScalingRatio($el) {
    const boundingRect = $el[0].getBoundingClientRect();
    return {
      x: boundingRect.width / $el.outerWidth(),
      y: boundingRect.height / $el.outerHeight()
    };
  }
  function findOrigin($el) {
    const offset = $el.offset();
    const scalingRatio = findScalingRatio($el);
    const paddingBorder = {
      left: parseInt($el.css("border-left-width")) + parseInt($el.css("padding-left")),
      top: parseInt($el.css("border-top-width")) + parseInt($el.css("padding-top"))
    };
    return {
      x: offset.left + scalingRatio.x * paddingBorder.left,
      y: offset.top + scalingRatio.y * paddingBorder.top
    };
  }
  function findDims($el) {
    const contentRatio = {
      x: $el.width() / $el.outerWidth(),
      y: $el.height() / $el.outerHeight()
    };
    const boundingRect = $el[0].getBoundingClientRect();
    return {
      x: contentRatio.x * boundingRect.width,
      y: contentRatio.y * boundingRect.height
    };
  }
  function initCoordmap($el, coordmap_) {
    const $img = $el.find("img");
    const img = $img[0];
    if (coordmap_.panels.length === 0) {
      const bounds = {
        top: 0,
        left: 0,
        right: img.naturalWidth - 1,
        bottom: img.naturalHeight - 1
      };
      coordmap_.panels[0] = {
        domain: bounds,
        range: bounds,
        mapping: {}
      };
    }
    const coordmap = coordmap_;
    coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
    coordmap.dims.width = coordmap.dims.width || img.naturalWidth;
    coordmap.panels = initPanelScales(coordmap_.panels);
    coordmap.mouseOffsetCss = function(mouseEvent) {
      const imgOrigin = findOrigin($img);
      return {
        x: mouseEvent.pageX - imgOrigin.x,
        y: mouseEvent.pageY - imgOrigin.y
      };
    };
    function scaleCssToImg(offsetCss) {
      const pixelScaling = coordmap.imgToCssScalingRatio();
      const result = mapValues(offsetCss, (value, key) => {
        const prefix = key.substring(0, 1);
        if (prefix === "x") {
          return offsetCss[key] / pixelScaling.x;
        } else if (prefix === "y") {
          return offsetCss[key] / pixelScaling.y;
        }
        return null;
      });
      return result;
    }
    coordmap.scaleCssToImg = scaleCssToImg;
    function scaleImgToCss(offsetImg) {
      const pixelScaling = coordmap.imgToCssScalingRatio();
      const result = mapValues(offsetImg, (value, key) => {
        const prefix = key.substring(0, 1);
        if (prefix === "x") {
          return offsetImg[key] * pixelScaling.x;
        } else if (prefix === "y") {
          return offsetImg[key] * pixelScaling.y;
        }
        return null;
      });
      return result;
    }
    coordmap.scaleImgToCss = scaleImgToCss;
    coordmap.imgToCssScalingRatio = function() {
      const imgDims = findDims($img);
      return {
        x: imgDims.x / coordmap.dims.width,
        y: imgDims.y / coordmap.dims.height
      };
    };
    coordmap.cssToImgScalingRatio = function() {
      const res = coordmap.imgToCssScalingRatio();
      return {
        x: 1 / res.x,
        y: 1 / res.y
      };
    };
    coordmap.getPanelCss = function(offsetCss, expand = 0) {
      const offsetImg = coordmap.scaleCssToImg(offsetCss);
      const x2 = offsetImg.x;
      const y3 = offsetImg.y;
      const cssToImgRatio = coordmap.cssToImgScalingRatio();
      const expandImg = {
        x: expand * cssToImgRatio.x,
        y: expand * cssToImgRatio.y
      };
      const matches = [];
      const dists = [];
      let i4;
      for (i4 = 0; i4 < coordmap.panels.length; i4++) {
        const b3 = coordmap.panels[i4].range;
        if (x2 <= b3.right + expandImg.x && x2 >= b3.left - expandImg.x && y3 <= b3.bottom + expandImg.y && y3 >= b3.top - expandImg.y) {
          matches.push(coordmap.panels[i4]);
          let xdist = 0;
          let ydist = 0;
          if (x2 > b3.right && x2 <= b3.right + expandImg.x) {
            xdist = x2 - b3.right;
          } else if (x2 < b3.left && x2 >= b3.left - expandImg.x) {
            xdist = x2 - b3.left;
          }
          if (y3 > b3.bottom && y3 <= b3.bottom + expandImg.y) {
            ydist = y3 - b3.bottom;
          } else if (y3 < b3.top && y3 >= b3.top - expandImg.y) {
            ydist = y3 - b3.top;
          }
          dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
        }
      }
      if (matches.length) {
        const minDist = Math.min.apply(null, dists);
        for (i4 = 0; i4 < matches.length; i4++) {
          if (dists[i4] === minDist) {
            return matches[i4];
          }
        }
      }
      return null;
    };
    coordmap.isInPanelCss = function(offsetCss, expand = 0) {
      if (coordmap.getPanelCss(offsetCss, expand))
        return true;
      return false;
    };
    coordmap.mouseCoordinateSender = function(inputId, clip = true, nullOutside = false) {
      return function(e4) {
        if (e4 === null) {
          shinySetInputValue(inputId, null);
          return;
        }
        const coordsCss = coordmap.mouseOffsetCss(e4);
        if (!coordmap.isInPanelCss(coordsCss)) {
          if (nullOutside) {
            shinySetInputValue(inputId, null);
            return;
          }
          if (clip)
            return;
          const coords2 = {
            coords_css: coordsCss,
            coords_img: coordmap.scaleCssToImg(coordsCss)
          };
          shinySetInputValue(inputId, coords2, { priority: "event" });
          return;
        }
        const panel = coordmap.getPanelCss(coordsCss);
        const coordsImg = coordmap.scaleCssToImg(coordsCss);
        const coordsData = panel.scaleImgToData(coordsImg);
        const coords = {
          x: coordsData?.x,
          y: coordsData?.y,
          coords_css: coordsCss,
          coords_img: coordsImg,
          img_css_ratio: coordmap.cssToImgScalingRatio()
        };
        import_jquery27.default.extend(coords, panel.panel_vars);
        coords.mapping = panel.mapping;
        coords.domain = panel.domain;
        coords.range = panel.range;
        coords.log = panel.log;
        shinySetInputValue(inputId, coords, { priority: "event" });
      };
    };
    return coordmap;
  }

  // srcts/src/imageutils/findbox.ts
  function findBox(offset1, offset2) {
    return {
      xmin: Math.min(offset1.x, offset2.x),
      xmax: Math.max(offset1.x, offset2.x),
      ymin: Math.min(offset1.y, offset2.y),
      ymax: Math.max(offset1.y, offset2.y)
    };
  }

  // srcts/src/imageutils/shiftToRange.ts
  function shiftToRange(vals, min, max) {
    if (!(vals instanceof Array))
      vals = [vals];
    const maxval = Math.max.apply(null, vals);
    const minval = Math.min.apply(null, vals);
    let shiftAmount = 0;
    if (maxval > max) {
      shiftAmount = max - maxval;
    } else if (minval < min) {
      shiftAmount = min - minval;
    }
    const newvals = [];
    for (let i4 = 0; i4 < vals.length; i4++) {
      newvals[i4] = vals[i4] + shiftAmount;
    }
    return newvals;
  }

  // srcts/src/imageutils/createBrush.ts
  function createBrush($el, opts, coordmap, expandPixels) {
    const resizeExpand = 10;
    const el = $el[0];
    let $div = null;
    const state = {};
    const cssToImg = coordmap.scaleCssToImg;
    const imgToCss = coordmap.scaleImgToCss;
    reset();
    function reset() {
      state.brushing = false;
      state.dragging = false;
      state.resizing = false;
      state.down = { x: NaN, y: NaN };
      state.up = { x: NaN, y: NaN };
      state.resizeSides = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };
      state.boundsCss = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };
      state.boundsData = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };
      state.panel = null;
      state.changeStartBounds = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };
      if ($div)
        $div.remove();
    }
    function hasOldBrush() {
      const oldDiv = $el.find("#" + el.id + "_brush");
      return oldDiv.length > 0;
    }
    function importOldBrush() {
      const oldDiv = $el.find("#" + el.id + "_brush");
      if (oldDiv.length === 0)
        return;
      const oldBoundsData = oldDiv.data("bounds-data");
      const oldPanel = oldDiv.data("panel");
      if (!oldBoundsData || !oldPanel)
        return;
      for (let i4 = 0; i4 < coordmap.panels.length; i4++) {
        const curPanel = coordmap.panels[i4];
        if (equal(oldPanel.mapping, curPanel.mapping) && equal(oldPanel.panel_vars, curPanel.panel_vars)) {
          state.panel = coordmap.panels[i4];
          break;
        }
      }
      if (state.panel === null) {
        oldDiv.remove();
        return;
      }
      $div = oldDiv;
      boundsData(oldBoundsData);
      updateDiv();
    }
    function onResize() {
      const boundsDataVal = boundsData();
      if (Object.values(boundsDataVal).some(isnan))
        return;
      boundsData(boundsDataVal);
      updateDiv();
    }
    function isInsideBrush(offsetCss) {
      const bounds = state.boundsCss;
      return offsetCss.x <= bounds.xmax && offsetCss.x >= bounds.xmin && offsetCss.y <= bounds.ymax && offsetCss.y >= bounds.ymin;
    }
    function isInResizeArea(offsetCss) {
      const sides = whichResizeSides(offsetCss);
      return sides.left || sides.right || sides.top || sides.bottom;
    }
    function whichResizeSides(offsetCss) {
      const b3 = state.boundsCss;
      const e4 = {
        xmin: b3.xmin - resizeExpand,
        xmax: b3.xmax + resizeExpand,
        ymin: b3.ymin - resizeExpand,
        ymax: b3.ymax + resizeExpand
      };
      const res = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };
      if ((opts.brushDirection === "xy" || opts.brushDirection === "x") && offsetCss.y <= e4.ymax && offsetCss.y >= e4.ymin) {
        if (offsetCss.x < b3.xmin && offsetCss.x >= e4.xmin)
          res.left = true;
        else if (offsetCss.x > b3.xmax && offsetCss.x <= e4.xmax)
          res.right = true;
      }
      if ((opts.brushDirection === "xy" || opts.brushDirection === "y") && offsetCss.x <= e4.xmax && offsetCss.x >= e4.xmin) {
        if (offsetCss.y < b3.ymin && offsetCss.y >= e4.ymin)
          res.top = true;
        else if (offsetCss.y > b3.ymax && offsetCss.y <= e4.ymax)
          res.bottom = true;
      }
      return res;
    }
    function boundsCss(boxCss) {
      if (boxCss === void 0) {
        return { ...state.boundsCss };
      }
      let minCss = { x: boxCss.xmin, y: boxCss.ymin };
      let maxCss = { x: boxCss.xmax, y: boxCss.ymax };
      const panel = state.panel;
      const panelBoundsImg = panel.range;
      if (opts.brushClip) {
        minCss = imgToCss(panel.clipImg(cssToImg(minCss)));
        maxCss = imgToCss(panel.clipImg(cssToImg(maxCss)));
      }
      if (opts.brushDirection === "xy") {
      } else if (opts.brushDirection === "x") {
        minCss.y = imgToCss({ y: panelBoundsImg.top }).y;
        maxCss.y = imgToCss({ y: panelBoundsImg.bottom }).y;
      } else if (opts.brushDirection === "y") {
        minCss.x = imgToCss({ x: panelBoundsImg.left }).x;
        maxCss.x = imgToCss({ x: panelBoundsImg.right }).x;
      }
      state.boundsCss = {
        xmin: minCss.x,
        xmax: maxCss.x,
        ymin: minCss.y,
        ymax: maxCss.y
      };
      const minData = panel.scaleImgToData(cssToImg(minCss));
      const maxData = panel.scaleImgToData(cssToImg(maxCss));
      state.boundsData = findBox(minData, maxData);
      state.boundsData = mapValues(
        state.boundsData,
        (val) => roundSignif(val, 14)
      );
      $div.data("bounds-data", state.boundsData);
      $div.data("panel", state.panel);
      return void 0;
    }
    function boundsData(boxData) {
      if (typeof boxData === "undefined") {
        return { ...state.boundsData };
      }
      let boxCss = imgToCss(state.panel.scaleDataToImg(boxData));
      boxCss = mapValues(boxCss, (val) => roundSignif(val, 13));
      boundsCss({
        xmin: Math.min(boxCss.xmin, boxCss.xmax),
        xmax: Math.max(boxCss.xmin, boxCss.xmax),
        ymin: Math.min(boxCss.ymin, boxCss.ymax),
        ymax: Math.max(boxCss.ymin, boxCss.ymax)
      });
      return void 0;
    }
    function getPanel2() {
      return state.panel;
    }
    function addDiv() {
      if ($div)
        $div.remove();
      $div = (0, import_jquery28.default)(document.createElement("div")).attr("id", el.id + "_brush").css({
        "background-color": opts.brushFill,
        opacity: opts.brushOpacity,
        "pointer-events": "none",
        position: "absolute"
      }).hide();
      const borderStyle = "1px solid " + opts.brushStroke;
      if (opts.brushDirection === "xy") {
        $div.css({
          border: borderStyle
        });
      } else if (opts.brushDirection === "x") {
        $div.css({
          "border-left": borderStyle,
          "border-right": borderStyle
        });
      } else if (opts.brushDirection === "y") {
        $div.css({
          "border-top": borderStyle,
          "border-bottom": borderStyle
        });
      }
      $el.append($div);
      $div.offset(
        { x: 0, y: 0 }
      ).width(0).outerHeight(0);
    }
    function updateDiv() {
      const imgOffsetCss = findOrigin($el.find("img"));
      const b3 = state.boundsCss;
      $div.offset({
        top: imgOffsetCss.y + b3.ymin,
        left: imgOffsetCss.x + b3.xmin
      }).outerWidth(b3.xmax - b3.xmin + 1).outerHeight(b3.ymax - b3.ymin + 1);
    }
    function down(offsetCss) {
      if (offsetCss === void 0)
        return state.down;
      state.down = offsetCss;
      return void 0;
    }
    function up(offsetCss) {
      if (offsetCss === void 0)
        return state.up;
      state.up = offsetCss;
      return void 0;
    }
    function isBrushing() {
      return state.brushing;
    }
    function startBrushing() {
      state.brushing = true;
      addDiv();
      state.panel = coordmap.getPanelCss(state.down, expandPixels);
      boundsCss(findBox(state.down, state.down));
      updateDiv();
    }
    function brushTo(offsetCss) {
      boundsCss(findBox(state.down, offsetCss));
      $div.show();
      updateDiv();
    }
    function stopBrushing() {
      state.brushing = false;
      boundsCss(findBox(state.down, state.up));
    }
    function isDragging() {
      return state.dragging;
    }
    function startDragging() {
      state.dragging = true;
      state.changeStartBounds = { ...state.boundsCss };
    }
    function dragTo(offsetCss) {
      const dx = offsetCss.x - state.down.x;
      const dy = offsetCss.y - state.down.y;
      const start = state.changeStartBounds;
      let newBoundsCss = {
        xmin: start.xmin + dx,
        xmax: start.xmax + dx,
        ymin: start.ymin + dy,
        ymax: start.ymax + dy
      };
      if (opts.brushClip) {
        const panel = state.panel;
        const panelBoundsImg = panel.range;
        const newBoundsImg = cssToImg(newBoundsCss);
        let xvalsImg = [newBoundsImg.xmin, newBoundsImg.xmax];
        let yvalsImg = [newBoundsImg.ymin, newBoundsImg.ymax];
        xvalsImg = shiftToRange(
          xvalsImg,
          panelBoundsImg.left,
          panelBoundsImg.right
        );
        yvalsImg = shiftToRange(
          yvalsImg,
          panelBoundsImg.top,
          panelBoundsImg.bottom
        );
        newBoundsCss = imgToCss({
          xmin: xvalsImg[0],
          xmax: xvalsImg[1],
          ymin: yvalsImg[0],
          ymax: yvalsImg[1]
        });
      }
      boundsCss(newBoundsCss);
      updateDiv();
    }
    function stopDragging() {
      state.dragging = false;
    }
    function isResizing() {
      return state.resizing;
    }
    function startResizing() {
      state.resizing = true;
      state.changeStartBounds = { ...state.boundsCss };
      state.resizeSides = whichResizeSides(state.down);
    }
    function resizeTo(offsetCss) {
      const dCss = {
        x: offsetCss.x - state.down.x,
        y: offsetCss.y - state.down.y
      };
      const dImg = cssToImg(dCss);
      const bImg = cssToImg(state.changeStartBounds);
      const panel = state.panel;
      const panelBoundsImg = panel.range;
      if (state.resizeSides.left) {
        const xminImg = shiftToRange(
          bImg.xmin + dImg.x,
          panelBoundsImg.left,
          bImg.xmax
        )[0];
        bImg.xmin = xminImg;
      } else if (state.resizeSides.right) {
        const xmaxImg = shiftToRange(
          bImg.xmax + dImg.x,
          bImg.xmin,
          panelBoundsImg.right
        )[0];
        bImg.xmax = xmaxImg;
      }
      if (state.resizeSides.top) {
        const yminImg = shiftToRange(
          bImg.ymin + dImg.y,
          panelBoundsImg.top,
          bImg.ymax
        )[0];
        bImg.ymin = yminImg;
      } else if (state.resizeSides.bottom) {
        const ymaxImg = shiftToRange(
          bImg.ymax + dImg.y,
          bImg.ymin,
          panelBoundsImg.bottom
        )[0];
        bImg.ymax = ymaxImg;
      }
      boundsCss(imgToCss(bImg));
      updateDiv();
    }
    function stopResizing() {
      state.resizing = false;
    }
    return {
      reset,
      hasOldBrush,
      importOldBrush,
      isInsideBrush,
      isInResizeArea,
      whichResizeSides,
      onResize,
      boundsCss,
      boundsData,
      getPanel: getPanel2,
      down,
      up,
      isBrushing,
      startBrushing,
      brushTo,
      stopBrushing,
      isDragging,
      startDragging,
      dragTo,
      stopDragging,
      isResizing,
      startResizing,
      resizeTo,
      stopResizing
    };
  }

  // srcts/src/imageutils/createClickInfo.ts
  var import_jquery29 = __toESM(require_jquery());
  function createClickInfo($el, dblclickId, dblclickDelay) {
    let clickTimer = void 0;
    let pendingE = null;
    function triggerEvent(newEventType, e4) {
      const e22 = import_jquery29.default.Event(newEventType, {
        which: e4.which,
        pageX: e4.pageX,
        pageY: e4.pageY
      });
      $el.trigger(e22);
    }
    function triggerPendingMousedown2() {
      if (pendingE) {
        triggerEvent("mousedown2", pendingE);
        pendingE = null;
      }
    }
    function scheduleMousedown2(e4) {
      pendingE = e4;
      clickTimer = window.setTimeout(function() {
        triggerPendingMousedown2();
      }, dblclickDelay);
    }
    function mousedown(e4) {
      if (e4.which !== 1)
        return;
      if (!dblclickId) {
        triggerEvent("mousedown2", e4);
        return;
      }
      if (pendingE === null) {
        scheduleMousedown2(e4);
      } else {
        clearTimeout(clickTimer);
        if (pendingE && Math.abs(pendingE.pageX - e4.pageX) > 2 || Math.abs(pendingE.pageY - e4.pageY) > 2) {
          triggerPendingMousedown2();
          scheduleMousedown2(e4);
        } else {
          pendingE = null;
          triggerEvent("dblclick2", e4);
        }
      }
    }
    function dblclickIE8(e4) {
      e4.which = 1;
      triggerEvent("dblclick2", e4);
    }
    return {
      mousedown,
      dblclickIE8
    };
  }

  // srcts/src/imageutils/createHandlers.ts
  var import_jquery30 = __toESM(require_jquery());
  function createClickHandler(inputId, clip, coordmap) {
    const clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);
    clickInfoSender(null);
    return {
      mousedown: function(e4) {
        if (e4.which !== 1)
          return;
        clickInfoSender(e4);
      },
      onResetImg: function() {
        clickInfoSender(null);
      },
      onResize: null
    };
  }
  function createHoverHandler(inputId, delay, delayType, clip, nullOutside, coordmap) {
    const sendHoverInfo = coordmap.mouseCoordinateSender(
      inputId,
      clip,
      nullOutside
    );
    let hoverInfoSender;
    if (delayType === "throttle")
      hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
    else
      hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);
    hoverInfoSender.immediateCall(null);
    let mouseout;
    if (nullOutside)
      mouseout = function() {
        hoverInfoSender.normalCall(null);
      };
    else
      mouseout = function() {
      };
    return {
      mousemove: function(e4) {
        hoverInfoSender.normalCall(e4);
      },
      mouseout,
      onResetImg: function() {
        hoverInfoSender.immediateCall(null);
      },
      onResize: null
    };
  }
  function createBrushHandler(inputId, $el, opts, coordmap, outputId) {
    const expandPixels = 20;
    const brush = createBrush($el, opts, coordmap, expandPixels);
    $el.on("shiny-internal:brushed.image_output", function(e4, coords) {
      if (coords.brushId === inputId && coords.outputId !== outputId) {
        $el.data("mostRecentBrush", false);
        brush.reset();
      }
    });
    function setCursorStyle(style) {
      $el.removeClass(
        "crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize"
      );
      if (style)
        $el.addClass(style);
    }
    function sendBrushInfo() {
      const coords = brush.boundsData();
      if (isNaN(coords.xmin)) {
        shinySetInputValue(inputId, null);
        imageOutputBinding.find(document.documentElement).trigger("shiny-internal:brushed", {
          brushId: inputId,
          outputId: null
        });
        return;
      }
      const panel = brush.getPanel();
      import_jquery30.default.extend(coords, panel.panel_vars);
      coords.coords_css = brush.boundsCss();
      coords.coords_img = coordmap.scaleCssToImg(coords.coords_css);
      coords.img_css_ratio = coordmap.cssToImgScalingRatio();
      coords.mapping = panel.mapping;
      coords.domain = panel.domain;
      coords.range = panel.range;
      coords.log = panel.log;
      coords.direction = opts.brushDirection;
      coords.brushId = inputId;
      coords.outputId = outputId;
      shinySetInputValue(inputId, coords);
      $el.data("mostRecentBrush", true);
      imageOutputBinding.find(document.documentElement).trigger("shiny-internal:brushed", coords);
    }
    let brushInfoSender;
    if (opts.brushDelayType === "throttle") {
      brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
    } else {
      brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
    }
    if (!brush.hasOldBrush()) {
      brushInfoSender.immediateCall();
    }
    function mousedown(e4) {
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing())
        return;
      if (e4.which !== 1)
        return;
      const offsetCss = coordmap.mouseOffsetCss(e4);
      if (opts.brushClip && !coordmap.isInPanelCss(offsetCss, expandPixels))
        return;
      brush.up({ x: NaN, y: NaN });
      brush.down(offsetCss);
      if (brush.isInResizeArea(offsetCss)) {
        brush.startResizing(offsetCss);
        (0, import_jquery30.default)(document).on("mousemove.image_brush", mousemoveResizing).on("mouseup.image_brush", mouseupResizing);
      } else if (brush.isInsideBrush(offsetCss)) {
        brush.startDragging(offsetCss);
        setCursorStyle("grabbing");
        (0, import_jquery30.default)(document).on("mousemove.image_brush", mousemoveDragging).on("mouseup.image_brush", mouseupDragging);
      } else {
        const panel = coordmap.getPanelCss(offsetCss, expandPixels);
        brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offsetCss)));
        (0, import_jquery30.default)(document).on("mousemove.image_brush", mousemoveBrushing).on("mouseup.image_brush", mouseupBrushing);
      }
    }
    function mousemove(e4) {
      const offsetCss = coordmap.mouseOffsetCss(e4);
      if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
        if (brush.isInResizeArea(offsetCss)) {
          const r5 = brush.whichResizeSides(offsetCss);
          if (r5.left && r5.top || r5.right && r5.bottom) {
            setCursorStyle("nwse-resize");
          } else if (r5.left && r5.bottom || r5.right && r5.top) {
            setCursorStyle("nesw-resize");
          } else if (r5.left || r5.right) {
            setCursorStyle("ew-resize");
          } else if (r5.top || r5.bottom) {
            setCursorStyle("ns-resize");
          }
        } else if (brush.isInsideBrush(offsetCss)) {
          setCursorStyle("grabbable");
        } else if (coordmap.isInPanelCss(offsetCss, expandPixels)) {
          setCursorStyle("crosshair");
        } else {
          setCursorStyle(null);
        }
      }
    }
    function mousemoveBrushing(e4) {
      brush.brushTo(coordmap.mouseOffsetCss(e4));
      brushInfoSender.normalCall();
    }
    function mousemoveDragging(e4) {
      brush.dragTo(coordmap.mouseOffsetCss(e4));
      brushInfoSender.normalCall();
    }
    function mousemoveResizing(e4) {
      brush.resizeTo(coordmap.mouseOffsetCss(e4));
      brushInfoSender.normalCall();
    }
    function mouseupBrushing(e4) {
      if (e4.which !== 1)
        return;
      (0, import_jquery30.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e4));
      brush.stopBrushing();
      setCursorStyle("crosshair");
      if (brush.down().x === brush.up().x && brush.down().y === brush.up().y) {
        brush.reset();
        brushInfoSender.immediateCall();
        return;
      }
      if (brushInfoSender.isPending())
        brushInfoSender.immediateCall();
    }
    function mouseupDragging(e4) {
      if (e4.which !== 1)
        return;
      (0, import_jquery30.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e4));
      brush.stopDragging();
      setCursorStyle("grabbable");
      if (brushInfoSender.isPending())
        brushInfoSender.immediateCall();
    }
    function mouseupResizing(e4) {
      if (e4.which !== 1)
        return;
      (0, import_jquery30.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e4));
      brush.stopResizing();
      if (brushInfoSender.isPending())
        brushInfoSender.immediateCall();
    }
    function onResetImg() {
      if (opts.brushResetOnNew) {
        if ($el.data("mostRecentBrush")) {
          brush.reset();
          brushInfoSender.immediateCall();
        }
      }
    }
    if (!opts.brushResetOnNew) {
      if ($el.data("mostRecentBrush")) {
        brush.importOldBrush();
        brushInfoSender.immediateCall();
      }
    }
    function onResize() {
      brush.onResize();
      brushInfoSender.immediateCall();
    }
    return {
      mousedown,
      mousemove,
      onResetImg,
      onResize
    };
  }

  // srcts/src/imageutils/disableDrag.ts
  function disableDrag($el, $img) {
    $img.css("-webkit-user-drag", "none");
    $img.off("dragstart.image_output");
    $img.on("dragstart.image_output", function() {
      return false;
    });
    $el.off("selectstart.image_output");
    $el.on("selectstart.image_output", function() {
      return false;
    });
  }

  // srcts/src/bindings/output/image.ts
  var ImageOutputBinding = class extends OutputBinding {
    find(scope) {
      return (0, import_jquery31.default)(scope).find(".shiny-image-output, .shiny-plot-output");
    }
    renderValue(el, data) {
      const outputId = this.getId(el);
      const $el = (0, import_jquery31.default)(el);
      let img;
      let $img = $el.find("img");
      if ($img.length === 0) {
        img = document.createElement("img");
        $el.append(img);
        $img = (0, import_jquery31.default)(img);
      } else {
        img = $img[0];
        $img.trigger("reset");
      }
      if (!data) {
        $el.empty();
        return;
      }
      const opts = {
        clickId: $el.data("click-id"),
        clickClip: ifUndefined(strToBool($el.data("click-clip")), true),
        dblclickId: $el.data("dblclick-id"),
        dblclickClip: ifUndefined(strToBool($el.data("dblclick-clip")), true),
        dblclickDelay: ifUndefined($el.data("dblclick-delay"), 400),
        hoverId: $el.data("hover-id"),
        hoverClip: ifUndefined(strToBool($el.data("hover-clip")), true),
        hoverDelayType: ifUndefined($el.data("hover-delay-type"), "debounce"),
        hoverDelay: ifUndefined($el.data("hover-delay"), 300),
        hoverNullOutside: ifUndefined(
          strToBool($el.data("hover-null-outside")),
          false
        ),
        brushId: $el.data("brush-id"),
        brushClip: ifUndefined(strToBool($el.data("brush-clip")), true),
        brushDelayType: ifUndefined($el.data("brush-delay-type"), "debounce"),
        brushDelay: ifUndefined($el.data("brush-delay"), 300),
        brushFill: ifUndefined($el.data("brush-fill"), "#666"),
        brushStroke: ifUndefined($el.data("brush-stroke"), "#000"),
        brushOpacity: ifUndefined($el.data("brush-opacity"), 0.3),
        brushDirection: ifUndefined($el.data("brush-direction"), "xy"),
        brushResetOnNew: ifUndefined(
          strToBool($el.data("brush-reset-on-new")),
          false
        ),
        coordmap: data.coordmap
      };
      if (opts.brushFill === "auto") {
        opts.brushFill = getComputedLinkColor($el[0]);
      }
      if (opts.brushStroke === "auto") {
        opts.brushStroke = getStyle($el[0], "color");
      }
      import_jquery31.default.each(data, function(key, value) {
        if (value === null || key === "coordmap") {
          return;
        }
        if (key === "src" && value === img.getAttribute("src")) {
          img.removeAttribute("src");
        }
        img.setAttribute(key, value);
      });
      for (let i4 = 0; i4 < img.attributes.length; i4++) {
        const attrib = img.attributes[i4];
        if (attrib.specified && !hasOwnProperty(data, attrib.name)) {
          img.removeAttribute(attrib.name);
        }
      }
      if (!opts.coordmap) {
        opts.coordmap = {
          panels: [],
          dims: {
            height: null,
            width: null
          }
        };
      }
      $el.off(".image_output");
      $img.off(".image_output");
      $img.off("load.shiny_image_interaction");
      $img.one("load.shiny_image_interaction", function() {
        const optsCoordmap = opts.coordmap = initCoordmap($el, opts.coordmap);
        const clickInfo = createClickInfo(
          $el,
          opts.dblclickId,
          opts.dblclickDelay
        );
        $el.on("mousedown.image_output", clickInfo.mousedown);
        if (isIE() && IEVersion() === 8) {
          $el.on("dblclick.image_output", clickInfo.dblclickIE8);
        }
        if (opts.clickId) {
          disableDrag($el, $img);
          const clickHandler = createClickHandler(
            opts.clickId,
            opts.clickClip,
            optsCoordmap
          );
          $el.on("mousedown2.image_output", clickHandler.mousedown);
          $el.on("resize.image_output", clickHandler.onResize);
          $img.on("reset.image_output", clickHandler.onResetImg);
        }
        if (opts.dblclickId) {
          disableDrag($el, $img);
          const dblclickHandler = createClickHandler(
            opts.dblclickId,
            opts.clickClip,
            optsCoordmap
          );
          $el.on("dblclick2.image_output", dblclickHandler.mousedown);
          $el.on("resize.image_output", dblclickHandler.onResize);
          $img.on("reset.image_output", dblclickHandler.onResetImg);
        }
        if (opts.hoverId) {
          disableDrag($el, $img);
          const hoverHandler = createHoverHandler(
            opts.hoverId,
            opts.hoverDelay,
            opts.hoverDelayType,
            opts.hoverClip,
            opts.hoverNullOutside,
            optsCoordmap
          );
          $el.on("mousemove.image_output", hoverHandler.mousemove);
          $el.on("mouseout.image_output", hoverHandler.mouseout);
          $el.on("resize.image_output", hoverHandler.onResize);
          $img.on("reset.image_output", hoverHandler.onResetImg);
        }
        if (opts.brushId) {
          disableDrag($el, $img);
          const brushHandler = createBrushHandler(
            opts.brushId,
            $el,
            opts,
            optsCoordmap,
            outputId
          );
          $el.on("mousedown.image_output", brushHandler.mousedown);
          $el.on("mousemove.image_output", brushHandler.mousemove);
          $el.on("resize.image_output", brushHandler.onResize);
          $img.on("reset.image_output", brushHandler.onResetImg);
        }
        if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
          $el.addClass("crosshair");
        }
        if (data.error)
          console.log("Error on server extracting coordmap: " + data.error);
      });
    }
    renderError(el, err) {
      (0, import_jquery31.default)(el).find("img").trigger("reset");
      OutputBinding.prototype.renderError.call(this, el, err);
    }
    clearError(el) {
      (0, import_jquery31.default)(el).contents().filter(function() {
        return !(this instanceof HTMLElement && (this.tagName === "IMG" || this.id === el.id + "_brush"));
      }).remove();
      OutputBinding.prototype.clearError.call(this, el);
    }
    resize(el, width, height) {
      (0, import_jquery31.default)(el).find("img").trigger("resize");
      return;
      width;
      height;
    }
  };
  var imageOutputBinding = new ImageOutputBinding();

  // srcts/src/bindings/output/text.ts
  var import_jquery32 = __toESM(require_jquery());
  var TextOutputBinding = class extends OutputBinding {
    find(scope) {
      return (0, import_jquery32.default)(scope).find(".shiny-text-output");
    }
    renderValue(el, data) {
      (0, import_jquery32.default)(el).text(data);
    }
  };

  // srcts/src/bindings/output/index.ts
  function initOutputBindings() {
    const outputBindings = new BindingRegistry();
    outputBindings.register(new TextOutputBinding(), "shiny.textOutput");
    outputBindings.register(
      new DownloadLinkOutputBinding(),
      "shiny.downloadLink"
    );
    outputBindings.register(
      new DatatableOutputBinding(),
      "shiny.datatableOutput"
    );
    outputBindings.register(new HtmlOutputBinding(), "shiny.htmlOutput");
    outputBindings.register(imageOutputBinding, "shiny.imageOutput");
    return { outputBindings };
  }

  // node_modules/@lit/reactive-element/css-tag.js
  var t = globalThis;
  var e = t.ShadowRoot && (void 0 === t.ShadyCSS || t.ShadyCSS.nativeShadow) && "adoptedStyleSheets" in Document.prototype && "replace" in CSSStyleSheet.prototype;
  var s = Symbol();
  var o = /* @__PURE__ */ new WeakMap();
  var n = class {
    constructor(t3, e4, o4) {
      if (this._$cssResult$ = true, o4 !== s)
        throw Error("CSSResult is not constructable. Use `unsafeCSS` or `css` instead.");
      this.cssText = t3, this.t = e4;
    }
    get styleSheet() {
      let t3 = this.o;
      const s4 = this.t;
      if (e && void 0 === t3) {
        const e4 = void 0 !== s4 && 1 === s4.length;
        e4 && (t3 = o.get(s4)), void 0 === t3 && ((this.o = t3 = new CSSStyleSheet()).replaceSync(this.cssText), e4 && o.set(s4, t3));
      }
      return t3;
    }
    toString() {
      return this.cssText;
    }
  };
  var r = (t3) => new n("string" == typeof t3 ? t3 : t3 + "", void 0, s);
  var i = (t3, ...e4) => {
    const o4 = 1 === t3.length ? t3[0] : e4.reduce((e5, s4, o5) => e5 + ((t4) => {
      if (true === t4._$cssResult$)
        return t4.cssText;
      if ("number" == typeof t4)
        return t4;
      throw Error("Value passed to 'css' function must be a 'css' function result: " + t4 + ". Use 'unsafeCSS' to pass non-literal values, but take care to ensure page security.");
    })(s4) + t3[o5 + 1], t3[0]);
    return new n(o4, t3, s);
  };
  var S = (s4, o4) => {
    if (e)
      s4.adoptedStyleSheets = o4.map((t3) => t3 instanceof CSSStyleSheet ? t3 : t3.styleSheet);
    else
      for (const e4 of o4) {
        const o5 = document.createElement("style"), n4 = t.litNonce;
        void 0 !== n4 && o5.setAttribute("nonce", n4), o5.textContent = e4.cssText, s4.appendChild(o5);
      }
  };
  var c = e ? (t3) => t3 : (t3) => t3 instanceof CSSStyleSheet ? ((t4) => {
    let e4 = "";
    for (const s4 of t4.cssRules)
      e4 += s4.cssText;
    return r(e4);
  })(t3) : t3;

  // node_modules/@lit/reactive-element/reactive-element.js
  var { is: i2, defineProperty: e2, getOwnPropertyDescriptor: r2, getOwnPropertyNames: h, getOwnPropertySymbols: o2, getPrototypeOf: n2 } = Object;
  var a = globalThis;
  var c2 = a.trustedTypes;
  var l = c2 ? c2.emptyScript : "";
  var p = a.reactiveElementPolyfillSupport;
  var d = (t3, s4) => t3;
  var u = { toAttribute(t3, s4) {
    switch (s4) {
      case Boolean:
        t3 = t3 ? l : null;
        break;
      case Object:
      case Array:
        t3 = null == t3 ? t3 : JSON.stringify(t3);
    }
    return t3;
  }, fromAttribute(t3, s4) {
    let i4 = t3;
    switch (s4) {
      case Boolean:
        i4 = null !== t3;
        break;
      case Number:
        i4 = null === t3 ? null : Number(t3);
        break;
      case Object:
      case Array:
        try {
          i4 = JSON.parse(t3);
        } catch (t4) {
          i4 = null;
        }
    }
    return i4;
  } };
  var f = (t3, s4) => !i2(t3, s4);
  var y = { attribute: true, type: String, converter: u, reflect: false, hasChanged: f };
  Symbol.metadata ??= Symbol("metadata"), a.litPropertyMetadata ??= /* @__PURE__ */ new WeakMap();
  var b = class extends HTMLElement {
    static addInitializer(t3) {
      this._$Ei(), (this.l ??= []).push(t3);
    }
    static get observedAttributes() {
      return this.finalize(), this._$Eh && [...this._$Eh.keys()];
    }
    static createProperty(t3, s4 = y) {
      if (s4.state && (s4.attribute = false), this._$Ei(), this.elementProperties.set(t3, s4), !s4.noAccessor) {
        const i4 = Symbol(), r5 = this.getPropertyDescriptor(t3, i4, s4);
        void 0 !== r5 && e2(this.prototype, t3, r5);
      }
    }
    static getPropertyDescriptor(t3, s4, i4) {
      const { get: e4, set: h3 } = r2(this.prototype, t3) ?? { get() {
        return this[s4];
      }, set(t4) {
        this[s4] = t4;
      } };
      return { get() {
        return e4?.call(this);
      }, set(s5) {
        const r5 = e4?.call(this);
        h3.call(this, s5), this.requestUpdate(t3, r5, i4);
      }, configurable: true, enumerable: true };
    }
    static getPropertyOptions(t3) {
      return this.elementProperties.get(t3) ?? y;
    }
    static _$Ei() {
      if (this.hasOwnProperty(d("elementProperties")))
        return;
      const t3 = n2(this);
      t3.finalize(), void 0 !== t3.l && (this.l = [...t3.l]), this.elementProperties = new Map(t3.elementProperties);
    }
    static finalize() {
      if (this.hasOwnProperty(d("finalized")))
        return;
      if (this.finalized = true, this._$Ei(), this.hasOwnProperty(d("properties"))) {
        const t4 = this.properties, s4 = [...h(t4), ...o2(t4)];
        for (const i4 of s4)
          this.createProperty(i4, t4[i4]);
      }
      const t3 = this[Symbol.metadata];
      if (null !== t3) {
        const s4 = litPropertyMetadata.get(t3);
        if (void 0 !== s4)
          for (const [t4, i4] of s4)
            this.elementProperties.set(t4, i4);
      }
      this._$Eh = /* @__PURE__ */ new Map();
      for (const [t4, s4] of this.elementProperties) {
        const i4 = this._$Eu(t4, s4);
        void 0 !== i4 && this._$Eh.set(i4, t4);
      }
      this.elementStyles = this.finalizeStyles(this.styles);
    }
    static finalizeStyles(s4) {
      const i4 = [];
      if (Array.isArray(s4)) {
        const e4 = new Set(s4.flat(1 / 0).reverse());
        for (const s5 of e4)
          i4.unshift(c(s5));
      } else
        void 0 !== s4 && i4.push(c(s4));
      return i4;
    }
    static _$Eu(t3, s4) {
      const i4 = s4.attribute;
      return false === i4 ? void 0 : "string" == typeof i4 ? i4 : "string" == typeof t3 ? t3.toLowerCase() : void 0;
    }
    constructor() {
      super(), this._$Ep = void 0, this.isUpdatePending = false, this.hasUpdated = false, this._$Em = null, this._$Ev();
    }
    _$Ev() {
      this._$Eg = new Promise((t3) => this.enableUpdating = t3), this._$AL = /* @__PURE__ */ new Map(), this._$E_(), this.requestUpdate(), this.constructor.l?.forEach((t3) => t3(this));
    }
    addController(t3) {
      (this._$ES ??= []).push(t3), void 0 !== this.renderRoot && this.isConnected && t3.hostConnected?.();
    }
    removeController(t3) {
      this._$ES?.splice(this._$ES.indexOf(t3) >>> 0, 1);
    }
    _$E_() {
      const t3 = /* @__PURE__ */ new Map(), s4 = this.constructor.elementProperties;
      for (const i4 of s4.keys())
        this.hasOwnProperty(i4) && (t3.set(i4, this[i4]), delete this[i4]);
      t3.size > 0 && (this._$Ep = t3);
    }
    createRenderRoot() {
      const t3 = this.shadowRoot ?? this.attachShadow(this.constructor.shadowRootOptions);
      return S(t3, this.constructor.elementStyles), t3;
    }
    connectedCallback() {
      this.renderRoot ??= this.createRenderRoot(), this.enableUpdating(true), this._$ES?.forEach((t3) => t3.hostConnected?.());
    }
    enableUpdating(t3) {
    }
    disconnectedCallback() {
      this._$ES?.forEach((t3) => t3.hostDisconnected?.());
    }
    attributeChangedCallback(t3, s4, i4) {
      this._$AK(t3, i4);
    }
    _$EO(t3, s4) {
      const i4 = this.constructor.elementProperties.get(t3), e4 = this.constructor._$Eu(t3, i4);
      if (void 0 !== e4 && true === i4.reflect) {
        const r5 = (void 0 !== i4.converter?.toAttribute ? i4.converter : u).toAttribute(s4, i4.type);
        this._$Em = t3, null == r5 ? this.removeAttribute(e4) : this.setAttribute(e4, r5), this._$Em = null;
      }
    }
    _$AK(t3, s4) {
      const i4 = this.constructor, e4 = i4._$Eh.get(t3);
      if (void 0 !== e4 && this._$Em !== e4) {
        const t4 = i4.getPropertyOptions(e4), r5 = "function" == typeof t4.converter ? { fromAttribute: t4.converter } : void 0 !== t4.converter?.fromAttribute ? t4.converter : u;
        this._$Em = e4, this[e4] = r5.fromAttribute(s4, t4.type), this._$Em = null;
      }
    }
    requestUpdate(t3, s4, i4, e4 = false, r5) {
      if (void 0 !== t3) {
        if (i4 ??= this.constructor.getPropertyOptions(t3), !(i4.hasChanged ?? f)(e4 ? r5 : this[t3], s4))
          return;
        this.C(t3, s4, i4);
      }
      false === this.isUpdatePending && (this._$Eg = this._$EP());
    }
    C(t3, s4, i4) {
      this._$AL.has(t3) || this._$AL.set(t3, s4), true === i4.reflect && this._$Em !== t3 && (this._$Ej ??= /* @__PURE__ */ new Set()).add(t3);
    }
    async _$EP() {
      this.isUpdatePending = true;
      try {
        await this._$Eg;
      } catch (t4) {
        Promise.reject(t4);
      }
      const t3 = this.scheduleUpdate();
      return null != t3 && await t3, !this.isUpdatePending;
    }
    scheduleUpdate() {
      return this.performUpdate();
    }
    performUpdate() {
      if (!this.isUpdatePending)
        return;
      if (!this.hasUpdated) {
        if (this._$Ep) {
          for (const [t5, s5] of this._$Ep)
            this[t5] = s5;
          this._$Ep = void 0;
        }
        const t4 = this.constructor.elementProperties;
        if (t4.size > 0)
          for (const [s5, i4] of t4)
            true !== i4.wrapped || this._$AL.has(s5) || void 0 === this[s5] || this.C(s5, this[s5], i4);
      }
      let t3 = false;
      const s4 = this._$AL;
      try {
        t3 = this.shouldUpdate(s4), t3 ? (this.willUpdate(s4), this._$ES?.forEach((t4) => t4.hostUpdate?.()), this.update(s4)) : this._$ET();
      } catch (s5) {
        throw t3 = false, this._$ET(), s5;
      }
      t3 && this._$AE(s4);
    }
    willUpdate(t3) {
    }
    _$AE(t3) {
      this._$ES?.forEach((t4) => t4.hostUpdated?.()), this.hasUpdated || (this.hasUpdated = true, this.firstUpdated(t3)), this.updated(t3);
    }
    _$ET() {
      this._$AL = /* @__PURE__ */ new Map(), this.isUpdatePending = false;
    }
    get updateComplete() {
      return this.getUpdateComplete();
    }
    getUpdateComplete() {
      return this._$Eg;
    }
    shouldUpdate(t3) {
      return true;
    }
    update(t3) {
      this._$Ej &&= this._$Ej.forEach((t4) => this._$EO(t4, this[t4])), this._$ET();
    }
    updated(t3) {
    }
    firstUpdated(t3) {
    }
  };
  b.elementStyles = [], b.shadowRootOptions = { mode: "open" }, b[d("elementProperties")] = /* @__PURE__ */ new Map(), b[d("finalized")] = /* @__PURE__ */ new Map(), p?.({ ReactiveElement: b }), (a.reactiveElementVersions ??= []).push("2.0.0");

  // node_modules/lit-html/lit-html.js
  var t2 = globalThis;
  var i3 = t2.trustedTypes;
  var s2 = i3 ? i3.createPolicy("lit-html", { createHTML: (t3) => t3 }) : void 0;
  var e3 = "$lit$";
  var h2 = `lit$${(Math.random() + "").slice(9)}$`;
  var o3 = "?" + h2;
  var n3 = `<${o3}>`;
  var r3 = document;
  var l2 = () => r3.createComment("");
  var c3 = (t3) => null === t3 || "object" != typeof t3 && "function" != typeof t3;
  var a2 = Array.isArray;
  var u2 = (t3) => a2(t3) || "function" == typeof t3?.[Symbol.iterator];
  var d2 = "[ 	\n\f\r]";
  var f2 = /<(?:(!--|\/[^a-zA-Z])|(\/?[a-zA-Z][^>\s]*)|(\/?$))/g;
  var v = /-->/g;
  var _ = />/g;
  var m = RegExp(`>|${d2}(?:([^\\s"'>=/]+)(${d2}*=${d2}*(?:[^ 	
\f\r"'\`<>=]|("|')|))|$)`, "g");
  var p2 = /'/g;
  var g = /"/g;
  var $33 = /^(?:script|style|textarea|title)$/i;
  var y2 = (t3) => (i4, ...s4) => ({ _$litType$: t3, strings: i4, values: s4 });
  var x = y2(1);
  var b2 = y2(2);
  var w = Symbol.for("lit-noChange");
  var T = Symbol.for("lit-nothing");
  var A = /* @__PURE__ */ new WeakMap();
  var E = r3.createTreeWalker(r3, 129);
  function C(t3, i4) {
    if (!Array.isArray(t3) || !t3.hasOwnProperty("raw"))
      throw Error("invalid template strings array");
    return void 0 !== s2 ? s2.createHTML(i4) : i4;
  }
  var P = (t3, i4) => {
    const s4 = t3.length - 1, o4 = [];
    let r5, l3 = 2 === i4 ? "<svg>" : "", c4 = f2;
    for (let i5 = 0; i5 < s4; i5++) {
      const s5 = t3[i5];
      let a3, u3, d3 = -1, y3 = 0;
      for (; y3 < s5.length && (c4.lastIndex = y3, u3 = c4.exec(s5), null !== u3); )
        y3 = c4.lastIndex, c4 === f2 ? "!--" === u3[1] ? c4 = v : void 0 !== u3[1] ? c4 = _ : void 0 !== u3[2] ? ($33.test(u3[2]) && (r5 = RegExp("</" + u3[2], "g")), c4 = m) : void 0 !== u3[3] && (c4 = m) : c4 === m ? ">" === u3[0] ? (c4 = r5 ?? f2, d3 = -1) : void 0 === u3[1] ? d3 = -2 : (d3 = c4.lastIndex - u3[2].length, a3 = u3[1], c4 = void 0 === u3[3] ? m : '"' === u3[3] ? g : p2) : c4 === g || c4 === p2 ? c4 = m : c4 === v || c4 === _ ? c4 = f2 : (c4 = m, r5 = void 0);
      const x2 = c4 === m && t3[i5 + 1].startsWith("/>") ? " " : "";
      l3 += c4 === f2 ? s5 + n3 : d3 >= 0 ? (o4.push(a3), s5.slice(0, d3) + e3 + s5.slice(d3) + h2 + x2) : s5 + h2 + (-2 === d3 ? i5 : x2);
    }
    return [C(t3, l3 + (t3[s4] || "<?>") + (2 === i4 ? "</svg>" : "")), o4];
  };
  var V = class {
    constructor({ strings: t3, _$litType$: s4 }, n4) {
      let r5;
      this.parts = [];
      let c4 = 0, a3 = 0;
      const u3 = t3.length - 1, d3 = this.parts, [f3, v2] = P(t3, s4);
      if (this.el = V.createElement(f3, n4), E.currentNode = this.el.content, 2 === s4) {
        const t4 = this.el.content.firstChild;
        t4.replaceWith(...t4.childNodes);
      }
      for (; null !== (r5 = E.nextNode()) && d3.length < u3; ) {
        if (1 === r5.nodeType) {
          if (r5.hasAttributes())
            for (const t4 of r5.getAttributeNames())
              if (t4.endsWith(e3)) {
                const i4 = v2[a3++], s5 = r5.getAttribute(t4).split(h2), e4 = /([.?@])?(.*)/.exec(i4);
                d3.push({ type: 1, index: c4, name: e4[2], strings: s5, ctor: "." === e4[1] ? k : "?" === e4[1] ? H : "@" === e4[1] ? I : R }), r5.removeAttribute(t4);
              } else
                t4.startsWith(h2) && (d3.push({ type: 6, index: c4 }), r5.removeAttribute(t4));
          if ($33.test(r5.tagName)) {
            const t4 = r5.textContent.split(h2), s5 = t4.length - 1;
            if (s5 > 0) {
              r5.textContent = i3 ? i3.emptyScript : "";
              for (let i4 = 0; i4 < s5; i4++)
                r5.append(t4[i4], l2()), E.nextNode(), d3.push({ type: 2, index: ++c4 });
              r5.append(t4[s5], l2());
            }
          }
        } else if (8 === r5.nodeType)
          if (r5.data === o3)
            d3.push({ type: 2, index: c4 });
          else {
            let t4 = -1;
            for (; -1 !== (t4 = r5.data.indexOf(h2, t4 + 1)); )
              d3.push({ type: 7, index: c4 }), t4 += h2.length - 1;
          }
        c4++;
      }
    }
    static createElement(t3, i4) {
      const s4 = r3.createElement("template");
      return s4.innerHTML = t3, s4;
    }
  };
  function N(t3, i4, s4 = t3, e4) {
    if (i4 === w)
      return i4;
    let h3 = void 0 !== e4 ? s4._$Co?.[e4] : s4._$Cl;
    const o4 = c3(i4) ? void 0 : i4._$litDirective$;
    return h3?.constructor !== o4 && (h3?._$AO?.(false), void 0 === o4 ? h3 = void 0 : (h3 = new o4(t3), h3._$AT(t3, s4, e4)), void 0 !== e4 ? (s4._$Co ??= [])[e4] = h3 : s4._$Cl = h3), void 0 !== h3 && (i4 = N(t3, h3._$AS(t3, i4.values), h3, e4)), i4;
  }
  var S2 = class {
    constructor(t3, i4) {
      this._$AV = [], this._$AN = void 0, this._$AD = t3, this._$AM = i4;
    }
    get parentNode() {
      return this._$AM.parentNode;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    u(t3) {
      const { el: { content: i4 }, parts: s4 } = this._$AD, e4 = (t3?.creationScope ?? r3).importNode(i4, true);
      E.currentNode = e4;
      let h3 = E.nextNode(), o4 = 0, n4 = 0, l3 = s4[0];
      for (; void 0 !== l3; ) {
        if (o4 === l3.index) {
          let i5;
          2 === l3.type ? i5 = new M(h3, h3.nextSibling, this, t3) : 1 === l3.type ? i5 = new l3.ctor(h3, l3.name, l3.strings, this, t3) : 6 === l3.type && (i5 = new L(h3, this, t3)), this._$AV.push(i5), l3 = s4[++n4];
        }
        o4 !== l3?.index && (h3 = E.nextNode(), o4++);
      }
      return E.currentNode = r3, e4;
    }
    p(t3) {
      let i4 = 0;
      for (const s4 of this._$AV)
        void 0 !== s4 && (void 0 !== s4.strings ? (s4._$AI(t3, s4, i4), i4 += s4.strings.length - 2) : s4._$AI(t3[i4])), i4++;
    }
  };
  var M = class {
    get _$AU() {
      return this._$AM?._$AU ?? this._$Cv;
    }
    constructor(t3, i4, s4, e4) {
      this.type = 2, this._$AH = T, this._$AN = void 0, this._$AA = t3, this._$AB = i4, this._$AM = s4, this.options = e4, this._$Cv = e4?.isConnected ?? true;
    }
    get parentNode() {
      let t3 = this._$AA.parentNode;
      const i4 = this._$AM;
      return void 0 !== i4 && 11 === t3?.nodeType && (t3 = i4.parentNode), t3;
    }
    get startNode() {
      return this._$AA;
    }
    get endNode() {
      return this._$AB;
    }
    _$AI(t3, i4 = this) {
      t3 = N(this, t3, i4), c3(t3) ? t3 === T || null == t3 || "" === t3 ? (this._$AH !== T && this._$AR(), this._$AH = T) : t3 !== this._$AH && t3 !== w && this._(t3) : void 0 !== t3._$litType$ ? this.g(t3) : void 0 !== t3.nodeType ? this.$(t3) : u2(t3) ? this.T(t3) : this._(t3);
    }
    k(t3) {
      return this._$AA.parentNode.insertBefore(t3, this._$AB);
    }
    $(t3) {
      this._$AH !== t3 && (this._$AR(), this._$AH = this.k(t3));
    }
    _(t3) {
      this._$AH !== T && c3(this._$AH) ? this._$AA.nextSibling.data = t3 : this.$(r3.createTextNode(t3)), this._$AH = t3;
    }
    g(t3) {
      const { values: i4, _$litType$: s4 } = t3, e4 = "number" == typeof s4 ? this._$AC(t3) : (void 0 === s4.el && (s4.el = V.createElement(C(s4.h, s4.h[0]), this.options)), s4);
      if (this._$AH?._$AD === e4)
        this._$AH.p(i4);
      else {
        const t4 = new S2(e4, this), s5 = t4.u(this.options);
        t4.p(i4), this.$(s5), this._$AH = t4;
      }
    }
    _$AC(t3) {
      let i4 = A.get(t3.strings);
      return void 0 === i4 && A.set(t3.strings, i4 = new V(t3)), i4;
    }
    T(t3) {
      a2(this._$AH) || (this._$AH = [], this._$AR());
      const i4 = this._$AH;
      let s4, e4 = 0;
      for (const h3 of t3)
        e4 === i4.length ? i4.push(s4 = new M(this.k(l2()), this.k(l2()), this, this.options)) : s4 = i4[e4], s4._$AI(h3), e4++;
      e4 < i4.length && (this._$AR(s4 && s4._$AB.nextSibling, e4), i4.length = e4);
    }
    _$AR(t3 = this._$AA.nextSibling, i4) {
      for (this._$AP?.(false, true, i4); t3 && t3 !== this._$AB; ) {
        const i5 = t3.nextSibling;
        t3.remove(), t3 = i5;
      }
    }
    setConnected(t3) {
      void 0 === this._$AM && (this._$Cv = t3, this._$AP?.(t3));
    }
  };
  var R = class {
    get tagName() {
      return this.element.tagName;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    constructor(t3, i4, s4, e4, h3) {
      this.type = 1, this._$AH = T, this._$AN = void 0, this.element = t3, this.name = i4, this._$AM = e4, this.options = h3, s4.length > 2 || "" !== s4[0] || "" !== s4[1] ? (this._$AH = Array(s4.length - 1).fill(new String()), this.strings = s4) : this._$AH = T;
    }
    _$AI(t3, i4 = this, s4, e4) {
      const h3 = this.strings;
      let o4 = false;
      if (void 0 === h3)
        t3 = N(this, t3, i4, 0), o4 = !c3(t3) || t3 !== this._$AH && t3 !== w, o4 && (this._$AH = t3);
      else {
        const e5 = t3;
        let n4, r5;
        for (t3 = h3[0], n4 = 0; n4 < h3.length - 1; n4++)
          r5 = N(this, e5[s4 + n4], i4, n4), r5 === w && (r5 = this._$AH[n4]), o4 ||= !c3(r5) || r5 !== this._$AH[n4], r5 === T ? t3 = T : t3 !== T && (t3 += (r5 ?? "") + h3[n4 + 1]), this._$AH[n4] = r5;
      }
      o4 && !e4 && this.j(t3);
    }
    j(t3) {
      t3 === T ? this.element.removeAttribute(this.name) : this.element.setAttribute(this.name, t3 ?? "");
    }
  };
  var k = class extends R {
    constructor() {
      super(...arguments), this.type = 3;
    }
    j(t3) {
      this.element[this.name] = t3 === T ? void 0 : t3;
    }
  };
  var H = class extends R {
    constructor() {
      super(...arguments), this.type = 4;
    }
    j(t3) {
      this.element.toggleAttribute(this.name, !!t3 && t3 !== T);
    }
  };
  var I = class extends R {
    constructor(t3, i4, s4, e4, h3) {
      super(t3, i4, s4, e4, h3), this.type = 5;
    }
    _$AI(t3, i4 = this) {
      if ((t3 = N(this, t3, i4, 0) ?? T) === w)
        return;
      const s4 = this._$AH, e4 = t3 === T && s4 !== T || t3.capture !== s4.capture || t3.once !== s4.once || t3.passive !== s4.passive, h3 = t3 !== T && (s4 === T || e4);
      e4 && this.element.removeEventListener(this.name, this, s4), h3 && this.element.addEventListener(this.name, this, t3), this._$AH = t3;
    }
    handleEvent(t3) {
      "function" == typeof this._$AH ? this._$AH.call(this.options?.host ?? this.element, t3) : this._$AH.handleEvent(t3);
    }
  };
  var L = class {
    constructor(t3, i4, s4) {
      this.element = t3, this.type = 6, this._$AN = void 0, this._$AM = i4, this.options = s4;
    }
    get _$AU() {
      return this._$AM._$AU;
    }
    _$AI(t3) {
      N(this, t3);
    }
  };
  var Z = t2.litHtmlPolyfillSupport;
  Z?.(V, M), (t2.litHtmlVersions ??= []).push("3.0.0");
  var j = (t3, i4, s4) => {
    const e4 = s4?.renderBefore ?? i4;
    let h3 = e4._$litPart$;
    if (void 0 === h3) {
      const t4 = s4?.renderBefore ?? null;
      e4._$litPart$ = h3 = new M(i4.insertBefore(l2(), t4), t4, void 0, s4 ?? {});
    }
    return h3._$AI(t3), h3;
  };

  // node_modules/lit-element/lit-element.js
  var s3 = class extends b {
    constructor() {
      super(...arguments), this.renderOptions = { host: this }, this._$Do = void 0;
    }
    createRenderRoot() {
      const t3 = super.createRenderRoot();
      return this.renderOptions.renderBefore ??= t3.firstChild, t3;
    }
    update(t3) {
      const i4 = this.render();
      this.hasUpdated || (this.renderOptions.isConnected = this.isConnected), super.update(t3), this._$Do = j(i4, this.renderRoot, this.renderOptions);
    }
    connectedCallback() {
      super.connectedCallback(), this._$Do?.setConnected(true);
    }
    disconnectedCallback() {
      super.disconnectedCallback(), this._$Do?.setConnected(false);
    }
    render() {
      return w;
    }
  };
  s3._$litElement$ = true, s3["finalized", "finalized"] = true, globalThis.litElementHydrateSupport?.({ LitElement: s3 });
  var r4 = globalThis.litElementPolyfillSupport;
  r4?.({ LitElement: s3 });
  (globalThis.litElementVersions ??= []).push("4.0.0");

  // srcts/src/shiny/error.ts
  var ShinyClientError = class extends Error {
    constructor({ headline, message }) {
      super(message);
      this.name = "ShinyClientError";
      this.headline = headline;
    }
  };

  // srcts/src/components/errorConsole.ts
  var buttonStyles = i`
  button {
    background-color: transparent;
    outline: none;
    border-style: none;
    padding: var(--space-3);
    border-radius: var(--space-1);
    font-size: var(--font-lg);
    background-color: inherit;
    display: block;
  }

  button > svg {
    display: block;
  }
`;
  var _ShinyErrorConsole = class extends s3 {
    toggleCollapsed() {
      this.classList.toggle("collapsed");
      this.querySelector(".toggle-button")?.blur();
    }
    handleDismissAll() {
      this.classList.add("leaving");
      this.addEventListener("animationend", () => {
        this.remove();
      });
    }
    static createClientMessageElement({ headline, message }) {
      const msg = document.createElement("shiny-error-message");
      msg.setAttribute("headline", headline || "");
      msg.setAttribute("message", message);
      return msg;
    }
    appendConsoleMessage({ headline, message }) {
      const content = this.shadowRoot?.querySelector("slot.content");
      if (content) {
        const nodeKey = (node) => {
          const headline2 = node.getAttribute("headline") || "";
          const message2 = node.getAttribute("message") || "";
          return `${headline2}::${message2}`;
        };
        const newKey = `${headline}::${message}`;
        for (const node of content.assignedElements()) {
          if (node.tagName.toLowerCase() === "shiny-error-message") {
            if (nodeKey(node) === newKey) {
              return;
            }
          }
        }
      }
      this.appendChild(
        _ShinyErrorConsole.createClientMessageElement({ headline, message })
      );
      return;
    }
    render() {
      return x` <div class="header">
        <span class="title"> Shiny Client Errors </span>
        <button
          class="close-button"
          @click=${this.handleDismissAll}
          title="Dismiss all console messages and close console"
        >
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            height="1em"
            width="1em"
            stroke="currentColor"
            class="close-icon"
          >
            <path
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M6 18L18 6M6 6l12 12"
            />
          </svg>
          Dismiss all
        </button>
        <button class="toggle-button" @click=${this.toggleCollapsed}>
          <svg
            xmlns="http://www.w3.org/2000/svg"
            fill="none"
            viewBox="0 0 24 24"
            stroke-width="1.5"
            height="1em"
            width="1em"
            stroke="currentColor"
            class="toggle-icon"
          >
            <path
              class="collapse"
              stroke-linecap="round"
              stroke-linejoin="round"
              d="M4.5 19.5l15-15m0 0H8.25m11.25 0v11.25"
            />
          </svg>
        </button>
      </div>
      <slot class="content"></slot>`;
    }
  };
  var ShinyErrorConsole = _ShinyErrorConsole;
  ShinyErrorConsole.styles = [
    i`
      :host {
        /* We declare hard pixel values here to avoid body font size changes
        messing up the size of the console. This was an issue with bslib setting
        the body font-size at 16px relative to base shiny's 14px. */
        --font-md: 14px;
        --font-lg: 16px;
        --font-xl: 18px;

        /* These are all taken from open-props */
        --space-1: 6px;
        --space-2: calc(var(--space-1) * 2);
        --space-3: calc(var(--space-1) * 3);
        --space-4: calc(var(--space-1) * 4);
        --space-8: calc(var(--space-1) * 8);

        --red-2: #ffc9c9;
        --red-6: #fa5252;
        --red-7: #f03e3e;
        --red-8: #e03131;
        --red-10: #b02525;
        --red-11: #962020;
        --red-12: #7d1a1a;

        --gray-1: #f8f9fa;
        --gray-2: #e9ecef;
        --gray-3: #dee2e6;
        --gray-4: #ced4da;
        --gray-6: #868e96;
        --gray-8: #6c757d;

        --green-8: #51cf66;

        --shadow-color: 220 3% 15%;
        --shadow-strength: 1%;
        --shadow-3: 0 -1px 3px 0 hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 2%)),
          0 1px 2px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 2%)),
          0 2px 5px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 4%)),
          0 4px 12px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 5%)),
          0 12px 15px -5px hsl(var(--shadow-color) /
                calc(var(--shadow-strength) + 7%));

        --ring-shadow: 0 0 0 1px var(--gray-2);

        /* How fast should the message pop in and out of the screen? */
        --animation-speed: 500ms;

        /* Taken from open-props */
        --ease-3: cubic-bezier(0.25, 0, 0.3, 1);
        --animation-slide-in-left: slide-in-left var(--animation-speed)
          var(--ease-3);

        --animation-slide-out-left: slide-out-left var(--animation-speed)
          var(--ease-3);
        --modal-bg-color: white;

        position: fixed;
        top: var(--space-1);
        right: var(--space-1);
        z-index: 1000;

        display: flex;
        flex-direction: column;

        background-color: var(--modal-bg-color);
        border-radius: var(--space-1);

        animation: var(--animation-slide-in-left);
        box-shadow: var(--shadow-3), var(--ring-shadow);

        /* Dont let the error console burst out of the viewport */
        max-height: calc(100vh - 2 * var(--space-1));
      }

      @keyframes slide-in-left {
        from {
          transform: translateX(100%);
        }
      }
      @keyframes slide-out-left {
        to {
          transform: translateX(100%);
        }
      }

      :host(.leaving) {
        animation: var(--animation-slide-out-left);
      }

      .header {
        display: flex;
        justify-content: flex-end;
        align-items: flex-start;
        gap: var(--space-2);
      }

      .title {
        font-size: var(--font-xl);
        margin-right: auto;
        padding: var(--space-3);
        line-height: 1;
        font-weight: 600;
        color: var(--red-12);
      }

      ${buttonStyles}

      button:hover {
        background-color: var(--gray-2);
      }

      .toggle-button {
        width: fit-content;
        border: none;
        aspect-ratio: 1;
        border-color: var(--gray-4);
      }

      .close-button {
        display: flex;
        align-items: center;
        color: var(--red-11);
      }

      .close-button > svg {
        margin-right: 3px;
      }

      .toggle-button:focus {
        outline: 1px solid black;
      }

      .toggle-icon {
        transition: transform var(--animation-speed) ease-in-out;
      }

      :host(.collapsed) .toggle-icon {
        transform: scaleX(-1) scaleY(-1);
      }

      :host(.collapsed) .close-button {
        display: none;
      }

      .content {
        display: block;
        padding-inline: var(--space-4);
        padding-block-start: 0;
        padding-block-end: var(--space-4);
        max-height: 100%;
        overflow: auto;
      }

      :host(.collapsed) .content {
        display: none;
      }
    `
  ];
  customElements.define("shiny-error-console", ShinyErrorConsole);
  var ShinyErrorMessage = class extends s3 {
    constructor() {
      super(...arguments);
      this.headline = "";
      this.message = "";
    }
    async copyErrorToClipboard() {
      await navigator.clipboard.writeText(this.message);
      this.classList.add("copy-success");
      setTimeout(() => {
        this.classList.remove("copy-success");
      }, 1e3);
    }
    render() {
      return x`
      <div class="container">
        <div class="decoration-container">
          <div class="vertical-line"></div>
          <div class="dot"></div>
        </div>
        <div class="contents">
          <h3>${this.headline}</h3>
          <pre class="error-message">${this.message}</pre>
        </div>

        <div class="actions">
          <button
            class="copy-button"
            @click=${this.copyErrorToClipboard}
            title="Copy error to clipboard"
          >
            <div class="copy-button-inner">
              <svg
                class="front"
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke-width="1.5"
                stroke="currentColor"
                height="1em"
                width="1em"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  d="M15.666 3.888A2.25 2.25 0 0013.5 2.25h-3c-1.03 0-1.9.693-2.166 1.638m7.332 0c.055.194.084.4.084.612v0a.75.75 0 01-.75.75H9a.75.75 0 01-.75-.75v0c0-.212.03-.418.084-.612m7.332 0c.646.049 1.288.11 1.927.184 1.1.128 1.907 1.077 1.907 2.185V19.5a2.25 2.25 0 01-2.25 2.25H6.75A2.25 2.25 0 014.5 19.5V6.257c0-1.108.806-2.057 1.907-2.185a48.208 48.208 0 011.927-.184"
                />
              </svg>

              <svg
                class="back"
                xmlns="http://www.w3.org/2000/svg"
                fill="none"
                viewBox="0 0 24 24"
                stroke-width="1.5"
                stroke="currentColor"
                height="1em"
                width="1em"
              >
                <path
                  stroke-linecap="round"
                  stroke-linejoin="round"
                  d="M9 12.75L11.25 15 15 9.75M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
                />
              </svg>
            </div>
          </button>
        </div>
      </div>
    `;
    }
  };
  ShinyErrorMessage.properties = {
    headline: {},
    message: {}
  };
  ShinyErrorMessage.styles = [
    i`
      :host {
        color: var(--red-11);
        display: block;
        font-size: var(--font-md);

        position: relative;
        --icon-size: var(--font-lg)

        /* Reset box sizing */
        box-sizing: border-box;
      }

      .container {
        display: flex;
        gap: var(--space-2);
      }

      .contents {
        width: 40ch;
        display: flex;
        flex-direction: column;
        gap: var(--space-1);
        padding-block-start: 0;
        padding-block-end: var(--space-3);
        overflow: auto;
      }

      :host(:last-of-type) .contents {

        padding-block-end: var(--space-1);
      }

      .contents > h3 {
        font-size: 1em;
        font-weight: 500;
        color: var(--red-12);
      }

      .contents > * {
        margin-block: 0;
      }

      .error-message {
        font-family: "Courier New", Courier, monospace;
        white-space: pre-wrap;
      }

      .decoration-container {
        flex-shrink: 0;
        position: relative;

        --line-w: 2px;
        --dot-size: 11px;
      }

      :host(:hover) .decoration-container {
        --scale: 1.25;
      }

      .vertical-line {
        margin-inline: auto;
        width: var(--line-w);
        height: 100%;

        background-color: var(--red-10);
      }

      :host(:first-of-type) .vertical-line {
        height: calc(100% - var(--dot-size));
        margin-top: var(--dot-size);
      }

      .dot {
        position: absolute;
        width: var(--dot-size);
        height: var(--dot-size);
        top: calc(-1px +  var(--dot-size) / 2);
        left: calc(50% - var(--dot-size) / 2);
        border-radius: 100%;
        transform: scale(var(--scale, 1));

        color: var(--red-6);
        background-color: var(--red-10);
      }

      .actions {
        transform: scaleX(0);
        transition: transform calc(var(--animation-speed) / 2) ease-in-out;
        display: flex;
        justify-content: center;
        flex-direction: column;
      }

      /* Delay transition on mouseout so the buttons don't jump away if the user
      overshoots them with their mouse */
      :host(:not(:hover)) .actions {
        transition-delay: 0.15s;
      }

      :host(:hover) .actions {
        transform: scaleX(1);
      }

      ${buttonStyles}

      .copy-button {
        padding: 0;
        width: var(--space-8);
        height: var(--space-8);
        position: relative;
        --pad: var(--space-2);
      }

      .copy-button-inner {
        position: relative;
        width: 100%;
        height: 100%;
        border-radius: inherit;
        transition: transform 0.5s;
        transform-style: preserve-3d;
      }

      /* Animate flipping to the other side when the .copy-success class is
      added to the host */
      :host(.copy-success) .copy-button-inner {
        transform: rotateY(180deg);
      }

      /* Position the front and back side */
      .copy-button .front,
      .copy-button .back {
        --side: calc(100% - 2 * var(--pad));
        position: absolute;
        inset: var(--pad);
        height: var(--side);
        width: var(--side);
        -webkit-backface-visibility: hidden; /* Safari */
        backface-visibility: hidden;
      }

      .copy-button:hover .copy-button-inner {
        background-color: var(--gray-2);
      }

      /* Style the back side */
      .copy-button .back {
        --pad: var(--space-1);
        color: var(--green-8);
        transform: rotateY(180deg);
      }
    `
  ];
  customElements.define("shiny-error-message", ShinyErrorMessage);
  function showShinyClientMessage({
    headline = "",
    message,
    status = "warning"
  }) {
    const consoleMessage = `[shiny] ${headline}${headline ? " - " : ""}${message}`;
    switch (status) {
      case "error":
        console.error(consoleMessage);
        break;
      case "warning":
        console.warn(consoleMessage);
        break;
      default:
        console.log(consoleMessage);
        break;
    }
    if (!Shiny.inDevMode()) {
      return;
    }
    let sec = document.querySelector("shiny-error-console");
    if (!sec) {
      sec = document.createElement("shiny-error-console");
      document.body.appendChild(sec);
    }
    sec.appendConsoleMessage({ headline, message });
  }
  function showErrorInClientConsole(e4) {
    let errorMsg = null;
    let headline = "Error on client while running Shiny app";
    if (typeof e4 === "string") {
      errorMsg = e4;
    } else if (e4 instanceof ShinyClientError) {
      errorMsg = e4.message;
      headline = e4.headline;
    } else if (e4 instanceof Error) {
      errorMsg = e4.message;
    } else {
      errorMsg = "Unknown error";
    }
    showShinyClientMessage({ headline, message: errorMsg, status: "error" });
  }
  var ShinyClientMessageEvent = class extends CustomEvent {
    constructor(detail) {
      super("shiny:client-message", { detail, bubbles: true, cancelable: true });
    }
  };
  window.addEventListener("shiny:client-message", (ev) => {
    if (!(ev instanceof CustomEvent)) {
      throw new Error("[shiny] shiny:client-message expected a CustomEvent");
    }
    const { headline, message, status } = ev.detail;
    if (!message) {
      throw new Error(
        "[shiny] shiny:client-message expected a `message` property in `event.detail`."
      );
    }
    showShinyClientMessage({ headline, message, status });
  });

  // srcts/src/imageutils/resetBrush.ts
  function resetBrush(brushId) {
    shinySetInputValue(brushId, null);
    imageOutputBinding.find(document.documentElement).trigger("shiny-internal:brushed", {
      brushId,
      outputId: null
    });
  }

  // srcts/src/inputPolicies/inputBatchSender.ts
  var InputBatchSender = class {
    constructor(shinyapp) {
      this.pendingData = {};
      this.reentrant = false;
      this.sendIsEnqueued = false;
      this.lastChanceCallback = [];
      this.shinyapp = shinyapp;
    }
    setInput(nameType, value, opts) {
      this.pendingData[nameType] = value;
      if (!this.reentrant) {
        if (opts.priority === "event") {
          this._sendNow();
        } else if (!this.sendIsEnqueued) {
          this.shinyapp.taskQueue.enqueue(() => {
            this.sendIsEnqueued = false;
            this._sendNow();
          });
        }
      }
    }
    _sendNow() {
      if (this.reentrant) {
        console.trace("Unexpected reentrancy in InputBatchSender!");
      }
      this.reentrant = true;
      try {
        this.lastChanceCallback.forEach((callback) => callback());
        const currentData = this.pendingData;
        this.pendingData = {};
        this.shinyapp.sendInput(currentData);
      } finally {
        this.reentrant = false;
      }
    }
  };

  // srcts/src/inputPolicies/inputDeferDecorator.ts
  var InputDeferDecorator = class {
    constructor(target) {
      this.pendingInput = {};
      this.target = target;
    }
    setInput(nameType, value, opts) {
      if (/^\./.test(nameType))
        this.target.setInput(nameType, value, opts);
      else
        this.pendingInput[nameType] = { value, opts };
    }
    submit() {
      for (const nameType in this.pendingInput) {
        if (hasDefinedProperty(this.pendingInput, nameType)) {
          const { value, opts } = this.pendingInput[nameType];
          this.target.setInput(nameType, value, opts);
        }
      }
    }
  };

  // srcts/src/inputPolicies/inputEventDecorator.ts
  var import_jquery33 = __toESM(require_jquery());

  // srcts/src/inputPolicies/splitInputNameType.ts
  function splitInputNameType(nameType) {
    const name2 = nameType.split(":");
    return {
      name: name2[0],
      inputType: name2.length > 1 ? name2[1] : ""
    };
  }

  // srcts/src/inputPolicies/inputEventDecorator.ts
  var InputEventDecorator = class {
    constructor(target) {
      this.target = target;
    }
    setInput(nameType, value, opts) {
      const evt = import_jquery33.default.Event("shiny:inputchanged");
      const input = splitInputNameType(nameType);
      evt.name = input.name;
      evt.inputType = input.inputType;
      evt.value = value;
      evt.binding = opts.binding || null;
      evt.el = opts.el || null;
      evt.priority = opts.priority;
      (0, import_jquery33.default)(opts.el || window.document).trigger(evt);
      if (!evt.isDefaultPrevented()) {
        let name = evt.name;
        if (evt.inputType !== "")
          name += ":" + evt.inputType;
        this.target.setInput(name, evt.value, { priority: opts.priority });
      }
    }
  };

  // srcts/src/inputPolicies/inputNoResendDecorator.ts
  var InputNoResendDecorator = class {
    constructor(target, initialValues = {}) {
      this.lastSentValues = {};
      this.target = target;
      this.reset(initialValues);
    }
    setInput(nameType, value, opts) {
      const { name: inputName, inputType } = splitInputNameType(nameType);
      const jsonValue = JSON.stringify(value);
      if (opts.priority !== "event" && this.lastSentValues[inputName] && this.lastSentValues[inputName].jsonValue === jsonValue && this.lastSentValues[inputName].inputType === inputType) {
        return;
      }
      this.lastSentValues[inputName] = { jsonValue, inputType };
      this.target.setInput(nameType, value, opts);
    }
    reset(values = {}) {
      const cacheValues = {};
      for (const inputName in values) {
        if (hasDefinedProperty(values, inputName)) {
          const { name, inputType } = splitInputNameType(inputName);
          cacheValues[name] = {
            jsonValue: JSON.stringify(values[inputName]),
            inputType
          };
        }
      }
      this.lastSentValues = cacheValues;
    }
    forget(name) {
      delete this.lastSentValues[name];
    }
  };

  // srcts/src/inputPolicies/inputRateDecorator.ts
  var InputRateDecorator = class {
    constructor(target) {
      this.inputRatePolicies = {};
      this.target = target;
    }
    setInput(nameType, value, opts) {
      const { name: inputName } = splitInputNameType(nameType);
      this._ensureInit(inputName);
      if (opts.priority !== "deferred")
        this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
      else
        this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
    }
    setRatePolicy(nameType, mode, millis) {
      const { name: inputName } = splitInputNameType(nameType);
      if (mode === "direct") {
        this.inputRatePolicies[inputName] = new Invoker(this, this._doSetInput);
      } else if (mode === "debounce") {
        this.inputRatePolicies[inputName] = new Debouncer(
          this,
          this._doSetInput,
          millis
        );
      } else if (mode === "throttle") {
        this.inputRatePolicies[inputName] = new Throttler(
          this,
          this._doSetInput,
          millis
        );
      }
    }
    _ensureInit(name) {
      if (!(name in this.inputRatePolicies))
        this.setRatePolicy(name, "direct");
    }
    _doSetInput(nameType, value, opts) {
      this.target.setInput(nameType, value, opts);
    }
  };

  // srcts/src/inputPolicies/inputValidateDecorator.ts
  function addDefaultInputOpts(opts) {
    const newOpts = {
      priority: "immediate",
      ...opts
    };
    switch (newOpts.priority) {
      case "deferred":
      case "immediate":
      case "event":
        break;
      default:
        throw new Error(
          "Unexpected input value mode: '" + newOpts.priority + "'"
        );
    }
    return newOpts;
  }
  var InputValidateDecorator = class {
    constructor(target) {
      this.target = target;
    }
    setInput(nameType, value, opts = {}) {
      if (!nameType)
        throw "Can't set input with empty name.";
      const newOpts = addDefaultInputOpts(opts);
      this.target.setInput(nameType, value, newOpts);
    }
  };

  // srcts/src/utils/promise.ts
  function promiseWithResolvers() {
    let resolve;
    let reject;
    const promise = new Promise(
      (res, rej) => {
        resolve = res;
        reject = rej;
      }
    );
    return { promise, resolve, reject };
  }
  function createInitStatus() {
    const { promise, resolve } = promiseWithResolvers();
    let _resolved = false;
    return {
      promise,
      resolve(x2) {
        _resolved = true;
        resolve(x2);
      },
      then: promise.then.bind(promise),
      catch: promise.catch.bind(promise),
      finally: promise.finally.bind(promise),
      [Symbol.toStringTag]: "InitStatus",
      resolved() {
        return _resolved;
      }
    };
  }

  // srcts/src/shiny/bind.ts
  var import_jquery34 = __toESM(require_jquery());

  // srcts/src/bindings/outputAdapter.ts
  var OutputBindingAdapter = class {
    constructor(el, binding) {
      this.el = el;
      this.binding = binding;
      if (binding.resize) {
        this.onResize = makeResizeFilter(el, function(width, height) {
          binding.resize(el, width, height);
        });
      }
    }
    getId() {
      return this.binding.getId(this.el);
    }
    async onValueChange(data) {
      await this.binding.onValueChange(this.el, data);
    }
    onValueError(err) {
      this.binding.onValueError(this.el, err);
    }
    showProgress(show3) {
      this.binding.showProgress(this.el, show3);
    }
    onResize() {
    }
  };

  // srcts/src/shiny/bind.ts
  function isJQuery(value) {
    return typeof value.jquery === "string";
  }
  function valueChangeCallback(inputs, binding, el, allowDeferred) {
    let id = binding.getId(el);
    if (id) {
      const value = binding.getValue(el);
      const type = binding.getType(el);
      if (type)
        id = id + ":" + type;
      const opts = {
        priority: allowDeferred ? "deferred" : "immediate",
        binding,
        el
      };
      inputs.setInput(id, value, opts);
    }
  }
  var bindingsRegistry = (() => {
    const bindings = /* @__PURE__ */ new Map();
    function checkValidity(scope) {
      if (!isJQuery(scope) && !(scope instanceof HTMLElement)) {
        return;
      }
      const duplicateIds = /* @__PURE__ */ new Map();
      const problems = /* @__PURE__ */ new Set();
      bindings.forEach((idTypes, id) => {
        const counts = { input: 0, output: 0 };
        idTypes.forEach((type) => counts[type] += 1);
        if (counts.input + counts.output < 2) {
          return;
        }
        duplicateIds.set(id, counts);
        if (counts.input > 1) {
          problems.add("input");
        }
        if (counts.output > 1) {
          problems.add("output");
        }
        if (counts.input >= 1 && counts.output >= 1) {
          problems.add("shared");
        }
      });
      if (duplicateIds.size === 0)
        return;
      const duplicateIdMsg = Array.from(duplicateIds.entries()).map(([id, counts]) => {
        const messages = [
          pluralize(counts.input, "input"),
          pluralize(counts.output, "output")
        ].filter((msg) => msg !== "").join(" and ");
        return `- "${id}": ${messages}`;
      }).join("\n");
      let txtVerb = "Duplicate";
      let txtNoun = "input/output";
      if (problems.has("input") && problems.has("output")) {
      } else if (problems.has("input")) {
        txtNoun = "input";
      } else if (problems.has("output")) {
        txtNoun = "output";
      } else if (problems.has("shared")) {
        txtVerb = "Shared";
      }
      const txtIdsWere = duplicateIds.size == 1 ? "ID was" : "IDs were";
      const headline = `${txtVerb} ${txtNoun} ${txtIdsWere} found`;
      const message = `The following ${txtIdsWere} used for more than one ${problems.has("shared") ? "input/output" : txtNoun}:
${duplicateIdMsg}`;
      const event = new ShinyClientMessageEvent({ headline, message });
      const scopeElement = isJQuery(scope) ? scope.get(0) : scope;
      (scopeElement || window).dispatchEvent(event);
    }
    function addBinding(id, bindingType) {
      const existingBinding = bindings.get(id);
      if (existingBinding) {
        existingBinding.push(bindingType);
      } else {
        bindings.set(id, [bindingType]);
      }
    }
    function removeBinding(id, bindingType) {
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
      checkValidity
    };
  })();
  function pluralize(num, word) {
    if (num === 0)
      return "";
    if (num === 1)
      return `${num} ${word}`;
    return `${num} ${word}s`;
  }
  function bindInputs(shinyCtx, scope = document.documentElement) {
    const { inputs, inputsRate, inputBindings } = shinyCtx;
    const bindings = inputBindings.getBindings();
    const inputItems = {};
    for (let i4 = 0; i4 < bindings.length; i4++) {
      const binding = bindings[i4].binding;
      const matches = binding.find(scope) || [];
      for (let j2 = 0; j2 < matches.length; j2++) {
        const el = matches[j2];
        if (el.hasAttribute("data-shiny-no-bind-input"))
          continue;
        const id = binding.getId(el);
        if (!id || (0, import_jquery34.default)(el).hasClass("shiny-bound-input"))
          continue;
        const type = binding.getType(el);
        const effectiveId = type ? id + ":" + type : id;
        inputItems[effectiveId] = {
          value: binding.getValue(el),
          opts: {
            immediate: true,
            binding,
            el
          }
        };
        const thisCallback = function() {
          const thisBinding = binding;
          const thisEl = el;
          return function(allowDeferred) {
            valueChangeCallback(inputs, thisBinding, thisEl, allowDeferred);
          };
        }();
        binding.subscribe(el, thisCallback);
        (0, import_jquery34.default)(el).data("shiny-input-binding", binding);
        (0, import_jquery34.default)(el).addClass("shiny-bound-input");
        const ratePolicy = binding.getRatePolicy(el);
        if (ratePolicy !== null) {
          inputsRate.setRatePolicy(
            effectiveId,
            ratePolicy.policy,
            ratePolicy.delay
          );
        }
        bindingsRegistry.addBinding(id, "input");
        (0, import_jquery34.default)(el).trigger({
          type: "shiny:bound",
          binding,
          bindingType: "input"
        });
      }
    }
    return inputItems;
  }
  async function bindOutputs({
    sendOutputHiddenState,
    maybeAddThemeObserver,
    outputBindings
  }, scope = document.documentElement) {
    const $scope = (0, import_jquery34.default)(scope);
    const bindings = outputBindings.getBindings();
    for (let i4 = 0; i4 < bindings.length; i4++) {
      const binding = bindings[i4].binding;
      const matches = binding.find($scope) || [];
      for (let j2 = 0; j2 < matches.length; j2++) {
        const el = matches[j2];
        const id = binding.getId(el);
        if (!id)
          continue;
        if (!import_jquery34.default.contains(document.documentElement, el))
          continue;
        const $el = (0, import_jquery34.default)(el);
        if ($el.hasClass("shiny-bound-output")) {
          continue;
        }
        maybeAddThemeObserver(el);
        const bindingAdapter = new OutputBindingAdapter(el, binding);
        await shinyAppBindOutput(id, bindingAdapter);
        $el.data("shiny-output-binding", bindingAdapter);
        $el.addClass("shiny-bound-output");
        if (!$el.attr("aria-live"))
          $el.attr("aria-live", "polite");
        if (Shiny.shinyapp?.$outputProgress.isRecalculating(id)) {
          bindingAdapter.showProgress(true);
        }
        bindingsRegistry.addBinding(id, "output");
        $el.trigger({
          type: "shiny:bound",
          binding,
          bindingType: "output"
        });
      }
    }
    setTimeout(sendImageSizeFns.regular, 0);
    setTimeout(sendOutputHiddenState, 0);
  }
  function unbindInputs(scope = document.documentElement, includeSelf = false) {
    const inputs = (0, import_jquery34.default)(scope).find(".shiny-bound-input").toArray();
    if (includeSelf && (0, import_jquery34.default)(scope).hasClass("shiny-bound-input")) {
      inputs.push(scope);
    }
    for (let i4 = 0; i4 < inputs.length; i4++) {
      const el = inputs[i4];
      const binding = (0, import_jquery34.default)(el).data("shiny-input-binding");
      if (!binding)
        continue;
      const id = binding.getId(el);
      (0, import_jquery34.default)(el).removeClass("shiny-bound-input");
      bindingsRegistry.removeBinding(id, "input");
      binding.unsubscribe(el);
      (0, import_jquery34.default)(el).trigger({
        type: "shiny:unbound",
        binding,
        bindingType: "input"
      });
    }
  }
  function unbindOutputs({ sendOutputHiddenState }, scope = document.documentElement, includeSelf = false) {
    const outputs = (0, import_jquery34.default)(scope).find(".shiny-bound-output").toArray();
    if (includeSelf && (0, import_jquery34.default)(scope).hasClass("shiny-bound-output")) {
      outputs.push(scope);
    }
    for (let i4 = 0; i4 < outputs.length; i4++) {
      const $el = (0, import_jquery34.default)(outputs[i4]);
      const bindingAdapter = $el.data("shiny-output-binding");
      if (!bindingAdapter)
        continue;
      const id = bindingAdapter.binding.getId(outputs[i4]);
      shinyAppUnbindOutput(id, bindingAdapter);
      bindingsRegistry.removeBinding(id, "output");
      $el.removeClass("shiny-bound-output");
      $el.removeData("shiny-output-binding");
      $el.trigger({
        type: "shiny:unbound",
        binding: bindingAdapter.binding,
        bindingType: "output"
      });
    }
    setTimeout(sendImageSizeFns.regular, 0);
    setTimeout(sendOutputHiddenState, 0);
  }
  async function _bindAll(shinyCtx, scope) {
    await bindOutputs(shinyCtx, scope);
    const currentInputs = bindInputs(shinyCtx, scope);
    bindingsRegistry.checkValidity(scope);
    return currentInputs;
  }
  function unbindAll(shinyCtx, scope, includeSelf = false) {
    unbindInputs(scope, includeSelf);
    unbindOutputs(shinyCtx, scope, includeSelf);
  }
  async function bindAll(shinyCtx, scope) {
    const currentInputItems = await _bindAll(shinyCtx, scope);
    const inputs = shinyCtx.inputs;
    import_jquery34.default.each(currentInputItems, function(name, item) {
      inputs.setInput(name, item.value, item.opts);
    });
    shinyCtx.initDeferredIframes();
  }

  // srcts/src/shiny/modal.ts
  var import_jquery35 = __toESM(require_jquery());
  async function show({
    html = "",
    deps = []
  } = {}) {
    await renderDependenciesAsync(deps);
    (0, import_jquery35.default)(".modal-backdrop").remove();
    let $modal = (0, import_jquery35.default)("#shiny-modal-wrapper");
    if ($modal.length === 0) {
      $modal = (0, import_jquery35.default)('<div id="shiny-modal-wrapper"></div>');
      (0, import_jquery35.default)(document.body).append($modal);
      $modal.on("hidden.bs.modal", function(e4) {
        if (e4.target === (0, import_jquery35.default)("#shiny-modal")[0]) {
          shinyUnbindAll($modal);
          $modal.remove();
        }
      });
    }
    $modal.on("keydown.shinymodal", function(e4) {
      if ((0, import_jquery35.default)("#shiny-modal").data("keyboard") === false)
        return;
      if (e4.keyCode === 27) {
        e4.stopPropagation();
        e4.preventDefault();
      }
    });
    await renderContentAsync($modal, { html });
  }
  function remove() {
    const $modal = (0, import_jquery35.default)("#shiny-modal-wrapper");
    $modal.off("keydown.shinymodal");
    const $bsModal = $modal.find(".modal");
    if ($bsModal.length > 0) {
      $bsModal.on("shown.bs.modal", () => $bsModal.modal("hide"));
      $bsModal.modal("hide");
    } else {
      shinyUnbindAll($modal);
      $modal.remove();
    }
  }

  // srcts/src/shiny/notifications.ts
  var import_jquery36 = __toESM(require_jquery());
  var fadeDuration = 250;
  async function show2({
    html = "",
    action = "",
    deps = [],
    duration = 5e3,
    id = null,
    closeButton = true,
    type = null
  } = {}) {
    if (!id)
      id = randomId();
    await renderDependenciesAsync(deps);
    createPanel();
    let $notificationInit = get(id);
    if ($notificationInit?.length === 0)
      $notificationInit = create(id);
    const $notification = $notificationInit;
    const newHtml = `<div class="shiny-notification-content-text">${html}</div><div class="shiny-notification-content-action">${action}</div>`;
    const $content = $notification.find(".shiny-notification-content");
    await renderContentAsync($content, { html: newHtml });
    const classes = $notification?.attr("class");
    if (classes) {
      const classVal = classes.split(/\s+/).filter((cls) => cls.match(/^shiny-notification-/)).join(" ");
      $notification.removeClass(classVal);
    }
    if (type && type !== "default")
      $notification.addClass("shiny-notification-" + type);
    const $close = $notification.find(".shiny-notification-close");
    if (closeButton && $close.length === 0) {
      $notification.append('<div class="shiny-notification-close">&times;</div>');
    } else if (!closeButton && $close.length !== 0) {
      $close.remove();
    }
    if (duration)
      addRemovalCallback(id, duration);
    else
      clearRemovalCallback(id);
    return id;
  }
  function remove2(id) {
    get(id)?.fadeOut(fadeDuration, function() {
      shinyUnbindAll(this);
      (0, import_jquery36.default)(this).remove();
      if (ids().length === 0) {
        getPanel().remove();
      }
    });
  }
  function get(id) {
    if (!id)
      return null;
    return getPanel().find("#shiny-notification-" + $escape(id));
  }
  function ids() {
    return getPanel().find(".shiny-notification").map(function() {
      return this.id.replace(/shiny-notification-/, "");
    }).get();
  }
  function getPanel() {
    return (0, import_jquery36.default)("#shiny-notification-panel");
  }
  function createPanel() {
    const $panel = getPanel();
    if ($panel.length > 0)
      return $panel;
    (0, import_jquery36.default)(document.body).append('<div id="shiny-notification-panel">');
    return $panel;
  }
  function create(id) {
    let $notification = get(id);
    if ($notification?.length === 0) {
      $notification = (0, import_jquery36.default)(
        `<div id="shiny-notification-${id}" class="shiny-notification"><div class="shiny-notification-close">&times;</div><div class="shiny-notification-content"></div></div>`
      );
      $notification.find(".shiny-notification-close").on("click", (e4) => {
        e4.preventDefault();
        e4.stopPropagation();
        remove2(id);
      });
      getPanel().append($notification);
    }
    return $notification;
  }
  function addRemovalCallback(id, delay) {
    clearRemovalCallback(id);
    const removalCallback = setTimeout(function() {
      remove2(id);
    }, delay);
    get(id)?.data("removalCallback", removalCallback);
  }
  function clearRemovalCallback(id) {
    const $notification = get(id);
    const oldRemovalCallback = $notification?.data("removalCallback");
    if (oldRemovalCallback) {
      clearTimeout(oldRemovalCallback);
    }
  }

  // srcts/src/shiny/reconnectDialog.ts
  var import_jquery37 = __toESM(require_jquery());
  function updateTime(reconnectTime) {
    const $time = (0, import_jquery37.default)("#shiny-reconnect-time");
    if ($time.length === 0)
      return;
    const seconds = Math.floor((reconnectTime - new Date().getTime()) / 1e3);
    if (seconds > 0) {
      $time.text(" in " + seconds + "s");
    } else {
      $time.text("...");
    }
    setTimeout(function() {
      updateTime(reconnectTime);
    }, 1e3);
  }
  async function showReconnectDialog(delay) {
    const reconnectTime = new Date().getTime() + delay;
    if ((0, import_jquery37.default)("#shiny-reconnect-text").length > 0)
      return;
    const html = '<span id="shiny-reconnect-text">Attempting to reconnect</span><span id="shiny-reconnect-time"></span>';
    const action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';
    await show2({
      id: "reconnect",
      html,
      action,
      duration: null,
      closeButton: false,
      type: "warning"
    });
    updateTime(reconnectTime);
  }
  function hideReconnectDialog() {
    remove2("reconnect");
  }

  // srcts/src/shiny/shinyapp.ts
  var import_jquery38 = __toESM(require_jquery());

  // srcts/src/utils/asyncQueue.ts
  var AsyncQueue = class {
    constructor() {
      this.$promises = [];
      this.$resolvers = [];
    }
    _add() {
      const p3 = new Promise((resolve) => {
        this.$resolvers.push(resolve);
      });
      this.$promises.push(p3);
    }
    enqueue(x2) {
      if (!this.$resolvers.length)
        this._add();
      const resolve = this.$resolvers.shift();
      resolve(x2);
    }
    async dequeue() {
      if (!this.$promises.length)
        this._add();
      const promise = this.$promises.shift();
      return promise;
    }
    isEmpty() {
      return !this.$promises.length;
    }
    isBlocked() {
      return !!this.$resolvers.length;
    }
    get length() {
      return this.$promises.length - this.$resolvers.length;
    }
  };

  // srcts/src/shiny/outputProgress.ts
  var _updateStateFromRecalculating, updateStateFromRecalculating_fn, _updateStateFromFlush, updateStateFromFlush_fn, _updateStateFromProgress, updateStateFromProgress_fn, _updateStateFromValueOrError, updateStateFromValueOrError_fn, _getState, getState_fn, _setState, setState_fn;
  var OutputProgressReporter = class {
    constructor() {
      __privateAdd(this, _updateStateFromRecalculating);
      __privateAdd(this, _updateStateFromFlush);
      __privateAdd(this, _updateStateFromProgress);
      __privateAdd(this, _updateStateFromValueOrError);
      __privateAdd(this, _getState);
      __privateAdd(this, _setState);
      this.outputStates = /* @__PURE__ */ new Map();
      this.changedOutputs = /* @__PURE__ */ new Map();
    }
    takeChanges() {
      const result = this.changedOutputs;
      this.changedOutputs = /* @__PURE__ */ new Map();
      return result;
    }
    isRecalculating(name) {
      const state = __privateMethod(this, _getState, getState_fn).call(this, name);
      const recalculatingStates = [
        "initial" /* Initial */,
        "running" /* Running */,
        "idle" /* Idle */,
        "persistent" /* Persistent */,
        "invalidated" /* Invalidated */
      ];
      return recalculatingStates.includes(state);
    }
    updateStateFromMessage(message) {
      if (isRecalculatingMessage(message)) {
        __privateMethod(this, _updateStateFromRecalculating, updateStateFromRecalculating_fn).call(this, message);
      }
      if (isFlushMessage(message)) {
        __privateMethod(this, _updateStateFromFlush, updateStateFromFlush_fn).call(this, message);
      }
      if (isProgressMessage(message)) {
        __privateMethod(this, _updateStateFromProgress, updateStateFromProgress_fn).call(this, message);
      }
    }
  };
  _updateStateFromRecalculating = new WeakSet();
  updateStateFromRecalculating_fn = function(message) {
    const { name, status } = message.recalculating;
    const state = __privateMethod(this, _getState, getState_fn).call(this, name);
    if (status === "recalculating") {
      switch (state) {
        case "initial" /* Initial */:
        case "invalidated" /* Invalidated */:
          __privateMethod(this, _setState, setState_fn).call(this, name, "running" /* Running */);
          break;
        default:
          throw new Error(
            `Shiny server sent a message that the output '${name}' is recalculating,
            but the output is in an unexpected state of: '${state}'.`
          );
      }
    }
    if (status === "recalculated") {
      switch (state) {
        case "running" /* Running */:
          __privateMethod(this, _setState, setState_fn).call(this, name, "idle" /* Idle */);
          break;
        default:
          throw new Error(
            `Shiny server sent a message that the output '${name}' has been recalculated,
            but the output is in an unexpected state of: '${state}'.`
          );
      }
    }
  };
  _updateStateFromFlush = new WeakSet();
  updateStateFromFlush_fn = function(message) {
    for (const name in message.values) {
      __privateMethod(this, _updateStateFromValueOrError, updateStateFromValueOrError_fn).call(this, name, "value" /* Value */);
    }
    for (const name in message.errors) {
      __privateMethod(this, _updateStateFromValueOrError, updateStateFromValueOrError_fn).call(this, name, "error" /* Error */);
    }
    for (const [name, state] of this.outputStates) {
      switch (state) {
        case "idle" /* Idle */:
          __privateMethod(this, _setState, setState_fn).call(this, name, "cancel" /* Cancel */);
          break;
        case "value" /* Value */:
        case "error" /* Error */:
        case "cancel" /* Cancel */:
        case "persistent" /* Persistent */:
        case "invalidated" /* Invalidated */:
          break;
        default:
          throw new Error(
            `Shiny server sent a flush message, and after processing the values and errors,
            the output '${name}' has an unexpected ending state of: '${state}'.`
          );
      }
    }
  };
  _updateStateFromProgress = new WeakSet();
  updateStateFromProgress_fn = function(message) {
    const { id, persistent } = message.progress.message;
    const state = __privateMethod(this, _getState, getState_fn).call(this, id);
    if (persistent) {
      switch (state) {
        case "running" /* Running */:
          __privateMethod(this, _setState, setState_fn).call(this, id, "persistent" /* Persistent */);
          break;
        default:
          throw new Error(
            `Shiny server has sent a 'persistent progress' message for ${id},
            but the output is in an unexpected state of: ${state}`
          );
      }
    } else {
      switch (state) {
        case "value" /* Value */:
        case "error" /* Error */:
        case "cancel" /* Cancel */:
        case "persistent" /* Persistent */:
        case "idle" /* Idle */:
          __privateMethod(this, _setState, setState_fn).call(this, id, "invalidated" /* Invalidated */);
          break;
        default:
          throw new Error(
            `Shiny server has sent a progress message for ${id},
            but the output is in an unexpected state of: ${state}`
          );
      }
    }
  };
  _updateStateFromValueOrError = new WeakSet();
  updateStateFromValueOrError_fn = function(name, type) {
    const state = __privateMethod(this, _getState, getState_fn).call(this, name);
    switch (state) {
      case "idle" /* Idle */:
        __privateMethod(this, _setState, setState_fn).call(this, name, type);
        break;
      default:
        throw new Error(
          `Shiny server has sent a ${type} for the output '${name}',
          but the output is in an unexpected state of: '${state}'.`
        );
    }
  };
  _getState = new WeakSet();
  getState_fn = function(name) {
    return this.outputStates.get(name) ?? "initial" /* Initial */;
  };
  _setState = new WeakSet();
  setState_fn = function(name, state) {
    const oldRecalc = this.isRecalculating(name);
    this.outputStates.set(name, state);
    const newRecalc = this.isRecalculating(name);
    if (oldRecalc !== newRecalc) {
      this.changedOutputs.set(name, newRecalc);
    }
  };
  function isRecalculatingMessage(x2) {
    const m2 = x2;
    return m2.recalculating !== void 0;
  }
  function isFlushMessage(x2) {
    const m2 = x2;
    return m2.values !== void 0 && m2.errors !== void 0;
  }
  function isProgressMessage(x2) {
    const m2 = x2;
    return m2.progress !== void 0 && m2.progress.type === "binding";
  }

  // srcts/src/shiny/shinyapp.ts
  var messageHandlerOrder = [];
  var messageHandlers = {};
  var customMessageHandlerOrder = [];
  var customMessageHandlers = {};
  function addMessageHandler(type, handler) {
    if (messageHandlers[type]) {
      throw 'handler for message of type "' + type + '" already added.';
    }
    if (typeof handler !== "function") {
      throw "handler must be a function.";
    }
    if (handler.length !== 1) {
      throw "handler must be a function that takes one argument.";
    }
    messageHandlerOrder.push(type);
    messageHandlers[type] = handler;
  }
  function addCustomMessageHandler(type, handler) {
    if (customMessageHandlers[type]) {
      const typeIdx = customMessageHandlerOrder.indexOf(type);
      if (typeIdx !== -1) {
        customMessageHandlerOrder.splice(typeIdx, 1);
        delete customMessageHandlers[type];
      }
    }
    if (typeof handler !== "function") {
      throw "handler must be a function.";
    }
    if (handler.length !== 1) {
      throw "handler must be a function that takes one argument.";
    }
    customMessageHandlerOrder.push(type);
    customMessageHandlers[type] = handler;
  }
  var ShinyApp = class {
    constructor() {
      this.$socket = null;
      this.taskQueue = new AsyncQueue();
      this.config = null;
      this.$inputValues = {};
      this.$initialInput = null;
      this.$bindings = {};
      this.$outputProgress = new OutputProgressReporter();
      this.$values = {};
      this.$errors = {};
      this.$conditionals = {};
      this.$pendingMessages = [];
      this.$activeRequests = {};
      this.$nextRequestId = 0;
      this.$allowReconnect = false;
      this.scheduledReconnect = void 0;
      this.reconnectDelay = function() {
        let attempts = 0;
        const delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];
        return {
          next: function() {
            let i4 = attempts;
            if (i4 >= delays.length) {
              i4 = delays.length - 1;
            }
            attempts++;
            return delays[i4];
          },
          reset: function() {
            attempts = 0;
          }
        };
      }();
      this.progressHandlers = {
        binding: function(message) {
          const key = message.id;
          const binding = this.$bindings[key];
          if (binding) {
            (0, import_jquery38.default)(binding.el).trigger({
              type: "shiny:outputinvalidated",
              binding,
              name: key
            });
          }
          this._updateProgress();
        },
        open: async function(message) {
          if (message.style === "notification") {
            await show2({
              html: `<div id="shiny-progress-${message.id}" class="shiny-progress-notification"><div class="progress active" style="display: none;"><div class="progress-bar"></div></div><div class="progress-text"><span class="progress-message">message</span> <span class="progress-detail"></span></div></div>`,
              id: message.id,
              duration: null
            });
          } else if (message.style === "old") {
            let $container = (0, import_jquery38.default)(".shiny-progress-container");
            if ($container.length === 0) {
              $container = (0, import_jquery38.default)('<div class="shiny-progress-container"></div>');
              (0, import_jquery38.default)(document.body).append($container);
            }
            const depth = (0, import_jquery38.default)(".shiny-progress.open").length;
            const $progress = (0, import_jquery38.default)(
              '<div class="shiny-progress open"><div class="progress active"><div class="progress-bar bar"></div></div><div class="progress-text"><span class="progress-message">message</span><span class="progress-detail"></span></div></div>'
            );
            $progress.attr("id", message.id);
            $container.append($progress);
            const $progressBar = $progress.find(".progress");
            if ($progressBar) {
              $progressBar.css(
                "top",
                depth * $progressBar.height() + "px"
              );
              const $progressText = $progress.find(".progress-text");
              $progressText.css(
                "top",
                3 * $progressBar.height() + depth * $progressText.outerHeight() + "px"
              );
              $progress.hide();
            }
          }
        },
        update: function(message) {
          if (message.style === "notification") {
            const $progress = (0, import_jquery38.default)("#shiny-progress-" + message.id);
            if ($progress.length === 0)
              return;
            if (typeof message.message !== "undefined") {
              $progress.find(".progress-message").text(message.message);
            }
            if (typeof message.detail !== "undefined") {
              $progress.find(".progress-detail").text(message.detail);
            }
            if (typeof message.value !== "undefined" && message.value !== null) {
              $progress.find(".progress").show();
              $progress.find(".progress-bar").width(message.value * 100 + "%");
            }
          } else if (message.style === "old") {
            const $progress = (0, import_jquery38.default)("#" + message.id + ".shiny-progress");
            if (typeof message.message !== "undefined") {
              $progress.find(".progress-message").text(message.message);
            }
            if (typeof message.detail !== "undefined") {
              $progress.find(".progress-detail").text(message.detail);
            }
            if (typeof message.value !== "undefined" && message.value !== null) {
              $progress.find(".progress").show();
              $progress.find(".bar").width(message.value * 100 + "%");
            }
            $progress.fadeIn();
          }
        },
        close: function(message) {
          if (message.style === "notification") {
            remove2(message.id);
          } else if (message.style === "old") {
            const $progress = (0, import_jquery38.default)("#" + message.id + ".shiny-progress");
            $progress.removeClass("open");
            $progress.fadeOut({
              complete: function() {
                $progress.remove();
                if ((0, import_jquery38.default)(".shiny-progress").length === 0)
                  (0, import_jquery38.default)(".shiny-progress-container").remove();
              }
            });
          }
        }
      };
      this._init();
    }
    connect(initialInput) {
      if (this.$socket)
        throw "Connect was already called on this application object";
      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
      import_jquery38.default.extend(this.$inputValues, initialInput);
      this.$updateConditionals();
    }
    isConnected() {
      return !!this.$socket;
    }
    reconnect() {
      clearTimeout(this.scheduledReconnect);
      if (this.isConnected())
        throw "Attempted to reconnect, but already connected.";
      this.$socket = this.createSocket();
      this.$initialInput = import_jquery38.default.extend({}, this.$inputValues);
      this.$updateConditionals();
    }
    createSocket() {
      const createSocketFunc = getShinyCreateWebsocket() || (() => {
        let protocol = "ws:";
        if (window.location.protocol === "https:")
          protocol = "wss:";
        let defaultPath = window.location.pathname;
        if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
          defaultPath = encodeURI(defaultPath);
          if (isQt()) {
            defaultPath = encodeURI(defaultPath);
          }
        }
        if (!/\/$/.test(defaultPath))
          defaultPath += "/";
        defaultPath += "websocket/";
        const ws = new WebSocket(
          protocol + "//" + window.location.host + defaultPath
        );
        ws.binaryType = "arraybuffer";
        return ws;
      });
      const socket = createSocketFunc();
      let hasOpened = false;
      socket.onopen = () => {
        hasOpened = true;
        (0, import_jquery38.default)(document).trigger({
          type: "shiny:connected",
          socket
        });
        this.onConnected();
        socket.send(
          JSON.stringify({
            method: "init",
            data: this.$initialInput
          })
        );
        while (this.$pendingMessages.length) {
          const msg = this.$pendingMessages.shift();
          socket.send(msg);
        }
        this.startActionQueueLoop();
      };
      socket.onmessage = (e4) => {
        this.taskQueue.enqueue(async () => await this.dispatchMessage(e4.data));
      };
      socket.onclose = (e4) => {
        const restarting = e4.code === 1012;
        if (hasOpened) {
          (0, import_jquery38.default)(document).trigger({
            type: "shiny:disconnected",
            socket
          });
          this.$notifyDisconnected();
        }
        this.onDisconnected(restarting);
        this.$removeSocket();
      };
      return socket;
    }
    async startActionQueueLoop() {
      while (true) {
        try {
          const action = await this.taskQueue.dequeue();
          await action();
        } catch (e4) {
          showErrorInClientConsole(e4);
          console.error(e4);
        }
      }
    }
    sendInput(values) {
      const msg = JSON.stringify({
        method: "update",
        data: values
      });
      this.$sendMsg(msg);
      import_jquery38.default.extend(this.$inputValues, values);
      this.$updateConditionals();
    }
    $notifyDisconnected() {
      if (window.parent) {
        window.parent.postMessage("disconnected", "*");
      }
    }
    $removeSocket() {
      this.$socket = null;
    }
    $scheduleReconnect(delay) {
      this.scheduledReconnect = window.setTimeout(() => {
        this.reconnect();
      }, delay);
    }
    onDisconnected(reloading = false) {
      if ((0, import_jquery38.default)("#shiny-disconnected-overlay").length === 0) {
        (0, import_jquery38.default)(document.body).append('<div id="shiny-disconnected-overlay"></div>');
      }
      (0, import_jquery38.default)("#shiny-disconnected-overlay").toggleClass("reloading", reloading);
      if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
        const delay = this.reconnectDelay.next();
        showReconnectDialog(delay);
        this.$scheduleReconnect(delay);
      }
    }
    onConnected() {
      (0, import_jquery38.default)("#shiny-disconnected-overlay").remove();
      hideReconnectDialog();
      this.reconnectDelay.reset();
    }
    makeRequest(method, args, onSuccess, onError, blobs) {
      let requestId = this.$nextRequestId;
      while (this.$activeRequests[requestId]) {
        requestId = (requestId + 1) % 1e9;
      }
      this.$nextRequestId = requestId + 1;
      this.$activeRequests[requestId] = {
        onSuccess,
        onError
      };
      let msg = JSON.stringify({
        method,
        args,
        tag: requestId
      });
      if (blobs) {
        const uint32ToBuf = function(val) {
          const buffer = new ArrayBuffer(4);
          const view = new DataView(buffer);
          view.setUint32(0, val, true);
          return buffer;
        };
        const payload = [];
        payload.push(uint32ToBuf(16908802));
        const jsonBuf = new Blob([msg]);
        payload.push(uint32ToBuf(jsonBuf.size));
        payload.push(jsonBuf);
        for (let i4 = 0; i4 < blobs.length; i4++) {
          const blob2 = blobs[i4];
          payload.push(
            uint32ToBuf(
              blob2.byteLength || blob2.size || 0
            )
          );
          payload.push(blob2);
        }
        const blob = new Blob(payload);
        msg = blob;
      }
      this.$sendMsg(msg);
    }
    $sendMsg(msg) {
      if (this.$socket && this.$socket.readyState) {
        this.$socket.send(msg);
      } else {
        this.$pendingMessages.push(msg);
      }
    }
    receiveError(name, error) {
      if (this.$errors[name] === error)
        return;
      this.$errors[name] = error;
      delete this.$values[name];
      const binding = this.$bindings[name];
      const evt = import_jquery38.default.Event("shiny:error");
      evt.name = name;
      evt.error = error;
      evt.binding = binding;
      (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
        binding.onValueError(evt.error);
      }
    }
    async receiveOutput(name, value) {
      const binding = this.$bindings[name];
      const evt = import_jquery38.default.Event("shiny:value");
      evt.name = name;
      evt.value = value;
      evt.binding = binding;
      if (this.$values[name] === value) {
        (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
        return void 0;
      }
      this.$values[name] = value;
      delete this.$errors[name];
      (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding) {
        await binding.onValueChange(evt.value);
      }
      return value;
    }
    async bindOutput(id, binding) {
      if (!id)
        throw new Error("Can't bind an element with no ID");
      this.$bindings[id] = binding;
      if (this.$values[id] !== void 0)
        await binding.onValueChange(this.$values[id]);
      else if (this.$errors[id] !== void 0)
        binding.onValueError(this.$errors[id]);
      return binding;
    }
    unbindOutput(id, binding) {
      if (this.$bindings[id] === binding) {
        delete this.$bindings[id];
        return true;
      } else {
        return false;
      }
    }
    _narrowScopeComponent(scopeComponent, nsPrefix) {
      return Object.keys(scopeComponent).filter((k2) => k2.indexOf(nsPrefix) === 0).map((k2) => ({ [k2.substring(nsPrefix.length)]: scopeComponent[k2] })).reduce((obj, pair) => import_jquery38.default.extend(obj, pair), {});
    }
    _narrowScope(scope, nsPrefix) {
      if (nsPrefix) {
        return {
          input: this._narrowScopeComponent(scope.input, nsPrefix),
          output: this._narrowScopeComponent(scope.output, nsPrefix)
        };
      }
      return scope;
    }
    $updateConditionals() {
      (0, import_jquery38.default)(document).trigger({
        type: "shiny:conditional"
      });
      const inputs = {};
      for (const name in this.$inputValues) {
        if (hasOwnProperty(this.$inputValues, name)) {
          const shortName = name.replace(/:.*/, "");
          inputs[shortName] = this.$inputValues[name];
        }
      }
      const scope = { input: inputs, output: this.$values };
      const conditionals = (0, import_jquery38.default)(document).find("[data-display-if]");
      for (let i4 = 0; i4 < conditionals.length; i4++) {
        const el = (0, import_jquery38.default)(conditionals[i4]);
        let condFunc = el.data("data-display-if-func");
        if (!condFunc) {
          const condExpr = el.attr("data-display-if");
          condFunc = scopeExprToFunc(condExpr);
          el.data("data-display-if-func", condFunc);
        }
        const nsPrefix = el.attr("data-ns-prefix");
        const nsScope = this._narrowScope(scope, nsPrefix);
        const show3 = Boolean(condFunc(nsScope));
        const showing = el.css("display") !== "none";
        if (show3 !== showing) {
          if (show3) {
            el.trigger("show");
            el.show();
            el.trigger("shown");
          } else {
            el.trigger("hide");
            el.hide();
            el.trigger("hidden");
          }
        }
      }
    }
    async dispatchMessage(data) {
      let msgObj = {};
      if (typeof data === "string") {
        msgObj = JSON.parse(data);
      } else {
        const len = new DataView(data, 0, 1).getUint8(0);
        const typedv = new DataView(data, 1, len);
        const typebuf = [];
        for (let i4 = 0; i4 < len; i4++) {
          typebuf.push(String.fromCharCode(typedv.getUint8(i4)));
        }
        const type = typebuf.join("");
        data = data.slice(len + 1);
        msgObj.custom = {};
        msgObj.custom[type] = data;
      }
      const evt = import_jquery38.default.Event("shiny:message");
      evt.message = msgObj;
      (0, import_jquery38.default)(document).trigger(evt);
      if (evt.isDefaultPrevented())
        return;
      this.$outputProgress.updateStateFromMessage(evt.message);
      await this._sendMessagesToHandlers(
        evt.message,
        messageHandlers,
        messageHandlerOrder
      );
      this.$updateConditionals();
    }
    async _sendMessagesToHandlers(msgObj, handlers, handlerOrder) {
      for (let i4 = 0; i4 < handlerOrder.length; i4++) {
        const msgType = handlerOrder[i4];
        if (hasOwnProperty(msgObj, msgType)) {
          await handlers[msgType].call(this, msgObj[msgType]);
        }
      }
    }
    _updateProgress() {
      const changed = this.$outputProgress.takeChanges();
      for (const [name, recalculating] of changed.entries()) {
        if (hasOwnProperty(this.$bindings, name)) {
          this.$bindings[name].showProgress(recalculating);
        }
      }
    }
    _init() {
      addMessageHandler("values", async (message) => {
        this._updateProgress();
        for (const key in message) {
          if (hasOwnProperty(message, key)) {
            await this.receiveOutput(key, message[key]);
          }
        }
      });
      addMessageHandler(
        "errors",
        (message) => {
          for (const key in message) {
            if (hasOwnProperty(message, key)) {
              this.receiveError(key, message[key]);
            }
          }
        }
      );
      addMessageHandler(
        "inputMessages",
        async (message) => {
          for (let i4 = 0; i4 < message.length; i4++) {
            const $obj = (0, import_jquery38.default)(".shiny-bound-input#" + $escape(message[i4].id));
            const inputBinding = $obj.data("shiny-input-binding");
            if ($obj.length > 0) {
              if (!$obj.attr("aria-live"))
                $obj.attr("aria-live", "polite");
              const el = $obj[0];
              const evt = import_jquery38.default.Event("shiny:updateinput");
              evt.message = message[i4].message;
              evt.binding = inputBinding;
              (0, import_jquery38.default)(el).trigger(evt);
              if (!evt.isDefaultPrevented()) {
                try {
                  await inputBinding.receiveMessage(el, evt.message);
                } catch (error) {
                  console.error(
                    "[shiny] Error in inputBinding.receiveMessage()",
                    { error, binding: inputBinding, message: evt.message }
                  );
                }
              }
            }
          }
        }
      );
      addMessageHandler("javascript", (message) => {
        indirectEval(message);
      });
      addMessageHandler("console", (message) => {
        for (let i4 = 0; i4 < message.length; i4++) {
          if (console.log)
            console.log(message[i4]);
        }
      });
      addMessageHandler(
        "progress",
        async (message) => {
          if (message.type && message.message) {
            const handler = await this.progressHandlers[message.type];
            if (handler)
              handler.call(this, message.message);
          }
        }
      );
      addMessageHandler(
        "notification",
        async (message) => {
          if (message.type === "show")
            await show2(message.message);
          else if (message.type === "remove")
            remove2(message.message);
          else
            throw "Unkown notification type: " + message.type;
        }
      );
      addMessageHandler(
        "modal",
        async (message) => {
          if (message.type === "show")
            await show(message.message);
          else if (message.type === "remove")
            remove();
          else
            throw "Unkown modal type: " + message.type;
        }
      );
      addMessageHandler(
        "response",
        (message) => {
          const requestId = message.tag;
          const request = this.$activeRequests[requestId];
          if (request) {
            delete this.$activeRequests[requestId];
            if ("value" in message)
              request.onSuccess(message.value);
            else
              request.onError(message.error);
          }
        }
      );
      addMessageHandler("allowReconnect", (message) => {
        switch (message) {
          case true:
          case false:
          case "force":
            this.$allowReconnect = message;
            break;
          default:
            throw "Invalid value for allowReconnect: " + message;
        }
      });
      addMessageHandler("custom", async (message) => {
        const shinyOnCustomMessage = getShinyOnCustomMessage();
        if (shinyOnCustomMessage)
          await shinyOnCustomMessage(message);
        await this._sendMessagesToHandlers(
          message,
          customMessageHandlers,
          customMessageHandlerOrder
        );
      });
      addMessageHandler(
        "config",
        (message) => {
          this.config = {
            workerId: message.workerId,
            sessionId: message.sessionId
          };
          if (message.user)
            setShinyUser(message.user);
          (0, import_jquery38.default)(document).trigger("shiny:sessioninitialized");
        }
      );
      addMessageHandler("busy", (message) => {
        if (message === "busy") {
          (0, import_jquery38.default)(document.documentElement).addClass("shiny-busy");
          (0, import_jquery38.default)(document).trigger("shiny:busy");
        } else if (message === "idle") {
          (0, import_jquery38.default)(document.documentElement).removeClass("shiny-busy");
          (0, import_jquery38.default)(document).trigger("shiny:idle");
        }
      });
      addMessageHandler(
        "recalculating",
        (message) => {
          if (hasOwnProperty(message, "name") && hasOwnProperty(message, "status")) {
            const binding = this.$bindings[message.name];
            if (binding) {
              (0, import_jquery38.default)(binding.el).trigger("shiny:" + message.status);
            } else {
              (0, import_jquery38.default)().trigger("shiny:" + message.status);
            }
          }
        }
      );
      addMessageHandler("reload", (message) => {
        window.location.reload();
        return;
        message;
      });
      addMessageHandler(
        "shiny-insert-ui",
        async (message) => {
          const targets = (0, import_jquery38.default)(message.selector);
          if (targets.length === 0) {
            console.warn(
              'The selector you chose ("' + message.selector + '") could not be found in the DOM.'
            );
            await renderHtmlAsync(
              message.content.html,
              (0, import_jquery38.default)([]),
              message.content.deps
            );
          } else {
            for (const target of targets) {
              await renderContentAsync(target, message.content, message.where);
              if (message.multiple === false)
                break;
            }
          }
        }
      );
      addMessageHandler(
        "shiny-remove-ui",
        (message) => {
          const els = (0, import_jquery38.default)(message.selector);
          els.each(function(i4, el) {
            shinyUnbindAll(el, true);
            (0, import_jquery38.default)(el).remove();
            return message.multiple === false ? false : void 0;
          });
        }
      );
      addMessageHandler("frozen", (message) => {
        for (let i4 = 0; i4 < message.ids.length; i4++) {
          shinyForgetLastInputValue(message.ids[i4]);
        }
      });
      function getTabset(id) {
        const $tabset = (0, import_jquery38.default)("#" + $escape(id));
        if ($tabset.length === 0)
          throw "There is no tabsetPanel (or navbarPage or navlistPanel) with id equal to '" + id + "'";
        return $tabset;
      }
      function getTabContent($tabset) {
        const tabsetId = $tabset.attr("data-tabsetid");
        const $tabContent = (0, import_jquery38.default)(
          "div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']"
        );
        return $tabContent;
      }
      function getTargetTabs($tabset, $tabContent, target) {
        const dataValue = "[data-value='" + $escape(target) + "']";
        const $aTag = $tabset.find("a" + dataValue);
        const $liTag = $aTag.parent();
        if ($liTag.length === 0) {
          throw "There is no tabPanel (or navbarMenu) with value (or menuName) equal to '" + target + "'";
        }
        const $liTags = [];
        const $divTags = [];
        if ($aTag.attr("data-toggle") === "dropdown") {
          const $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
          const dropdownId = $dropdownTabset.attr("data-tabsetid");
          const $dropdownLiTags = $dropdownTabset.find("a[data-toggle='tab']").parent("li");
          $dropdownLiTags.each(function(i4, el) {
            $liTags.push((0, import_jquery38.default)(el));
          });
          const selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
          const $dropdownDivs = $tabContent.find(selector);
          $dropdownDivs.each(function(i4, el) {
            $divTags.push((0, import_jquery38.default)(el));
          });
        } else {
          $divTags.push($tabContent.find("div" + dataValue));
        }
        return { $liTag, $liTags, $divTags };
      }
      addMessageHandler(
        "shiny-insert-tab",
        async (message) => {
          const $parentTabset = getTabset(message.inputId);
          let $tabset = $parentTabset;
          const $tabContent = getTabContent($tabset);
          let tabsetId = $parentTabset.attr("data-tabsetid");
          const $fragLi = (0, import_jquery38.default)("<div>");
          await renderContentAsync($fragLi, message.liTag, "afterBegin");
          const $liTag = (0, import_jquery38.default)($fragLi).find("> li");
          const $aTag = $liTag.find("> a");
          let $targetLiTag = null;
          if (message.target !== null) {
            const targetInfo = getTargetTabs(
              $tabset,
              $tabContent,
              message.target
            );
            $targetLiTag = targetInfo.$liTag;
          }
          const dropdown = getDropdown();
          if (dropdown !== null) {
            if ($aTag.attr("data-toggle") === "dropdown")
              throw "Cannot insert a navbarMenu inside another one";
            $tabset = dropdown.$tabset;
            tabsetId = dropdown.id;
            $liTag.removeClass("nav-item").find(".nav-link").removeClass("nav-link").addClass("dropdown-item");
          }
          let fixupDivId = "";
          if ($aTag.attr("data-toggle") === "tab") {
            const index = getTabIndex($tabset, tabsetId);
            const tabId = "tab-" + tabsetId + "-" + index;
            $liTag.find("> a").attr("href", "#" + tabId);
            fixupDivId = tabId;
          }
          if (message.position === "before") {
            if ($targetLiTag) {
              $targetLiTag.before($liTag);
            } else {
              $tabset.prepend($liTag);
            }
          } else if (message.position === "after") {
            if ($targetLiTag) {
              $targetLiTag.after($liTag);
            } else {
              $tabset.append($liTag);
            }
          }
          await shinyBindAll($targetLiTag?.parent() || $tabset);
          await renderContentAsync($tabContent[0], message.divTag, "beforeEnd");
          if (fixupDivId) {
            $tabContent.find('[id="tab-tsid-id"]').attr("id", fixupDivId);
          }
          if (message.select) {
            $liTag.find("a").tab("show");
          }
          function getTabIndex($tabset2, tabsetId2) {
            const existingTabIds = [0];
            $tabset2.find("> li").each(function() {
              const $tab = (0, import_jquery38.default)(this).find("> a[data-toggle='tab']");
              if ($tab.length > 0) {
                const href = $tab.attr("href").replace(/.*(?=#[^\s]+$)/, "");
                const index = href.replace("#tab-" + tabsetId2 + "-", "");
                existingTabIds.push(Number(index));
              }
            });
            return Math.max.apply(null, existingTabIds) + 1;
          }
          function getDropdown() {
            if (message.menuName !== null) {
              const $dropdownATag = (0, import_jquery38.default)(
                "a.dropdown-toggle[data-value='" + $escape(message.menuName) + "']"
              );
              if ($dropdownATag.length === 0) {
                throw "There is no navbarMenu with menuName equal to '" + message.menuName + "'";
              }
              const $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
              const dropdownId = $dropdownTabset.attr("data-tabsetid");
              return { $tabset: $dropdownTabset, id: dropdownId };
            } else if (message.target !== null && $targetLiTag !== null) {
              const $uncleTabset = $targetLiTag.parent("ul");
              if ($uncleTabset.hasClass("dropdown-menu")) {
                const uncleId = $uncleTabset.attr("data-tabsetid");
                return { $tabset: $uncleTabset, id: uncleId };
              }
            }
            return null;
          }
        }
      );
      function ensureTabsetHasVisibleTab($tabset) {
        const inputBinding = $tabset.data("shiny-input-binding");
        if (!inputBinding.getValue($tabset)) {
          const destTabValue = getFirstTab($tabset);
          const evt = import_jquery38.default.Event("shiny:updateinput");
          evt.binding = inputBinding;
          $tabset.trigger(evt);
          inputBinding.setValue($tabset[0], destTabValue);
        }
      }
      function getFirstTab($ul) {
        return $ul.find("li:visible a[data-toggle='tab']").first().attr("data-value") || null;
      }
      function tabApplyFunction(target, func, liTags = false) {
        import_jquery38.default.each(target, function(key, el) {
          if (key === "$liTag") {
            func(el);
          } else if (key === "$divTags") {
            import_jquery38.default.each(
              el,
              function(i4, div) {
                func(div);
              }
            );
          } else if (liTags && key === "$liTags") {
            import_jquery38.default.each(
              el,
              function(i4, div) {
                func(div);
              }
            );
          }
        });
      }
      addMessageHandler(
        "shiny-remove-tab",
        (message) => {
          const $tabset = getTabset(message.inputId);
          const $tabContent = getTabContent($tabset);
          const target = getTargetTabs($tabset, $tabContent, message.target);
          tabApplyFunction(target, removeEl);
          ensureTabsetHasVisibleTab($tabset);
          function removeEl($el) {
            shinyUnbindAll($el, true);
            $el.remove();
          }
        }
      );
      addMessageHandler(
        "shiny-change-tab-visibility",
        (message) => {
          const $tabset = getTabset(message.inputId);
          const $tabContent = getTabContent($tabset);
          const target = getTargetTabs($tabset, $tabContent, message.target);
          tabApplyFunction(target, changeVisibility, true);
          ensureTabsetHasVisibleTab($tabset);
          function changeVisibility($el) {
            if (message.type === "show")
              $el.css("display", "");
            else if (message.type === "hide") {
              $el.hide();
              $el.removeClass("active");
            }
          }
        }
      );
      addMessageHandler(
        "updateQueryString",
        (message) => {
          if (message.mode === "replace") {
            window.history.replaceState(null, null, message.queryString);
            return;
          }
          let what = null;
          if (message.queryString.charAt(0) === "#")
            what = "hash";
          else if (message.queryString.charAt(0) === "?")
            what = "query";
          else
            throw "The 'query' string must start with either '?' (to update the query string) or with '#' (to update the hash).";
          const path = window.location.pathname;
          const oldQS = window.location.search;
          const oldHash = window.location.hash;
          let relURL = path;
          if (what === "query")
            relURL += message.queryString;
          else
            relURL += oldQS + message.queryString;
          window.history.pushState(null, null, relURL);
          if (message.queryString.indexOf("#") !== -1)
            what = "hash";
          if (window.location.hash !== oldHash)
            what = "hash";
          if (what === "hash")
            (0, import_jquery38.default)(document).trigger("hashchange");
        }
      );
      addMessageHandler(
        "resetBrush",
        (message) => {
          resetBrush(message.brushId);
        }
      );
    }
    getTestSnapshotBaseUrl({ fullUrl = true } = {}) {
      const loc = window.location;
      let url = "";
      if (fullUrl) {
        url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
      }
      url += "/session/" + encodeURIComponent(this.config.sessionId) + "/dataobj/shinytest?w=" + encodeURIComponent(this.config.workerId) + "&nonce=" + randomId();
      return url;
    }
  };

  // srcts/src/shiny/index.ts
  var ShinyClass = class {
    constructor() {
      this.version = "1.10.0.9000";
      const { inputBindings, fileInputBinding: fileInputBinding2 } = initInputBindings();
      const { outputBindings } = initOutputBindings();
      setFileInputBinding(fileInputBinding2);
      this.$escape = $escape;
      this.compareVersion = compareVersion;
      this.inputBindings = inputBindings;
      this.InputBinding = InputBinding;
      this.outputBindings = outputBindings;
      this.OutputBinding = OutputBinding;
      this.resetBrush = resetBrush;
      this.notifications = {
        show: show2,
        remove: remove2
      };
      this.modal = { show, remove };
      this.addCustomMessageHandler = addCustomMessageHandler;
      this.showReconnectDialog = showReconnectDialog;
      this.hideReconnectDialog = hideReconnectDialog;
      this.renderDependenciesAsync = renderDependenciesAsync;
      this.renderDependencies = renderDependencies;
      this.renderContentAsync = renderContentAsync;
      this.renderContent = renderContent;
      this.renderHtmlAsync = renderHtmlAsync;
      this.renderHtml = renderHtml2;
      this.initializedPromise = createInitStatus();
      (0, import_jquery39.default)(() => {
        setTimeout(async () => {
          try {
            await this.initialize();
          } catch (e4) {
            showErrorInClientConsole(e4);
            throw e4;
          }
        }, 1);
      });
    }
    inDevMode() {
      if ("__SHINY_DEV_MODE__" in window)
        return Boolean(window.__SHINY_DEV_MODE__);
      return false;
    }
    async initialize() {
      setShinyObj(this);
      this.shinyapp = new ShinyApp();
      const shinyapp = this.shinyapp;
      this.progressHandlers = shinyapp.progressHandlers;
      const inputBatchSender = new InputBatchSender(shinyapp);
      const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
      const inputsEvent = new InputEventDecorator(inputsNoResend);
      const inputsRate = new InputRateDecorator(inputsEvent);
      const inputsDefer = new InputDeferDecorator(inputsEvent);
      let target;
      if ((0, import_jquery39.default)('input[type="submit"], button[type="submit"]').length > 0) {
        target = inputsDefer;
        (0, import_jquery39.default)('input[type="submit"], button[type="submit"]').each(function() {
          (0, import_jquery39.default)(this).click(function(event) {
            event.preventDefault();
            inputsDefer.submit();
          });
        });
      } else {
        target = inputsRate;
      }
      const inputs = new InputValidateDecorator(target);
      this.setInputValue = this.onInputChange = function(name, value, opts = {}) {
        const newOpts = addDefaultInputOpts(opts);
        inputs.setInput(name, value, newOpts);
      };
      this.forgetLastInputValue = function(name) {
        inputsNoResend.forget(name);
      };
      const inputBindings = this.inputBindings;
      const outputBindings = this.outputBindings;
      function shinyBindCtx() {
        return {
          inputs,
          inputsRate,
          sendOutputHiddenState,
          maybeAddThemeObserver,
          inputBindings,
          outputBindings,
          initDeferredIframes
        };
      }
      this.bindAll = async function(scope) {
        await bindAll(shinyBindCtx(), scope);
      };
      this.unbindAll = function(scope, includeSelf = false) {
        unbindAll(shinyBindCtx(), scope, includeSelf);
      };
      function initializeInputs(scope = document.documentElement) {
        const bindings = inputBindings.getBindings();
        for (let i4 = 0; i4 < bindings.length; i4++) {
          const binding = bindings[i4].binding;
          const inputObjects = binding.find(scope);
          if (inputObjects) {
            for (let j2 = 0; j2 < inputObjects.length; j2++) {
              const $inputObjectJ = (0, import_jquery39.default)(inputObjects[j2]);
              if (!$inputObjectJ.data("_shiny_initialized")) {
                $inputObjectJ.data("_shiny_initialized", true);
                binding.initialize(inputObjects[j2]);
              }
            }
          }
        }
      }
      this.initializeInputs = initializeInputs;
      function getIdFromEl(el) {
        const $el = (0, import_jquery39.default)(el);
        const bindingAdapter = $el.data("shiny-output-binding");
        if (!bindingAdapter)
          return null;
        else
          return bindingAdapter.getId();
      }
      initializeInputs(document.documentElement);
      const initialValues = mapValues(
        await _bindAll(shinyBindCtx(), document.documentElement),
        (x2) => x2.value
      );
      (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
        function() {
          const id = getIdFromEl(this), rect = getBoundingClientSizeBeforeZoom(this);
          if (rect.width !== 0 || rect.height !== 0) {
            initialValues[".clientdata_output_" + id + "_width"] = rect.width;
            initialValues[".clientdata_output_" + id + "_height"] = rect.height;
          }
        }
      );
      function getComputedBgColor(el) {
        if (!el) {
          return null;
        }
        const bgColor = getStyle(el, "background-color");
        if (!bgColor)
          return bgColor;
        const m2 = bgColor.match(
          /^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/
        );
        if (bgColor === "transparent" || m2 && parseFloat(m2[4]) === 0) {
          const bgImage = getStyle(el, "background-image");
          if (bgImage && bgImage !== "none") {
            return null;
          } else {
            return getComputedBgColor(el.parentElement);
          }
        }
        return bgColor;
      }
      function getComputedFont(el) {
        const fontFamily = getStyle(el, "font-family");
        const fontSize = getStyle(el, "font-size");
        return {
          families: fontFamily?.replace(/"/g, "").split(", "),
          size: fontSize
        };
      }
      (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
        function() {
          const el = this;
          const id = getIdFromEl(el);
          initialValues[".clientdata_output_" + id + "_bg"] = getComputedBgColor(el);
          initialValues[".clientdata_output_" + id + "_fg"] = getStyle(
            el,
            "color"
          );
          initialValues[".clientdata_output_" + id + "_accent"] = getComputedLinkColor(el);
          initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(el);
          maybeAddThemeObserver(el);
        }
      );
      function maybeAddThemeObserver(el) {
        if (!window.MutationObserver) {
          return;
        }
        const cl = el.classList;
        const reportTheme = cl.contains("shiny-image-output") || cl.contains("shiny-plot-output") || cl.contains("shiny-report-theme");
        if (!reportTheme) {
          return;
        }
        const $el = (0, import_jquery39.default)(el);
        if ($el.data("shiny-theme-observer")) {
          return;
        }
        const observerCallback = new Debouncer(null, () => doSendTheme(el), 100);
        const observer = new MutationObserver(
          () => observerCallback.normalCall()
        );
        const config = { attributes: true, attributeFilter: ["style", "class"] };
        observer.observe(el, config);
        $el.data("shiny-theme-observer", observer);
      }
      function doSendTheme(el) {
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
        (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(
          function() {
            const id = getIdFromEl(this), rect = getBoundingClientSizeBeforeZoom(this);
            if (rect.width !== 0 || rect.height !== 0) {
              inputs.setInput(".clientdata_output_" + id + "_width", rect.width);
              inputs.setInput(
                ".clientdata_output_" + id + "_height",
                rect.height
              );
            }
          }
        );
        (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(
          function() {
            doSendTheme(this);
          }
        );
        (0, import_jquery39.default)(".shiny-bound-output").each(function() {
          const $this = (0, import_jquery39.default)(this), binding = $this.data("shiny-output-binding");
          $this.trigger({
            type: "shiny:visualchange",
            visible: !isHidden(this),
            binding
          });
          binding.onResize();
        });
      }
      sendImageSizeFns.setImageSend(inputBatchSender, doSendImageSize);
      function isHidden(obj) {
        if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
          return false;
        } else if (getStyle(obj, "display") === "none") {
          return true;
        } else {
          return isHidden(obj.parentNode);
        }
      }
      let lastKnownVisibleOutputs = {};
      (0, import_jquery39.default)(".shiny-bound-output").each(function() {
        const id = getIdFromEl(this);
        if (isHidden(this)) {
          initialValues[".clientdata_output_" + id + "_hidden"] = true;
        } else {
          lastKnownVisibleOutputs[id] = true;
          initialValues[".clientdata_output_" + id + "_hidden"] = false;
        }
      });
      function doSendOutputHiddenState() {
        const visibleOutputs = {};
        (0, import_jquery39.default)(".shiny-bound-output").each(function() {
          const id = getIdFromEl(this);
          delete lastKnownVisibleOutputs[id];
          const hidden = isHidden(this), evt = {
            type: "shiny:visualchange",
            visible: !hidden
          };
          if (hidden) {
            inputs.setInput(".clientdata_output_" + id + "_hidden", true);
          } else {
            visibleOutputs[id] = true;
            inputs.setInput(".clientdata_output_" + id + "_hidden", false);
          }
          const $this = (0, import_jquery39.default)(this);
          evt.binding = $this.data("shiny-output-binding");
          $this.trigger(evt);
        });
        for (const name in lastKnownVisibleOutputs) {
          if (hasDefinedProperty(lastKnownVisibleOutputs, name))
            inputs.setInput(".clientdata_output_" + name + "_hidden", true);
        }
        lastKnownVisibleOutputs = visibleOutputs;
      }
      const sendOutputHiddenStateDebouncer = new Debouncer(
        null,
        doSendOutputHiddenState,
        0
      );
      function sendOutputHiddenState() {
        sendOutputHiddenStateDebouncer.normalCall();
      }
      inputBatchSender.lastChanceCallback.push(function() {
        if (sendOutputHiddenStateDebouncer.isPending())
          sendOutputHiddenStateDebouncer.immediateCall();
      });
      function filterEventsByNamespace(namespace, handler, ...args) {
        const namespaceArr = namespace.split(".");
        return function(e4) {
          const eventNamespace = e4.namespace?.split(".") ?? [];
          for (let i4 = 0; i4 < namespaceArr.length; i4++) {
            if (eventNamespace.indexOf(namespaceArr[i4]) === -1)
              return;
          }
          handler.apply(this, [namespaceArr, handler, ...args]);
        };
      }
      (0, import_jquery39.default)(window).resize(debounce(500, sendImageSizeFns.regular));
      const bs3classes = [
        "modal",
        "dropdown",
        "tab",
        "tooltip",
        "popover",
        "collapse"
      ];
      import_jquery39.default.each(bs3classes, function(idx, classname) {
        (0, import_jquery39.default)(document.body).on(
          "shown.bs." + classname + ".sendImageSize",
          "*",
          filterEventsByNamespace("bs", sendImageSizeFns.regular)
        );
        (0, import_jquery39.default)(document.body).on(
          "shown.bs." + classname + ".sendOutputHiddenState hidden.bs." + classname + ".sendOutputHiddenState",
          "*",
          filterEventsByNamespace("bs", sendOutputHiddenState)
        );
      });
      (0, import_jquery39.default)(document.body).on("shown.sendImageSize", "*", sendImageSizeFns.regular);
      (0, import_jquery39.default)(document.body).on(
        "shown.sendOutputHiddenState hidden.sendOutputHiddenState",
        "*",
        sendOutputHiddenState
      );
      initialValues[".clientdata_pixelratio"] = pixelRatio();
      (0, import_jquery39.default)(window).resize(function() {
        inputs.setInput(".clientdata_pixelratio", pixelRatio());
      });
      initialValues[".clientdata_url_protocol"] = window.location.protocol;
      initialValues[".clientdata_url_hostname"] = window.location.hostname;
      initialValues[".clientdata_url_port"] = window.location.port;
      initialValues[".clientdata_url_pathname"] = window.location.pathname;
      initialValues[".clientdata_url_search"] = window.location.search;
      (0, import_jquery39.default)(window).on("pushstate", function(e4) {
        inputs.setInput(".clientdata_url_search", window.location.search);
        return;
        e4;
      });
      (0, import_jquery39.default)(window).on("popstate", function(e4) {
        inputs.setInput(".clientdata_url_search", window.location.search);
        return;
        e4;
      });
      initialValues[".clientdata_url_hash_initial"] = window.location.hash;
      initialValues[".clientdata_url_hash"] = window.location.hash;
      (0, import_jquery39.default)(window).on("hashchange", function(e4) {
        inputs.setInput(".clientdata_url_hash", window.location.hash);
        return;
        e4;
      });
      const singletonText = initialValues[".clientdata_singletons"] = (0, import_jquery39.default)(
        'script[type="application/shiny-singletons"]'
      ).text();
      registerNames(singletonText.split(/,/));
      const dependencyText = (0, import_jquery39.default)(
        'script[type="application/html-dependencies"]'
      ).text();
      import_jquery39.default.each(dependencyText.split(/;/), function(i4, depStr) {
        const match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
        if (match) {
          registerDependency(match[1], match[2]);
        }
      });
      inputsNoResend.reset(initialValues);
      shinyapp.connect(initialValues);
      (0, import_jquery39.default)(document).one("shiny:connected", () => {
        initDeferredIframes();
      });
      (0, import_jquery39.default)(document).one("shiny:sessioninitialized", () => {
        this.initializedPromise.resolve();
      });
    }
  };
  function initDeferredIframes() {
    if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
      return;
    }
    (0, import_jquery39.default)(".shiny-frame-deferred").each(function(i4, el) {
      const $el = (0, import_jquery39.default)(el);
      $el.removeClass("shiny-frame-deferred");
      $el.attr("src", $el.attr("data-deferred-src"));
      $el.attr("data-deferred-src", null);
    });
  }

  // srcts/src/window/userAgent.ts
  function windowUserAgent() {
    return window.navigator.userAgent;
  }

  // srcts/src/shiny/reactlog.ts
  var import_jquery40 = __toESM(require_jquery());
  function shinyAppConfig() {
    return shinyShinyApp().config;
  }
  function initReactlog() {
    (0, import_jquery40.default)(document).on("keydown", function(e4) {
      if (e4.which !== 114 || !e4.ctrlKey && !e4.metaKey || e4.shiftKey || e4.altKey)
        return;
      const url = "reactlog?w=" + window.escape(shinyAppConfig().workerId) + "&s=" + window.escape(shinyAppConfig().sessionId);
      window.open(url);
      e4.preventDefault();
    });
    (0, import_jquery40.default)(document).on("keydown", function(e4) {
      if (!(e4.which === 115 && (e4.ctrlKey || e4.metaKey) && !e4.shiftKey && !e4.altKey || e4.which === 114 && (e4.ctrlKey || e4.metaKey) && e4.shiftKey && !e4.altKey)) {
        return;
      }
      const url = "reactlog/mark?w=" + window.escape(shinyAppConfig().workerId) + "&s=" + window.escape(shinyAppConfig().sessionId);
      import_jquery40.default.get(url, function(result) {
        if (result !== "marked")
          return;
        const html = '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';
        show2({
          html,
          closeButton: true
        });
      }).fail(function() {
        window.open(url);
      });
      e4.preventDefault();
    });
  }

  // srcts/src/initialize/index.ts
  var Shiny;
  function init() {
    if (window.Shiny) {
      throw new Error("Trying to create window.Shiny, but it already exists!");
    }
    Shiny = window.Shiny = new ShinyClass();
    setUserAgent(windowUserAgent());
    determineBrowserInfo();
    trackHistory();
    disableFormSubmission();
    initReactlog();
  }

  // srcts/src/index.ts
  init();
})();
/**
 * @license
 * Copyright 2017 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
/**
 * @license
 * Copyright 2019 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
/**
 * @license
 * Copyright 2022 Google LLC
 * SPDX-License-Identifier: BSD-3-Clause
 */
//# sourceMappingURL=shiny.js.map
