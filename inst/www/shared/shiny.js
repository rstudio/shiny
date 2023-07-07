/*! shiny 1.7.4.9002 | (c) 2012-2023 RStudio, PBC. | License: GPL-3 | file LICENSE */
"use strict";
(function() {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __commonJS = function(cb, mod) {
    return function __require() {
      return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
    };
  };
  var __copyProps = function(to, from2, except, desc) {
    if (from2 && typeof from2 === "object" || typeof from2 === "function")
      for (var keys2 = __getOwnPropNames(from2), i = 0, n = keys2.length, key; i < n; i++) {
        key = keys2[i];
        if (!__hasOwnProp.call(to, key) && key !== except)
          __defProp(to, key, { get: function(k) {
            return from2[k];
          }.bind(null, key), enumerable: !(desc = __getOwnPropDesc(from2, key)) || desc.enumerable });
      }
    return to;
  };
  var __toESM = function(mod, isNodeMode, target) {
    return target = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
      isNodeMode || !mod || !mod.__esModule ? __defProp(target, "default", { value: mod, enumerable: true }) : target,
      mod
    );
  };

  // globals:jquery
  var require_jquery = __commonJS({
    "globals:jquery": function(exports, module) {
      module.exports = window.jQuery;
    }
  });

  // node_modules/core-js/internals/global.js
  var require_global = __commonJS({
    "node_modules/core-js/internals/global.js": function(exports, module) {
      var check = function(it) {
        return it && it.Math == Math && it;
      };
      module.exports = check(typeof globalThis == "object" && globalThis) || check(typeof window == "object" && window) || check(typeof self == "object" && self) || check(typeof global == "object" && global) || function() {
        return this;
      }() || Function("return this")();
    }
  });

  // node_modules/core-js/internals/fails.js
  var require_fails = __commonJS({
    "node_modules/core-js/internals/fails.js": function(exports, module) {
      module.exports = function(exec2) {
        try {
          return !!exec2();
        } catch (error) {
          return true;
        }
      };
    }
  });

  // node_modules/core-js/internals/descriptors.js
  var require_descriptors = __commonJS({
    "node_modules/core-js/internals/descriptors.js": function(exports, module) {
      var fails12 = require_fails();
      module.exports = !fails12(function() {
        return Object.defineProperty({}, 1, { get: function() {
          return 7;
        } })[1] != 7;
      });
    }
  });

  // node_modules/core-js/internals/function-bind-native.js
  var require_function_bind_native = __commonJS({
    "node_modules/core-js/internals/function-bind-native.js": function(exports, module) {
      var fails12 = require_fails();
      module.exports = !fails12(function() {
        var test2 = function() {
        }.bind();
        return typeof test2 != "function" || test2.hasOwnProperty("prototype");
      });
    }
  });

  // node_modules/core-js/internals/function-call.js
  var require_function_call = __commonJS({
    "node_modules/core-js/internals/function-call.js": function(exports, module) {
      var NATIVE_BIND = require_function_bind_native();
      var call8 = Function.prototype.call;
      module.exports = NATIVE_BIND ? call8.bind(call8) : function() {
        return call8.apply(call8, arguments);
      };
    }
  });

  // node_modules/core-js/internals/object-property-is-enumerable.js
  var require_object_property_is_enumerable = __commonJS({
    "node_modules/core-js/internals/object-property-is-enumerable.js": function(exports) {
      "use strict";
      var $propertyIsEnumerable = {}.propertyIsEnumerable;
      var getOwnPropertyDescriptor3 = Object.getOwnPropertyDescriptor;
      var NASHORN_BUG = getOwnPropertyDescriptor3 && !$propertyIsEnumerable.call({ 1: 2 }, 1);
      exports.f = NASHORN_BUG ? function propertyIsEnumerable(V) {
        var descriptor = getOwnPropertyDescriptor3(this, V);
        return !!descriptor && descriptor.enumerable;
      } : $propertyIsEnumerable;
    }
  });

  // node_modules/core-js/internals/create-property-descriptor.js
  var require_create_property_descriptor = __commonJS({
    "node_modules/core-js/internals/create-property-descriptor.js": function(exports, module) {
      module.exports = function(bitmap, value) {
        return {
          enumerable: !(bitmap & 1),
          configurable: !(bitmap & 2),
          writable: !(bitmap & 4),
          value: value
        };
      };
    }
  });

  // node_modules/core-js/internals/function-uncurry-this.js
  var require_function_uncurry_this = __commonJS({
    "node_modules/core-js/internals/function-uncurry-this.js": function(exports, module) {
      var NATIVE_BIND = require_function_bind_native();
      var FunctionPrototype2 = Function.prototype;
      var call8 = FunctionPrototype2.call;
      var uncurryThisWithBind = NATIVE_BIND && FunctionPrototype2.bind.bind(call8, call8);
      module.exports = NATIVE_BIND ? uncurryThisWithBind : function(fn) {
        return function() {
          return call8.apply(fn, arguments);
        };
      };
    }
  });

  // node_modules/core-js/internals/classof-raw.js
  var require_classof_raw = __commonJS({
    "node_modules/core-js/internals/classof-raw.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var toString9 = uncurryThis11({}.toString);
      var stringSlice4 = uncurryThis11("".slice);
      module.exports = function(it) {
        return stringSlice4(toString9(it), 8, -1);
      };
    }
  });

  // node_modules/core-js/internals/indexed-object.js
  var require_indexed_object = __commonJS({
    "node_modules/core-js/internals/indexed-object.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var fails12 = require_fails();
      var classof = require_classof_raw();
      var $Object = Object;
      var split = uncurryThis11("".split);
      module.exports = fails12(function() {
        return !$Object("z").propertyIsEnumerable(0);
      }) ? function(it) {
        return classof(it) == "String" ? split(it, "") : $Object(it);
      } : $Object;
    }
  });

  // node_modules/core-js/internals/is-null-or-undefined.js
  var require_is_null_or_undefined = __commonJS({
    "node_modules/core-js/internals/is-null-or-undefined.js": function(exports, module) {
      module.exports = function(it) {
        return it === null || it === void 0;
      };
    }
  });

  // node_modules/core-js/internals/require-object-coercible.js
  var require_require_object_coercible = __commonJS({
    "node_modules/core-js/internals/require-object-coercible.js": function(exports, module) {
      var isNullOrUndefined5 = require_is_null_or_undefined();
      var $TypeError = TypeError;
      module.exports = function(it) {
        if (isNullOrUndefined5(it))
          throw $TypeError("Can't call method on " + it);
        return it;
      };
    }
  });

  // node_modules/core-js/internals/to-indexed-object.js
  var require_to_indexed_object = __commonJS({
    "node_modules/core-js/internals/to-indexed-object.js": function(exports, module) {
      var IndexedObject2 = require_indexed_object();
      var requireObjectCoercible5 = require_require_object_coercible();
      module.exports = function(it) {
        return IndexedObject2(requireObjectCoercible5(it));
      };
    }
  });

  // node_modules/core-js/internals/document-all.js
  var require_document_all = __commonJS({
    "node_modules/core-js/internals/document-all.js": function(exports, module) {
      var documentAll = typeof document == "object" && document.all;
      var IS_HTMLDDA = typeof documentAll == "undefined" && documentAll !== void 0;
      module.exports = {
        all: documentAll,
        IS_HTMLDDA: IS_HTMLDDA
      };
    }
  });

  // node_modules/core-js/internals/is-callable.js
  var require_is_callable = __commonJS({
    "node_modules/core-js/internals/is-callable.js": function(exports, module) {
      var $documentAll = require_document_all();
      var documentAll = $documentAll.all;
      module.exports = $documentAll.IS_HTMLDDA ? function(argument) {
        return typeof argument == "function" || argument === documentAll;
      } : function(argument) {
        return typeof argument == "function";
      };
    }
  });

  // node_modules/core-js/internals/is-object.js
  var require_is_object = __commonJS({
    "node_modules/core-js/internals/is-object.js": function(exports, module) {
      var isCallable4 = require_is_callable();
      var $documentAll = require_document_all();
      var documentAll = $documentAll.all;
      module.exports = $documentAll.IS_HTMLDDA ? function(it) {
        return typeof it == "object" ? it !== null : isCallable4(it) || it === documentAll;
      } : function(it) {
        return typeof it == "object" ? it !== null : isCallable4(it);
      };
    }
  });

  // node_modules/core-js/internals/get-built-in.js
  var require_get_built_in = __commonJS({
    "node_modules/core-js/internals/get-built-in.js": function(exports, module) {
      var global9 = require_global();
      var isCallable4 = require_is_callable();
      var aFunction = function(argument) {
        return isCallable4(argument) ? argument : void 0;
      };
      module.exports = function(namespace, method) {
        return arguments.length < 2 ? aFunction(global9[namespace]) : global9[namespace] && global9[namespace][method];
      };
    }
  });

  // node_modules/core-js/internals/object-is-prototype-of.js
  var require_object_is_prototype_of = __commonJS({
    "node_modules/core-js/internals/object-is-prototype-of.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      module.exports = uncurryThis11({}.isPrototypeOf);
    }
  });

  // node_modules/core-js/internals/engine-user-agent.js
  var require_engine_user_agent = __commonJS({
    "node_modules/core-js/internals/engine-user-agent.js": function(exports, module) {
      module.exports = typeof navigator != "undefined" && String(navigator.userAgent) || "";
    }
  });

  // node_modules/core-js/internals/engine-v8-version.js
  var require_engine_v8_version = __commonJS({
    "node_modules/core-js/internals/engine-v8-version.js": function(exports, module) {
      var global9 = require_global();
      var userAgent2 = require_engine_user_agent();
      var process2 = global9.process;
      var Deno2 = global9.Deno;
      var versions = process2 && process2.versions || Deno2 && Deno2.version;
      var v8 = versions && versions.v8;
      var match;
      var version;
      if (v8) {
        match = v8.split(".");
        version = match[0] > 0 && match[0] < 4 ? 1 : +(match[0] + match[1]);
      }
      if (!version && userAgent2) {
        match = userAgent2.match(/Edge\/(\d+)/);
        if (!match || match[1] >= 74) {
          match = userAgent2.match(/Chrome\/(\d+)/);
          if (match)
            version = +match[1];
        }
      }
      module.exports = version;
    }
  });

  // node_modules/core-js/internals/symbol-constructor-detection.js
  var require_symbol_constructor_detection = __commonJS({
    "node_modules/core-js/internals/symbol-constructor-detection.js": function(exports, module) {
      var V8_VERSION2 = require_engine_v8_version();
      var fails12 = require_fails();
      module.exports = !!Object.getOwnPropertySymbols && !fails12(function() {
        var symbol = Symbol();
        return !String(symbol) || !(Object(symbol) instanceof Symbol) || !Symbol.sham && V8_VERSION2 && V8_VERSION2 < 41;
      });
    }
  });

  // node_modules/core-js/internals/use-symbol-as-uid.js
  var require_use_symbol_as_uid = __commonJS({
    "node_modules/core-js/internals/use-symbol-as-uid.js": function(exports, module) {
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      module.exports = NATIVE_SYMBOL && !Symbol.sham && typeof Symbol.iterator == "symbol";
    }
  });

  // node_modules/core-js/internals/is-symbol.js
  var require_is_symbol = __commonJS({
    "node_modules/core-js/internals/is-symbol.js": function(exports, module) {
      var getBuiltIn3 = require_get_built_in();
      var isCallable4 = require_is_callable();
      var isPrototypeOf3 = require_object_is_prototype_of();
      var USE_SYMBOL_AS_UID = require_use_symbol_as_uid();
      var $Object = Object;
      module.exports = USE_SYMBOL_AS_UID ? function(it) {
        return typeof it == "symbol";
      } : function(it) {
        var $Symbol = getBuiltIn3("Symbol");
        return isCallable4($Symbol) && isPrototypeOf3($Symbol.prototype, $Object(it));
      };
    }
  });

  // node_modules/core-js/internals/try-to-string.js
  var require_try_to_string = __commonJS({
    "node_modules/core-js/internals/try-to-string.js": function(exports, module) {
      var $String = String;
      module.exports = function(argument) {
        try {
          return $String(argument);
        } catch (error) {
          return "Object";
        }
      };
    }
  });

  // node_modules/core-js/internals/a-callable.js
  var require_a_callable = __commonJS({
    "node_modules/core-js/internals/a-callable.js": function(exports, module) {
      var isCallable4 = require_is_callable();
      var tryToString = require_try_to_string();
      var $TypeError = TypeError;
      module.exports = function(argument) {
        if (isCallable4(argument))
          return argument;
        throw $TypeError(tryToString(argument) + " is not a function");
      };
    }
  });

  // node_modules/core-js/internals/get-method.js
  var require_get_method = __commonJS({
    "node_modules/core-js/internals/get-method.js": function(exports, module) {
      var aCallable2 = require_a_callable();
      var isNullOrUndefined5 = require_is_null_or_undefined();
      module.exports = function(V, P) {
        var func = V[P];
        return isNullOrUndefined5(func) ? void 0 : aCallable2(func);
      };
    }
  });

  // node_modules/core-js/internals/ordinary-to-primitive.js
  var require_ordinary_to_primitive = __commonJS({
    "node_modules/core-js/internals/ordinary-to-primitive.js": function(exports, module) {
      var call8 = require_function_call();
      var isCallable4 = require_is_callable();
      var isObject5 = require_is_object();
      var $TypeError = TypeError;
      module.exports = function(input, pref) {
        var fn, val;
        if (pref === "string" && isCallable4(fn = input.toString) && !isObject5(val = call8(fn, input)))
          return val;
        if (isCallable4(fn = input.valueOf) && !isObject5(val = call8(fn, input)))
          return val;
        if (pref !== "string" && isCallable4(fn = input.toString) && !isObject5(val = call8(fn, input)))
          return val;
        throw $TypeError("Can't convert object to primitive value");
      };
    }
  });

  // node_modules/core-js/internals/is-pure.js
  var require_is_pure = __commonJS({
    "node_modules/core-js/internals/is-pure.js": function(exports, module) {
      module.exports = false;
    }
  });

  // node_modules/core-js/internals/define-global-property.js
  var require_define_global_property = __commonJS({
    "node_modules/core-js/internals/define-global-property.js": function(exports, module) {
      var global9 = require_global();
      var defineProperty3 = Object.defineProperty;
      module.exports = function(key, value) {
        try {
          defineProperty3(global9, key, { value: value, configurable: true, writable: true });
        } catch (error) {
          global9[key] = value;
        }
        return value;
      };
    }
  });

  // node_modules/core-js/internals/shared-store.js
  var require_shared_store = __commonJS({
    "node_modules/core-js/internals/shared-store.js": function(exports, module) {
      var global9 = require_global();
      var defineGlobalProperty = require_define_global_property();
      var SHARED = "__core-js_shared__";
      var store = global9[SHARED] || defineGlobalProperty(SHARED, {});
      module.exports = store;
    }
  });

  // node_modules/core-js/internals/shared.js
  var require_shared = __commonJS({
    "node_modules/core-js/internals/shared.js": function(exports, module) {
      var IS_PURE2 = require_is_pure();
      var store = require_shared_store();
      (module.exports = function(key, value) {
        return store[key] || (store[key] = value !== void 0 ? value : {});
      })("versions", []).push({
        version: "3.29.0",
        mode: IS_PURE2 ? "pure" : "global",
        copyright: "\xA9 2014-2023 Denis Pushkarev (zloirock.ru)",
        license: "https://github.com/zloirock/core-js/blob/v3.29.0/LICENSE",
        source: "https://github.com/zloirock/core-js"
      });
    }
  });

  // node_modules/core-js/internals/to-object.js
  var require_to_object = __commonJS({
    "node_modules/core-js/internals/to-object.js": function(exports, module) {
      var requireObjectCoercible5 = require_require_object_coercible();
      var $Object = Object;
      module.exports = function(argument) {
        return $Object(requireObjectCoercible5(argument));
      };
    }
  });

  // node_modules/core-js/internals/has-own-property.js
  var require_has_own_property = __commonJS({
    "node_modules/core-js/internals/has-own-property.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var toObject5 = require_to_object();
      var hasOwnProperty2 = uncurryThis11({}.hasOwnProperty);
      module.exports = Object.hasOwn || function hasOwn4(it, key) {
        return hasOwnProperty2(toObject5(it), key);
      };
    }
  });

  // node_modules/core-js/internals/uid.js
  var require_uid = __commonJS({
    "node_modules/core-js/internals/uid.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var id = 0;
      var postfix = Math.random();
      var toString9 = uncurryThis11(1 .toString);
      module.exports = function(key) {
        return "Symbol(" + (key === void 0 ? "" : key) + ")_" + toString9(++id + postfix, 36);
      };
    }
  });

  // node_modules/core-js/internals/well-known-symbol.js
  var require_well_known_symbol = __commonJS({
    "node_modules/core-js/internals/well-known-symbol.js": function(exports, module) {
      var global9 = require_global();
      var shared = require_shared();
      var hasOwn4 = require_has_own_property();
      var uid = require_uid();
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      var USE_SYMBOL_AS_UID = require_use_symbol_as_uid();
      var Symbol2 = global9.Symbol;
      var WellKnownSymbolsStore = shared("wks");
      var createWellKnownSymbol = USE_SYMBOL_AS_UID ? Symbol2["for"] || Symbol2 : Symbol2 && Symbol2.withoutSetter || uid;
      module.exports = function(name) {
        if (!hasOwn4(WellKnownSymbolsStore, name)) {
          WellKnownSymbolsStore[name] = NATIVE_SYMBOL && hasOwn4(Symbol2, name) ? Symbol2[name] : createWellKnownSymbol("Symbol." + name);
        }
        return WellKnownSymbolsStore[name];
      };
    }
  });

  // node_modules/core-js/internals/to-primitive.js
  var require_to_primitive = __commonJS({
    "node_modules/core-js/internals/to-primitive.js": function(exports, module) {
      var call8 = require_function_call();
      var isObject5 = require_is_object();
      var isSymbol2 = require_is_symbol();
      var getMethod5 = require_get_method();
      var ordinaryToPrimitive = require_ordinary_to_primitive();
      var wellKnownSymbol6 = require_well_known_symbol();
      var $TypeError = TypeError;
      var TO_PRIMITIVE2 = wellKnownSymbol6("toPrimitive");
      module.exports = function(input, pref) {
        if (!isObject5(input) || isSymbol2(input))
          return input;
        var exoticToPrim = getMethod5(input, TO_PRIMITIVE2);
        var result;
        if (exoticToPrim) {
          if (pref === void 0)
            pref = "default";
          result = call8(exoticToPrim, input, pref);
          if (!isObject5(result) || isSymbol2(result))
            return result;
          throw $TypeError("Can't convert object to primitive value");
        }
        if (pref === void 0)
          pref = "number";
        return ordinaryToPrimitive(input, pref);
      };
    }
  });

  // node_modules/core-js/internals/to-property-key.js
  var require_to_property_key = __commonJS({
    "node_modules/core-js/internals/to-property-key.js": function(exports, module) {
      var toPrimitive2 = require_to_primitive();
      var isSymbol2 = require_is_symbol();
      module.exports = function(argument) {
        var key = toPrimitive2(argument, "string");
        return isSymbol2(key) ? key : key + "";
      };
    }
  });

  // node_modules/core-js/internals/document-create-element.js
  var require_document_create_element = __commonJS({
    "node_modules/core-js/internals/document-create-element.js": function(exports, module) {
      var global9 = require_global();
      var isObject5 = require_is_object();
      var document2 = global9.document;
      var EXISTS = isObject5(document2) && isObject5(document2.createElement);
      module.exports = function(it) {
        return EXISTS ? document2.createElement(it) : {};
      };
    }
  });

  // node_modules/core-js/internals/ie8-dom-define.js
  var require_ie8_dom_define = __commonJS({
    "node_modules/core-js/internals/ie8-dom-define.js": function(exports, module) {
      var DESCRIPTORS8 = require_descriptors();
      var fails12 = require_fails();
      var createElement = require_document_create_element();
      module.exports = !DESCRIPTORS8 && !fails12(function() {
        return Object.defineProperty(createElement("div"), "a", {
          get: function() {
            return 7;
          }
        }).a != 7;
      });
    }
  });

  // node_modules/core-js/internals/object-get-own-property-descriptor.js
  var require_object_get_own_property_descriptor = __commonJS({
    "node_modules/core-js/internals/object-get-own-property-descriptor.js": function(exports) {
      var DESCRIPTORS8 = require_descriptors();
      var call8 = require_function_call();
      var propertyIsEnumerableModule = require_object_property_is_enumerable();
      var createPropertyDescriptor = require_create_property_descriptor();
      var toIndexedObject5 = require_to_indexed_object();
      var toPropertyKey = require_to_property_key();
      var hasOwn4 = require_has_own_property();
      var IE8_DOM_DEFINE = require_ie8_dom_define();
      var $getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
      exports.f = DESCRIPTORS8 ? $getOwnPropertyDescriptor : function getOwnPropertyDescriptor3(O, P) {
        O = toIndexedObject5(O);
        P = toPropertyKey(P);
        if (IE8_DOM_DEFINE)
          try {
            return $getOwnPropertyDescriptor(O, P);
          } catch (error) {
          }
        if (hasOwn4(O, P))
          return createPropertyDescriptor(!call8(propertyIsEnumerableModule.f, O, P), O[P]);
      };
    }
  });

  // node_modules/core-js/internals/v8-prototype-define-bug.js
  var require_v8_prototype_define_bug = __commonJS({
    "node_modules/core-js/internals/v8-prototype-define-bug.js": function(exports, module) {
      var DESCRIPTORS8 = require_descriptors();
      var fails12 = require_fails();
      module.exports = DESCRIPTORS8 && fails12(function() {
        return Object.defineProperty(function() {
        }, "prototype", {
          value: 42,
          writable: false
        }).prototype != 42;
      });
    }
  });

  // node_modules/core-js/internals/an-object.js
  var require_an_object = __commonJS({
    "node_modules/core-js/internals/an-object.js": function(exports, module) {
      var isObject5 = require_is_object();
      var $String = String;
      var $TypeError = TypeError;
      module.exports = function(argument) {
        if (isObject5(argument))
          return argument;
        throw $TypeError($String(argument) + " is not an object");
      };
    }
  });

  // node_modules/core-js/internals/object-define-property.js
  var require_object_define_property = __commonJS({
    "node_modules/core-js/internals/object-define-property.js": function(exports) {
      var DESCRIPTORS8 = require_descriptors();
      var IE8_DOM_DEFINE = require_ie8_dom_define();
      var V8_PROTOTYPE_DEFINE_BUG = require_v8_prototype_define_bug();
      var anObject10 = require_an_object();
      var toPropertyKey = require_to_property_key();
      var $TypeError = TypeError;
      var $defineProperty = Object.defineProperty;
      var $getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
      var ENUMERABLE = "enumerable";
      var CONFIGURABLE = "configurable";
      var WRITABLE = "writable";
      exports.f = DESCRIPTORS8 ? V8_PROTOTYPE_DEFINE_BUG ? function defineProperty3(O, P, Attributes) {
        anObject10(O);
        P = toPropertyKey(P);
        anObject10(Attributes);
        if (typeof O === "function" && P === "prototype" && "value" in Attributes && WRITABLE in Attributes && !Attributes[WRITABLE]) {
          var current = $getOwnPropertyDescriptor(O, P);
          if (current && current[WRITABLE]) {
            O[P] = Attributes.value;
            Attributes = {
              configurable: CONFIGURABLE in Attributes ? Attributes[CONFIGURABLE] : current[CONFIGURABLE],
              enumerable: ENUMERABLE in Attributes ? Attributes[ENUMERABLE] : current[ENUMERABLE],
              writable: false
            };
          }
        }
        return $defineProperty(O, P, Attributes);
      } : $defineProperty : function defineProperty3(O, P, Attributes) {
        anObject10(O);
        P = toPropertyKey(P);
        anObject10(Attributes);
        if (IE8_DOM_DEFINE)
          try {
            return $defineProperty(O, P, Attributes);
          } catch (error) {
          }
        if ("get" in Attributes || "set" in Attributes)
          throw $TypeError("Accessors not supported");
        if ("value" in Attributes)
          O[P] = Attributes.value;
        return O;
      };
    }
  });

  // node_modules/core-js/internals/create-non-enumerable-property.js
  var require_create_non_enumerable_property = __commonJS({
    "node_modules/core-js/internals/create-non-enumerable-property.js": function(exports, module) {
      var DESCRIPTORS8 = require_descriptors();
      var definePropertyModule = require_object_define_property();
      var createPropertyDescriptor = require_create_property_descriptor();
      module.exports = DESCRIPTORS8 ? function(object, key, value) {
        return definePropertyModule.f(object, key, createPropertyDescriptor(1, value));
      } : function(object, key, value) {
        object[key] = value;
        return object;
      };
    }
  });

  // node_modules/core-js/internals/function-name.js
  var require_function_name = __commonJS({
    "node_modules/core-js/internals/function-name.js": function(exports, module) {
      var DESCRIPTORS8 = require_descriptors();
      var hasOwn4 = require_has_own_property();
      var FunctionPrototype2 = Function.prototype;
      var getDescriptor = DESCRIPTORS8 && Object.getOwnPropertyDescriptor;
      var EXISTS = hasOwn4(FunctionPrototype2, "name");
      var PROPER = EXISTS && function something() {
      }.name === "something";
      var CONFIGURABLE = EXISTS && (!DESCRIPTORS8 || DESCRIPTORS8 && getDescriptor(FunctionPrototype2, "name").configurable);
      module.exports = {
        EXISTS: EXISTS,
        PROPER: PROPER,
        CONFIGURABLE: CONFIGURABLE
      };
    }
  });

  // node_modules/core-js/internals/inspect-source.js
  var require_inspect_source = __commonJS({
    "node_modules/core-js/internals/inspect-source.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var isCallable4 = require_is_callable();
      var store = require_shared_store();
      var functionToString2 = uncurryThis11(Function.toString);
      if (!isCallable4(store.inspectSource)) {
        store.inspectSource = function(it) {
          return functionToString2(it);
        };
      }
      module.exports = store.inspectSource;
    }
  });

  // node_modules/core-js/internals/weak-map-basic-detection.js
  var require_weak_map_basic_detection = __commonJS({
    "node_modules/core-js/internals/weak-map-basic-detection.js": function(exports, module) {
      var global9 = require_global();
      var isCallable4 = require_is_callable();
      var WeakMap = global9.WeakMap;
      module.exports = isCallable4(WeakMap) && /native code/.test(String(WeakMap));
    }
  });

  // node_modules/core-js/internals/shared-key.js
  var require_shared_key = __commonJS({
    "node_modules/core-js/internals/shared-key.js": function(exports, module) {
      var shared = require_shared();
      var uid = require_uid();
      var keys2 = shared("keys");
      module.exports = function(key) {
        return keys2[key] || (keys2[key] = uid(key));
      };
    }
  });

  // node_modules/core-js/internals/hidden-keys.js
  var require_hidden_keys = __commonJS({
    "node_modules/core-js/internals/hidden-keys.js": function(exports, module) {
      module.exports = {};
    }
  });

  // node_modules/core-js/internals/internal-state.js
  var require_internal_state = __commonJS({
    "node_modules/core-js/internals/internal-state.js": function(exports, module) {
      var NATIVE_WEAK_MAP = require_weak_map_basic_detection();
      var global9 = require_global();
      var isObject5 = require_is_object();
      var createNonEnumerableProperty3 = require_create_non_enumerable_property();
      var hasOwn4 = require_has_own_property();
      var shared = require_shared_store();
      var sharedKey = require_shared_key();
      var hiddenKeys = require_hidden_keys();
      var OBJECT_ALREADY_INITIALIZED = "Object already initialized";
      var TypeError3 = global9.TypeError;
      var WeakMap = global9.WeakMap;
      var set;
      var get3;
      var has;
      var enforce = function(it) {
        return has(it) ? get3(it) : set(it, {});
      };
      var getterFor = function(TYPE) {
        return function(it) {
          var state;
          if (!isObject5(it) || (state = get3(it)).type !== TYPE) {
            throw TypeError3("Incompatible receiver, " + TYPE + " required");
          }
          return state;
        };
      };
      if (NATIVE_WEAK_MAP || shared.state) {
        store = shared.state || (shared.state = new WeakMap());
        store.get = store.get;
        store.has = store.has;
        store.set = store.set;
        set = function(it, metadata) {
          if (store.has(it))
            throw TypeError3(OBJECT_ALREADY_INITIALIZED);
          metadata.facade = it;
          store.set(it, metadata);
          return metadata;
        };
        get3 = function(it) {
          return store.get(it) || {};
        };
        has = function(it) {
          return store.has(it);
        };
      } else {
        STATE = sharedKey("state");
        hiddenKeys[STATE] = true;
        set = function(it, metadata) {
          if (hasOwn4(it, STATE))
            throw TypeError3(OBJECT_ALREADY_INITIALIZED);
          metadata.facade = it;
          createNonEnumerableProperty3(it, STATE, metadata);
          return metadata;
        };
        get3 = function(it) {
          return hasOwn4(it, STATE) ? it[STATE] : {};
        };
        has = function(it) {
          return hasOwn4(it, STATE);
        };
      }
      var store;
      var STATE;
      module.exports = {
        set: set,
        get: get3,
        has: has,
        enforce: enforce,
        getterFor: getterFor
      };
    }
  });

  // node_modules/core-js/internals/make-built-in.js
  var require_make_built_in = __commonJS({
    "node_modules/core-js/internals/make-built-in.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var fails12 = require_fails();
      var isCallable4 = require_is_callable();
      var hasOwn4 = require_has_own_property();
      var DESCRIPTORS8 = require_descriptors();
      var CONFIGURABLE_FUNCTION_NAME = require_function_name().CONFIGURABLE;
      var inspectSource = require_inspect_source();
      var InternalStateModule2 = require_internal_state();
      var enforceInternalState = InternalStateModule2.enforce;
      var getInternalState2 = InternalStateModule2.get;
      var $String = String;
      var defineProperty3 = Object.defineProperty;
      var stringSlice4 = uncurryThis11("".slice);
      var replace = uncurryThis11("".replace);
      var join2 = uncurryThis11([].join);
      var CONFIGURABLE_LENGTH = DESCRIPTORS8 && !fails12(function() {
        return defineProperty3(function() {
        }, "length", { value: 8 }).length !== 8;
      });
      var TEMPLATE = String(String).split("String");
      var makeBuiltIn = module.exports = function(value, name, options) {
        if (stringSlice4($String(name), 0, 7) === "Symbol(") {
          name = "[" + replace($String(name), /^Symbol\(([^)]*)\)/, "$1") + "]";
        }
        if (options && options.getter)
          name = "get " + name;
        if (options && options.setter)
          name = "set " + name;
        if (!hasOwn4(value, "name") || CONFIGURABLE_FUNCTION_NAME && value.name !== name) {
          if (DESCRIPTORS8)
            defineProperty3(value, "name", { value: name, configurable: true });
          else
            value.name = name;
        }
        if (CONFIGURABLE_LENGTH && options && hasOwn4(options, "arity") && value.length !== options.arity) {
          defineProperty3(value, "length", { value: options.arity });
        }
        try {
          if (options && hasOwn4(options, "constructor") && options.constructor) {
            if (DESCRIPTORS8)
              defineProperty3(value, "prototype", { writable: false });
          } else if (value.prototype)
            value.prototype = void 0;
        } catch (error) {
        }
        var state = enforceInternalState(value);
        if (!hasOwn4(state, "source")) {
          state.source = join2(TEMPLATE, typeof name == "string" ? name : "");
        }
        return value;
      };
      Function.prototype.toString = makeBuiltIn(function toString9() {
        return isCallable4(this) && getInternalState2(this).source || inspectSource(this);
      }, "toString");
    }
  });

  // node_modules/core-js/internals/define-built-in.js
  var require_define_built_in = __commonJS({
    "node_modules/core-js/internals/define-built-in.js": function(exports, module) {
      var isCallable4 = require_is_callable();
      var definePropertyModule = require_object_define_property();
      var makeBuiltIn = require_make_built_in();
      var defineGlobalProperty = require_define_global_property();
      module.exports = function(O, key, value, options) {
        if (!options)
          options = {};
        var simple = options.enumerable;
        var name = options.name !== void 0 ? options.name : key;
        if (isCallable4(value))
          makeBuiltIn(value, name, options);
        if (options.global) {
          if (simple)
            O[key] = value;
          else
            defineGlobalProperty(key, value);
        } else {
          try {
            if (!options.unsafe)
              delete O[key];
            else if (O[key])
              simple = true;
          } catch (error) {
          }
          if (simple)
            O[key] = value;
          else
            definePropertyModule.f(O, key, {
              value: value,
              enumerable: false,
              configurable: !options.nonConfigurable,
              writable: !options.nonWritable
            });
        }
        return O;
      };
    }
  });

  // node_modules/core-js/internals/math-trunc.js
  var require_math_trunc = __commonJS({
    "node_modules/core-js/internals/math-trunc.js": function(exports, module) {
      var ceil = Math.ceil;
      var floor = Math.floor;
      module.exports = Math.trunc || function trunc(x) {
        var n = +x;
        return (n > 0 ? floor : ceil)(n);
      };
    }
  });

  // node_modules/core-js/internals/to-integer-or-infinity.js
  var require_to_integer_or_infinity = __commonJS({
    "node_modules/core-js/internals/to-integer-or-infinity.js": function(exports, module) {
      var trunc = require_math_trunc();
      module.exports = function(argument) {
        var number = +argument;
        return number !== number || number === 0 ? 0 : trunc(number);
      };
    }
  });

  // node_modules/core-js/internals/to-absolute-index.js
  var require_to_absolute_index = __commonJS({
    "node_modules/core-js/internals/to-absolute-index.js": function(exports, module) {
      var toIntegerOrInfinity3 = require_to_integer_or_infinity();
      var max4 = Math.max;
      var min4 = Math.min;
      module.exports = function(index, length) {
        var integer = toIntegerOrInfinity3(index);
        return integer < 0 ? max4(integer + length, 0) : min4(integer, length);
      };
    }
  });

  // node_modules/core-js/internals/to-length.js
  var require_to_length = __commonJS({
    "node_modules/core-js/internals/to-length.js": function(exports, module) {
      var toIntegerOrInfinity3 = require_to_integer_or_infinity();
      var min4 = Math.min;
      module.exports = function(argument) {
        return argument > 0 ? min4(toIntegerOrInfinity3(argument), 9007199254740991) : 0;
      };
    }
  });

  // node_modules/core-js/internals/length-of-array-like.js
  var require_length_of_array_like = __commonJS({
    "node_modules/core-js/internals/length-of-array-like.js": function(exports, module) {
      var toLength5 = require_to_length();
      module.exports = function(obj) {
        return toLength5(obj.length);
      };
    }
  });

  // node_modules/core-js/internals/array-includes.js
  var require_array_includes = __commonJS({
    "node_modules/core-js/internals/array-includes.js": function(exports, module) {
      var toIndexedObject5 = require_to_indexed_object();
      var toAbsoluteIndex4 = require_to_absolute_index();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var createMethod = function(IS_INCLUDES) {
        return function($this, el, fromIndex) {
          var O = toIndexedObject5($this);
          var length = lengthOfArrayLike4(O);
          var index = toAbsoluteIndex4(fromIndex, length);
          var value;
          if (IS_INCLUDES && el != el)
            while (length > index) {
              value = O[index++];
              if (value != value)
                return true;
            }
          else
            for (; length > index; index++) {
              if ((IS_INCLUDES || index in O) && O[index] === el)
                return IS_INCLUDES || index || 0;
            }
          return !IS_INCLUDES && -1;
        };
      };
      module.exports = {
        includes: createMethod(true),
        indexOf: createMethod(false)
      };
    }
  });

  // node_modules/core-js/internals/object-keys-internal.js
  var require_object_keys_internal = __commonJS({
    "node_modules/core-js/internals/object-keys-internal.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var hasOwn4 = require_has_own_property();
      var toIndexedObject5 = require_to_indexed_object();
      var indexOf2 = require_array_includes().indexOf;
      var hiddenKeys = require_hidden_keys();
      var push4 = uncurryThis11([].push);
      module.exports = function(object, names) {
        var O = toIndexedObject5(object);
        var i = 0;
        var result = [];
        var key;
        for (key in O)
          !hasOwn4(hiddenKeys, key) && hasOwn4(O, key) && push4(result, key);
        while (names.length > i)
          if (hasOwn4(O, key = names[i++])) {
            ~indexOf2(result, key) || push4(result, key);
          }
        return result;
      };
    }
  });

  // node_modules/core-js/internals/enum-bug-keys.js
  var require_enum_bug_keys = __commonJS({
    "node_modules/core-js/internals/enum-bug-keys.js": function(exports, module) {
      module.exports = [
        "constructor",
        "hasOwnProperty",
        "isPrototypeOf",
        "propertyIsEnumerable",
        "toLocaleString",
        "toString",
        "valueOf"
      ];
    }
  });

  // node_modules/core-js/internals/object-get-own-property-names.js
  var require_object_get_own_property_names = __commonJS({
    "node_modules/core-js/internals/object-get-own-property-names.js": function(exports) {
      var internalObjectKeys = require_object_keys_internal();
      var enumBugKeys = require_enum_bug_keys();
      var hiddenKeys = enumBugKeys.concat("length", "prototype");
      exports.f = Object.getOwnPropertyNames || function getOwnPropertyNames2(O) {
        return internalObjectKeys(O, hiddenKeys);
      };
    }
  });

  // node_modules/core-js/internals/object-get-own-property-symbols.js
  var require_object_get_own_property_symbols = __commonJS({
    "node_modules/core-js/internals/object-get-own-property-symbols.js": function(exports) {
      exports.f = Object.getOwnPropertySymbols;
    }
  });

  // node_modules/core-js/internals/own-keys.js
  var require_own_keys = __commonJS({
    "node_modules/core-js/internals/own-keys.js": function(exports, module) {
      var getBuiltIn3 = require_get_built_in();
      var uncurryThis11 = require_function_uncurry_this();
      var getOwnPropertyNamesModule = require_object_get_own_property_names();
      var getOwnPropertySymbolsModule = require_object_get_own_property_symbols();
      var anObject10 = require_an_object();
      var concat3 = uncurryThis11([].concat);
      module.exports = getBuiltIn3("Reflect", "ownKeys") || function ownKeys4(it) {
        var keys2 = getOwnPropertyNamesModule.f(anObject10(it));
        var getOwnPropertySymbols = getOwnPropertySymbolsModule.f;
        return getOwnPropertySymbols ? concat3(keys2, getOwnPropertySymbols(it)) : keys2;
      };
    }
  });

  // node_modules/core-js/internals/copy-constructor-properties.js
  var require_copy_constructor_properties = __commonJS({
    "node_modules/core-js/internals/copy-constructor-properties.js": function(exports, module) {
      var hasOwn4 = require_has_own_property();
      var ownKeys4 = require_own_keys();
      var getOwnPropertyDescriptorModule3 = require_object_get_own_property_descriptor();
      var definePropertyModule = require_object_define_property();
      module.exports = function(target, source, exceptions) {
        var keys2 = ownKeys4(source);
        var defineProperty3 = definePropertyModule.f;
        var getOwnPropertyDescriptor3 = getOwnPropertyDescriptorModule3.f;
        for (var i = 0; i < keys2.length; i++) {
          var key = keys2[i];
          if (!hasOwn4(target, key) && !(exceptions && hasOwn4(exceptions, key))) {
            defineProperty3(target, key, getOwnPropertyDescriptor3(source, key));
          }
        }
      };
    }
  });

  // node_modules/core-js/internals/is-forced.js
  var require_is_forced = __commonJS({
    "node_modules/core-js/internals/is-forced.js": function(exports, module) {
      var fails12 = require_fails();
      var isCallable4 = require_is_callable();
      var replacement = /#|\.prototype\./;
      var isForced2 = function(feature, detection) {
        var value = data[normalize(feature)];
        return value == POLYFILL ? true : value == NATIVE ? false : isCallable4(detection) ? fails12(detection) : !!detection;
      };
      var normalize = isForced2.normalize = function(string) {
        return String(string).replace(replacement, ".").toLowerCase();
      };
      var data = isForced2.data = {};
      var NATIVE = isForced2.NATIVE = "N";
      var POLYFILL = isForced2.POLYFILL = "P";
      module.exports = isForced2;
    }
  });

  // node_modules/core-js/internals/export.js
  var require_export = __commonJS({
    "node_modules/core-js/internals/export.js": function(exports, module) {
      var global9 = require_global();
      var getOwnPropertyDescriptor3 = require_object_get_own_property_descriptor().f;
      var createNonEnumerableProperty3 = require_create_non_enumerable_property();
      var defineBuiltIn4 = require_define_built_in();
      var defineGlobalProperty = require_define_global_property();
      var copyConstructorProperties3 = require_copy_constructor_properties();
      var isForced2 = require_is_forced();
      module.exports = function(options, source) {
        var TARGET = options.target;
        var GLOBAL = options.global;
        var STATIC = options.stat;
        var FORCED9, target, key, targetProperty, sourceProperty, descriptor;
        if (GLOBAL) {
          target = global9;
        } else if (STATIC) {
          target = global9[TARGET] || defineGlobalProperty(TARGET, {});
        } else {
          target = (global9[TARGET] || {}).prototype;
        }
        if (target)
          for (key in source) {
            sourceProperty = source[key];
            if (options.dontCallGetSet) {
              descriptor = getOwnPropertyDescriptor3(target, key);
              targetProperty = descriptor && descriptor.value;
            } else
              targetProperty = target[key];
            FORCED9 = isForced2(GLOBAL ? key : TARGET + (STATIC ? "." : "#") + key, options.forced);
            if (!FORCED9 && targetProperty !== void 0) {
              if (typeof sourceProperty == typeof targetProperty)
                continue;
              copyConstructorProperties3(sourceProperty, targetProperty);
            }
            if (options.sham || targetProperty && targetProperty.sham) {
              createNonEnumerableProperty3(sourceProperty, "sham", true);
            }
            defineBuiltIn4(target, key, sourceProperty, options);
          }
      };
    }
  });

  // node_modules/core-js/internals/function-uncurry-this-clause.js
  var require_function_uncurry_this_clause = __commonJS({
    "node_modules/core-js/internals/function-uncurry-this-clause.js": function(exports, module) {
      var classofRaw = require_classof_raw();
      var uncurryThis11 = require_function_uncurry_this();
      module.exports = function(fn) {
        if (classofRaw(fn) === "Function")
          return uncurryThis11(fn);
      };
    }
  });

  // node_modules/core-js/internals/array-method-is-strict.js
  var require_array_method_is_strict = __commonJS({
    "node_modules/core-js/internals/array-method-is-strict.js": function(exports, module) {
      "use strict";
      var fails12 = require_fails();
      module.exports = function(METHOD_NAME, argument) {
        var method = [][METHOD_NAME];
        return !!method && fails12(function() {
          method.call(null, argument || function() {
            return 1;
          }, 1);
        });
      };
    }
  });

  // node_modules/core-js/internals/to-string-tag-support.js
  var require_to_string_tag_support = __commonJS({
    "node_modules/core-js/internals/to-string-tag-support.js": function(exports, module) {
      var wellKnownSymbol6 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol6("toStringTag");
      var test2 = {};
      test2[TO_STRING_TAG2] = "z";
      module.exports = String(test2) === "[object z]";
    }
  });

  // node_modules/core-js/internals/classof.js
  var require_classof = __commonJS({
    "node_modules/core-js/internals/classof.js": function(exports, module) {
      var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
      var isCallable4 = require_is_callable();
      var classofRaw = require_classof_raw();
      var wellKnownSymbol6 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol6("toStringTag");
      var $Object = Object;
      var CORRECT_ARGUMENTS = classofRaw(function() {
        return arguments;
      }()) == "Arguments";
      var tryGet = function(it, key) {
        try {
          return it[key];
        } catch (error) {
        }
      };
      module.exports = TO_STRING_TAG_SUPPORT2 ? classofRaw : function(it) {
        var O, tag, result;
        return it === void 0 ? "Undefined" : it === null ? "Null" : typeof (tag = tryGet(O = $Object(it), TO_STRING_TAG2)) == "string" ? tag : CORRECT_ARGUMENTS ? classofRaw(O) : (result = classofRaw(O)) == "Object" && isCallable4(O.callee) ? "Arguments" : result;
      };
    }
  });

  // node_modules/core-js/internals/to-string.js
  var require_to_string = __commonJS({
    "node_modules/core-js/internals/to-string.js": function(exports, module) {
      var classof = require_classof();
      var $String = String;
      module.exports = function(argument) {
        if (classof(argument) === "Symbol")
          throw TypeError("Cannot convert a Symbol value to a string");
        return $String(argument);
      };
    }
  });

  // node_modules/core-js/internals/whitespaces.js
  var require_whitespaces = __commonJS({
    "node_modules/core-js/internals/whitespaces.js": function(exports, module) {
      module.exports = "	\n\v\f\r \xA0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\u2028\u2029\uFEFF";
    }
  });

  // node_modules/core-js/internals/string-trim.js
  var require_string_trim = __commonJS({
    "node_modules/core-js/internals/string-trim.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var requireObjectCoercible5 = require_require_object_coercible();
      var toString9 = require_to_string();
      var whitespaces = require_whitespaces();
      var replace = uncurryThis11("".replace);
      var ltrim = RegExp("^[" + whitespaces + "]+");
      var rtrim = RegExp("(^|[^" + whitespaces + "])[" + whitespaces + "]+$");
      var createMethod = function(TYPE) {
        return function($this) {
          var string = toString9(requireObjectCoercible5($this));
          if (TYPE & 1)
            string = replace(string, ltrim, "");
          if (TYPE & 2)
            string = replace(string, rtrim, "$1");
          return string;
        };
      };
      module.exports = {
        start: createMethod(1),
        end: createMethod(2),
        trim: createMethod(3)
      };
    }
  });

  // node_modules/core-js/internals/number-parse-int.js
  var require_number_parse_int = __commonJS({
    "node_modules/core-js/internals/number-parse-int.js": function(exports, module) {
      var global9 = require_global();
      var fails12 = require_fails();
      var uncurryThis11 = require_function_uncurry_this();
      var toString9 = require_to_string();
      var trim3 = require_string_trim().trim;
      var whitespaces = require_whitespaces();
      var $parseInt2 = global9.parseInt;
      var Symbol2 = global9.Symbol;
      var ITERATOR2 = Symbol2 && Symbol2.iterator;
      var hex = /^[+-]?0x/i;
      var exec2 = uncurryThis11(hex.exec);
      var FORCED9 = $parseInt2(whitespaces + "08") !== 8 || $parseInt2(whitespaces + "0x16") !== 22 || ITERATOR2 && !fails12(function() {
        $parseInt2(Object(ITERATOR2));
      });
      module.exports = FORCED9 ? function parseInt2(string, radix) {
        var S = trim3(toString9(string));
        return $parseInt2(S, radix >>> 0 || (exec2(hex, S) ? 16 : 10));
      } : $parseInt2;
    }
  });

  // node_modules/core-js/internals/regexp-flags.js
  var require_regexp_flags = __commonJS({
    "node_modules/core-js/internals/regexp-flags.js": function(exports, module) {
      "use strict";
      var anObject10 = require_an_object();
      module.exports = function() {
        var that = anObject10(this);
        var result = "";
        if (that.hasIndices)
          result += "d";
        if (that.global)
          result += "g";
        if (that.ignoreCase)
          result += "i";
        if (that.multiline)
          result += "m";
        if (that.dotAll)
          result += "s";
        if (that.unicode)
          result += "u";
        if (that.unicodeSets)
          result += "v";
        if (that.sticky)
          result += "y";
        return result;
      };
    }
  });

  // node_modules/core-js/internals/regexp-sticky-helpers.js
  var require_regexp_sticky_helpers = __commonJS({
    "node_modules/core-js/internals/regexp-sticky-helpers.js": function(exports, module) {
      var fails12 = require_fails();
      var global9 = require_global();
      var $RegExp = global9.RegExp;
      var UNSUPPORTED_Y2 = fails12(function() {
        var re = $RegExp("a", "y");
        re.lastIndex = 2;
        return re.exec("abcd") != null;
      });
      var MISSED_STICKY = UNSUPPORTED_Y2 || fails12(function() {
        return !$RegExp("a", "y").sticky;
      });
      var BROKEN_CARET = UNSUPPORTED_Y2 || fails12(function() {
        var re = $RegExp("^r", "gy");
        re.lastIndex = 2;
        return re.exec("str") != null;
      });
      module.exports = {
        BROKEN_CARET: BROKEN_CARET,
        MISSED_STICKY: MISSED_STICKY,
        UNSUPPORTED_Y: UNSUPPORTED_Y2
      };
    }
  });

  // node_modules/core-js/internals/object-keys.js
  var require_object_keys = __commonJS({
    "node_modules/core-js/internals/object-keys.js": function(exports, module) {
      var internalObjectKeys = require_object_keys_internal();
      var enumBugKeys = require_enum_bug_keys();
      module.exports = Object.keys || function keys2(O) {
        return internalObjectKeys(O, enumBugKeys);
      };
    }
  });

  // node_modules/core-js/internals/object-define-properties.js
  var require_object_define_properties = __commonJS({
    "node_modules/core-js/internals/object-define-properties.js": function(exports) {
      var DESCRIPTORS8 = require_descriptors();
      var V8_PROTOTYPE_DEFINE_BUG = require_v8_prototype_define_bug();
      var definePropertyModule = require_object_define_property();
      var anObject10 = require_an_object();
      var toIndexedObject5 = require_to_indexed_object();
      var objectKeys = require_object_keys();
      exports.f = DESCRIPTORS8 && !V8_PROTOTYPE_DEFINE_BUG ? Object.defineProperties : function defineProperties2(O, Properties) {
        anObject10(O);
        var props = toIndexedObject5(Properties);
        var keys2 = objectKeys(Properties);
        var length = keys2.length;
        var index = 0;
        var key;
        while (length > index)
          definePropertyModule.f(O, key = keys2[index++], props[key]);
        return O;
      };
    }
  });

  // node_modules/core-js/internals/html.js
  var require_html = __commonJS({
    "node_modules/core-js/internals/html.js": function(exports, module) {
      var getBuiltIn3 = require_get_built_in();
      module.exports = getBuiltIn3("document", "documentElement");
    }
  });

  // node_modules/core-js/internals/object-create.js
  var require_object_create = __commonJS({
    "node_modules/core-js/internals/object-create.js": function(exports, module) {
      var anObject10 = require_an_object();
      var definePropertiesModule = require_object_define_properties();
      var enumBugKeys = require_enum_bug_keys();
      var hiddenKeys = require_hidden_keys();
      var html = require_html();
      var documentCreateElement = require_document_create_element();
      var sharedKey = require_shared_key();
      var GT = ">";
      var LT = "<";
      var PROTOTYPE = "prototype";
      var SCRIPT = "script";
      var IE_PROTO = sharedKey("IE_PROTO");
      var EmptyConstructor = function() {
      };
      var scriptTag = function(content) {
        return LT + SCRIPT + GT + content + LT + "/" + SCRIPT + GT;
      };
      var NullProtoObjectViaActiveX = function(activeXDocument2) {
        activeXDocument2.write(scriptTag(""));
        activeXDocument2.close();
        var temp = activeXDocument2.parentWindow.Object;
        activeXDocument2 = null;
        return temp;
      };
      var NullProtoObjectViaIFrame = function() {
        var iframe = documentCreateElement("iframe");
        var JS = "java" + SCRIPT + ":";
        var iframeDocument;
        iframe.style.display = "none";
        html.appendChild(iframe);
        iframe.src = String(JS);
        iframeDocument = iframe.contentWindow.document;
        iframeDocument.open();
        iframeDocument.write(scriptTag("document.F=Object"));
        iframeDocument.close();
        return iframeDocument.F;
      };
      var activeXDocument;
      var NullProtoObject = function() {
        try {
          activeXDocument = new ActiveXObject("htmlfile");
        } catch (error) {
        }
        NullProtoObject = typeof document != "undefined" ? document.domain && activeXDocument ? NullProtoObjectViaActiveX(activeXDocument) : NullProtoObjectViaIFrame() : NullProtoObjectViaActiveX(activeXDocument);
        var length = enumBugKeys.length;
        while (length--)
          delete NullProtoObject[PROTOTYPE][enumBugKeys[length]];
        return NullProtoObject();
      };
      hiddenKeys[IE_PROTO] = true;
      module.exports = Object.create || function create3(O, Properties) {
        var result;
        if (O !== null) {
          EmptyConstructor[PROTOTYPE] = anObject10(O);
          result = new EmptyConstructor();
          EmptyConstructor[PROTOTYPE] = null;
          result[IE_PROTO] = O;
        } else
          result = NullProtoObject();
        return Properties === void 0 ? result : definePropertiesModule.f(result, Properties);
      };
    }
  });

  // node_modules/core-js/internals/regexp-unsupported-dot-all.js
  var require_regexp_unsupported_dot_all = __commonJS({
    "node_modules/core-js/internals/regexp-unsupported-dot-all.js": function(exports, module) {
      var fails12 = require_fails();
      var global9 = require_global();
      var $RegExp = global9.RegExp;
      module.exports = fails12(function() {
        var re = $RegExp(".", "s");
        return !(re.dotAll && re.exec("\n") && re.flags === "s");
      });
    }
  });

  // node_modules/core-js/internals/regexp-unsupported-ncg.js
  var require_regexp_unsupported_ncg = __commonJS({
    "node_modules/core-js/internals/regexp-unsupported-ncg.js": function(exports, module) {
      var fails12 = require_fails();
      var global9 = require_global();
      var $RegExp = global9.RegExp;
      module.exports = fails12(function() {
        var re = $RegExp("(?<a>b)", "g");
        return re.exec("b").groups.a !== "b" || "b".replace(re, "$<a>c") !== "bc";
      });
    }
  });

  // node_modules/core-js/internals/regexp-exec.js
  var require_regexp_exec = __commonJS({
    "node_modules/core-js/internals/regexp-exec.js": function(exports, module) {
      "use strict";
      var call8 = require_function_call();
      var uncurryThis11 = require_function_uncurry_this();
      var toString9 = require_to_string();
      var regexpFlags = require_regexp_flags();
      var stickyHelpers2 = require_regexp_sticky_helpers();
      var shared = require_shared();
      var create3 = require_object_create();
      var getInternalState2 = require_internal_state().get;
      var UNSUPPORTED_DOT_ALL = require_regexp_unsupported_dot_all();
      var UNSUPPORTED_NCG = require_regexp_unsupported_ncg();
      var nativeReplace = shared("native-string-replace", String.prototype.replace);
      var nativeExec = RegExp.prototype.exec;
      var patchedExec = nativeExec;
      var charAt2 = uncurryThis11("".charAt);
      var indexOf2 = uncurryThis11("".indexOf);
      var replace = uncurryThis11("".replace);
      var stringSlice4 = uncurryThis11("".slice);
      var UPDATES_LAST_INDEX_WRONG = function() {
        var re1 = /a/;
        var re2 = /b*/g;
        call8(nativeExec, re1, "a");
        call8(nativeExec, re2, "a");
        return re1.lastIndex !== 0 || re2.lastIndex !== 0;
      }();
      var UNSUPPORTED_Y2 = stickyHelpers2.BROKEN_CARET;
      var NPCG_INCLUDED = /()??/.exec("")[1] !== void 0;
      var PATCH = UPDATES_LAST_INDEX_WRONG || NPCG_INCLUDED || UNSUPPORTED_Y2 || UNSUPPORTED_DOT_ALL || UNSUPPORTED_NCG;
      if (PATCH) {
        patchedExec = function exec2(string) {
          var re = this;
          var state = getInternalState2(re);
          var str = toString9(string);
          var raw = state.raw;
          var result, reCopy, lastIndex, match, i, object, group;
          if (raw) {
            raw.lastIndex = re.lastIndex;
            result = call8(patchedExec, raw, str);
            re.lastIndex = raw.lastIndex;
            return result;
          }
          var groups = state.groups;
          var sticky = UNSUPPORTED_Y2 && re.sticky;
          var flags = call8(regexpFlags, re);
          var source = re.source;
          var charsAdded = 0;
          var strCopy = str;
          if (sticky) {
            flags = replace(flags, "y", "");
            if (indexOf2(flags, "g") === -1) {
              flags += "g";
            }
            strCopy = stringSlice4(str, re.lastIndex);
            if (re.lastIndex > 0 && (!re.multiline || re.multiline && charAt2(str, re.lastIndex - 1) !== "\n")) {
              source = "(?: " + source + ")";
              strCopy = " " + strCopy;
              charsAdded++;
            }
            reCopy = new RegExp("^(?:" + source + ")", flags);
          }
          if (NPCG_INCLUDED) {
            reCopy = new RegExp("^" + source + "$(?!\\s)", flags);
          }
          if (UPDATES_LAST_INDEX_WRONG)
            lastIndex = re.lastIndex;
          match = call8(nativeExec, sticky ? reCopy : re, strCopy);
          if (sticky) {
            if (match) {
              match.input = stringSlice4(match.input, charsAdded);
              match[0] = stringSlice4(match[0], charsAdded);
              match.index = re.lastIndex;
              re.lastIndex += match[0].length;
            } else
              re.lastIndex = 0;
          } else if (UPDATES_LAST_INDEX_WRONG && match) {
            re.lastIndex = re.global ? match.index + match[0].length : lastIndex;
          }
          if (NPCG_INCLUDED && match && match.length > 1) {
            call8(nativeReplace, match[0], reCopy, function() {
              for (i = 1; i < arguments.length - 2; i++) {
                if (arguments[i] === void 0)
                  match[i] = void 0;
              }
            });
          }
          if (match && groups) {
            match.groups = object = create3(null);
            for (i = 0; i < groups.length; i++) {
              group = groups[i];
              object[group[0]] = match[group[1]];
            }
          }
          return match;
        };
      }
      module.exports = patchedExec;
    }
  });

  // node_modules/core-js/modules/es.regexp.exec.js
  var require_es_regexp_exec = __commonJS({
    "node_modules/core-js/modules/es.regexp.exec.js": function() {
      "use strict";
      var $77 = require_export();
      var exec2 = require_regexp_exec();
      $77({ target: "RegExp", proto: true, forced: /./.exec !== exec2 }, {
        exec: exec2
      });
    }
  });

  // node_modules/core-js/internals/define-built-in-accessor.js
  var require_define_built_in_accessor = __commonJS({
    "node_modules/core-js/internals/define-built-in-accessor.js": function(exports, module) {
      var makeBuiltIn = require_make_built_in();
      var defineProperty3 = require_object_define_property();
      module.exports = function(target, name, descriptor) {
        if (descriptor.get)
          makeBuiltIn(descriptor.get, name, { getter: true });
        if (descriptor.set)
          makeBuiltIn(descriptor.set, name, { setter: true });
        return defineProperty3.f(target, name, descriptor);
      };
    }
  });

  // node_modules/core-js/internals/path.js
  var require_path = __commonJS({
    "node_modules/core-js/internals/path.js": function(exports, module) {
      var global9 = require_global();
      module.exports = global9;
    }
  });

  // node_modules/core-js/internals/well-known-symbol-wrapped.js
  var require_well_known_symbol_wrapped = __commonJS({
    "node_modules/core-js/internals/well-known-symbol-wrapped.js": function(exports) {
      var wellKnownSymbol6 = require_well_known_symbol();
      exports.f = wellKnownSymbol6;
    }
  });

  // node_modules/core-js/internals/well-known-symbol-define.js
  var require_well_known_symbol_define = __commonJS({
    "node_modules/core-js/internals/well-known-symbol-define.js": function(exports, module) {
      var path2 = require_path();
      var hasOwn4 = require_has_own_property();
      var wrappedWellKnownSymbolModule = require_well_known_symbol_wrapped();
      var defineProperty3 = require_object_define_property().f;
      module.exports = function(NAME2) {
        var Symbol2 = path2.Symbol || (path2.Symbol = {});
        if (!hasOwn4(Symbol2, NAME2))
          defineProperty3(Symbol2, NAME2, {
            value: wrappedWellKnownSymbolModule.f(NAME2)
          });
      };
    }
  });

  // node_modules/core-js/internals/symbol-define-to-primitive.js
  var require_symbol_define_to_primitive = __commonJS({
    "node_modules/core-js/internals/symbol-define-to-primitive.js": function(exports, module) {
      var call8 = require_function_call();
      var getBuiltIn3 = require_get_built_in();
      var wellKnownSymbol6 = require_well_known_symbol();
      var defineBuiltIn4 = require_define_built_in();
      module.exports = function() {
        var Symbol2 = getBuiltIn3("Symbol");
        var SymbolPrototype2 = Symbol2 && Symbol2.prototype;
        var valueOf = SymbolPrototype2 && SymbolPrototype2.valueOf;
        var TO_PRIMITIVE2 = wellKnownSymbol6("toPrimitive");
        if (SymbolPrototype2 && !SymbolPrototype2[TO_PRIMITIVE2]) {
          defineBuiltIn4(SymbolPrototype2, TO_PRIMITIVE2, function(hint) {
            return call8(valueOf, this);
          }, { arity: 1 });
        }
      };
    }
  });

  // node_modules/core-js/internals/date-to-primitive.js
  var require_date_to_primitive = __commonJS({
    "node_modules/core-js/internals/date-to-primitive.js": function(exports, module) {
      "use strict";
      var anObject10 = require_an_object();
      var ordinaryToPrimitive = require_ordinary_to_primitive();
      var $TypeError = TypeError;
      module.exports = function(hint) {
        anObject10(this);
        if (hint === "string" || hint === "default")
          hint = "string";
        else if (hint !== "number")
          throw $TypeError("Incorrect hint");
        return ordinaryToPrimitive(this, hint);
      };
    }
  });

  // node_modules/core-js/internals/create-property.js
  var require_create_property = __commonJS({
    "node_modules/core-js/internals/create-property.js": function(exports, module) {
      "use strict";
      var toPropertyKey = require_to_property_key();
      var definePropertyModule = require_object_define_property();
      var createPropertyDescriptor = require_create_property_descriptor();
      module.exports = function(object, key, value) {
        var propertyKey = toPropertyKey(key);
        if (propertyKey in object)
          definePropertyModule.f(object, propertyKey, createPropertyDescriptor(0, value));
        else
          object[propertyKey] = value;
      };
    }
  });

  // node_modules/core-js/internals/array-slice-simple.js
  var require_array_slice_simple = __commonJS({
    "node_modules/core-js/internals/array-slice-simple.js": function(exports, module) {
      var toAbsoluteIndex4 = require_to_absolute_index();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var createProperty5 = require_create_property();
      var $Array2 = Array;
      var max4 = Math.max;
      module.exports = function(O, start, end) {
        var length = lengthOfArrayLike4(O);
        var k = toAbsoluteIndex4(start, length);
        var fin = toAbsoluteIndex4(end === void 0 ? length : end, length);
        var result = $Array2(max4(fin - k, 0));
        for (var n = 0; k < fin; k++, n++)
          createProperty5(result, n, O[k]);
        result.length = n;
        return result;
      };
    }
  });

  // node_modules/core-js/internals/object-get-own-property-names-external.js
  var require_object_get_own_property_names_external = __commonJS({
    "node_modules/core-js/internals/object-get-own-property-names-external.js": function(exports, module) {
      var classof = require_classof_raw();
      var toIndexedObject5 = require_to_indexed_object();
      var $getOwnPropertyNames = require_object_get_own_property_names().f;
      var arraySlice2 = require_array_slice_simple();
      var windowNames = typeof window == "object" && window && Object.getOwnPropertyNames ? Object.getOwnPropertyNames(window) : [];
      var getWindowNames = function(it) {
        try {
          return $getOwnPropertyNames(it);
        } catch (error) {
          return arraySlice2(windowNames);
        }
      };
      module.exports.f = function getOwnPropertyNames2(it) {
        return windowNames && classof(it) == "Window" ? getWindowNames(it) : $getOwnPropertyNames(toIndexedObject5(it));
      };
    }
  });

  // node_modules/core-js/internals/set-to-string-tag.js
  var require_set_to_string_tag = __commonJS({
    "node_modules/core-js/internals/set-to-string-tag.js": function(exports, module) {
      var defineProperty3 = require_object_define_property().f;
      var hasOwn4 = require_has_own_property();
      var wellKnownSymbol6 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol6("toStringTag");
      module.exports = function(target, TAG, STATIC) {
        if (target && !STATIC)
          target = target.prototype;
        if (target && !hasOwn4(target, TO_STRING_TAG2)) {
          defineProperty3(target, TO_STRING_TAG2, { configurable: true, value: TAG });
        }
      };
    }
  });

  // node_modules/core-js/internals/function-bind-context.js
  var require_function_bind_context = __commonJS({
    "node_modules/core-js/internals/function-bind-context.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this_clause();
      var aCallable2 = require_a_callable();
      var NATIVE_BIND = require_function_bind_native();
      var bind2 = uncurryThis11(uncurryThis11.bind);
      module.exports = function(fn, that) {
        aCallable2(fn);
        return that === void 0 ? fn : NATIVE_BIND ? bind2(fn, that) : function() {
          return fn.apply(that, arguments);
        };
      };
    }
  });

  // node_modules/core-js/internals/is-array.js
  var require_is_array = __commonJS({
    "node_modules/core-js/internals/is-array.js": function(exports, module) {
      var classof = require_classof_raw();
      module.exports = Array.isArray || function isArray4(argument) {
        return classof(argument) == "Array";
      };
    }
  });

  // node_modules/core-js/internals/is-constructor.js
  var require_is_constructor = __commonJS({
    "node_modules/core-js/internals/is-constructor.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var fails12 = require_fails();
      var isCallable4 = require_is_callable();
      var classof = require_classof();
      var getBuiltIn3 = require_get_built_in();
      var inspectSource = require_inspect_source();
      var noop = function() {
      };
      var empty = [];
      var construct2 = getBuiltIn3("Reflect", "construct");
      var constructorRegExp = /^\s*(?:class|function)\b/;
      var exec2 = uncurryThis11(constructorRegExp.exec);
      var INCORRECT_TO_STRING = !constructorRegExp.exec(noop);
      var isConstructorModern = function isConstructor2(argument) {
        if (!isCallable4(argument))
          return false;
        try {
          construct2(noop, empty, argument);
          return true;
        } catch (error) {
          return false;
        }
      };
      var isConstructorLegacy = function isConstructor2(argument) {
        if (!isCallable4(argument))
          return false;
        switch (classof(argument)) {
          case "AsyncFunction":
          case "GeneratorFunction":
          case "AsyncGeneratorFunction":
            return false;
        }
        try {
          return INCORRECT_TO_STRING || !!exec2(constructorRegExp, inspectSource(argument));
        } catch (error) {
          return true;
        }
      };
      isConstructorLegacy.sham = true;
      module.exports = !construct2 || fails12(function() {
        var called;
        return isConstructorModern(isConstructorModern.call) || !isConstructorModern(Object) || !isConstructorModern(function() {
          called = true;
        }) || called;
      }) ? isConstructorLegacy : isConstructorModern;
    }
  });

  // node_modules/core-js/internals/array-species-constructor.js
  var require_array_species_constructor = __commonJS({
    "node_modules/core-js/internals/array-species-constructor.js": function(exports, module) {
      var isArray4 = require_is_array();
      var isConstructor2 = require_is_constructor();
      var isObject5 = require_is_object();
      var wellKnownSymbol6 = require_well_known_symbol();
      var SPECIES2 = wellKnownSymbol6("species");
      var $Array2 = Array;
      module.exports = function(originalArray) {
        var C;
        if (isArray4(originalArray)) {
          C = originalArray.constructor;
          if (isConstructor2(C) && (C === $Array2 || isArray4(C.prototype)))
            C = void 0;
          else if (isObject5(C)) {
            C = C[SPECIES2];
            if (C === null)
              C = void 0;
          }
        }
        return C === void 0 ? $Array2 : C;
      };
    }
  });

  // node_modules/core-js/internals/array-species-create.js
  var require_array_species_create = __commonJS({
    "node_modules/core-js/internals/array-species-create.js": function(exports, module) {
      var arraySpeciesConstructor = require_array_species_constructor();
      module.exports = function(originalArray, length) {
        return new (arraySpeciesConstructor(originalArray))(length === 0 ? 0 : length);
      };
    }
  });

  // node_modules/core-js/internals/array-iteration.js
  var require_array_iteration = __commonJS({
    "node_modules/core-js/internals/array-iteration.js": function(exports, module) {
      var bind2 = require_function_bind_context();
      var uncurryThis11 = require_function_uncurry_this();
      var IndexedObject2 = require_indexed_object();
      var toObject5 = require_to_object();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var arraySpeciesCreate3 = require_array_species_create();
      var push4 = uncurryThis11([].push);
      var createMethod = function(TYPE) {
        var IS_MAP = TYPE == 1;
        var IS_FILTER = TYPE == 2;
        var IS_SOME = TYPE == 3;
        var IS_EVERY = TYPE == 4;
        var IS_FIND_INDEX = TYPE == 6;
        var IS_FILTER_REJECT = TYPE == 7;
        var NO_HOLES = TYPE == 5 || IS_FIND_INDEX;
        return function($this, callbackfn, that, specificCreate) {
          var O = toObject5($this);
          var self2 = IndexedObject2(O);
          var boundFunction = bind2(callbackfn, that);
          var length = lengthOfArrayLike4(self2);
          var index = 0;
          var create3 = specificCreate || arraySpeciesCreate3;
          var target = IS_MAP ? create3($this, length) : IS_FILTER || IS_FILTER_REJECT ? create3($this, 0) : void 0;
          var value, result;
          for (; length > index; index++)
            if (NO_HOLES || index in self2) {
              value = self2[index];
              result = boundFunction(value, index, O);
              if (TYPE) {
                if (IS_MAP)
                  target[index] = result;
                else if (result)
                  switch (TYPE) {
                    case 3:
                      return true;
                    case 5:
                      return value;
                    case 6:
                      return index;
                    case 2:
                      push4(target, value);
                  }
                else
                  switch (TYPE) {
                    case 4:
                      return false;
                    case 7:
                      push4(target, value);
                  }
              }
            }
          return IS_FIND_INDEX ? -1 : IS_SOME || IS_EVERY ? IS_EVERY : target;
        };
      };
      module.exports = {
        forEach: createMethod(0),
        map: createMethod(1),
        filter: createMethod(2),
        some: createMethod(3),
        every: createMethod(4),
        find: createMethod(5),
        findIndex: createMethod(6),
        filterReject: createMethod(7)
      };
    }
  });

  // node_modules/core-js/modules/es.symbol.constructor.js
  var require_es_symbol_constructor = __commonJS({
    "node_modules/core-js/modules/es.symbol.constructor.js": function() {
      "use strict";
      var $77 = require_export();
      var global9 = require_global();
      var call8 = require_function_call();
      var uncurryThis11 = require_function_uncurry_this();
      var IS_PURE2 = require_is_pure();
      var DESCRIPTORS8 = require_descriptors();
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      var fails12 = require_fails();
      var hasOwn4 = require_has_own_property();
      var isPrototypeOf3 = require_object_is_prototype_of();
      var anObject10 = require_an_object();
      var toIndexedObject5 = require_to_indexed_object();
      var toPropertyKey = require_to_property_key();
      var $toString2 = require_to_string();
      var createPropertyDescriptor = require_create_property_descriptor();
      var nativeObjectCreate = require_object_create();
      var objectKeys = require_object_keys();
      var getOwnPropertyNamesModule = require_object_get_own_property_names();
      var getOwnPropertyNamesExternal = require_object_get_own_property_names_external();
      var getOwnPropertySymbolsModule = require_object_get_own_property_symbols();
      var getOwnPropertyDescriptorModule3 = require_object_get_own_property_descriptor();
      var definePropertyModule = require_object_define_property();
      var definePropertiesModule = require_object_define_properties();
      var propertyIsEnumerableModule = require_object_property_is_enumerable();
      var defineBuiltIn4 = require_define_built_in();
      var defineBuiltInAccessor3 = require_define_built_in_accessor();
      var shared = require_shared();
      var sharedKey = require_shared_key();
      var hiddenKeys = require_hidden_keys();
      var uid = require_uid();
      var wellKnownSymbol6 = require_well_known_symbol();
      var wrappedWellKnownSymbolModule = require_well_known_symbol_wrapped();
      var defineWellKnownSymbol5 = require_well_known_symbol_define();
      var defineSymbolToPrimitive2 = require_symbol_define_to_primitive();
      var setToStringTag5 = require_set_to_string_tag();
      var InternalStateModule2 = require_internal_state();
      var $forEach = require_array_iteration().forEach;
      var HIDDEN = sharedKey("hidden");
      var SYMBOL = "Symbol";
      var PROTOTYPE = "prototype";
      var setInternalState2 = InternalStateModule2.set;
      var getInternalState2 = InternalStateModule2.getterFor(SYMBOL);
      var ObjectPrototype2 = Object[PROTOTYPE];
      var $Symbol = global9.Symbol;
      var SymbolPrototype2 = $Symbol && $Symbol[PROTOTYPE];
      var TypeError3 = global9.TypeError;
      var QObject = global9.QObject;
      var nativeGetOwnPropertyDescriptor2 = getOwnPropertyDescriptorModule3.f;
      var nativeDefineProperty = definePropertyModule.f;
      var nativeGetOwnPropertyNames = getOwnPropertyNamesExternal.f;
      var nativePropertyIsEnumerable = propertyIsEnumerableModule.f;
      var push4 = uncurryThis11([].push);
      var AllSymbols = shared("symbols");
      var ObjectPrototypeSymbols = shared("op-symbols");
      var WellKnownSymbolsStore = shared("wks");
      var USE_SETTER = !QObject || !QObject[PROTOTYPE] || !QObject[PROTOTYPE].findChild;
      var setSymbolDescriptor = DESCRIPTORS8 && fails12(function() {
        return nativeObjectCreate(nativeDefineProperty({}, "a", {
          get: function() {
            return nativeDefineProperty(this, "a", { value: 7 }).a;
          }
        })).a != 7;
      }) ? function(O, P, Attributes) {
        var ObjectPrototypeDescriptor = nativeGetOwnPropertyDescriptor2(ObjectPrototype2, P);
        if (ObjectPrototypeDescriptor)
          delete ObjectPrototype2[P];
        nativeDefineProperty(O, P, Attributes);
        if (ObjectPrototypeDescriptor && O !== ObjectPrototype2) {
          nativeDefineProperty(ObjectPrototype2, P, ObjectPrototypeDescriptor);
        }
      } : nativeDefineProperty;
      var wrap = function(tag, description) {
        var symbol = AllSymbols[tag] = nativeObjectCreate(SymbolPrototype2);
        setInternalState2(symbol, {
          type: SYMBOL,
          tag: tag,
          description: description
        });
        if (!DESCRIPTORS8)
          symbol.description = description;
        return symbol;
      };
      var $defineProperty = function defineProperty3(O, P, Attributes) {
        if (O === ObjectPrototype2)
          $defineProperty(ObjectPrototypeSymbols, P, Attributes);
        anObject10(O);
        var key = toPropertyKey(P);
        anObject10(Attributes);
        if (hasOwn4(AllSymbols, key)) {
          if (!Attributes.enumerable) {
            if (!hasOwn4(O, HIDDEN))
              nativeDefineProperty(O, HIDDEN, createPropertyDescriptor(1, {}));
            O[HIDDEN][key] = true;
          } else {
            if (hasOwn4(O, HIDDEN) && O[HIDDEN][key])
              O[HIDDEN][key] = false;
            Attributes = nativeObjectCreate(Attributes, { enumerable: createPropertyDescriptor(0, false) });
          }
          return setSymbolDescriptor(O, key, Attributes);
        }
        return nativeDefineProperty(O, key, Attributes);
      };
      var $defineProperties = function defineProperties2(O, Properties) {
        anObject10(O);
        var properties = toIndexedObject5(Properties);
        var keys2 = objectKeys(properties).concat($getOwnPropertySymbols(properties));
        $forEach(keys2, function(key) {
          if (!DESCRIPTORS8 || call8($propertyIsEnumerable, properties, key))
            $defineProperty(O, key, properties[key]);
        });
        return O;
      };
      var $create = function create3(O, Properties) {
        return Properties === void 0 ? nativeObjectCreate(O) : $defineProperties(nativeObjectCreate(O), Properties);
      };
      var $propertyIsEnumerable = function propertyIsEnumerable(V) {
        var P = toPropertyKey(V);
        var enumerable = call8(nativePropertyIsEnumerable, this, P);
        if (this === ObjectPrototype2 && hasOwn4(AllSymbols, P) && !hasOwn4(ObjectPrototypeSymbols, P))
          return false;
        return enumerable || !hasOwn4(this, P) || !hasOwn4(AllSymbols, P) || hasOwn4(this, HIDDEN) && this[HIDDEN][P] ? enumerable : true;
      };
      var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor3(O, P) {
        var it = toIndexedObject5(O);
        var key = toPropertyKey(P);
        if (it === ObjectPrototype2 && hasOwn4(AllSymbols, key) && !hasOwn4(ObjectPrototypeSymbols, key))
          return;
        var descriptor = nativeGetOwnPropertyDescriptor2(it, key);
        if (descriptor && hasOwn4(AllSymbols, key) && !(hasOwn4(it, HIDDEN) && it[HIDDEN][key])) {
          descriptor.enumerable = true;
        }
        return descriptor;
      };
      var $getOwnPropertyNames = function getOwnPropertyNames2(O) {
        var names = nativeGetOwnPropertyNames(toIndexedObject5(O));
        var result = [];
        $forEach(names, function(key) {
          if (!hasOwn4(AllSymbols, key) && !hasOwn4(hiddenKeys, key))
            push4(result, key);
        });
        return result;
      };
      var $getOwnPropertySymbols = function(O) {
        var IS_OBJECT_PROTOTYPE = O === ObjectPrototype2;
        var names = nativeGetOwnPropertyNames(IS_OBJECT_PROTOTYPE ? ObjectPrototypeSymbols : toIndexedObject5(O));
        var result = [];
        $forEach(names, function(key) {
          if (hasOwn4(AllSymbols, key) && (!IS_OBJECT_PROTOTYPE || hasOwn4(ObjectPrototype2, key))) {
            push4(result, AllSymbols[key]);
          }
        });
        return result;
      };
      if (!NATIVE_SYMBOL) {
        $Symbol = function Symbol2() {
          if (isPrototypeOf3(SymbolPrototype2, this))
            throw TypeError3("Symbol is not a constructor");
          var description = !arguments.length || arguments[0] === void 0 ? void 0 : $toString2(arguments[0]);
          var tag = uid(description);
          var setter = function(value) {
            if (this === ObjectPrototype2)
              call8(setter, ObjectPrototypeSymbols, value);
            if (hasOwn4(this, HIDDEN) && hasOwn4(this[HIDDEN], tag))
              this[HIDDEN][tag] = false;
            setSymbolDescriptor(this, tag, createPropertyDescriptor(1, value));
          };
          if (DESCRIPTORS8 && USE_SETTER)
            setSymbolDescriptor(ObjectPrototype2, tag, { configurable: true, set: setter });
          return wrap(tag, description);
        };
        SymbolPrototype2 = $Symbol[PROTOTYPE];
        defineBuiltIn4(SymbolPrototype2, "toString", function toString9() {
          return getInternalState2(this).tag;
        });
        defineBuiltIn4($Symbol, "withoutSetter", function(description) {
          return wrap(uid(description), description);
        });
        propertyIsEnumerableModule.f = $propertyIsEnumerable;
        definePropertyModule.f = $defineProperty;
        definePropertiesModule.f = $defineProperties;
        getOwnPropertyDescriptorModule3.f = $getOwnPropertyDescriptor;
        getOwnPropertyNamesModule.f = getOwnPropertyNamesExternal.f = $getOwnPropertyNames;
        getOwnPropertySymbolsModule.f = $getOwnPropertySymbols;
        wrappedWellKnownSymbolModule.f = function(name) {
          return wrap(wellKnownSymbol6(name), name);
        };
        if (DESCRIPTORS8) {
          defineBuiltInAccessor3(SymbolPrototype2, "description", {
            configurable: true,
            get: function description() {
              return getInternalState2(this).description;
            }
          });
          if (!IS_PURE2) {
            defineBuiltIn4(ObjectPrototype2, "propertyIsEnumerable", $propertyIsEnumerable, { unsafe: true });
          }
        }
      }
      $77({ global: true, constructor: true, wrap: true, forced: !NATIVE_SYMBOL, sham: !NATIVE_SYMBOL }, {
        Symbol: $Symbol
      });
      $forEach(objectKeys(WellKnownSymbolsStore), function(name) {
        defineWellKnownSymbol5(name);
      });
      $77({ target: SYMBOL, stat: true, forced: !NATIVE_SYMBOL }, {
        useSetter: function() {
          USE_SETTER = true;
        },
        useSimple: function() {
          USE_SETTER = false;
        }
      });
      $77({ target: "Object", stat: true, forced: !NATIVE_SYMBOL, sham: !DESCRIPTORS8 }, {
        create: $create,
        defineProperty: $defineProperty,
        defineProperties: $defineProperties,
        getOwnPropertyDescriptor: $getOwnPropertyDescriptor
      });
      $77({ target: "Object", stat: true, forced: !NATIVE_SYMBOL }, {
        getOwnPropertyNames: $getOwnPropertyNames
      });
      defineSymbolToPrimitive2();
      setToStringTag5($Symbol, SYMBOL);
      hiddenKeys[HIDDEN] = true;
    }
  });

  // node_modules/core-js/internals/symbol-registry-detection.js
  var require_symbol_registry_detection = __commonJS({
    "node_modules/core-js/internals/symbol-registry-detection.js": function(exports, module) {
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      module.exports = NATIVE_SYMBOL && !!Symbol["for"] && !!Symbol.keyFor;
    }
  });

  // node_modules/core-js/modules/es.symbol.for.js
  var require_es_symbol_for = __commonJS({
    "node_modules/core-js/modules/es.symbol.for.js": function() {
      var $77 = require_export();
      var getBuiltIn3 = require_get_built_in();
      var hasOwn4 = require_has_own_property();
      var toString9 = require_to_string();
      var shared = require_shared();
      var NATIVE_SYMBOL_REGISTRY = require_symbol_registry_detection();
      var StringToSymbolRegistry = shared("string-to-symbol-registry");
      var SymbolToStringRegistry = shared("symbol-to-string-registry");
      $77({ target: "Symbol", stat: true, forced: !NATIVE_SYMBOL_REGISTRY }, {
        "for": function(key) {
          var string = toString9(key);
          if (hasOwn4(StringToSymbolRegistry, string))
            return StringToSymbolRegistry[string];
          var symbol = getBuiltIn3("Symbol")(string);
          StringToSymbolRegistry[string] = symbol;
          SymbolToStringRegistry[symbol] = string;
          return symbol;
        }
      });
    }
  });

  // node_modules/core-js/modules/es.symbol.key-for.js
  var require_es_symbol_key_for = __commonJS({
    "node_modules/core-js/modules/es.symbol.key-for.js": function() {
      var $77 = require_export();
      var hasOwn4 = require_has_own_property();
      var isSymbol2 = require_is_symbol();
      var tryToString = require_try_to_string();
      var shared = require_shared();
      var NATIVE_SYMBOL_REGISTRY = require_symbol_registry_detection();
      var SymbolToStringRegistry = shared("symbol-to-string-registry");
      $77({ target: "Symbol", stat: true, forced: !NATIVE_SYMBOL_REGISTRY }, {
        keyFor: function keyFor(sym) {
          if (!isSymbol2(sym))
            throw TypeError(tryToString(sym) + " is not a symbol");
          if (hasOwn4(SymbolToStringRegistry, sym))
            return SymbolToStringRegistry[sym];
        }
      });
    }
  });

  // node_modules/core-js/internals/function-apply.js
  var require_function_apply = __commonJS({
    "node_modules/core-js/internals/function-apply.js": function(exports, module) {
      var NATIVE_BIND = require_function_bind_native();
      var FunctionPrototype2 = Function.prototype;
      var apply4 = FunctionPrototype2.apply;
      var call8 = FunctionPrototype2.call;
      module.exports = typeof Reflect == "object" && Reflect.apply || (NATIVE_BIND ? call8.bind(apply4) : function() {
        return call8.apply(apply4, arguments);
      });
    }
  });

  // node_modules/core-js/internals/array-slice.js
  var require_array_slice = __commonJS({
    "node_modules/core-js/internals/array-slice.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      module.exports = uncurryThis11([].slice);
    }
  });

  // node_modules/core-js/internals/get-json-replacer-function.js
  var require_get_json_replacer_function = __commonJS({
    "node_modules/core-js/internals/get-json-replacer-function.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var isArray4 = require_is_array();
      var isCallable4 = require_is_callable();
      var classof = require_classof_raw();
      var toString9 = require_to_string();
      var push4 = uncurryThis11([].push);
      module.exports = function(replacer) {
        if (isCallable4(replacer))
          return replacer;
        if (!isArray4(replacer))
          return;
        var rawLength = replacer.length;
        var keys2 = [];
        for (var i = 0; i < rawLength; i++) {
          var element = replacer[i];
          if (typeof element == "string")
            push4(keys2, element);
          else if (typeof element == "number" || classof(element) == "Number" || classof(element) == "String")
            push4(keys2, toString9(element));
        }
        var keysLength = keys2.length;
        var root = true;
        return function(key, value) {
          if (root) {
            root = false;
            return value;
          }
          if (isArray4(this))
            return value;
          for (var j = 0; j < keysLength; j++)
            if (keys2[j] === key)
              return value;
        };
      };
    }
  });

  // node_modules/core-js/modules/es.json.stringify.js
  var require_es_json_stringify = __commonJS({
    "node_modules/core-js/modules/es.json.stringify.js": function() {
      var $77 = require_export();
      var getBuiltIn3 = require_get_built_in();
      var apply4 = require_function_apply();
      var call8 = require_function_call();
      var uncurryThis11 = require_function_uncurry_this();
      var fails12 = require_fails();
      var isCallable4 = require_is_callable();
      var isSymbol2 = require_is_symbol();
      var arraySlice2 = require_array_slice();
      var getReplacerFunction = require_get_json_replacer_function();
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      var $String = String;
      var $stringify = getBuiltIn3("JSON", "stringify");
      var exec2 = uncurryThis11(/./.exec);
      var charAt2 = uncurryThis11("".charAt);
      var charCodeAt2 = uncurryThis11("".charCodeAt);
      var replace = uncurryThis11("".replace);
      var numberToString = uncurryThis11(1 .toString);
      var tester = /[\uD800-\uDFFF]/g;
      var low = /^[\uD800-\uDBFF]$/;
      var hi = /^[\uDC00-\uDFFF]$/;
      var WRONG_SYMBOLS_CONVERSION = !NATIVE_SYMBOL || fails12(function() {
        var symbol = getBuiltIn3("Symbol")();
        return $stringify([symbol]) != "[null]" || $stringify({ a: symbol }) != "{}" || $stringify(Object(symbol)) != "{}";
      });
      var ILL_FORMED_UNICODE = fails12(function() {
        return $stringify("\uDF06\uD834") !== '"\\udf06\\ud834"' || $stringify("\uDEAD") !== '"\\udead"';
      });
      var stringifyWithSymbolsFix = function(it, replacer) {
        var args = arraySlice2(arguments);
        var $replacer = getReplacerFunction(replacer);
        if (!isCallable4($replacer) && (it === void 0 || isSymbol2(it)))
          return;
        args[1] = function(key, value) {
          if (isCallable4($replacer))
            value = call8($replacer, this, $String(key), value);
          if (!isSymbol2(value))
            return value;
        };
        return apply4($stringify, null, args);
      };
      var fixIllFormed = function(match, offset, string) {
        var prev = charAt2(string, offset - 1);
        var next2 = charAt2(string, offset + 1);
        if (exec2(low, match) && !exec2(hi, next2) || exec2(hi, match) && !exec2(low, prev)) {
          return "\\u" + numberToString(charCodeAt2(match, 0), 16);
        }
        return match;
      };
      if ($stringify) {
        $77({ target: "JSON", stat: true, arity: 3, forced: WRONG_SYMBOLS_CONVERSION || ILL_FORMED_UNICODE }, {
          stringify: function stringify(it, replacer, space) {
            var args = arraySlice2(arguments);
            var result = apply4(WRONG_SYMBOLS_CONVERSION ? stringifyWithSymbolsFix : $stringify, null, args);
            return ILL_FORMED_UNICODE && typeof result == "string" ? replace(result, tester, fixIllFormed) : result;
          }
        });
      }
    }
  });

  // node_modules/core-js/modules/es.object.get-own-property-symbols.js
  var require_es_object_get_own_property_symbols = __commonJS({
    "node_modules/core-js/modules/es.object.get-own-property-symbols.js": function() {
      var $77 = require_export();
      var NATIVE_SYMBOL = require_symbol_constructor_detection();
      var fails12 = require_fails();
      var getOwnPropertySymbolsModule = require_object_get_own_property_symbols();
      var toObject5 = require_to_object();
      var FORCED9 = !NATIVE_SYMBOL || fails12(function() {
        getOwnPropertySymbolsModule.f(1);
      });
      $77({ target: "Object", stat: true, forced: FORCED9 }, {
        getOwnPropertySymbols: function getOwnPropertySymbols(it) {
          var $getOwnPropertySymbols = getOwnPropertySymbolsModule.f;
          return $getOwnPropertySymbols ? $getOwnPropertySymbols(toObject5(it)) : [];
        }
      });
    }
  });

  // node_modules/core-js/internals/object-to-string.js
  var require_object_to_string = __commonJS({
    "node_modules/core-js/internals/object-to-string.js": function(exports, module) {
      "use strict";
      var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
      var classof = require_classof();
      module.exports = TO_STRING_TAG_SUPPORT2 ? {}.toString : function toString9() {
        return "[object " + classof(this) + "]";
      };
    }
  });

  // node_modules/core-js/internals/function-uncurry-this-accessor.js
  var require_function_uncurry_this_accessor = __commonJS({
    "node_modules/core-js/internals/function-uncurry-this-accessor.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var aCallable2 = require_a_callable();
      module.exports = function(object, key, method) {
        try {
          return uncurryThis11(aCallable2(Object.getOwnPropertyDescriptor(object, key)[method]));
        } catch (error) {
        }
      };
    }
  });

  // node_modules/core-js/internals/a-possible-prototype.js
  var require_a_possible_prototype = __commonJS({
    "node_modules/core-js/internals/a-possible-prototype.js": function(exports, module) {
      var isCallable4 = require_is_callable();
      var $String = String;
      var $TypeError = TypeError;
      module.exports = function(argument) {
        if (typeof argument == "object" || isCallable4(argument))
          return argument;
        throw $TypeError("Can't set " + $String(argument) + " as a prototype");
      };
    }
  });

  // node_modules/core-js/internals/object-set-prototype-of.js
  var require_object_set_prototype_of = __commonJS({
    "node_modules/core-js/internals/object-set-prototype-of.js": function(exports, module) {
      var uncurryThisAccessor = require_function_uncurry_this_accessor();
      var anObject10 = require_an_object();
      var aPossiblePrototype = require_a_possible_prototype();
      module.exports = Object.setPrototypeOf || ("__proto__" in {} ? function() {
        var CORRECT_SETTER = false;
        var test2 = {};
        var setter;
        try {
          setter = uncurryThisAccessor(Object.prototype, "__proto__", "set");
          setter(test2, []);
          CORRECT_SETTER = test2 instanceof Array;
        } catch (error) {
        }
        return function setPrototypeOf2(O, proto) {
          anObject10(O);
          aPossiblePrototype(proto);
          if (CORRECT_SETTER)
            setter(O, proto);
          else
            O.__proto__ = proto;
          return O;
        };
      }() : void 0);
    }
  });

  // node_modules/core-js/internals/inherit-if-required.js
  var require_inherit_if_required = __commonJS({
    "node_modules/core-js/internals/inherit-if-required.js": function(exports, module) {
      var isCallable4 = require_is_callable();
      var isObject5 = require_is_object();
      var setPrototypeOf2 = require_object_set_prototype_of();
      module.exports = function($this, dummy, Wrapper) {
        var NewTarget, NewTargetPrototype;
        if (setPrototypeOf2 && isCallable4(NewTarget = dummy.constructor) && NewTarget !== Wrapper && isObject5(NewTargetPrototype = NewTarget.prototype) && NewTargetPrototype !== Wrapper.prototype)
          setPrototypeOf2($this, NewTargetPrototype);
        return $this;
      };
    }
  });

  // node_modules/core-js/internals/this-number-value.js
  var require_this_number_value = __commonJS({
    "node_modules/core-js/internals/this-number-value.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      module.exports = uncurryThis11(1 .valueOf);
    }
  });

  // node_modules/core-js/internals/add-to-unscopables.js
  var require_add_to_unscopables = __commonJS({
    "node_modules/core-js/internals/add-to-unscopables.js": function(exports, module) {
      var wellKnownSymbol6 = require_well_known_symbol();
      var create3 = require_object_create();
      var defineProperty3 = require_object_define_property().f;
      var UNSCOPABLES = wellKnownSymbol6("unscopables");
      var ArrayPrototype = Array.prototype;
      if (ArrayPrototype[UNSCOPABLES] == void 0) {
        defineProperty3(ArrayPrototype, UNSCOPABLES, {
          configurable: true,
          value: create3(null)
        });
      }
      module.exports = function(key) {
        ArrayPrototype[UNSCOPABLES][key] = true;
      };
    }
  });

  // node_modules/core-js/internals/iterators.js
  var require_iterators = __commonJS({
    "node_modules/core-js/internals/iterators.js": function(exports, module) {
      module.exports = {};
    }
  });

  // node_modules/core-js/internals/correct-prototype-getter.js
  var require_correct_prototype_getter = __commonJS({
    "node_modules/core-js/internals/correct-prototype-getter.js": function(exports, module) {
      var fails12 = require_fails();
      module.exports = !fails12(function() {
        function F() {
        }
        F.prototype.constructor = null;
        return Object.getPrototypeOf(new F()) !== F.prototype;
      });
    }
  });

  // node_modules/core-js/internals/object-get-prototype-of.js
  var require_object_get_prototype_of = __commonJS({
    "node_modules/core-js/internals/object-get-prototype-of.js": function(exports, module) {
      var hasOwn4 = require_has_own_property();
      var isCallable4 = require_is_callable();
      var toObject5 = require_to_object();
      var sharedKey = require_shared_key();
      var CORRECT_PROTOTYPE_GETTER2 = require_correct_prototype_getter();
      var IE_PROTO = sharedKey("IE_PROTO");
      var $Object = Object;
      var ObjectPrototype2 = $Object.prototype;
      module.exports = CORRECT_PROTOTYPE_GETTER2 ? $Object.getPrototypeOf : function(O) {
        var object = toObject5(O);
        if (hasOwn4(object, IE_PROTO))
          return object[IE_PROTO];
        var constructor = object.constructor;
        if (isCallable4(constructor) && object instanceof constructor) {
          return constructor.prototype;
        }
        return object instanceof $Object ? ObjectPrototype2 : null;
      };
    }
  });

  // node_modules/core-js/internals/iterators-core.js
  var require_iterators_core = __commonJS({
    "node_modules/core-js/internals/iterators-core.js": function(exports, module) {
      "use strict";
      var fails12 = require_fails();
      var isCallable4 = require_is_callable();
      var isObject5 = require_is_object();
      var create3 = require_object_create();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var defineBuiltIn4 = require_define_built_in();
      var wellKnownSymbol6 = require_well_known_symbol();
      var IS_PURE2 = require_is_pure();
      var ITERATOR2 = wellKnownSymbol6("iterator");
      var BUGGY_SAFARI_ITERATORS = false;
      var IteratorPrototype;
      var PrototypeOfArrayIteratorPrototype;
      var arrayIterator;
      if ([].keys) {
        arrayIterator = [].keys();
        if (!("next" in arrayIterator))
          BUGGY_SAFARI_ITERATORS = true;
        else {
          PrototypeOfArrayIteratorPrototype = getPrototypeOf3(getPrototypeOf3(arrayIterator));
          if (PrototypeOfArrayIteratorPrototype !== Object.prototype)
            IteratorPrototype = PrototypeOfArrayIteratorPrototype;
        }
      }
      var NEW_ITERATOR_PROTOTYPE = !isObject5(IteratorPrototype) || fails12(function() {
        var test2 = {};
        return IteratorPrototype[ITERATOR2].call(test2) !== test2;
      });
      if (NEW_ITERATOR_PROTOTYPE)
        IteratorPrototype = {};
      else if (IS_PURE2)
        IteratorPrototype = create3(IteratorPrototype);
      if (!isCallable4(IteratorPrototype[ITERATOR2])) {
        defineBuiltIn4(IteratorPrototype, ITERATOR2, function() {
          return this;
        });
      }
      module.exports = {
        IteratorPrototype: IteratorPrototype,
        BUGGY_SAFARI_ITERATORS: BUGGY_SAFARI_ITERATORS
      };
    }
  });

  // node_modules/core-js/internals/iterator-create-constructor.js
  var require_iterator_create_constructor = __commonJS({
    "node_modules/core-js/internals/iterator-create-constructor.js": function(exports, module) {
      "use strict";
      var IteratorPrototype = require_iterators_core().IteratorPrototype;
      var create3 = require_object_create();
      var createPropertyDescriptor = require_create_property_descriptor();
      var setToStringTag5 = require_set_to_string_tag();
      var Iterators = require_iterators();
      var returnThis = function() {
        return this;
      };
      module.exports = function(IteratorConstructor, NAME2, next2, ENUMERABLE_NEXT) {
        var TO_STRING_TAG2 = NAME2 + " Iterator";
        IteratorConstructor.prototype = create3(IteratorPrototype, { next: createPropertyDescriptor(+!ENUMERABLE_NEXT, next2) });
        setToStringTag5(IteratorConstructor, TO_STRING_TAG2, false, true);
        Iterators[TO_STRING_TAG2] = returnThis;
        return IteratorConstructor;
      };
    }
  });

  // node_modules/core-js/internals/iterator-define.js
  var require_iterator_define = __commonJS({
    "node_modules/core-js/internals/iterator-define.js": function(exports, module) {
      "use strict";
      var $77 = require_export();
      var call8 = require_function_call();
      var IS_PURE2 = require_is_pure();
      var FunctionName = require_function_name();
      var isCallable4 = require_is_callable();
      var createIteratorConstructor = require_iterator_create_constructor();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var setPrototypeOf2 = require_object_set_prototype_of();
      var setToStringTag5 = require_set_to_string_tag();
      var createNonEnumerableProperty3 = require_create_non_enumerable_property();
      var defineBuiltIn4 = require_define_built_in();
      var wellKnownSymbol6 = require_well_known_symbol();
      var Iterators = require_iterators();
      var IteratorsCore = require_iterators_core();
      var PROPER_FUNCTION_NAME2 = FunctionName.PROPER;
      var CONFIGURABLE_FUNCTION_NAME = FunctionName.CONFIGURABLE;
      var IteratorPrototype = IteratorsCore.IteratorPrototype;
      var BUGGY_SAFARI_ITERATORS = IteratorsCore.BUGGY_SAFARI_ITERATORS;
      var ITERATOR2 = wellKnownSymbol6("iterator");
      var KEYS = "keys";
      var VALUES = "values";
      var ENTRIES = "entries";
      var returnThis = function() {
        return this;
      };
      module.exports = function(Iterable, NAME2, IteratorConstructor, next2, DEFAULT, IS_SET, FORCED9) {
        createIteratorConstructor(IteratorConstructor, NAME2, next2);
        var getIterationMethod = function(KIND) {
          if (KIND === DEFAULT && defaultIterator)
            return defaultIterator;
          if (!BUGGY_SAFARI_ITERATORS && KIND in IterablePrototype)
            return IterablePrototype[KIND];
          switch (KIND) {
            case KEYS:
              return function keys2() {
                return new IteratorConstructor(this, KIND);
              };
            case VALUES:
              return function values2() {
                return new IteratorConstructor(this, KIND);
              };
            case ENTRIES:
              return function entries2() {
                return new IteratorConstructor(this, KIND);
              };
          }
          return function() {
            return new IteratorConstructor(this);
          };
        };
        var TO_STRING_TAG2 = NAME2 + " Iterator";
        var INCORRECT_VALUES_NAME = false;
        var IterablePrototype = Iterable.prototype;
        var nativeIterator = IterablePrototype[ITERATOR2] || IterablePrototype["@@iterator"] || DEFAULT && IterablePrototype[DEFAULT];
        var defaultIterator = !BUGGY_SAFARI_ITERATORS && nativeIterator || getIterationMethod(DEFAULT);
        var anyNativeIterator = NAME2 == "Array" ? IterablePrototype.entries || nativeIterator : nativeIterator;
        var CurrentIteratorPrototype, methods, KEY;
        if (anyNativeIterator) {
          CurrentIteratorPrototype = getPrototypeOf3(anyNativeIterator.call(new Iterable()));
          if (CurrentIteratorPrototype !== Object.prototype && CurrentIteratorPrototype.next) {
            if (!IS_PURE2 && getPrototypeOf3(CurrentIteratorPrototype) !== IteratorPrototype) {
              if (setPrototypeOf2) {
                setPrototypeOf2(CurrentIteratorPrototype, IteratorPrototype);
              } else if (!isCallable4(CurrentIteratorPrototype[ITERATOR2])) {
                defineBuiltIn4(CurrentIteratorPrototype, ITERATOR2, returnThis);
              }
            }
            setToStringTag5(CurrentIteratorPrototype, TO_STRING_TAG2, true, true);
            if (IS_PURE2)
              Iterators[TO_STRING_TAG2] = returnThis;
          }
        }
        if (PROPER_FUNCTION_NAME2 && DEFAULT == VALUES && nativeIterator && nativeIterator.name !== VALUES) {
          if (!IS_PURE2 && CONFIGURABLE_FUNCTION_NAME) {
            createNonEnumerableProperty3(IterablePrototype, "name", VALUES);
          } else {
            INCORRECT_VALUES_NAME = true;
            defaultIterator = function values2() {
              return call8(nativeIterator, this);
            };
          }
        }
        if (DEFAULT) {
          methods = {
            values: getIterationMethod(VALUES),
            keys: IS_SET ? defaultIterator : getIterationMethod(KEYS),
            entries: getIterationMethod(ENTRIES)
          };
          if (FORCED9)
            for (KEY in methods) {
              if (BUGGY_SAFARI_ITERATORS || INCORRECT_VALUES_NAME || !(KEY in IterablePrototype)) {
                defineBuiltIn4(IterablePrototype, KEY, methods[KEY]);
              }
            }
          else
            $77({ target: NAME2, proto: true, forced: BUGGY_SAFARI_ITERATORS || INCORRECT_VALUES_NAME }, methods);
        }
        if ((!IS_PURE2 || FORCED9) && IterablePrototype[ITERATOR2] !== defaultIterator) {
          defineBuiltIn4(IterablePrototype, ITERATOR2, defaultIterator, { name: DEFAULT });
        }
        Iterators[NAME2] = defaultIterator;
        return methods;
      };
    }
  });

  // node_modules/core-js/internals/create-iter-result-object.js
  var require_create_iter_result_object = __commonJS({
    "node_modules/core-js/internals/create-iter-result-object.js": function(exports, module) {
      module.exports = function(value, done) {
        return { value: value, done: done };
      };
    }
  });

  // node_modules/core-js/modules/es.array.iterator.js
  var require_es_array_iterator = __commonJS({
    "node_modules/core-js/modules/es.array.iterator.js": function(exports, module) {
      "use strict";
      var toIndexedObject5 = require_to_indexed_object();
      var addToUnscopables2 = require_add_to_unscopables();
      var Iterators = require_iterators();
      var InternalStateModule2 = require_internal_state();
      var defineProperty3 = require_object_define_property().f;
      var defineIterator2 = require_iterator_define();
      var createIterResultObject2 = require_create_iter_result_object();
      var IS_PURE2 = require_is_pure();
      var DESCRIPTORS8 = require_descriptors();
      var ARRAY_ITERATOR = "Array Iterator";
      var setInternalState2 = InternalStateModule2.set;
      var getInternalState2 = InternalStateModule2.getterFor(ARRAY_ITERATOR);
      module.exports = defineIterator2(Array, "Array", function(iterated, kind) {
        setInternalState2(this, {
          type: ARRAY_ITERATOR,
          target: toIndexedObject5(iterated),
          index: 0,
          kind: kind
        });
      }, function() {
        var state = getInternalState2(this);
        var target = state.target;
        var kind = state.kind;
        var index = state.index++;
        if (!target || index >= target.length) {
          state.target = void 0;
          return createIterResultObject2(void 0, true);
        }
        if (kind == "keys")
          return createIterResultObject2(index, false);
        if (kind == "values")
          return createIterResultObject2(target[index], false);
        return createIterResultObject2([index, target[index]], false);
      }, "values");
      var values2 = Iterators.Arguments = Iterators.Array;
      addToUnscopables2("keys");
      addToUnscopables2("values");
      addToUnscopables2("entries");
      if (!IS_PURE2 && DESCRIPTORS8 && values2.name !== "values")
        try {
          defineProperty3(values2, "name", { value: "values" });
        } catch (error) {
        }
    }
  });

  // node_modules/core-js/internals/string-multibyte.js
  var require_string_multibyte = __commonJS({
    "node_modules/core-js/internals/string-multibyte.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var toIntegerOrInfinity3 = require_to_integer_or_infinity();
      var toString9 = require_to_string();
      var requireObjectCoercible5 = require_require_object_coercible();
      var charAt2 = uncurryThis11("".charAt);
      var charCodeAt2 = uncurryThis11("".charCodeAt);
      var stringSlice4 = uncurryThis11("".slice);
      var createMethod = function(CONVERT_TO_STRING) {
        return function($this, pos) {
          var S = toString9(requireObjectCoercible5($this));
          var position = toIntegerOrInfinity3(pos);
          var size = S.length;
          var first, second;
          if (position < 0 || position >= size)
            return CONVERT_TO_STRING ? "" : void 0;
          first = charCodeAt2(S, position);
          return first < 55296 || first > 56319 || position + 1 === size || (second = charCodeAt2(S, position + 1)) < 56320 || second > 57343 ? CONVERT_TO_STRING ? charAt2(S, position) : first : CONVERT_TO_STRING ? stringSlice4(S, position, position + 2) : (first - 55296 << 10) + (second - 56320) + 65536;
        };
      };
      module.exports = {
        codeAt: createMethod(false),
        charAt: createMethod(true)
      };
    }
  });

  // node_modules/core-js/internals/dom-iterables.js
  var require_dom_iterables = __commonJS({
    "node_modules/core-js/internals/dom-iterables.js": function(exports, module) {
      module.exports = {
        CSSRuleList: 0,
        CSSStyleDeclaration: 0,
        CSSValueList: 0,
        ClientRectList: 0,
        DOMRectList: 0,
        DOMStringList: 0,
        DOMTokenList: 1,
        DataTransferItemList: 0,
        FileList: 0,
        HTMLAllCollection: 0,
        HTMLCollection: 0,
        HTMLFormElement: 0,
        HTMLSelectElement: 0,
        MediaList: 0,
        MimeTypeArray: 0,
        NamedNodeMap: 0,
        NodeList: 1,
        PaintRequestList: 0,
        Plugin: 0,
        PluginArray: 0,
        SVGLengthList: 0,
        SVGNumberList: 0,
        SVGPathSegList: 0,
        SVGPointList: 0,
        SVGStringList: 0,
        SVGTransformList: 0,
        SourceBufferList: 0,
        StyleSheetList: 0,
        TextTrackCueList: 0,
        TextTrackList: 0,
        TouchList: 0
      };
    }
  });

  // node_modules/core-js/internals/dom-token-list-prototype.js
  var require_dom_token_list_prototype = __commonJS({
    "node_modules/core-js/internals/dom-token-list-prototype.js": function(exports, module) {
      var documentCreateElement = require_document_create_element();
      var classList = documentCreateElement("span").classList;
      var DOMTokenListPrototype3 = classList && classList.constructor && classList.constructor.prototype;
      module.exports = DOMTokenListPrototype3 === Object.prototype ? void 0 : DOMTokenListPrototype3;
    }
  });

  // node_modules/core-js/internals/fix-regexp-well-known-symbol-logic.js
  var require_fix_regexp_well_known_symbol_logic = __commonJS({
    "node_modules/core-js/internals/fix-regexp-well-known-symbol-logic.js": function(exports, module) {
      "use strict";
      require_es_regexp_exec();
      var uncurryThis11 = require_function_uncurry_this_clause();
      var defineBuiltIn4 = require_define_built_in();
      var regexpExec2 = require_regexp_exec();
      var fails12 = require_fails();
      var wellKnownSymbol6 = require_well_known_symbol();
      var createNonEnumerableProperty3 = require_create_non_enumerable_property();
      var SPECIES2 = wellKnownSymbol6("species");
      var RegExpPrototype2 = RegExp.prototype;
      module.exports = function(KEY, exec2, FORCED9, SHAM) {
        var SYMBOL = wellKnownSymbol6(KEY);
        var DELEGATES_TO_SYMBOL = !fails12(function() {
          var O = {};
          O[SYMBOL] = function() {
            return 7;
          };
          return ""[KEY](O) != 7;
        });
        var DELEGATES_TO_EXEC2 = DELEGATES_TO_SYMBOL && !fails12(function() {
          var execCalled = false;
          var re = /a/;
          if (KEY === "split") {
            re = {};
            re.constructor = {};
            re.constructor[SPECIES2] = function() {
              return re;
            };
            re.flags = "";
            re[SYMBOL] = /./[SYMBOL];
          }
          re.exec = function() {
            execCalled = true;
            return null;
          };
          re[SYMBOL]("");
          return !execCalled;
        });
        if (!DELEGATES_TO_SYMBOL || !DELEGATES_TO_EXEC2 || FORCED9) {
          var uncurriedNativeRegExpMethod = uncurryThis11(/./[SYMBOL]);
          var methods = exec2(SYMBOL, ""[KEY], function(nativeMethod, regexp, str, arg2, forceStringMethod) {
            var uncurriedNativeMethod = uncurryThis11(nativeMethod);
            var $exec = regexp.exec;
            if ($exec === regexpExec2 || $exec === RegExpPrototype2.exec) {
              if (DELEGATES_TO_SYMBOL && !forceStringMethod) {
                return { done: true, value: uncurriedNativeRegExpMethod(regexp, str, arg2) };
              }
              return { done: true, value: uncurriedNativeMethod(str, regexp, arg2) };
            }
            return { done: false };
          });
          defineBuiltIn4(String.prototype, KEY, methods[0]);
          defineBuiltIn4(RegExpPrototype2, SYMBOL, methods[1]);
        }
        if (SHAM)
          createNonEnumerableProperty3(RegExpPrototype2[SYMBOL], "sham", true);
      };
    }
  });

  // node_modules/core-js/internals/advance-string-index.js
  var require_advance_string_index = __commonJS({
    "node_modules/core-js/internals/advance-string-index.js": function(exports, module) {
      "use strict";
      var charAt2 = require_string_multibyte().charAt;
      module.exports = function(S, index, unicode) {
        return index + (unicode ? charAt2(S, index).length : 1);
      };
    }
  });

  // node_modules/core-js/internals/get-substitution.js
  var require_get_substitution = __commonJS({
    "node_modules/core-js/internals/get-substitution.js": function(exports, module) {
      var uncurryThis11 = require_function_uncurry_this();
      var toObject5 = require_to_object();
      var floor = Math.floor;
      var charAt2 = uncurryThis11("".charAt);
      var replace = uncurryThis11("".replace);
      var stringSlice4 = uncurryThis11("".slice);
      var SUBSTITUTION_SYMBOLS = /\$([$&'`]|\d{1,2}|<[^>]*>)/g;
      var SUBSTITUTION_SYMBOLS_NO_NAMED = /\$([$&'`]|\d{1,2})/g;
      module.exports = function(matched, str, position, captures, namedCaptures, replacement) {
        var tailPos = position + matched.length;
        var m = captures.length;
        var symbols = SUBSTITUTION_SYMBOLS_NO_NAMED;
        if (namedCaptures !== void 0) {
          namedCaptures = toObject5(namedCaptures);
          symbols = SUBSTITUTION_SYMBOLS;
        }
        return replace(replacement, symbols, function(match, ch) {
          var capture;
          switch (charAt2(ch, 0)) {
            case "$":
              return "$";
            case "&":
              return matched;
            case "`":
              return stringSlice4(str, 0, position);
            case "'":
              return stringSlice4(str, tailPos);
            case "<":
              capture = namedCaptures[stringSlice4(ch, 1, -1)];
              break;
            default:
              var n = +ch;
              if (n === 0)
                return match;
              if (n > m) {
                var f = floor(n / 10);
                if (f === 0)
                  return match;
                if (f <= m)
                  return captures[f - 1] === void 0 ? charAt2(ch, 1) : captures[f - 1] + charAt2(ch, 1);
                return match;
              }
              capture = captures[n - 1];
          }
          return capture === void 0 ? "" : capture;
        });
      };
    }
  });

  // node_modules/core-js/internals/regexp-exec-abstract.js
  var require_regexp_exec_abstract = __commonJS({
    "node_modules/core-js/internals/regexp-exec-abstract.js": function(exports, module) {
      var call8 = require_function_call();
      var anObject10 = require_an_object();
      var isCallable4 = require_is_callable();
      var classof = require_classof_raw();
      var regexpExec2 = require_regexp_exec();
      var $TypeError = TypeError;
      module.exports = function(R, S) {
        var exec2 = R.exec;
        if (isCallable4(exec2)) {
          var result = call8(exec2, R, S);
          if (result !== null)
            anObject10(result);
          return result;
        }
        if (classof(R) === "RegExp")
          return call8(regexpExec2, R, S);
        throw $TypeError("RegExp#exec called on incompatible receiver");
      };
    }
  });

  // node_modules/core-js/internals/regexp-get-flags.js
  var require_regexp_get_flags = __commonJS({
    "node_modules/core-js/internals/regexp-get-flags.js": function(exports, module) {
      var call8 = require_function_call();
      var hasOwn4 = require_has_own_property();
      var isPrototypeOf3 = require_object_is_prototype_of();
      var regExpFlags = require_regexp_flags();
      var RegExpPrototype2 = RegExp.prototype;
      module.exports = function(R) {
        var flags = R.flags;
        return flags === void 0 && !("flags" in RegExpPrototype2) && !hasOwn4(R, "flags") && isPrototypeOf3(RegExpPrototype2, R) ? call8(regExpFlags, R) : flags;
      };
    }
  });

  // node_modules/core-js/internals/number-parse-float.js
  var require_number_parse_float = __commonJS({
    "node_modules/core-js/internals/number-parse-float.js": function(exports, module) {
      var global9 = require_global();
      var fails12 = require_fails();
      var uncurryThis11 = require_function_uncurry_this();
      var toString9 = require_to_string();
      var trim3 = require_string_trim().trim;
      var whitespaces = require_whitespaces();
      var charAt2 = uncurryThis11("".charAt);
      var $parseFloat2 = global9.parseFloat;
      var Symbol2 = global9.Symbol;
      var ITERATOR2 = Symbol2 && Symbol2.iterator;
      var FORCED9 = 1 / $parseFloat2(whitespaces + "-0") !== -Infinity || ITERATOR2 && !fails12(function() {
        $parseFloat2(Object(ITERATOR2));
      });
      module.exports = FORCED9 ? function parseFloat2(string) {
        var trimmedString = trim3(toString9(string));
        var result = $parseFloat2(trimmedString);
        return result === 0 && charAt2(trimmedString, 0) == "-" ? -0 : result;
      } : $parseFloat2;
    }
  });

  // node_modules/core-js/internals/does-not-exceed-safe-integer.js
  var require_does_not_exceed_safe_integer = __commonJS({
    "node_modules/core-js/internals/does-not-exceed-safe-integer.js": function(exports, module) {
      var $TypeError = TypeError;
      var MAX_SAFE_INTEGER = 9007199254740991;
      module.exports = function(it) {
        if (it > MAX_SAFE_INTEGER)
          throw $TypeError("Maximum allowed index exceeded");
        return it;
      };
    }
  });

  // node_modules/core-js/internals/array-method-has-species-support.js
  var require_array_method_has_species_support = __commonJS({
    "node_modules/core-js/internals/array-method-has-species-support.js": function(exports, module) {
      var fails12 = require_fails();
      var wellKnownSymbol6 = require_well_known_symbol();
      var V8_VERSION2 = require_engine_v8_version();
      var SPECIES2 = wellKnownSymbol6("species");
      module.exports = function(METHOD_NAME) {
        return V8_VERSION2 >= 51 || !fails12(function() {
          var array = [];
          var constructor = array.constructor = {};
          constructor[SPECIES2] = function() {
            return { foo: 1 };
          };
          return array[METHOD_NAME](Boolean).foo !== 1;
        });
      };
    }
  });

  // node_modules/core-js/internals/array-set-length.js
  var require_array_set_length = __commonJS({
    "node_modules/core-js/internals/array-set-length.js": function(exports, module) {
      "use strict";
      var DESCRIPTORS8 = require_descriptors();
      var isArray4 = require_is_array();
      var $TypeError = TypeError;
      var getOwnPropertyDescriptor3 = Object.getOwnPropertyDescriptor;
      var SILENT_ON_NON_WRITABLE_LENGTH_SET = DESCRIPTORS8 && !function() {
        if (this !== void 0)
          return true;
        try {
          Object.defineProperty([], "length", { writable: false }).length = 1;
        } catch (error) {
          return error instanceof TypeError;
        }
      }();
      module.exports = SILENT_ON_NON_WRITABLE_LENGTH_SET ? function(O, length) {
        if (isArray4(O) && !getOwnPropertyDescriptor3(O, "length").writable) {
          throw $TypeError("Cannot set read only .length");
        }
        return O.length = length;
      } : function(O, length) {
        return O.length = length;
      };
    }
  });

  // node_modules/core-js/internals/delete-property-or-throw.js
  var require_delete_property_or_throw = __commonJS({
    "node_modules/core-js/internals/delete-property-or-throw.js": function(exports, module) {
      "use strict";
      var tryToString = require_try_to_string();
      var $TypeError = TypeError;
      module.exports = function(O, P) {
        if (!delete O[P])
          throw $TypeError("Cannot delete property " + tryToString(P) + " of " + tryToString(O));
      };
    }
  });

  // node_modules/core-js/internals/array-for-each.js
  var require_array_for_each = __commonJS({
    "node_modules/core-js/internals/array-for-each.js": function(exports, module) {
      "use strict";
      var $forEach = require_array_iteration().forEach;
      var arrayMethodIsStrict5 = require_array_method_is_strict();
      var STRICT_METHOD2 = arrayMethodIsStrict5("forEach");
      module.exports = !STRICT_METHOD2 ? function forEach3(callbackfn) {
        return $forEach(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
      } : [].forEach;
    }
  });

  // node_modules/core-js/internals/function-bind.js
  var require_function_bind = __commonJS({
    "node_modules/core-js/internals/function-bind.js": function(exports, module) {
      "use strict";
      var uncurryThis11 = require_function_uncurry_this();
      var aCallable2 = require_a_callable();
      var isObject5 = require_is_object();
      var hasOwn4 = require_has_own_property();
      var arraySlice2 = require_array_slice();
      var NATIVE_BIND = require_function_bind_native();
      var $Function = Function;
      var concat3 = uncurryThis11([].concat);
      var join2 = uncurryThis11([].join);
      var factories = {};
      var construct2 = function(C, argsLength, args) {
        if (!hasOwn4(factories, argsLength)) {
          for (var list = [], i = 0; i < argsLength; i++)
            list[i] = "a[" + i + "]";
          factories[argsLength] = $Function("C,a", "return new C(" + join2(list, ",") + ")");
        }
        return factories[argsLength](C, args);
      };
      module.exports = NATIVE_BIND ? $Function.bind : function bind2(that) {
        var F = aCallable2(this);
        var Prototype = F.prototype;
        var partArgs = arraySlice2(arguments, 1);
        var boundFunction = function bound() {
          var args = concat3(partArgs, arraySlice2(arguments));
          return this instanceof boundFunction ? construct2(F, args.length, args) : F.apply(that, args);
        };
        if (isObject5(Prototype))
          boundFunction.prototype = Prototype;
        return boundFunction;
      };
    }
  });

  // node_modules/core-js/internals/a-constructor.js
  var require_a_constructor = __commonJS({
    "node_modules/core-js/internals/a-constructor.js": function(exports, module) {
      var isConstructor2 = require_is_constructor();
      var tryToString = require_try_to_string();
      var $TypeError = TypeError;
      module.exports = function(argument) {
        if (isConstructor2(argument))
          return argument;
        throw $TypeError(tryToString(argument) + " is not a constructor");
      };
    }
  });

  // node_modules/core-js/internals/string-trim-forced.js
  var require_string_trim_forced = __commonJS({
    "node_modules/core-js/internals/string-trim-forced.js": function(exports, module) {
      var PROPER_FUNCTION_NAME2 = require_function_name().PROPER;
      var fails12 = require_fails();
      var whitespaces = require_whitespaces();
      var non = "\u200B\x85\u180E";
      module.exports = function(METHOD_NAME) {
        return fails12(function() {
          return !!whitespaces[METHOD_NAME]() || non[METHOD_NAME]() !== non || PROPER_FUNCTION_NAME2 && whitespaces[METHOD_NAME].name !== METHOD_NAME;
        });
      };
    }
  });

  // node_modules/core-js/internals/is-data-descriptor.js
  var require_is_data_descriptor = __commonJS({
    "node_modules/core-js/internals/is-data-descriptor.js": function(exports, module) {
      var hasOwn4 = require_has_own_property();
      module.exports = function(descriptor) {
        return descriptor !== void 0 && (hasOwn4(descriptor, "value") || hasOwn4(descriptor, "writable"));
      };
    }
  });

  // node_modules/core-js/internals/iterator-close.js
  var require_iterator_close = __commonJS({
    "node_modules/core-js/internals/iterator-close.js": function(exports, module) {
      var call8 = require_function_call();
      var anObject10 = require_an_object();
      var getMethod5 = require_get_method();
      module.exports = function(iterator, kind, value) {
        var innerResult, innerError;
        anObject10(iterator);
        try {
          innerResult = getMethod5(iterator, "return");
          if (!innerResult) {
            if (kind === "throw")
              throw value;
            return value;
          }
          innerResult = call8(innerResult, iterator);
        } catch (error) {
          innerError = true;
          innerResult = error;
        }
        if (kind === "throw")
          throw value;
        if (innerError)
          throw innerResult;
        anObject10(innerResult);
        return value;
      };
    }
  });

  // node_modules/core-js/internals/call-with-safe-iteration-closing.js
  var require_call_with_safe_iteration_closing = __commonJS({
    "node_modules/core-js/internals/call-with-safe-iteration-closing.js": function(exports, module) {
      var anObject10 = require_an_object();
      var iteratorClose = require_iterator_close();
      module.exports = function(iterator, fn, value, ENTRIES) {
        try {
          return ENTRIES ? fn(anObject10(value)[0], value[1]) : fn(value);
        } catch (error) {
          iteratorClose(iterator, "throw", error);
        }
      };
    }
  });

  // node_modules/core-js/internals/is-array-iterator-method.js
  var require_is_array_iterator_method = __commonJS({
    "node_modules/core-js/internals/is-array-iterator-method.js": function(exports, module) {
      var wellKnownSymbol6 = require_well_known_symbol();
      var Iterators = require_iterators();
      var ITERATOR2 = wellKnownSymbol6("iterator");
      var ArrayPrototype = Array.prototype;
      module.exports = function(it) {
        return it !== void 0 && (Iterators.Array === it || ArrayPrototype[ITERATOR2] === it);
      };
    }
  });

  // node_modules/core-js/internals/get-iterator-method.js
  var require_get_iterator_method = __commonJS({
    "node_modules/core-js/internals/get-iterator-method.js": function(exports, module) {
      var classof = require_classof();
      var getMethod5 = require_get_method();
      var isNullOrUndefined5 = require_is_null_or_undefined();
      var Iterators = require_iterators();
      var wellKnownSymbol6 = require_well_known_symbol();
      var ITERATOR2 = wellKnownSymbol6("iterator");
      module.exports = function(it) {
        if (!isNullOrUndefined5(it))
          return getMethod5(it, ITERATOR2) || getMethod5(it, "@@iterator") || Iterators[classof(it)];
      };
    }
  });

  // node_modules/core-js/internals/get-iterator.js
  var require_get_iterator = __commonJS({
    "node_modules/core-js/internals/get-iterator.js": function(exports, module) {
      var call8 = require_function_call();
      var aCallable2 = require_a_callable();
      var anObject10 = require_an_object();
      var tryToString = require_try_to_string();
      var getIteratorMethod = require_get_iterator_method();
      var $TypeError = TypeError;
      module.exports = function(argument, usingIterator) {
        var iteratorMethod = arguments.length < 2 ? getIteratorMethod(argument) : usingIterator;
        if (aCallable2(iteratorMethod))
          return anObject10(call8(iteratorMethod, argument));
        throw $TypeError(tryToString(argument) + " is not iterable");
      };
    }
  });

  // node_modules/core-js/internals/array-from.js
  var require_array_from = __commonJS({
    "node_modules/core-js/internals/array-from.js": function(exports, module) {
      "use strict";
      var bind2 = require_function_bind_context();
      var call8 = require_function_call();
      var toObject5 = require_to_object();
      var callWithSafeIterationClosing = require_call_with_safe_iteration_closing();
      var isArrayIteratorMethod = require_is_array_iterator_method();
      var isConstructor2 = require_is_constructor();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var createProperty5 = require_create_property();
      var getIterator = require_get_iterator();
      var getIteratorMethod = require_get_iterator_method();
      var $Array2 = Array;
      module.exports = function from2(arrayLike) {
        var O = toObject5(arrayLike);
        var IS_CONSTRUCTOR = isConstructor2(this);
        var argumentsLength = arguments.length;
        var mapfn = argumentsLength > 1 ? arguments[1] : void 0;
        var mapping = mapfn !== void 0;
        if (mapping)
          mapfn = bind2(mapfn, argumentsLength > 2 ? arguments[2] : void 0);
        var iteratorMethod = getIteratorMethod(O);
        var index = 0;
        var length, result, step, iterator, next2, value;
        if (iteratorMethod && !(this === $Array2 && isArrayIteratorMethod(iteratorMethod))) {
          iterator = getIterator(O, iteratorMethod);
          next2 = iterator.next;
          result = IS_CONSTRUCTOR ? new this() : [];
          for (; !(step = call8(next2, iterator)).done; index++) {
            value = mapping ? callWithSafeIterationClosing(iterator, mapfn, [step.value, index], true) : step.value;
            createProperty5(result, index, value);
          }
        } else {
          length = lengthOfArrayLike4(O);
          result = IS_CONSTRUCTOR ? new this(length) : $Array2(length);
          for (; length > index; index++) {
            value = mapping ? mapfn(O[index], index) : O[index];
            createProperty5(result, index, value);
          }
        }
        result.length = index;
        return result;
      };
    }
  });

  // node_modules/core-js/internals/check-correctness-of-iteration.js
  var require_check_correctness_of_iteration = __commonJS({
    "node_modules/core-js/internals/check-correctness-of-iteration.js": function(exports, module) {
      var wellKnownSymbol6 = require_well_known_symbol();
      var ITERATOR2 = wellKnownSymbol6("iterator");
      var SAFE_CLOSING = false;
      try {
        called = 0;
        iteratorWithReturn = {
          next: function() {
            return { done: !!called++ };
          },
          "return": function() {
            SAFE_CLOSING = true;
          }
        };
        iteratorWithReturn[ITERATOR2] = function() {
          return this;
        };
        Array.from(iteratorWithReturn, function() {
          throw 2;
        });
      } catch (error) {
      }
      var called;
      var iteratorWithReturn;
      module.exports = function(exec2, SKIP_CLOSING) {
        if (!SKIP_CLOSING && !SAFE_CLOSING)
          return false;
        var ITERATION_SUPPORT = false;
        try {
          var object = {};
          object[ITERATOR2] = function() {
            return {
              next: function() {
                return { done: ITERATION_SUPPORT = true };
              }
            };
          };
          exec2(object);
        } catch (error) {
        }
        return ITERATION_SUPPORT;
      };
    }
  });

  // node_modules/core-js/internals/engine-is-node.js
  var require_engine_is_node = __commonJS({
    "node_modules/core-js/internals/engine-is-node.js": function(exports, module) {
      var classof = require_classof_raw();
      module.exports = typeof process != "undefined" && classof(process) == "process";
    }
  });

  // node_modules/core-js/internals/set-species.js
  var require_set_species = __commonJS({
    "node_modules/core-js/internals/set-species.js": function(exports, module) {
      "use strict";
      var getBuiltIn3 = require_get_built_in();
      var defineBuiltInAccessor3 = require_define_built_in_accessor();
      var wellKnownSymbol6 = require_well_known_symbol();
      var DESCRIPTORS8 = require_descriptors();
      var SPECIES2 = wellKnownSymbol6("species");
      module.exports = function(CONSTRUCTOR_NAME) {
        var Constructor = getBuiltIn3(CONSTRUCTOR_NAME);
        if (DESCRIPTORS8 && Constructor && !Constructor[SPECIES2]) {
          defineBuiltInAccessor3(Constructor, SPECIES2, {
            configurable: true,
            get: function() {
              return this;
            }
          });
        }
      };
    }
  });

  // node_modules/core-js/internals/an-instance.js
  var require_an_instance = __commonJS({
    "node_modules/core-js/internals/an-instance.js": function(exports, module) {
      var isPrototypeOf3 = require_object_is_prototype_of();
      var $TypeError = TypeError;
      module.exports = function(it, Prototype) {
        if (isPrototypeOf3(Prototype, it))
          return it;
        throw $TypeError("Incorrect invocation");
      };
    }
  });

  // node_modules/core-js/internals/species-constructor.js
  var require_species_constructor = __commonJS({
    "node_modules/core-js/internals/species-constructor.js": function(exports, module) {
      var anObject10 = require_an_object();
      var aConstructor2 = require_a_constructor();
      var isNullOrUndefined5 = require_is_null_or_undefined();
      var wellKnownSymbol6 = require_well_known_symbol();
      var SPECIES2 = wellKnownSymbol6("species");
      module.exports = function(O, defaultConstructor) {
        var C = anObject10(O).constructor;
        var S;
        return C === void 0 || isNullOrUndefined5(S = anObject10(C)[SPECIES2]) ? defaultConstructor : aConstructor2(S);
      };
    }
  });

  // node_modules/core-js/internals/validate-arguments-length.js
  var require_validate_arguments_length = __commonJS({
    "node_modules/core-js/internals/validate-arguments-length.js": function(exports, module) {
      var $TypeError = TypeError;
      module.exports = function(passed, required) {
        if (passed < required)
          throw $TypeError("Not enough arguments");
        return passed;
      };
    }
  });

  // node_modules/core-js/internals/engine-is-ios.js
  var require_engine_is_ios = __commonJS({
    "node_modules/core-js/internals/engine-is-ios.js": function(exports, module) {
      var userAgent2 = require_engine_user_agent();
      module.exports = /(?:ipad|iphone|ipod).*applewebkit/i.test(userAgent2);
    }
  });

  // node_modules/core-js/internals/task.js
  var require_task = __commonJS({
    "node_modules/core-js/internals/task.js": function(exports, module) {
      var global9 = require_global();
      var apply4 = require_function_apply();
      var bind2 = require_function_bind_context();
      var isCallable4 = require_is_callable();
      var hasOwn4 = require_has_own_property();
      var fails12 = require_fails();
      var html = require_html();
      var arraySlice2 = require_array_slice();
      var createElement = require_document_create_element();
      var validateArgumentsLength = require_validate_arguments_length();
      var IS_IOS = require_engine_is_ios();
      var IS_NODE2 = require_engine_is_node();
      var set = global9.setImmediate;
      var clear = global9.clearImmediate;
      var process2 = global9.process;
      var Dispatch = global9.Dispatch;
      var Function2 = global9.Function;
      var MessageChannel = global9.MessageChannel;
      var String2 = global9.String;
      var counter = 0;
      var queue = {};
      var ONREADYSTATECHANGE = "onreadystatechange";
      var $location;
      var defer;
      var channel;
      var port;
      fails12(function() {
        $location = global9.location;
      });
      var run = function(id) {
        if (hasOwn4(queue, id)) {
          var fn = queue[id];
          delete queue[id];
          fn();
        }
      };
      var runner = function(id) {
        return function() {
          run(id);
        };
      };
      var eventListener = function(event) {
        run(event.data);
      };
      var globalPostMessageDefer = function(id) {
        global9.postMessage(String2(id), $location.protocol + "//" + $location.host);
      };
      if (!set || !clear) {
        set = function setImmediate(handler) {
          validateArgumentsLength(arguments.length, 1);
          var fn = isCallable4(handler) ? handler : Function2(handler);
          var args = arraySlice2(arguments, 1);
          queue[++counter] = function() {
            apply4(fn, void 0, args);
          };
          defer(counter);
          return counter;
        };
        clear = function clearImmediate(id) {
          delete queue[id];
        };
        if (IS_NODE2) {
          defer = function(id) {
            process2.nextTick(runner(id));
          };
        } else if (Dispatch && Dispatch.now) {
          defer = function(id) {
            Dispatch.now(runner(id));
          };
        } else if (MessageChannel && !IS_IOS) {
          channel = new MessageChannel();
          port = channel.port2;
          channel.port1.onmessage = eventListener;
          defer = bind2(port.postMessage, port);
        } else if (global9.addEventListener && isCallable4(global9.postMessage) && !global9.importScripts && $location && $location.protocol !== "file:" && !fails12(globalPostMessageDefer)) {
          defer = globalPostMessageDefer;
          global9.addEventListener("message", eventListener, false);
        } else if (ONREADYSTATECHANGE in createElement("script")) {
          defer = function(id) {
            html.appendChild(createElement("script"))[ONREADYSTATECHANGE] = function() {
              html.removeChild(this);
              run(id);
            };
          };
        } else {
          defer = function(id) {
            setTimeout(runner(id), 0);
          };
        }
      }
      module.exports = {
        set: set,
        clear: clear
      };
    }
  });

  // node_modules/core-js/internals/queue.js
  var require_queue = __commonJS({
    "node_modules/core-js/internals/queue.js": function(exports, module) {
      var Queue = function() {
        this.head = null;
        this.tail = null;
      };
      Queue.prototype = {
        add: function(item) {
          var entry = { item: item, next: null };
          var tail = this.tail;
          if (tail)
            tail.next = entry;
          else
            this.head = entry;
          this.tail = entry;
        },
        get: function() {
          var entry = this.head;
          if (entry) {
            var next2 = this.head = entry.next;
            if (next2 === null)
              this.tail = null;
            return entry.item;
          }
        }
      };
      module.exports = Queue;
    }
  });

  // node_modules/core-js/internals/engine-is-ios-pebble.js
  var require_engine_is_ios_pebble = __commonJS({
    "node_modules/core-js/internals/engine-is-ios-pebble.js": function(exports, module) {
      var userAgent2 = require_engine_user_agent();
      module.exports = /ipad|iphone|ipod/i.test(userAgent2) && typeof Pebble != "undefined";
    }
  });

  // node_modules/core-js/internals/engine-is-webos-webkit.js
  var require_engine_is_webos_webkit = __commonJS({
    "node_modules/core-js/internals/engine-is-webos-webkit.js": function(exports, module) {
      var userAgent2 = require_engine_user_agent();
      module.exports = /web0s(?!.*chrome)/i.test(userAgent2);
    }
  });

  // node_modules/core-js/internals/microtask.js
  var require_microtask = __commonJS({
    "node_modules/core-js/internals/microtask.js": function(exports, module) {
      var global9 = require_global();
      var bind2 = require_function_bind_context();
      var getOwnPropertyDescriptor3 = require_object_get_own_property_descriptor().f;
      var macrotask = require_task().set;
      var Queue = require_queue();
      var IS_IOS = require_engine_is_ios();
      var IS_IOS_PEBBLE = require_engine_is_ios_pebble();
      var IS_WEBOS_WEBKIT = require_engine_is_webos_webkit();
      var IS_NODE2 = require_engine_is_node();
      var MutationObserver2 = global9.MutationObserver || global9.WebKitMutationObserver;
      var document2 = global9.document;
      var process2 = global9.process;
      var Promise2 = global9.Promise;
      var queueMicrotaskDescriptor = getOwnPropertyDescriptor3(global9, "queueMicrotask");
      var microtask = queueMicrotaskDescriptor && queueMicrotaskDescriptor.value;
      var notify;
      var toggle;
      var node;
      var promise;
      var then;
      if (!microtask) {
        queue = new Queue();
        flush = function() {
          var parent, fn;
          if (IS_NODE2 && (parent = process2.domain))
            parent.exit();
          while (fn = queue.get())
            try {
              fn();
            } catch (error) {
              if (queue.head)
                notify();
              throw error;
            }
          if (parent)
            parent.enter();
        };
        if (!IS_IOS && !IS_NODE2 && !IS_WEBOS_WEBKIT && MutationObserver2 && document2) {
          toggle = true;
          node = document2.createTextNode("");
          new MutationObserver2(flush).observe(node, { characterData: true });
          notify = function() {
            node.data = toggle = !toggle;
          };
        } else if (!IS_IOS_PEBBLE && Promise2 && Promise2.resolve) {
          promise = Promise2.resolve(void 0);
          promise.constructor = Promise2;
          then = bind2(promise.then, promise);
          notify = function() {
            then(flush);
          };
        } else if (IS_NODE2) {
          notify = function() {
            process2.nextTick(flush);
          };
        } else {
          macrotask = bind2(macrotask, global9);
          notify = function() {
            macrotask(flush);
          };
        }
        microtask = function(fn) {
          if (!queue.head)
            notify();
          queue.add(fn);
        };
      }
      var queue;
      var flush;
      module.exports = microtask;
    }
  });

  // node_modules/core-js/internals/host-report-errors.js
  var require_host_report_errors = __commonJS({
    "node_modules/core-js/internals/host-report-errors.js": function(exports, module) {
      module.exports = function(a, b) {
        try {
          arguments.length == 1 ? console.error(a) : console.error(a, b);
        } catch (error) {
        }
      };
    }
  });

  // node_modules/core-js/internals/perform.js
  var require_perform = __commonJS({
    "node_modules/core-js/internals/perform.js": function(exports, module) {
      module.exports = function(exec2) {
        try {
          return { error: false, value: exec2() };
        } catch (error) {
          return { error: true, value: error };
        }
      };
    }
  });

  // node_modules/core-js/internals/promise-native-constructor.js
  var require_promise_native_constructor = __commonJS({
    "node_modules/core-js/internals/promise-native-constructor.js": function(exports, module) {
      var global9 = require_global();
      module.exports = global9.Promise;
    }
  });

  // node_modules/core-js/internals/engine-is-deno.js
  var require_engine_is_deno = __commonJS({
    "node_modules/core-js/internals/engine-is-deno.js": function(exports, module) {
      module.exports = typeof Deno == "object" && Deno && typeof Deno.version == "object";
    }
  });

  // node_modules/core-js/internals/engine-is-browser.js
  var require_engine_is_browser = __commonJS({
    "node_modules/core-js/internals/engine-is-browser.js": function(exports, module) {
      var IS_DENO = require_engine_is_deno();
      var IS_NODE2 = require_engine_is_node();
      module.exports = !IS_DENO && !IS_NODE2 && typeof window == "object" && typeof document == "object";
    }
  });

  // node_modules/core-js/internals/promise-constructor-detection.js
  var require_promise_constructor_detection = __commonJS({
    "node_modules/core-js/internals/promise-constructor-detection.js": function(exports, module) {
      var global9 = require_global();
      var NativePromiseConstructor = require_promise_native_constructor();
      var isCallable4 = require_is_callable();
      var isForced2 = require_is_forced();
      var inspectSource = require_inspect_source();
      var wellKnownSymbol6 = require_well_known_symbol();
      var IS_BROWSER = require_engine_is_browser();
      var IS_DENO = require_engine_is_deno();
      var IS_PURE2 = require_is_pure();
      var V8_VERSION2 = require_engine_v8_version();
      var NativePromisePrototype = NativePromiseConstructor && NativePromiseConstructor.prototype;
      var SPECIES2 = wellKnownSymbol6("species");
      var SUBCLASSING = false;
      var NATIVE_PROMISE_REJECTION_EVENT = isCallable4(global9.PromiseRejectionEvent);
      var FORCED_PROMISE_CONSTRUCTOR = isForced2("Promise", function() {
        var PROMISE_CONSTRUCTOR_SOURCE = inspectSource(NativePromiseConstructor);
        var GLOBAL_CORE_JS_PROMISE = PROMISE_CONSTRUCTOR_SOURCE !== String(NativePromiseConstructor);
        if (!GLOBAL_CORE_JS_PROMISE && V8_VERSION2 === 66)
          return true;
        if (IS_PURE2 && !(NativePromisePrototype["catch"] && NativePromisePrototype["finally"]))
          return true;
        if (!V8_VERSION2 || V8_VERSION2 < 51 || !/native code/.test(PROMISE_CONSTRUCTOR_SOURCE)) {
          var promise = new NativePromiseConstructor(function(resolve) {
            resolve(1);
          });
          var FakePromise = function(exec2) {
            exec2(function() {
            }, function() {
            });
          };
          var constructor = promise.constructor = {};
          constructor[SPECIES2] = FakePromise;
          SUBCLASSING = promise.then(function() {
          }) instanceof FakePromise;
          if (!SUBCLASSING)
            return true;
        }
        return !GLOBAL_CORE_JS_PROMISE && (IS_BROWSER || IS_DENO) && !NATIVE_PROMISE_REJECTION_EVENT;
      });
      module.exports = {
        CONSTRUCTOR: FORCED_PROMISE_CONSTRUCTOR,
        REJECTION_EVENT: NATIVE_PROMISE_REJECTION_EVENT,
        SUBCLASSING: SUBCLASSING
      };
    }
  });

  // node_modules/core-js/internals/new-promise-capability.js
  var require_new_promise_capability = __commonJS({
    "node_modules/core-js/internals/new-promise-capability.js": function(exports, module) {
      "use strict";
      var aCallable2 = require_a_callable();
      var $TypeError = TypeError;
      var PromiseCapability = function(C) {
        var resolve, reject;
        this.promise = new C(function($$resolve, $$reject) {
          if (resolve !== void 0 || reject !== void 0)
            throw $TypeError("Bad Promise constructor");
          resolve = $$resolve;
          reject = $$reject;
        });
        this.resolve = aCallable2(resolve);
        this.reject = aCallable2(reject);
      };
      module.exports.f = function(C) {
        return new PromiseCapability(C);
      };
    }
  });

  // node_modules/core-js/modules/es.promise.constructor.js
  var require_es_promise_constructor = __commonJS({
    "node_modules/core-js/modules/es.promise.constructor.js": function() {
      "use strict";
      var $77 = require_export();
      var IS_PURE2 = require_is_pure();
      var IS_NODE2 = require_engine_is_node();
      var global9 = require_global();
      var call8 = require_function_call();
      var defineBuiltIn4 = require_define_built_in();
      var setPrototypeOf2 = require_object_set_prototype_of();
      var setToStringTag5 = require_set_to_string_tag();
      var setSpecies2 = require_set_species();
      var aCallable2 = require_a_callable();
      var isCallable4 = require_is_callable();
      var isObject5 = require_is_object();
      var anInstance = require_an_instance();
      var speciesConstructor3 = require_species_constructor();
      var task = require_task().set;
      var microtask = require_microtask();
      var hostReportErrors = require_host_report_errors();
      var perform2 = require_perform();
      var Queue = require_queue();
      var InternalStateModule2 = require_internal_state();
      var NativePromiseConstructor = require_promise_native_constructor();
      var PromiseConstructorDetection = require_promise_constructor_detection();
      var newPromiseCapabilityModule2 = require_new_promise_capability();
      var PROMISE = "Promise";
      var FORCED_PROMISE_CONSTRUCTOR = PromiseConstructorDetection.CONSTRUCTOR;
      var NATIVE_PROMISE_REJECTION_EVENT = PromiseConstructorDetection.REJECTION_EVENT;
      var NATIVE_PROMISE_SUBCLASSING = PromiseConstructorDetection.SUBCLASSING;
      var getInternalPromiseState = InternalStateModule2.getterFor(PROMISE);
      var setInternalState2 = InternalStateModule2.set;
      var NativePromisePrototype = NativePromiseConstructor && NativePromiseConstructor.prototype;
      var PromiseConstructor = NativePromiseConstructor;
      var PromisePrototype = NativePromisePrototype;
      var TypeError3 = global9.TypeError;
      var document2 = global9.document;
      var process2 = global9.process;
      var newPromiseCapability = newPromiseCapabilityModule2.f;
      var newGenericPromiseCapability = newPromiseCapability;
      var DISPATCH_EVENT = !!(document2 && document2.createEvent && global9.dispatchEvent);
      var UNHANDLED_REJECTION = "unhandledrejection";
      var REJECTION_HANDLED = "rejectionhandled";
      var PENDING = 0;
      var FULFILLED = 1;
      var REJECTED = 2;
      var HANDLED = 1;
      var UNHANDLED = 2;
      var Internal;
      var OwnPromiseCapability;
      var PromiseWrapper;
      var nativeThen;
      var isThenable = function(it) {
        var then;
        return isObject5(it) && isCallable4(then = it.then) ? then : false;
      };
      var callReaction = function(reaction, state) {
        var value = state.value;
        var ok = state.state == FULFILLED;
        var handler = ok ? reaction.ok : reaction.fail;
        var resolve = reaction.resolve;
        var reject = reaction.reject;
        var domain = reaction.domain;
        var result, then, exited;
        try {
          if (handler) {
            if (!ok) {
              if (state.rejection === UNHANDLED)
                onHandleUnhandled(state);
              state.rejection = HANDLED;
            }
            if (handler === true)
              result = value;
            else {
              if (domain)
                domain.enter();
              result = handler(value);
              if (domain) {
                domain.exit();
                exited = true;
              }
            }
            if (result === reaction.promise) {
              reject(TypeError3("Promise-chain cycle"));
            } else if (then = isThenable(result)) {
              call8(then, result, resolve, reject);
            } else
              resolve(result);
          } else
            reject(value);
        } catch (error) {
          if (domain && !exited)
            domain.exit();
          reject(error);
        }
      };
      var notify = function(state, isReject) {
        if (state.notified)
          return;
        state.notified = true;
        microtask(function() {
          var reactions = state.reactions;
          var reaction;
          while (reaction = reactions.get()) {
            callReaction(reaction, state);
          }
          state.notified = false;
          if (isReject && !state.rejection)
            onUnhandled(state);
        });
      };
      var dispatchEvent = function(name, promise, reason) {
        var event, handler;
        if (DISPATCH_EVENT) {
          event = document2.createEvent("Event");
          event.promise = promise;
          event.reason = reason;
          event.initEvent(name, false, true);
          global9.dispatchEvent(event);
        } else
          event = { promise: promise, reason: reason };
        if (!NATIVE_PROMISE_REJECTION_EVENT && (handler = global9["on" + name]))
          handler(event);
        else if (name === UNHANDLED_REJECTION)
          hostReportErrors("Unhandled promise rejection", reason);
      };
      var onUnhandled = function(state) {
        call8(task, global9, function() {
          var promise = state.facade;
          var value = state.value;
          var IS_UNHANDLED = isUnhandled(state);
          var result;
          if (IS_UNHANDLED) {
            result = perform2(function() {
              if (IS_NODE2) {
                process2.emit("unhandledRejection", value, promise);
              } else
                dispatchEvent(UNHANDLED_REJECTION, promise, value);
            });
            state.rejection = IS_NODE2 || isUnhandled(state) ? UNHANDLED : HANDLED;
            if (result.error)
              throw result.value;
          }
        });
      };
      var isUnhandled = function(state) {
        return state.rejection !== HANDLED && !state.parent;
      };
      var onHandleUnhandled = function(state) {
        call8(task, global9, function() {
          var promise = state.facade;
          if (IS_NODE2) {
            process2.emit("rejectionHandled", promise);
          } else
            dispatchEvent(REJECTION_HANDLED, promise, state.value);
        });
      };
      var bind2 = function(fn, state, unwrap) {
        return function(value) {
          fn(state, value, unwrap);
        };
      };
      var internalReject = function(state, value, unwrap) {
        if (state.done)
          return;
        state.done = true;
        if (unwrap)
          state = unwrap;
        state.value = value;
        state.state = REJECTED;
        notify(state, true);
      };
      var internalResolve = function(state, value, unwrap) {
        if (state.done)
          return;
        state.done = true;
        if (unwrap)
          state = unwrap;
        try {
          if (state.facade === value)
            throw TypeError3("Promise can't be resolved itself");
          var then = isThenable(value);
          if (then) {
            microtask(function() {
              var wrapper = { done: false };
              try {
                call8(
                  then,
                  value,
                  bind2(internalResolve, wrapper, state),
                  bind2(internalReject, wrapper, state)
                );
              } catch (error) {
                internalReject(wrapper, error, state);
              }
            });
          } else {
            state.value = value;
            state.state = FULFILLED;
            notify(state, false);
          }
        } catch (error) {
          internalReject({ done: false }, error, state);
        }
      };
      if (FORCED_PROMISE_CONSTRUCTOR) {
        PromiseConstructor = function Promise2(executor) {
          anInstance(this, PromisePrototype);
          aCallable2(executor);
          call8(Internal, this);
          var state = getInternalPromiseState(this);
          try {
            executor(bind2(internalResolve, state), bind2(internalReject, state));
          } catch (error) {
            internalReject(state, error);
          }
        };
        PromisePrototype = PromiseConstructor.prototype;
        Internal = function Promise2(executor) {
          setInternalState2(this, {
            type: PROMISE,
            done: false,
            notified: false,
            parent: false,
            reactions: new Queue(),
            rejection: false,
            state: PENDING,
            value: void 0
          });
        };
        Internal.prototype = defineBuiltIn4(PromisePrototype, "then", function then(onFulfilled, onRejected) {
          var state = getInternalPromiseState(this);
          var reaction = newPromiseCapability(speciesConstructor3(this, PromiseConstructor));
          state.parent = true;
          reaction.ok = isCallable4(onFulfilled) ? onFulfilled : true;
          reaction.fail = isCallable4(onRejected) && onRejected;
          reaction.domain = IS_NODE2 ? process2.domain : void 0;
          if (state.state == PENDING)
            state.reactions.add(reaction);
          else
            microtask(function() {
              callReaction(reaction, state);
            });
          return reaction.promise;
        });
        OwnPromiseCapability = function() {
          var promise = new Internal();
          var state = getInternalPromiseState(promise);
          this.promise = promise;
          this.resolve = bind2(internalResolve, state);
          this.reject = bind2(internalReject, state);
        };
        newPromiseCapabilityModule2.f = newPromiseCapability = function(C) {
          return C === PromiseConstructor || C === PromiseWrapper ? new OwnPromiseCapability(C) : newGenericPromiseCapability(C);
        };
        if (!IS_PURE2 && isCallable4(NativePromiseConstructor) && NativePromisePrototype !== Object.prototype) {
          nativeThen = NativePromisePrototype.then;
          if (!NATIVE_PROMISE_SUBCLASSING) {
            defineBuiltIn4(NativePromisePrototype, "then", function then(onFulfilled, onRejected) {
              var that = this;
              return new PromiseConstructor(function(resolve, reject) {
                call8(nativeThen, that, resolve, reject);
              }).then(onFulfilled, onRejected);
            }, { unsafe: true });
          }
          try {
            delete NativePromisePrototype.constructor;
          } catch (error) {
          }
          if (setPrototypeOf2) {
            setPrototypeOf2(NativePromisePrototype, PromisePrototype);
          }
        }
      }
      $77({ global: true, constructor: true, wrap: true, forced: FORCED_PROMISE_CONSTRUCTOR }, {
        Promise: PromiseConstructor
      });
      setToStringTag5(PromiseConstructor, PROMISE, false, true);
      setSpecies2(PROMISE);
    }
  });

  // node_modules/core-js/internals/iterate.js
  var require_iterate = __commonJS({
    "node_modules/core-js/internals/iterate.js": function(exports, module) {
      var bind2 = require_function_bind_context();
      var call8 = require_function_call();
      var anObject10 = require_an_object();
      var tryToString = require_try_to_string();
      var isArrayIteratorMethod = require_is_array_iterator_method();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var isPrototypeOf3 = require_object_is_prototype_of();
      var getIterator = require_get_iterator();
      var getIteratorMethod = require_get_iterator_method();
      var iteratorClose = require_iterator_close();
      var $TypeError = TypeError;
      var Result = function(stopped, result) {
        this.stopped = stopped;
        this.result = result;
      };
      var ResultPrototype = Result.prototype;
      module.exports = function(iterable, unboundFunction, options) {
        var that = options && options.that;
        var AS_ENTRIES = !!(options && options.AS_ENTRIES);
        var IS_RECORD = !!(options && options.IS_RECORD);
        var IS_ITERATOR = !!(options && options.IS_ITERATOR);
        var INTERRUPTED = !!(options && options.INTERRUPTED);
        var fn = bind2(unboundFunction, that);
        var iterator, iterFn, index, length, result, next2, step;
        var stop = function(condition) {
          if (iterator)
            iteratorClose(iterator, "normal", condition);
          return new Result(true, condition);
        };
        var callFn = function(value) {
          if (AS_ENTRIES) {
            anObject10(value);
            return INTERRUPTED ? fn(value[0], value[1], stop) : fn(value[0], value[1]);
          }
          return INTERRUPTED ? fn(value, stop) : fn(value);
        };
        if (IS_RECORD) {
          iterator = iterable.iterator;
        } else if (IS_ITERATOR) {
          iterator = iterable;
        } else {
          iterFn = getIteratorMethod(iterable);
          if (!iterFn)
            throw $TypeError(tryToString(iterable) + " is not iterable");
          if (isArrayIteratorMethod(iterFn)) {
            for (index = 0, length = lengthOfArrayLike4(iterable); length > index; index++) {
              result = callFn(iterable[index]);
              if (result && isPrototypeOf3(ResultPrototype, result))
                return result;
            }
            return new Result(false);
          }
          iterator = getIterator(iterable, iterFn);
        }
        next2 = IS_RECORD ? iterable.next : iterator.next;
        while (!(step = call8(next2, iterator)).done) {
          try {
            result = callFn(step.value);
          } catch (error) {
            iteratorClose(iterator, "throw", error);
          }
          if (typeof result == "object" && result && isPrototypeOf3(ResultPrototype, result))
            return result;
        }
        return new Result(false);
      };
    }
  });

  // node_modules/core-js/internals/promise-statics-incorrect-iteration.js
  var require_promise_statics_incorrect_iteration = __commonJS({
    "node_modules/core-js/internals/promise-statics-incorrect-iteration.js": function(exports, module) {
      var NativePromiseConstructor = require_promise_native_constructor();
      var checkCorrectnessOfIteration2 = require_check_correctness_of_iteration();
      var FORCED_PROMISE_CONSTRUCTOR = require_promise_constructor_detection().CONSTRUCTOR;
      module.exports = FORCED_PROMISE_CONSTRUCTOR || !checkCorrectnessOfIteration2(function(iterable) {
        NativePromiseConstructor.all(iterable).then(void 0, function() {
        });
      });
    }
  });

  // node_modules/core-js/modules/es.promise.all.js
  var require_es_promise_all = __commonJS({
    "node_modules/core-js/modules/es.promise.all.js": function() {
      "use strict";
      var $77 = require_export();
      var call8 = require_function_call();
      var aCallable2 = require_a_callable();
      var newPromiseCapabilityModule2 = require_new_promise_capability();
      var perform2 = require_perform();
      var iterate2 = require_iterate();
      var PROMISE_STATICS_INCORRECT_ITERATION2 = require_promise_statics_incorrect_iteration();
      $77({ target: "Promise", stat: true, forced: PROMISE_STATICS_INCORRECT_ITERATION2 }, {
        all: function all(iterable) {
          var C = this;
          var capability = newPromiseCapabilityModule2.f(C);
          var resolve = capability.resolve;
          var reject = capability.reject;
          var result = perform2(function() {
            var $promiseResolve = aCallable2(C.resolve);
            var values2 = [];
            var counter = 0;
            var remaining = 1;
            iterate2(iterable, function(promise) {
              var index = counter++;
              var alreadyCalled = false;
              remaining++;
              call8($promiseResolve, C, promise).then(function(value) {
                if (alreadyCalled)
                  return;
                alreadyCalled = true;
                values2[index] = value;
                --remaining || resolve(values2);
              }, reject);
            });
            --remaining || resolve(values2);
          });
          if (result.error)
            reject(result.value);
          return capability.promise;
        }
      });
    }
  });

  // node_modules/core-js/modules/es.promise.catch.js
  var require_es_promise_catch = __commonJS({
    "node_modules/core-js/modules/es.promise.catch.js": function() {
      "use strict";
      var $77 = require_export();
      var IS_PURE2 = require_is_pure();
      var FORCED_PROMISE_CONSTRUCTOR = require_promise_constructor_detection().CONSTRUCTOR;
      var NativePromiseConstructor = require_promise_native_constructor();
      var getBuiltIn3 = require_get_built_in();
      var isCallable4 = require_is_callable();
      var defineBuiltIn4 = require_define_built_in();
      var NativePromisePrototype = NativePromiseConstructor && NativePromiseConstructor.prototype;
      $77({ target: "Promise", proto: true, forced: FORCED_PROMISE_CONSTRUCTOR, real: true }, {
        "catch": function(onRejected) {
          return this.then(void 0, onRejected);
        }
      });
      if (!IS_PURE2 && isCallable4(NativePromiseConstructor)) {
        method = getBuiltIn3("Promise").prototype["catch"];
        if (NativePromisePrototype["catch"] !== method) {
          defineBuiltIn4(NativePromisePrototype, "catch", method, { unsafe: true });
        }
      }
      var method;
    }
  });

  // node_modules/core-js/modules/es.promise.race.js
  var require_es_promise_race = __commonJS({
    "node_modules/core-js/modules/es.promise.race.js": function() {
      "use strict";
      var $77 = require_export();
      var call8 = require_function_call();
      var aCallable2 = require_a_callable();
      var newPromiseCapabilityModule2 = require_new_promise_capability();
      var perform2 = require_perform();
      var iterate2 = require_iterate();
      var PROMISE_STATICS_INCORRECT_ITERATION2 = require_promise_statics_incorrect_iteration();
      $77({ target: "Promise", stat: true, forced: PROMISE_STATICS_INCORRECT_ITERATION2 }, {
        race: function race(iterable) {
          var C = this;
          var capability = newPromiseCapabilityModule2.f(C);
          var reject = capability.reject;
          var result = perform2(function() {
            var $promiseResolve = aCallable2(C.resolve);
            iterate2(iterable, function(promise) {
              call8($promiseResolve, C, promise).then(capability.resolve, reject);
            });
          });
          if (result.error)
            reject(result.value);
          return capability.promise;
        }
      });
    }
  });

  // node_modules/core-js/modules/es.promise.reject.js
  var require_es_promise_reject = __commonJS({
    "node_modules/core-js/modules/es.promise.reject.js": function() {
      "use strict";
      var $77 = require_export();
      var call8 = require_function_call();
      var newPromiseCapabilityModule2 = require_new_promise_capability();
      var FORCED_PROMISE_CONSTRUCTOR = require_promise_constructor_detection().CONSTRUCTOR;
      $77({ target: "Promise", stat: true, forced: FORCED_PROMISE_CONSTRUCTOR }, {
        reject: function reject(r) {
          var capability = newPromiseCapabilityModule2.f(this);
          call8(capability.reject, void 0, r);
          return capability.promise;
        }
      });
    }
  });

  // node_modules/core-js/internals/promise-resolve.js
  var require_promise_resolve = __commonJS({
    "node_modules/core-js/internals/promise-resolve.js": function(exports, module) {
      var anObject10 = require_an_object();
      var isObject5 = require_is_object();
      var newPromiseCapability = require_new_promise_capability();
      module.exports = function(C, x) {
        anObject10(C);
        if (isObject5(x) && x.constructor === C)
          return x;
        var promiseCapability = newPromiseCapability.f(C);
        var resolve = promiseCapability.resolve;
        resolve(x);
        return promiseCapability.promise;
      };
    }
  });

  // node_modules/core-js/modules/es.promise.resolve.js
  var require_es_promise_resolve = __commonJS({
    "node_modules/core-js/modules/es.promise.resolve.js": function() {
      "use strict";
      var $77 = require_export();
      var getBuiltIn3 = require_get_built_in();
      var IS_PURE2 = require_is_pure();
      var NativePromiseConstructor = require_promise_native_constructor();
      var FORCED_PROMISE_CONSTRUCTOR = require_promise_constructor_detection().CONSTRUCTOR;
      var promiseResolve = require_promise_resolve();
      var PromiseConstructorWrapper = getBuiltIn3("Promise");
      var CHECK_WRAPPER = IS_PURE2 && !FORCED_PROMISE_CONSTRUCTOR;
      $77({ target: "Promise", stat: true, forced: IS_PURE2 || FORCED_PROMISE_CONSTRUCTOR }, {
        resolve: function resolve(x) {
          return promiseResolve(CHECK_WRAPPER && this === PromiseConstructorWrapper ? NativePromiseConstructor : this, x);
        }
      });
    }
  });

  // node_modules/core-js/internals/same-value.js
  var require_same_value = __commonJS({
    "node_modules/core-js/internals/same-value.js": function(exports, module) {
      module.exports = Object.is || function is(x, y) {
        return x === y ? x !== 0 || 1 / x === 1 / y : x != x && y != y;
      };
    }
  });

  // node_modules/core-js/internals/object-to-array.js
  var require_object_to_array = __commonJS({
    "node_modules/core-js/internals/object-to-array.js": function(exports, module) {
      var DESCRIPTORS8 = require_descriptors();
      var uncurryThis11 = require_function_uncurry_this();
      var objectKeys = require_object_keys();
      var toIndexedObject5 = require_to_indexed_object();
      var $propertyIsEnumerable = require_object_property_is_enumerable().f;
      var propertyIsEnumerable = uncurryThis11($propertyIsEnumerable);
      var push4 = uncurryThis11([].push);
      var createMethod = function(TO_ENTRIES) {
        return function(it) {
          var O = toIndexedObject5(it);
          var keys2 = objectKeys(O);
          var length = keys2.length;
          var i = 0;
          var result = [];
          var key;
          while (length > i) {
            key = keys2[i++];
            if (!DESCRIPTORS8 || propertyIsEnumerable(O, key)) {
              push4(result, TO_ENTRIES ? [key, O[key]] : O[key]);
            }
          }
          return result;
        };
      };
      module.exports = {
        entries: createMethod(true),
        values: createMethod(false)
      };
    }
  });

  // node_modules/core-js/internals/is-regexp.js
  var require_is_regexp = __commonJS({
    "node_modules/core-js/internals/is-regexp.js": function(exports, module) {
      var isObject5 = require_is_object();
      var classof = require_classof_raw();
      var wellKnownSymbol6 = require_well_known_symbol();
      var MATCH = wellKnownSymbol6("match");
      module.exports = function(it) {
        var isRegExp2;
        return isObject5(it) && ((isRegExp2 = it[MATCH]) !== void 0 ? !!isRegExp2 : classof(it) == "RegExp");
      };
    }
  });

  // node_modules/core-js/internals/array-buffer-basic-detection.js
  var require_array_buffer_basic_detection = __commonJS({
    "node_modules/core-js/internals/array-buffer-basic-detection.js": function(exports, module) {
      module.exports = typeof ArrayBuffer != "undefined" && typeof DataView != "undefined";
    }
  });

  // node_modules/core-js/internals/define-built-ins.js
  var require_define_built_ins = __commonJS({
    "node_modules/core-js/internals/define-built-ins.js": function(exports, module) {
      var defineBuiltIn4 = require_define_built_in();
      module.exports = function(target, src, options) {
        for (var key in src)
          defineBuiltIn4(target, key, src[key], options);
        return target;
      };
    }
  });

  // node_modules/core-js/internals/to-index.js
  var require_to_index = __commonJS({
    "node_modules/core-js/internals/to-index.js": function(exports, module) {
      var toIntegerOrInfinity3 = require_to_integer_or_infinity();
      var toLength5 = require_to_length();
      var $RangeError = RangeError;
      module.exports = function(it) {
        if (it === void 0)
          return 0;
        var number = toIntegerOrInfinity3(it);
        var length = toLength5(number);
        if (number !== length)
          throw $RangeError("Wrong length or index");
        return length;
      };
    }
  });

  // node_modules/core-js/internals/ieee754.js
  var require_ieee754 = __commonJS({
    "node_modules/core-js/internals/ieee754.js": function(exports, module) {
      var $Array2 = Array;
      var abs = Math.abs;
      var pow = Math.pow;
      var floor = Math.floor;
      var log = Math.log;
      var LN2 = Math.LN2;
      var pack = function(number, mantissaLength, bytes) {
        var buffer = $Array2(bytes);
        var exponentLength = bytes * 8 - mantissaLength - 1;
        var eMax = (1 << exponentLength) - 1;
        var eBias = eMax >> 1;
        var rt = mantissaLength === 23 ? pow(2, -24) - pow(2, -77) : 0;
        var sign = number < 0 || number === 0 && 1 / number < 0 ? 1 : 0;
        var index = 0;
        var exponent, mantissa, c;
        number = abs(number);
        if (number != number || number === Infinity) {
          mantissa = number != number ? 1 : 0;
          exponent = eMax;
        } else {
          exponent = floor(log(number) / LN2);
          c = pow(2, -exponent);
          if (number * c < 1) {
            exponent--;
            c *= 2;
          }
          if (exponent + eBias >= 1) {
            number += rt / c;
          } else {
            number += rt * pow(2, 1 - eBias);
          }
          if (number * c >= 2) {
            exponent++;
            c /= 2;
          }
          if (exponent + eBias >= eMax) {
            mantissa = 0;
            exponent = eMax;
          } else if (exponent + eBias >= 1) {
            mantissa = (number * c - 1) * pow(2, mantissaLength);
            exponent = exponent + eBias;
          } else {
            mantissa = number * pow(2, eBias - 1) * pow(2, mantissaLength);
            exponent = 0;
          }
        }
        while (mantissaLength >= 8) {
          buffer[index++] = mantissa & 255;
          mantissa /= 256;
          mantissaLength -= 8;
        }
        exponent = exponent << mantissaLength | mantissa;
        exponentLength += mantissaLength;
        while (exponentLength > 0) {
          buffer[index++] = exponent & 255;
          exponent /= 256;
          exponentLength -= 8;
        }
        buffer[--index] |= sign * 128;
        return buffer;
      };
      var unpack = function(buffer, mantissaLength) {
        var bytes = buffer.length;
        var exponentLength = bytes * 8 - mantissaLength - 1;
        var eMax = (1 << exponentLength) - 1;
        var eBias = eMax >> 1;
        var nBits = exponentLength - 7;
        var index = bytes - 1;
        var sign = buffer[index--];
        var exponent = sign & 127;
        var mantissa;
        sign >>= 7;
        while (nBits > 0) {
          exponent = exponent * 256 + buffer[index--];
          nBits -= 8;
        }
        mantissa = exponent & (1 << -nBits) - 1;
        exponent >>= -nBits;
        nBits += mantissaLength;
        while (nBits > 0) {
          mantissa = mantissa * 256 + buffer[index--];
          nBits -= 8;
        }
        if (exponent === 0) {
          exponent = 1 - eBias;
        } else if (exponent === eMax) {
          return mantissa ? NaN : sign ? -Infinity : Infinity;
        } else {
          mantissa = mantissa + pow(2, mantissaLength);
          exponent = exponent - eBias;
        }
        return (sign ? -1 : 1) * mantissa * pow(2, exponent - mantissaLength);
      };
      module.exports = {
        pack: pack,
        unpack: unpack
      };
    }
  });

  // node_modules/core-js/internals/array-fill.js
  var require_array_fill = __commonJS({
    "node_modules/core-js/internals/array-fill.js": function(exports, module) {
      "use strict";
      var toObject5 = require_to_object();
      var toAbsoluteIndex4 = require_to_absolute_index();
      var lengthOfArrayLike4 = require_length_of_array_like();
      module.exports = function fill(value) {
        var O = toObject5(this);
        var length = lengthOfArrayLike4(O);
        var argumentsLength = arguments.length;
        var index = toAbsoluteIndex4(argumentsLength > 1 ? arguments[1] : void 0, length);
        var end = argumentsLength > 2 ? arguments[2] : void 0;
        var endPos = end === void 0 ? length : toAbsoluteIndex4(end, length);
        while (endPos > index)
          O[index++] = value;
        return O;
      };
    }
  });

  // node_modules/core-js/internals/array-buffer.js
  var require_array_buffer = __commonJS({
    "node_modules/core-js/internals/array-buffer.js": function(exports, module) {
      "use strict";
      var global9 = require_global();
      var uncurryThis11 = require_function_uncurry_this();
      var DESCRIPTORS8 = require_descriptors();
      var NATIVE_ARRAY_BUFFER = require_array_buffer_basic_detection();
      var FunctionName = require_function_name();
      var createNonEnumerableProperty3 = require_create_non_enumerable_property();
      var defineBuiltInAccessor3 = require_define_built_in_accessor();
      var defineBuiltIns = require_define_built_ins();
      var fails12 = require_fails();
      var anInstance = require_an_instance();
      var toIntegerOrInfinity3 = require_to_integer_or_infinity();
      var toLength5 = require_to_length();
      var toIndex = require_to_index();
      var IEEE754 = require_ieee754();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var setPrototypeOf2 = require_object_set_prototype_of();
      var getOwnPropertyNames2 = require_object_get_own_property_names().f;
      var arrayFill = require_array_fill();
      var arraySlice2 = require_array_slice_simple();
      var setToStringTag5 = require_set_to_string_tag();
      var InternalStateModule2 = require_internal_state();
      var PROPER_FUNCTION_NAME2 = FunctionName.PROPER;
      var CONFIGURABLE_FUNCTION_NAME = FunctionName.CONFIGURABLE;
      var ARRAY_BUFFER2 = "ArrayBuffer";
      var DATA_VIEW = "DataView";
      var PROTOTYPE = "prototype";
      var WRONG_LENGTH = "Wrong length";
      var WRONG_INDEX = "Wrong index";
      var getInternalArrayBufferState = InternalStateModule2.getterFor(ARRAY_BUFFER2);
      var getInternalDataViewState = InternalStateModule2.getterFor(DATA_VIEW);
      var setInternalState2 = InternalStateModule2.set;
      var NativeArrayBuffer2 = global9[ARRAY_BUFFER2];
      var $ArrayBuffer = NativeArrayBuffer2;
      var ArrayBufferPrototype = $ArrayBuffer && $ArrayBuffer[PROTOTYPE];
      var $DataView = global9[DATA_VIEW];
      var DataViewPrototype2 = $DataView && $DataView[PROTOTYPE];
      var ObjectPrototype2 = Object.prototype;
      var Array2 = global9.Array;
      var RangeError2 = global9.RangeError;
      var fill = uncurryThis11(arrayFill);
      var reverse2 = uncurryThis11([].reverse);
      var packIEEE754 = IEEE754.pack;
      var unpackIEEE754 = IEEE754.unpack;
      var packInt8 = function(number) {
        return [number & 255];
      };
      var packInt16 = function(number) {
        return [number & 255, number >> 8 & 255];
      };
      var packInt32 = function(number) {
        return [number & 255, number >> 8 & 255, number >> 16 & 255, number >> 24 & 255];
      };
      var unpackInt32 = function(buffer) {
        return buffer[3] << 24 | buffer[2] << 16 | buffer[1] << 8 | buffer[0];
      };
      var packFloat32 = function(number) {
        return packIEEE754(number, 23, 4);
      };
      var packFloat64 = function(number) {
        return packIEEE754(number, 52, 8);
      };
      var addGetter = function(Constructor, key2, getInternalState2) {
        defineBuiltInAccessor3(Constructor[PROTOTYPE], key2, {
          configurable: true,
          get: function() {
            return getInternalState2(this)[key2];
          }
        });
      };
      var get3 = function(view, count, index, isLittleEndian) {
        var intIndex = toIndex(index);
        var store = getInternalDataViewState(view);
        if (intIndex + count > store.byteLength)
          throw RangeError2(WRONG_INDEX);
        var bytes = store.bytes;
        var start = intIndex + store.byteOffset;
        var pack = arraySlice2(bytes, start, start + count);
        return isLittleEndian ? pack : reverse2(pack);
      };
      var set = function(view, count, index, conversion, value, isLittleEndian) {
        var intIndex = toIndex(index);
        var store = getInternalDataViewState(view);
        if (intIndex + count > store.byteLength)
          throw RangeError2(WRONG_INDEX);
        var bytes = store.bytes;
        var start = intIndex + store.byteOffset;
        var pack = conversion(+value);
        for (var i = 0; i < count; i++)
          bytes[start + i] = pack[isLittleEndian ? i : count - i - 1];
      };
      if (!NATIVE_ARRAY_BUFFER) {
        $ArrayBuffer = function ArrayBuffer4(length) {
          anInstance(this, ArrayBufferPrototype);
          var byteLength = toIndex(length);
          setInternalState2(this, {
            type: ARRAY_BUFFER2,
            bytes: fill(Array2(byteLength), 0),
            byteLength: byteLength
          });
          if (!DESCRIPTORS8) {
            this.byteLength = byteLength;
            this.detached = false;
          }
        };
        ArrayBufferPrototype = $ArrayBuffer[PROTOTYPE];
        $DataView = function DataView3(buffer, byteOffset, byteLength) {
          anInstance(this, DataViewPrototype2);
          anInstance(buffer, ArrayBufferPrototype);
          var bufferState = getInternalArrayBufferState(buffer);
          var bufferLength = bufferState.byteLength;
          var offset = toIntegerOrInfinity3(byteOffset);
          if (offset < 0 || offset > bufferLength)
            throw RangeError2("Wrong offset");
          byteLength = byteLength === void 0 ? bufferLength - offset : toLength5(byteLength);
          if (offset + byteLength > bufferLength)
            throw RangeError2(WRONG_LENGTH);
          setInternalState2(this, {
            type: DATA_VIEW,
            buffer: buffer,
            byteLength: byteLength,
            byteOffset: offset,
            bytes: bufferState.bytes
          });
          if (!DESCRIPTORS8) {
            this.buffer = buffer;
            this.byteLength = byteLength;
            this.byteOffset = offset;
          }
        };
        DataViewPrototype2 = $DataView[PROTOTYPE];
        if (DESCRIPTORS8) {
          addGetter($ArrayBuffer, "byteLength", getInternalArrayBufferState);
          addGetter($DataView, "buffer", getInternalDataViewState);
          addGetter($DataView, "byteLength", getInternalDataViewState);
          addGetter($DataView, "byteOffset", getInternalDataViewState);
        }
        defineBuiltIns(DataViewPrototype2, {
          getInt8: function getInt8(byteOffset) {
            return get3(this, 1, byteOffset)[0] << 24 >> 24;
          },
          getUint8: function getUint82(byteOffset) {
            return get3(this, 1, byteOffset)[0];
          },
          getInt16: function getInt16(byteOffset) {
            var bytes = get3(this, 2, byteOffset, arguments.length > 1 ? arguments[1] : void 0);
            return (bytes[1] << 8 | bytes[0]) << 16 >> 16;
          },
          getUint16: function getUint16(byteOffset) {
            var bytes = get3(this, 2, byteOffset, arguments.length > 1 ? arguments[1] : void 0);
            return bytes[1] << 8 | bytes[0];
          },
          getInt32: function getInt32(byteOffset) {
            return unpackInt32(get3(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0));
          },
          getUint32: function getUint32(byteOffset) {
            return unpackInt32(get3(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0)) >>> 0;
          },
          getFloat32: function getFloat32(byteOffset) {
            return unpackIEEE754(get3(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0), 23);
          },
          getFloat64: function getFloat64(byteOffset) {
            return unpackIEEE754(get3(this, 8, byteOffset, arguments.length > 1 ? arguments[1] : void 0), 52);
          },
          setInt8: function setInt8(byteOffset, value) {
            set(this, 1, byteOffset, packInt8, value);
          },
          setUint8: function setUint82(byteOffset, value) {
            set(this, 1, byteOffset, packInt8, value);
          },
          setInt16: function setInt16(byteOffset, value) {
            set(this, 2, byteOffset, packInt16, value, arguments.length > 2 ? arguments[2] : void 0);
          },
          setUint16: function setUint16(byteOffset, value) {
            set(this, 2, byteOffset, packInt16, value, arguments.length > 2 ? arguments[2] : void 0);
          },
          setInt32: function setInt32(byteOffset, value) {
            set(this, 4, byteOffset, packInt32, value, arguments.length > 2 ? arguments[2] : void 0);
          },
          setUint32: function setUint32(byteOffset, value) {
            set(this, 4, byteOffset, packInt32, value, arguments.length > 2 ? arguments[2] : void 0);
          },
          setFloat32: function setFloat32(byteOffset, value) {
            set(this, 4, byteOffset, packFloat32, value, arguments.length > 2 ? arguments[2] : void 0);
          },
          setFloat64: function setFloat64(byteOffset, value) {
            set(this, 8, byteOffset, packFloat64, value, arguments.length > 2 ? arguments[2] : void 0);
          }
        });
      } else {
        INCORRECT_ARRAY_BUFFER_NAME = PROPER_FUNCTION_NAME2 && NativeArrayBuffer2.name !== ARRAY_BUFFER2;
        if (!fails12(function() {
          NativeArrayBuffer2(1);
        }) || !fails12(function() {
          new NativeArrayBuffer2(-1);
        }) || fails12(function() {
          new NativeArrayBuffer2();
          new NativeArrayBuffer2(1.5);
          new NativeArrayBuffer2(NaN);
          return NativeArrayBuffer2.length != 1 || INCORRECT_ARRAY_BUFFER_NAME && !CONFIGURABLE_FUNCTION_NAME;
        })) {
          $ArrayBuffer = function ArrayBuffer4(length) {
            anInstance(this, ArrayBufferPrototype);
            return new NativeArrayBuffer2(toIndex(length));
          };
          $ArrayBuffer[PROTOTYPE] = ArrayBufferPrototype;
          for (keys2 = getOwnPropertyNames2(NativeArrayBuffer2), j = 0; keys2.length > j; ) {
            if (!((key = keys2[j++]) in $ArrayBuffer)) {
              createNonEnumerableProperty3($ArrayBuffer, key, NativeArrayBuffer2[key]);
            }
          }
          ArrayBufferPrototype.constructor = $ArrayBuffer;
        } else if (INCORRECT_ARRAY_BUFFER_NAME && CONFIGURABLE_FUNCTION_NAME) {
          createNonEnumerableProperty3(NativeArrayBuffer2, "name", ARRAY_BUFFER2);
        }
        if (setPrototypeOf2 && getPrototypeOf3(DataViewPrototype2) !== ObjectPrototype2) {
          setPrototypeOf2(DataViewPrototype2, ObjectPrototype2);
        }
        testView = new $DataView(new $ArrayBuffer(2));
        $setInt8 = uncurryThis11(DataViewPrototype2.setInt8);
        testView.setInt8(0, 2147483648);
        testView.setInt8(1, 2147483649);
        if (testView.getInt8(0) || !testView.getInt8(1))
          defineBuiltIns(DataViewPrototype2, {
            setInt8: function setInt8(byteOffset, value) {
              $setInt8(this, byteOffset, value << 24 >> 24);
            },
            setUint8: function setUint82(byteOffset, value) {
              $setInt8(this, byteOffset, value << 24 >> 24);
            }
          }, { unsafe: true });
      }
      var INCORRECT_ARRAY_BUFFER_NAME;
      var keys2;
      var j;
      var key;
      var testView;
      var $setInt8;
      setToStringTag5($ArrayBuffer, ARRAY_BUFFER2);
      setToStringTag5($DataView, DATA_VIEW);
      module.exports = {
        ArrayBuffer: $ArrayBuffer,
        DataView: $DataView
      };
    }
  });

  // node_modules/core-js/modules/es.data-view.constructor.js
  var require_es_data_view_constructor = __commonJS({
    "node_modules/core-js/modules/es.data-view.constructor.js": function() {
      var $77 = require_export();
      var ArrayBufferModule2 = require_array_buffer();
      var NATIVE_ARRAY_BUFFER = require_array_buffer_basic_detection();
      $77({ global: true, constructor: true, forced: !NATIVE_ARRAY_BUFFER }, {
        DataView: ArrayBufferModule2.DataView
      });
    }
  });

  // node_modules/core-js/internals/array-reduce.js
  var require_array_reduce = __commonJS({
    "node_modules/core-js/internals/array-reduce.js": function(exports, module) {
      var aCallable2 = require_a_callable();
      var toObject5 = require_to_object();
      var IndexedObject2 = require_indexed_object();
      var lengthOfArrayLike4 = require_length_of_array_like();
      var $TypeError = TypeError;
      var createMethod = function(IS_RIGHT) {
        return function(that, callbackfn, argumentsLength, memo) {
          aCallable2(callbackfn);
          var O = toObject5(that);
          var self2 = IndexedObject2(O);
          var length = lengthOfArrayLike4(O);
          var index = IS_RIGHT ? length - 1 : 0;
          var i = IS_RIGHT ? -1 : 1;
          if (argumentsLength < 2)
            while (true) {
              if (index in self2) {
                memo = self2[index];
                index += i;
                break;
              }
              index += i;
              if (IS_RIGHT ? index < 0 : length <= index) {
                throw $TypeError("Reduce of empty array with no initial value");
              }
            }
          for (; IS_RIGHT ? index >= 0 : length > index; index += i)
            if (index in self2) {
              memo = callbackfn(memo, self2[index], index, O);
            }
          return memo;
        };
      };
      module.exports = {
        left: createMethod(false),
        right: createMethod(true)
      };
    }
  });

  // srcts/src/initialize/disableForm.ts
  var import_jquery = __toESM(require_jquery());
  function disableFormSubmission() {
    (0, import_jquery.default)(document).on("submit", "form:not([action])", function(e) {
      e.preventDefault();
    });
  }

  // srcts/src/initialize/history.ts
  var import_jquery2 = __toESM(require_jquery());
  function trackHistory() {
    var origPushState = window.history.pushState;
    window.history.pushState = function() {
      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }
      var result = origPushState.apply(this, args);
      (0, import_jquery2.default)(document).trigger("pushstate");
      return result;
    };
  }

  // node_modules/core-js/modules/es.array.index-of.js
  var $3 = require_export();
  var uncurryThis = require_function_uncurry_this_clause();
  var $indexOf = require_array_includes().indexOf;
  var arrayMethodIsStrict = require_array_method_is_strict();
  var nativeIndexOf = uncurryThis([].indexOf);
  var NEGATIVE_ZERO = !!nativeIndexOf && 1 / nativeIndexOf([1], 1, -0) < 0;
  var FORCED = NEGATIVE_ZERO || !arrayMethodIsStrict("indexOf");
  $3({ target: "Array", proto: true, forced: FORCED }, {
    indexOf: function indexOf(searchElement) {
      var fromIndex = arguments.length > 1 ? arguments[1] : void 0;
      return NEGATIVE_ZERO ? nativeIndexOf(this, searchElement, fromIndex) || 0 : $indexOf(this, searchElement, fromIndex);
    }
  });

  // node_modules/core-js/modules/es.parse-int.js
  var $4 = require_export();
  var $parseInt = require_number_parse_int();
  $4({ global: true, forced: parseInt != $parseInt }, {
    parseInt: $parseInt
  });

  // srcts/src/initialize/browser.ts
  var import_es_regexp_exec = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.regexp.test.js
  require_es_regexp_exec();
  var $5 = require_export();
  var call = require_function_call();
  var isCallable = require_is_callable();
  var anObject = require_an_object();
  var toString = require_to_string();
  var DELEGATES_TO_EXEC = function() {
    var execCalled = false;
    var re = /[ac]/;
    re.exec = function() {
      execCalled = true;
      return /./.exec.apply(this, arguments);
    };
    return re.test("abc") === true && execCalled;
  }();
  var nativeTest = /./.test;
  $5({ target: "RegExp", proto: true, forced: !DELEGATES_TO_EXEC }, {
    test: function(S) {
      var R = anObject(this);
      var string = toString(S);
      var exec2 = R.exec;
      if (!isCallable(exec2))
        return call(nativeTest, R, string);
      var result = call(exec2, R, string);
      if (result === null)
        return false;
      anObject(result);
      return true;
    }
  });

  // srcts/src/initialize/browser.ts
  var import_jquery3 = __toESM(require_jquery());

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
    var msie = userAgent.indexOf("MSIE ");
    if (isIE() && msie > 0) {
      return parseInt(userAgent.substring(msie + 5, userAgent.indexOf(".", msie)), 10);
    }
    var trident = userAgent.indexOf("Trident/");
    if (trident > 0) {
      var rv = userAgent.indexOf("rv:");
      return parseInt(userAgent.substring(rv + 3, userAgent.indexOf(".", rv)), 10);
    }
    return -1;
  }
  function determineBrowserInfo() {
    if (/\bQt\//.test(userAgent)) {
      (0, import_jquery3.default)(document.documentElement).addClass("qt");
      setIsQt(true);
    } else {
      setIsQt(false);
    }
    if (/\bQt/.test(userAgent) && /\bMacintosh/.test(userAgent)) {
      (0, import_jquery3.default)(document.documentElement).addClass("qtmac");
    }
    if (/\bQt\/5/.test(userAgent) && /Linux/.test(userAgent)) {
      (0, import_jquery3.default)(document.documentElement).addClass("qt5");
    }
    setIsIE(/MSIE|Trident|Edge/.test(userAgent));
    setIEVersion(getIEVersion());
  }

  // srcts/src/window/libraries.ts
  function windowShiny() {
    if (!window["Shiny"]) {
      window["Shiny"] = {};
    }
    return window["Shiny"];
  }

  // srcts/src/shiny/index.ts
  var import_jquery40 = __toESM(require_jquery());

  // node_modules/core-js/modules/es.function.name.js
  var DESCRIPTORS = require_descriptors();
  var FUNCTION_NAME_EXISTS = require_function_name().EXISTS;
  var uncurryThis2 = require_function_uncurry_this();
  var defineBuiltInAccessor = require_define_built_in_accessor();
  var FunctionPrototype = Function.prototype;
  var functionToString = uncurryThis2(FunctionPrototype.toString);
  var nameRE = /function\b(?:\s|\/\*[\S\s]*?\*\/|\/\/[^\n\r]*[\n\r]+)*([^\s(/]*)/;
  var regExpExec = uncurryThis2(nameRE.exec);
  var NAME = "name";
  if (DESCRIPTORS && !FUNCTION_NAME_EXISTS) {
    defineBuiltInAccessor(FunctionPrototype, NAME, {
      configurable: true,
      get: function() {
        try {
          return regExpExec(nameRE, functionToString(this))[1];
        } catch (error) {
          return "";
        }
      }
    });
  }

  // node_modules/core-js/modules/es.symbol.to-primitive.js
  var defineWellKnownSymbol = require_well_known_symbol_define();
  var defineSymbolToPrimitive = require_symbol_define_to_primitive();
  defineWellKnownSymbol("toPrimitive");
  defineSymbolToPrimitive();

  // node_modules/core-js/modules/es.date.to-primitive.js
  var hasOwn = require_has_own_property();
  var defineBuiltIn = require_define_built_in();
  var dateToPrimitive = require_date_to_primitive();
  var wellKnownSymbol = require_well_known_symbol();
  var TO_PRIMITIVE = wellKnownSymbol("toPrimitive");
  var DatePrototype = Date.prototype;
  if (!hasOwn(DatePrototype, TO_PRIMITIVE)) {
    defineBuiltIn(DatePrototype, TO_PRIMITIVE, dateToPrimitive);
  }

  // node_modules/core-js/modules/es.symbol.js
  require_es_symbol_constructor();
  require_es_symbol_for();
  require_es_symbol_key_for();
  require_es_json_stringify();
  require_es_object_get_own_property_symbols();

  // node_modules/core-js/modules/es.symbol.description.js
  var $7 = require_export();
  var DESCRIPTORS2 = require_descriptors();
  var global2 = require_global();
  var uncurryThis3 = require_function_uncurry_this();
  var hasOwn2 = require_has_own_property();
  var isCallable2 = require_is_callable();
  var isPrototypeOf = require_object_is_prototype_of();
  var toString2 = require_to_string();
  var defineBuiltInAccessor2 = require_define_built_in_accessor();
  var copyConstructorProperties = require_copy_constructor_properties();
  var NativeSymbol = global2.Symbol;
  var SymbolPrototype = NativeSymbol && NativeSymbol.prototype;
  if (DESCRIPTORS2 && isCallable2(NativeSymbol) && (!("description" in SymbolPrototype) || NativeSymbol().description !== void 0)) {
    EmptyStringDescriptionStore = {};
    SymbolWrapper = function Symbol2() {
      var description = arguments.length < 1 || arguments[0] === void 0 ? void 0 : toString2(arguments[0]);
      var result = isPrototypeOf(SymbolPrototype, this) ? new NativeSymbol(description) : description === void 0 ? NativeSymbol() : NativeSymbol(description);
      if (description === "")
        EmptyStringDescriptionStore[result] = true;
      return result;
    };
    copyConstructorProperties(SymbolWrapper, NativeSymbol);
    SymbolWrapper.prototype = SymbolPrototype;
    SymbolPrototype.constructor = SymbolWrapper;
    NATIVE_SYMBOL = String(NativeSymbol("test")) == "Symbol(test)";
    thisSymbolValue = uncurryThis3(SymbolPrototype.valueOf);
    symbolDescriptiveString = uncurryThis3(SymbolPrototype.toString);
    regexp = /^Symbol\((.*)\)[^)]+$/;
    replace = uncurryThis3("".replace);
    stringSlice4 = uncurryThis3("".slice);
    defineBuiltInAccessor2(SymbolPrototype, "description", {
      configurable: true,
      get: function description() {
        var symbol = thisSymbolValue(this);
        if (hasOwn2(EmptyStringDescriptionStore, symbol))
          return "";
        var string = symbolDescriptiveString(symbol);
        var desc = NATIVE_SYMBOL ? stringSlice4(string, 7, -1) : replace(string, regexp, "$1");
        return desc === "" ? void 0 : desc;
      }
    });
    $7({ global: true, constructor: true, forced: true }, {
      Symbol: SymbolWrapper
    });
  }
  var EmptyStringDescriptionStore;
  var SymbolWrapper;
  var NATIVE_SYMBOL;
  var thisSymbolValue;
  var symbolDescriptiveString;
  var regexp;
  var replace;
  var stringSlice4;

  // node_modules/core-js/modules/es.object.to-string.js
  var TO_STRING_TAG_SUPPORT = require_to_string_tag_support();
  var defineBuiltIn2 = require_define_built_in();
  var toString3 = require_object_to_string();
  if (!TO_STRING_TAG_SUPPORT) {
    defineBuiltIn2(Object.prototype, "toString", toString3, { unsafe: true });
  }

  // node_modules/core-js/modules/es.number.constructor.js
  var $8 = require_export();
  var IS_PURE = require_is_pure();
  var DESCRIPTORS3 = require_descriptors();
  var global3 = require_global();
  var path = require_path();
  var uncurryThis4 = require_function_uncurry_this();
  var isForced = require_is_forced();
  var hasOwn3 = require_has_own_property();
  var inheritIfRequired = require_inherit_if_required();
  var isPrototypeOf2 = require_object_is_prototype_of();
  var isSymbol = require_is_symbol();
  var toPrimitive = require_to_primitive();
  var fails = require_fails();
  var getOwnPropertyNames = require_object_get_own_property_names().f;
  var getOwnPropertyDescriptor = require_object_get_own_property_descriptor().f;
  var defineProperty = require_object_define_property().f;
  var thisNumberValue = require_this_number_value();
  var trim = require_string_trim().trim;
  var NUMBER = "Number";
  var NativeNumber = global3[NUMBER];
  var PureNumberNamespace = path[NUMBER];
  var NumberPrototype = NativeNumber.prototype;
  var TypeError2 = global3.TypeError;
  var stringSlice = uncurryThis4("".slice);
  var charCodeAt = uncurryThis4("".charCodeAt);
  var toNumeric = function(value) {
    var primValue = toPrimitive(value, "number");
    return typeof primValue == "bigint" ? primValue : toNumber(primValue);
  };
  var toNumber = function(argument) {
    var it = toPrimitive(argument, "number");
    var first, third, radix, maxCode, digits, length, index, code;
    if (isSymbol(it))
      throw TypeError2("Cannot convert a Symbol value to a number");
    if (typeof it == "string" && it.length > 2) {
      it = trim(it);
      first = charCodeAt(it, 0);
      if (first === 43 || first === 45) {
        third = charCodeAt(it, 2);
        if (third === 88 || third === 120)
          return NaN;
      } else if (first === 48) {
        switch (charCodeAt(it, 1)) {
          case 66:
          case 98:
            radix = 2;
            maxCode = 49;
            break;
          case 79:
          case 111:
            radix = 8;
            maxCode = 55;
            break;
          default:
            return +it;
        }
        digits = stringSlice(it, 2);
        length = digits.length;
        for (index = 0; index < length; index++) {
          code = charCodeAt(digits, index);
          if (code < 48 || code > maxCode)
            return NaN;
        }
        return parseInt(digits, radix);
      }
    }
    return +it;
  };
  var FORCED2 = isForced(NUMBER, !NativeNumber(" 0o1") || !NativeNumber("0b1") || NativeNumber("+0x1"));
  var calledWithNew = function(dummy) {
    return isPrototypeOf2(NumberPrototype, dummy) && fails(function() {
      thisNumberValue(dummy);
    });
  };
  var NumberWrapper = function Number2(value) {
    var n = arguments.length < 1 ? 0 : NativeNumber(toNumeric(value));
    return calledWithNew(this) ? inheritIfRequired(Object(n), this, NumberWrapper) : n;
  };
  NumberWrapper.prototype = NumberPrototype;
  if (FORCED2 && !IS_PURE)
    NumberPrototype.constructor = NumberWrapper;
  $8({ global: true, constructor: true, wrap: true, forced: FORCED2 }, {
    Number: NumberWrapper
  });
  var copyConstructorProperties2 = function(target, source) {
    for (var keys2 = DESCRIPTORS3 ? getOwnPropertyNames(source) : "MAX_VALUE,MIN_VALUE,NaN,NEGATIVE_INFINITY,POSITIVE_INFINITY,EPSILON,MAX_SAFE_INTEGER,MIN_SAFE_INTEGER,isFinite,isInteger,isNaN,isSafeInteger,parseFloat,parseInt,fromString,range".split(","), j = 0, key; keys2.length > j; j++) {
      if (hasOwn3(source, key = keys2[j]) && !hasOwn3(target, key)) {
        defineProperty(target, key, getOwnPropertyDescriptor(source, key));
      }
    }
  };
  if (IS_PURE && PureNumberNamespace)
    copyConstructorProperties2(path[NUMBER], PureNumberNamespace);
  if (FORCED2 || IS_PURE)
    copyConstructorProperties2(path[NUMBER], NativeNumber);

  // node_modules/core-js/modules/es.object.define-property.js
  var $9 = require_export();
  var DESCRIPTORS4 = require_descriptors();
  var defineProperty2 = require_object_define_property().f;
  $9({ target: "Object", stat: true, forced: Object.defineProperty !== defineProperty2, sham: !DESCRIPTORS4 }, {
    defineProperty: defineProperty2
  });

  // node_modules/core-js/modules/es.symbol.iterator.js
  var defineWellKnownSymbol2 = require_well_known_symbol_define();
  defineWellKnownSymbol2("iterator");

  // srcts/src/bindings/registry.ts
  var import_es_array_iterator = __toESM(require_es_array_iterator());

  // node_modules/core-js/modules/es.string.iterator.js
  var charAt = require_string_multibyte().charAt;
  var toString4 = require_to_string();
  var InternalStateModule = require_internal_state();
  var defineIterator = require_iterator_define();
  var createIterResultObject = require_create_iter_result_object();
  var STRING_ITERATOR = "String Iterator";
  var setInternalState = InternalStateModule.set;
  var getInternalState = InternalStateModule.getterFor(STRING_ITERATOR);
  defineIterator(String, "String", function(iterated) {
    setInternalState(this, {
      type: STRING_ITERATOR,
      string: toString4(iterated),
      index: 0
    });
  }, function next() {
    var state = getInternalState(this);
    var string = state.string;
    var index = state.index;
    var point;
    if (index >= string.length)
      return createIterResultObject(void 0, true);
    point = charAt(string, index);
    state.index += point.length;
    return createIterResultObject(point, false);
  });

  // node_modules/core-js/modules/web.dom-collections.iterator.js
  var global4 = require_global();
  var DOMIterables = require_dom_iterables();
  var DOMTokenListPrototype = require_dom_token_list_prototype();
  var ArrayIteratorMethods = require_es_array_iterator();
  var createNonEnumerableProperty = require_create_non_enumerable_property();
  var wellKnownSymbol2 = require_well_known_symbol();
  var ITERATOR = wellKnownSymbol2("iterator");
  var TO_STRING_TAG = wellKnownSymbol2("toStringTag");
  var ArrayValues = ArrayIteratorMethods.values;
  var handlePrototype = function(CollectionPrototype, COLLECTION_NAME) {
    if (CollectionPrototype) {
      if (CollectionPrototype[ITERATOR] !== ArrayValues)
        try {
          createNonEnumerableProperty(CollectionPrototype, ITERATOR, ArrayValues);
        } catch (error) {
          CollectionPrototype[ITERATOR] = ArrayValues;
        }
      if (!CollectionPrototype[TO_STRING_TAG]) {
        createNonEnumerableProperty(CollectionPrototype, TO_STRING_TAG, COLLECTION_NAME);
      }
      if (DOMIterables[COLLECTION_NAME])
        for (var METHOD_NAME in ArrayIteratorMethods) {
          if (CollectionPrototype[METHOD_NAME] !== ArrayIteratorMethods[METHOD_NAME])
            try {
              createNonEnumerableProperty(CollectionPrototype, METHOD_NAME, ArrayIteratorMethods[METHOD_NAME]);
            } catch (error) {
              CollectionPrototype[METHOD_NAME] = ArrayIteratorMethods[METHOD_NAME];
            }
        }
    }
  };
  for (COLLECTION_NAME in DOMIterables) {
    handlePrototype(global4[COLLECTION_NAME] && global4[COLLECTION_NAME].prototype, COLLECTION_NAME);
  }
  var COLLECTION_NAME;
  handlePrototype(DOMTokenListPrototype, "DOMTokenList");

  // srcts/src/utils/index.ts
  var import_es_regexp_exec2 = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.replace.js
  var apply = require_function_apply();
  var call2 = require_function_call();
  var uncurryThis5 = require_function_uncurry_this();
  var fixRegExpWellKnownSymbolLogic = require_fix_regexp_well_known_symbol_logic();
  var fails2 = require_fails();
  var anObject2 = require_an_object();
  var isCallable3 = require_is_callable();
  var isNullOrUndefined = require_is_null_or_undefined();
  var toIntegerOrInfinity = require_to_integer_or_infinity();
  var toLength = require_to_length();
  var toString5 = require_to_string();
  var requireObjectCoercible = require_require_object_coercible();
  var advanceStringIndex = require_advance_string_index();
  var getMethod = require_get_method();
  var getSubstitution = require_get_substitution();
  var regExpExec2 = require_regexp_exec_abstract();
  var wellKnownSymbol3 = require_well_known_symbol();
  var REPLACE = wellKnownSymbol3("replace");
  var max = Math.max;
  var min = Math.min;
  var concat = uncurryThis5([].concat);
  var push = uncurryThis5([].push);
  var stringIndexOf = uncurryThis5("".indexOf);
  var stringSlice2 = uncurryThis5("".slice);
  var maybeToString = function(it) {
    return it === void 0 ? it : String(it);
  };
  var REPLACE_KEEPS_$0 = function() {
    return "a".replace(/./, "$0") === "$0";
  }();
  var REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE = function() {
    if (/./[REPLACE]) {
      return /./[REPLACE]("a", "$0") === "";
    }
    return false;
  }();
  var REPLACE_SUPPORTS_NAMED_GROUPS = !fails2(function() {
    var re = /./;
    re.exec = function() {
      var result = [];
      result.groups = { a: "7" };
      return result;
    };
    return "".replace(re, "$<a>") !== "7";
  });
  fixRegExpWellKnownSymbolLogic("replace", function(_, nativeReplace, maybeCallNative) {
    var UNSAFE_SUBSTITUTE = REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE ? "$" : "$0";
    return [
      function replace(searchValue, replaceValue) {
        var O = requireObjectCoercible(this);
        var replacer = isNullOrUndefined(searchValue) ? void 0 : getMethod(searchValue, REPLACE);
        return replacer ? call2(replacer, searchValue, O, replaceValue) : call2(nativeReplace, toString5(O), searchValue, replaceValue);
      },
      function(string, replaceValue) {
        var rx = anObject2(this);
        var S = toString5(string);
        if (typeof replaceValue == "string" && stringIndexOf(replaceValue, UNSAFE_SUBSTITUTE) === -1 && stringIndexOf(replaceValue, "$<") === -1) {
          var res = maybeCallNative(nativeReplace, rx, S, replaceValue);
          if (res.done)
            return res.value;
        }
        var functionalReplace = isCallable3(replaceValue);
        if (!functionalReplace)
          replaceValue = toString5(replaceValue);
        var global9 = rx.global;
        if (global9) {
          var fullUnicode = rx.unicode;
          rx.lastIndex = 0;
        }
        var results = [];
        while (true) {
          var result = regExpExec2(rx, S);
          if (result === null)
            break;
          push(results, result);
          if (!global9)
            break;
          var matchStr = toString5(result[0]);
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex(S, toLength(rx.lastIndex), fullUnicode);
        }
        var accumulatedResult = "";
        var nextSourcePosition = 0;
        for (var i = 0; i < results.length; i++) {
          result = results[i];
          var matched = toString5(result[0]);
          var position = max(min(toIntegerOrInfinity(result.index), S.length), 0);
          var captures = [];
          for (var j = 1; j < result.length; j++)
            push(captures, maybeToString(result[j]));
          var namedCaptures = result.groups;
          if (functionalReplace) {
            var replacerArgs = concat([matched], captures, position, S);
            if (namedCaptures !== void 0)
              push(replacerArgs, namedCaptures);
            var replacement = toString5(apply(replaceValue, void 0, replacerArgs));
          } else {
            replacement = getSubstitution(matched, S, position, captures, namedCaptures, replaceValue);
          }
          if (position >= nextSourcePosition) {
            accumulatedResult += stringSlice2(S, nextSourcePosition, position) + replacement;
            nextSourcePosition = position + matched.length;
          }
        }
        return accumulatedResult + stringSlice2(S, nextSourcePosition);
      }
    ];
  }, !REPLACE_SUPPORTS_NAMED_GROUPS || !REPLACE_KEEPS_$0 || REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE);

  // node_modules/core-js/modules/es.regexp.to-string.js
  var PROPER_FUNCTION_NAME = require_function_name().PROPER;
  var defineBuiltIn3 = require_define_built_in();
  var anObject3 = require_an_object();
  var $toString = require_to_string();
  var fails3 = require_fails();
  var getRegExpFlags = require_regexp_get_flags();
  var TO_STRING = "toString";
  var RegExpPrototype = RegExp.prototype;
  var nativeToString = RegExpPrototype[TO_STRING];
  var NOT_GENERIC = fails3(function() {
    return nativeToString.call({ source: "a", flags: "b" }) != "/a/b";
  });
  var INCORRECT_NAME = PROPER_FUNCTION_NAME && nativeToString.name != TO_STRING;
  if (NOT_GENERIC || INCORRECT_NAME) {
    defineBuiltIn3(RegExp.prototype, TO_STRING, function toString9() {
      var R = anObject3(this);
      var pattern = $toString(R.source);
      var flags = $toString(getRegExpFlags(R));
      return "/" + pattern + "/" + flags;
    }, { unsafe: true });
  }

  // node_modules/core-js/modules/es.parse-float.js
  var $10 = require_export();
  var $parseFloat = require_number_parse_float();
  $10({ global: true, forced: parseFloat != $parseFloat }, {
    parseFloat: $parseFloat
  });

  // node_modules/core-js/modules/es.number.to-precision.js
  var $11 = require_export();
  var uncurryThis6 = require_function_uncurry_this();
  var fails4 = require_fails();
  var thisNumberValue2 = require_this_number_value();
  var nativeToPrecision = uncurryThis6(1 .toPrecision);
  var FORCED3 = fails4(function() {
    return nativeToPrecision(1, void 0) !== "1";
  }) || !fails4(function() {
    nativeToPrecision({});
  });
  $11({ target: "Number", proto: true, forced: FORCED3 }, {
    toPrecision: function toPrecision(precision) {
      return precision === void 0 ? nativeToPrecision(thisNumberValue2(this)) : nativeToPrecision(thisNumberValue2(this), precision);
    }
  });

  // node_modules/core-js/modules/es.array.concat.js
  var $12 = require_export();
  var fails5 = require_fails();
  var isArray = require_is_array();
  var isObject = require_is_object();
  var toObject = require_to_object();
  var lengthOfArrayLike = require_length_of_array_like();
  var doesNotExceedSafeInteger = require_does_not_exceed_safe_integer();
  var createProperty = require_create_property();
  var arraySpeciesCreate = require_array_species_create();
  var arrayMethodHasSpeciesSupport = require_array_method_has_species_support();
  var wellKnownSymbol4 = require_well_known_symbol();
  var V8_VERSION = require_engine_v8_version();
  var IS_CONCAT_SPREADABLE = wellKnownSymbol4("isConcatSpreadable");
  var IS_CONCAT_SPREADABLE_SUPPORT = V8_VERSION >= 51 || !fails5(function() {
    var array = [];
    array[IS_CONCAT_SPREADABLE] = false;
    return array.concat()[0] !== array;
  });
  var isConcatSpreadable = function(O) {
    if (!isObject(O))
      return false;
    var spreadable = O[IS_CONCAT_SPREADABLE];
    return spreadable !== void 0 ? !!spreadable : isArray(O);
  };
  var FORCED4 = !IS_CONCAT_SPREADABLE_SUPPORT || !arrayMethodHasSpeciesSupport("concat");
  $12({ target: "Array", proto: true, arity: 1, forced: FORCED4 }, {
    concat: function concat2(arg) {
      var O = toObject(this);
      var A = arraySpeciesCreate(O, 0);
      var n = 0;
      var i, k, length, len, E;
      for (i = -1, length = arguments.length; i < length; i++) {
        E = i === -1 ? O : arguments[i];
        if (isConcatSpreadable(E)) {
          len = lengthOfArrayLike(E);
          doesNotExceedSafeInteger(n + len);
          for (k = 0; k < len; k++, n++)
            if (k in E)
              createProperty(A, n, E[k]);
        } else {
          doesNotExceedSafeInteger(n + 1);
          createProperty(A, n++, E);
        }
      }
      A.length = n;
      return A;
    }
  });

  // node_modules/core-js/modules/es.array.slice.js
  var $13 = require_export();
  var isArray2 = require_is_array();
  var isConstructor = require_is_constructor();
  var isObject2 = require_is_object();
  var toAbsoluteIndex = require_to_absolute_index();
  var lengthOfArrayLike2 = require_length_of_array_like();
  var toIndexedObject = require_to_indexed_object();
  var createProperty2 = require_create_property();
  var wellKnownSymbol5 = require_well_known_symbol();
  var arrayMethodHasSpeciesSupport2 = require_array_method_has_species_support();
  var nativeSlice = require_array_slice();
  var HAS_SPECIES_SUPPORT = arrayMethodHasSpeciesSupport2("slice");
  var SPECIES = wellKnownSymbol5("species");
  var $Array = Array;
  var max2 = Math.max;
  $13({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT }, {
    slice: function slice(start, end) {
      var O = toIndexedObject(this);
      var length = lengthOfArrayLike2(O);
      var k = toAbsoluteIndex(start, length);
      var fin = toAbsoluteIndex(end === void 0 ? length : end, length);
      var Constructor, result, n;
      if (isArray2(O)) {
        Constructor = O.constructor;
        if (isConstructor(Constructor) && (Constructor === $Array || isArray2(Constructor.prototype))) {
          Constructor = void 0;
        } else if (isObject2(Constructor)) {
          Constructor = Constructor[SPECIES];
          if (Constructor === null)
            Constructor = void 0;
        }
        if (Constructor === $Array || Constructor === void 0) {
          return nativeSlice(O, k, fin);
        }
      }
      result = new (Constructor === void 0 ? $Array : Constructor)(max2(fin - k, 0));
      for (n = 0; k < fin; k++, n++)
        if (k in O)
          createProperty2(result, n, O[k]);
      result.length = n;
      return result;
    }
  });

  // node_modules/core-js/modules/es.array.splice.js
  var $14 = require_export();
  var toObject2 = require_to_object();
  var toAbsoluteIndex2 = require_to_absolute_index();
  var toIntegerOrInfinity2 = require_to_integer_or_infinity();
  var lengthOfArrayLike3 = require_length_of_array_like();
  var setArrayLength = require_array_set_length();
  var doesNotExceedSafeInteger2 = require_does_not_exceed_safe_integer();
  var arraySpeciesCreate2 = require_array_species_create();
  var createProperty3 = require_create_property();
  var deletePropertyOrThrow = require_delete_property_or_throw();
  var arrayMethodHasSpeciesSupport3 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT2 = arrayMethodHasSpeciesSupport3("splice");
  var max3 = Math.max;
  var min2 = Math.min;
  $14({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT2 }, {
    splice: function splice(start, deleteCount) {
      var O = toObject2(this);
      var len = lengthOfArrayLike3(O);
      var actualStart = toAbsoluteIndex2(start, len);
      var argumentsLength = arguments.length;
      var insertCount, actualDeleteCount, A, k, from2, to;
      if (argumentsLength === 0) {
        insertCount = actualDeleteCount = 0;
      } else if (argumentsLength === 1) {
        insertCount = 0;
        actualDeleteCount = len - actualStart;
      } else {
        insertCount = argumentsLength - 2;
        actualDeleteCount = min2(max3(toIntegerOrInfinity2(deleteCount), 0), len - actualStart);
      }
      doesNotExceedSafeInteger2(len + insertCount - actualDeleteCount);
      A = arraySpeciesCreate2(O, actualDeleteCount);
      for (k = 0; k < actualDeleteCount; k++) {
        from2 = actualStart + k;
        if (from2 in O)
          createProperty3(A, k, O[from2]);
      }
      A.length = actualDeleteCount;
      if (insertCount < actualDeleteCount) {
        for (k = actualStart; k < len - actualDeleteCount; k++) {
          from2 = k + actualDeleteCount;
          to = k + insertCount;
          if (from2 in O)
            O[to] = O[from2];
          else
            deletePropertyOrThrow(O, to);
        }
        for (k = len; k > len - actualDeleteCount + insertCount; k--)
          deletePropertyOrThrow(O, k - 1);
      } else if (insertCount > actualDeleteCount) {
        for (k = len - actualDeleteCount; k > actualStart; k--) {
          from2 = k + actualDeleteCount - 1;
          to = k + insertCount - 1;
          if (from2 in O)
            O[to] = O[from2];
          else
            deletePropertyOrThrow(O, to);
        }
      }
      for (k = 0; k < insertCount; k++) {
        O[k + actualStart] = arguments[k + 2];
      }
      setArrayLength(O, len - actualDeleteCount + insertCount);
      return A;
    }
  });

  // node_modules/core-js/modules/es.array.for-each.js
  var $15 = require_export();
  var forEach = require_array_for_each();
  $15({ target: "Array", proto: true, forced: [].forEach != forEach }, {
    forEach: forEach
  });

  // node_modules/core-js/modules/web.dom-collections.for-each.js
  var global5 = require_global();
  var DOMIterables2 = require_dom_iterables();
  var DOMTokenListPrototype2 = require_dom_token_list_prototype();
  var forEach2 = require_array_for_each();
  var createNonEnumerableProperty2 = require_create_non_enumerable_property();
  var handlePrototype2 = function(CollectionPrototype) {
    if (CollectionPrototype && CollectionPrototype.forEach !== forEach2)
      try {
        createNonEnumerableProperty2(CollectionPrototype, "forEach", forEach2);
      } catch (error) {
        CollectionPrototype.forEach = forEach2;
      }
  };
  for (COLLECTION_NAME in DOMIterables2) {
    if (DOMIterables2[COLLECTION_NAME]) {
      handlePrototype2(global5[COLLECTION_NAME] && global5[COLLECTION_NAME].prototype);
    }
  }
  var COLLECTION_NAME;
  handlePrototype2(DOMTokenListPrototype2);

  // node_modules/core-js/modules/es.object.keys.js
  var $16 = require_export();
  var toObject3 = require_to_object();
  var nativeKeys = require_object_keys();
  var fails6 = require_fails();
  var FAILS_ON_PRIMITIVES = fails6(function() {
    nativeKeys(1);
  });
  $16({ target: "Object", stat: true, forced: FAILS_ON_PRIMITIVES }, {
    keys: function keys(it) {
      return nativeKeys(toObject3(it));
    }
  });

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
    var escaped = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#039;",
      "/": "&#x2F;"
    };
    return str.replace(/[&<>'"/]/g, function(m) {
      return escaped[m];
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
    var x = void 0;
    if ("currentStyle" in el) {
      x = el.currentStyle[styleProp];
    } else {
      var _document, _document$defaultView;
      var style = (_document = document) === null || _document === void 0 ? void 0 : (_document$defaultView = _document.defaultView) === null || _document$defaultView === void 0 ? void 0 : _document$defaultView.getComputedStyle(el, null);
      if (style)
        x = style.getPropertyValue(styleProp);
    }
    return x;
  }
  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits)
      str = "0" + str;
    return str;
  }
  function roundSignif(x) {
    var digits = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 1;
    if (digits < 1)
      throw "Significant digits must be at least 1.";
    return parseFloat(x.toPrecision(digits));
  }
  function parseDate(dateString) {
    var date = new Date(dateString);
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
    var lastSize = {};
    return function() {
      var rect = el.getBoundingClientRect();
      var size = {
        w: rect.width,
        h: rect.height
      };
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
  function scopeExprToFunc(expr) {
    var exprEscaped = expr.replace(/[\\"']/g, "\\$&").replace(/\u0000/g, "\\0").replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/[\b]/g, "\\b");
    var func;
    try {
      func = new Function("with (this) {\n        try {\n          return (".concat(expr, ");\n        } catch (e) {\n          console.error('Error evaluating expression: ").concat(exprEscaped, "');\n          throw e;\n        }\n      }"));
    } catch (e) {
      console.error("Error parsing expression: " + expr);
      throw e;
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
    function merge(a, b) {
      var ia = 0;
      var ib = 0;
      var sorted = [];
      while (ia < a.length && ib < b.length) {
        if (sortfunc(a[ia], b[ib]) <= 0) {
          sorted.push(a[ia++]);
        } else {
          sorted.push(b[ib++]);
        }
      }
      while (ia < a.length)
        sorted.push(a[ia++]);
      while (ib < b.length)
        sorted.push(b[ib++]);
      return sorted;
    }
    list = list.slice(0);
    for (var chunkSize = 1; chunkSize < list.length; chunkSize *= 2) {
      for (var i = 0; i < list.length; i += chunkSize * 2) {
        var listA = list.slice(i, i + chunkSize);
        var listB = list.slice(i + chunkSize, i + chunkSize * 2);
        var merged = merge(listA, listB);
        var args = [i, merged.length];
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
  function mapValues(obj, f) {
    var newObj = {};
    Object.keys(obj).forEach(function(key) {
      newObj[key] = f(obj[key], key, obj);
    });
    return newObj;
  }
  function isnan(x) {
    return typeof x === "number" && isNaN(x);
  }
  function _equal(x, y) {
    if (import_jquery4.default.type(x) === "object" && import_jquery4.default.type(y) === "object") {
      var xo = x;
      var yo = y;
      if (Object.keys(xo).length !== Object.keys(yo).length)
        return false;
      for (var prop in xo) {
        if (!hasOwnProperty(yo, prop) || !_equal(xo[prop], yo[prop]))
          return false;
      }
      return true;
    } else if (import_jquery4.default.type(x) === "array" && import_jquery4.default.type(y) === "array") {
      var xa = x;
      var ya = y;
      if (xa.length !== ya.length)
        return false;
      for (var i = 0; i < xa.length; i++)
        if (!_equal(xa[i], ya[i]))
          return false;
      return true;
    } else {
      return x === y;
    }
  }
  function equal() {
    if (arguments.length < 2)
      throw new Error("equal requires at least two arguments.");
    for (var i = 0; i < arguments.length - 1; i++) {
      if (!_equal(i < 0 || arguments.length <= i ? void 0 : arguments[i], i + 1 < 0 || arguments.length <= i + 1 ? void 0 : arguments[i + 1]))
        return false;
    }
    return true;
  }
  var compareVersion = function compareVersion2(a, op, b) {
    function versionParts(ver) {
      return (ver + "").replace(/-/, ".").replace(/(\.0)+[^.]*$/, "").split(".");
    }
    function cmpVersion(a2, b2) {
      var aParts = versionParts(a2);
      var bParts = versionParts(b2);
      var len = Math.min(aParts.length, bParts.length);
      var cmp;
      for (var i = 0; i < len; i++) {
        cmp = parseInt(aParts[i], 10) - parseInt(bParts[i], 10);
        if (cmp !== 0) {
          return cmp;
        }
      }
      return aParts.length - bParts.length;
    }
    var diff = cmpVersion(a, b);
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
      throw "Unknown operator: ".concat(op);
  };
  function updateLabel(labelTxt, labelNode) {
    if (typeof labelTxt === "undefined")
      return;
    if (labelNode.length !== 1) {
      throw new Error("labelNode must be of length 1");
    }
    var emptyLabel = Array.isArray(labelTxt) && labelTxt.length === 0;
    if (emptyLabel) {
      labelNode.addClass("shiny-label-null");
    } else {
      labelNode.text(labelTxt);
      labelNode.removeClass("shiny-label-null");
    }
  }
  function getComputedLinkColor(el) {
    var a = document.createElement("a");
    a.href = "/";
    var div = document.createElement("div");
    div.style.setProperty("position", "absolute", "important");
    div.style.setProperty("top", "-1000px", "important");
    div.style.setProperty("left", "0", "important");
    div.style.setProperty("width", "30px", "important");
    div.style.setProperty("height", "10px", "important");
    div.appendChild(a);
    el.appendChild(div);
    var linkColor = window.getComputedStyle(a).getPropertyValue("color");
    el.removeChild(div);
    return linkColor;
  }
  function isBS3() {
    return !window.bootstrap;
  }

  // srcts/src/bindings/registry.ts
  function _typeof(obj) {
    "@babel/helpers - typeof";
    return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof(obj);
  }
  function _classCallCheck(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey(descriptor.key), descriptor);
    }
  }
  function _createClass(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty(obj, key, value) {
    key = _toPropertyKey(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey(arg) {
    var key = _toPrimitive(arg, "string");
    return _typeof(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive(input, hint) {
    if (_typeof(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var BindingRegistry = /* @__PURE__ */ function() {
    function BindingRegistry2() {
      _classCallCheck(this, BindingRegistry2);
      _defineProperty(this, "name", void 0);
      _defineProperty(this, "bindings", []);
      _defineProperty(this, "bindingNames", {});
    }
    _createClass(BindingRegistry2, [{
      key: "register",
      value: function register2(binding, bindingName) {
        var priority = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : 0;
        var bindingObj = {
          binding: binding,
          priority: priority
        };
        this.bindings.unshift(bindingObj);
        if (bindingName) {
          this.bindingNames[bindingName] = bindingObj;
          binding.name = bindingName;
        }
      }
    }, {
      key: "setPriority",
      value: function setPriority(bindingName, priority) {
        var bindingObj = this.bindingNames[bindingName];
        if (!bindingObj)
          throw "Tried to set priority on unknown binding " + bindingName;
        bindingObj.priority = priority || 0;
      }
    }, {
      key: "getPriority",
      value: function getPriority(bindingName) {
        var bindingObj = this.bindingNames[bindingName];
        if (!bindingObj)
          return false;
        return bindingObj.priority;
      }
    }, {
      key: "getBindings",
      value: function getBindings() {
        return mergeSort(this.bindings, function(a, b) {
          return b.priority - a.priority;
        });
      }
    }]);
    return BindingRegistry2;
  }();

  // srcts/src/bindings/input/inputBinding.ts
  var import_es_array_iterator2 = __toESM(require_es_array_iterator());
  function _typeof2(obj) {
    "@babel/helpers - typeof";
    return _typeof2 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof2(obj);
  }
  function _classCallCheck2(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties2(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey2(descriptor.key), descriptor);
    }
  }
  function _createClass2(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties2(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties2(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty2(obj, key, value) {
    key = _toPropertyKey2(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey2(arg) {
    var key = _toPrimitive2(arg, "string");
    return _typeof2(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive2(input, hint) {
    if (_typeof2(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof2(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputBinding = /* @__PURE__ */ function() {
    function InputBinding2() {
      _classCallCheck2(this, InputBinding2);
      _defineProperty2(this, "name", void 0);
    }
    _createClass2(InputBinding2, [{
      key: "find",
      value: function find2(scope) {
        throw "Not implemented";
        scope;
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return el.getAttribute("data-input-id") || el.id;
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return null;
        el;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        throw "Not implemented";
        el;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        el;
        callback;
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        el;
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        throw "Not implemented";
        el;
        data;
      }
    }, {
      key: "getState",
      value: function getState(el) {
        throw "Not implemented";
        el;
      }
    }, {
      key: "getRatePolicy",
      value: function getRatePolicy(el) {
        return null;
        el;
      }
    }, {
      key: "initialize",
      value: function initialize(el) {
        el;
      }
    }, {
      key: "dispose",
      value: function dispose(el) {
        el;
      }
    }]);
    return InputBinding2;
  }();

  // node_modules/core-js/modules/es.array.find.js
  var $18 = require_export();
  var $find = require_array_iteration().find;
  var addToUnscopables = require_add_to_unscopables();
  var FIND = "find";
  var SKIPS_HOLES = true;
  if (FIND in [])
    Array(1)[FIND](function() {
      SKIPS_HOLES = false;
    });
  $18({ target: "Array", proto: true, forced: SKIPS_HOLES }, {
    find: function find(callbackfn) {
      return $find(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });
  addToUnscopables(FIND);

  // node_modules/core-js/modules/es.object.set-prototype-of.js
  var $19 = require_export();
  var setPrototypeOf = require_object_set_prototype_of();
  $19({ target: "Object", stat: true }, {
    setPrototypeOf: setPrototypeOf
  });

  // node_modules/core-js/modules/es.object.get-prototype-of.js
  var $20 = require_export();
  var fails7 = require_fails();
  var toObject4 = require_to_object();
  var nativeGetPrototypeOf = require_object_get_prototype_of();
  var CORRECT_PROTOTYPE_GETTER = require_correct_prototype_getter();
  var FAILS_ON_PRIMITIVES2 = fails7(function() {
    nativeGetPrototypeOf(1);
  });
  $20({ target: "Object", stat: true, forced: FAILS_ON_PRIMITIVES2, sham: !CORRECT_PROTOTYPE_GETTER }, {
    getPrototypeOf: function getPrototypeOf(it) {
      return nativeGetPrototypeOf(toObject4(it));
    }
  });

  // node_modules/core-js/modules/es.reflect.to-string-tag.js
  var $21 = require_export();
  var global6 = require_global();
  var setToStringTag = require_set_to_string_tag();
  $21({ global: true }, { Reflect: {} });
  setToStringTag(global6.Reflect, "Reflect", true);

  // node_modules/core-js/modules/es.reflect.construct.js
  var $22 = require_export();
  var getBuiltIn = require_get_built_in();
  var apply2 = require_function_apply();
  var bind = require_function_bind();
  var aConstructor = require_a_constructor();
  var anObject4 = require_an_object();
  var isObject3 = require_is_object();
  var create = require_object_create();
  var fails8 = require_fails();
  var nativeConstruct = getBuiltIn("Reflect", "construct");
  var ObjectPrototype = Object.prototype;
  var push2 = [].push;
  var NEW_TARGET_BUG = fails8(function() {
    function F() {
    }
    return !(nativeConstruct(function() {
    }, [], F) instanceof F);
  });
  var ARGS_BUG = !fails8(function() {
    nativeConstruct(function() {
    });
  });
  var FORCED5 = NEW_TARGET_BUG || ARGS_BUG;
  $22({ target: "Reflect", stat: true, forced: FORCED5, sham: FORCED5 }, {
    construct: function construct(Target, args) {
      aConstructor(Target);
      anObject4(args);
      var newTarget = arguments.length < 3 ? Target : aConstructor(arguments[2]);
      if (ARGS_BUG && !NEW_TARGET_BUG)
        return nativeConstruct(Target, args, newTarget);
      if (Target == newTarget) {
        switch (args.length) {
          case 0:
            return new Target();
          case 1:
            return new Target(args[0]);
          case 2:
            return new Target(args[0], args[1]);
          case 3:
            return new Target(args[0], args[1], args[2]);
          case 4:
            return new Target(args[0], args[1], args[2], args[3]);
        }
        var $args = [null];
        apply2(push2, $args, args);
        return new (apply2(bind, Target, $args))();
      }
      var proto = newTarget.prototype;
      var instance = create(isObject3(proto) ? proto : ObjectPrototype);
      var result = apply2(Target, instance, args);
      return isObject3(result) ? result : instance;
    }
  });

  // srcts/src/bindings/input/checkbox.ts
  var import_es_array_iterator3 = __toESM(require_es_array_iterator());
  var import_jquery5 = __toESM(require_jquery());
  function _typeof3(obj) {
    "@babel/helpers - typeof";
    return _typeof3 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof3(obj);
  }
  function _classCallCheck3(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties3(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey3(descriptor.key), descriptor);
    }
  }
  function _createClass3(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties3(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties3(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey3(arg) {
    var key = _toPrimitive3(arg, "string");
    return _typeof3(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive3(input, hint) {
    if (_typeof3(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof3(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf(subClass, superClass);
  }
  function _setPrototypeOf(o, p) {
    _setPrototypeOf = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf(o, p);
  }
  function _createSuper(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn(this, result);
    };
  }
  function _possibleConstructorReturn(self2, call8) {
    if (call8 && (_typeof3(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized(self2);
  }
  function _assertThisInitialized(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf(o) {
    _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf(o);
  }
  var CheckboxInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits(CheckboxInputBinding2, _InputBinding);
    var _super = _createSuper(CheckboxInputBinding2);
    function CheckboxInputBinding2() {
      _classCallCheck3(this, CheckboxInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass3(CheckboxInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery5.default)(scope).find('input[type="checkbox"]');
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        return el.checked;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        el.checked = value;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery5.default)(el).on("change.checkboxInputBinding", function() {
          callback(true);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery5.default)(el).off(".checkboxInputBinding");
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          label: (0, import_jquery5.default)(el).parent().find("span").text(),
          value: el.checked
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        if (hasDefinedProperty(data, "value")) {
          el.checked = data.value;
        }
        if (hasDefinedProperty(data, "label")) {
          (0, import_jquery5.default)(el).parent().find("span").text(data.label);
        }
        (0, import_jquery5.default)(el).trigger("change");
      }
    }]);
    return CheckboxInputBinding2;
  }(InputBinding);

  // node_modules/core-js/modules/es.string.trim.js
  var $24 = require_export();
  var $trim = require_string_trim().trim;
  var forcedStringTrimMethod = require_string_trim_forced();
  $24({ target: "String", proto: true, forced: forcedStringTrimMethod("trim") }, {
    trim: function trim2() {
      return $trim(this);
    }
  });

  // srcts/src/bindings/input/checkboxgroup.ts
  var import_es_array_iterator4 = __toESM(require_es_array_iterator());
  var import_jquery6 = __toESM(require_jquery());
  function _typeof4(obj) {
    "@babel/helpers - typeof";
    return _typeof4 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof4(obj);
  }
  function _classCallCheck4(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties4(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey4(descriptor.key), descriptor);
    }
  }
  function _createClass4(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties4(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties4(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey4(arg) {
    var key = _toPrimitive4(arg, "string");
    return _typeof4(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive4(input, hint) {
    if (_typeof4(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof4(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits2(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf2(subClass, superClass);
  }
  function _setPrototypeOf2(o, p) {
    _setPrototypeOf2 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf2(o, p);
  }
  function _createSuper2(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct2();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf2(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf2(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn2(this, result);
    };
  }
  function _possibleConstructorReturn2(self2, call8) {
    if (call8 && (_typeof4(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized2(self2);
  }
  function _assertThisInitialized2(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct2() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf2(o) {
    _getPrototypeOf2 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf2(o);
  }
  function getLabelNode(el) {
    return (0, import_jquery6.default)(el).find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel(obj) {
    var parentNode = obj.parentNode;
    if (parentNode.tagName === "LABEL") {
      return (0, import_jquery6.default)(parentNode).find("span").text().trim();
    }
    return null;
  }
  var CheckboxGroupInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits2(CheckboxGroupInputBinding2, _InputBinding);
    var _super = _createSuper2(CheckboxGroupInputBinding2);
    function CheckboxGroupInputBinding2() {
      _classCallCheck4(this, CheckboxGroupInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass4(CheckboxGroupInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery6.default)(scope).find(".shiny-input-checkboxgroup");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $objs = (0, import_jquery6.default)('input:checkbox[name="' + $escape(el.id) + '"]:checked');
        var values2 = new Array($objs.length);
        for (var i = 0; i < $objs.length; i++) {
          values2[i] = $objs[i].value;
        }
        return values2;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        var _value;
        value = (_value = value) !== null && _value !== void 0 ? _value : [];
        (0, import_jquery6.default)('input:checkbox[name="' + $escape(el.id) + '"]').prop("checked", false);
        if (value instanceof Array) {
          for (var i = 0; i < value.length; i++) {
            (0, import_jquery6.default)('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]').prop("checked", true);
          }
        } else {
          (0, import_jquery6.default)('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $objs = (0, import_jquery6.default)('input:checkbox[name="' + $escape(el.id) + '"]');
        var options = new Array($objs.length);
        for (var i = 0; i < options.length; i++) {
          options[i] = {
            value: $objs[i].value,
            label: getLabel($objs[i])
          };
        }
        return {
          label: getLabelNode(el).text(),
          value: this.getValue(el),
          options: options
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery6.default)(el);
        if (hasDefinedProperty(data, "options")) {
          $el.find("div.shiny-options-group").remove();
          $el.find("label.checkbox").remove();
          $el.append(data.options);
        }
        if (hasDefinedProperty(data, "value")) {
          this.setValue(el, data.value);
        }
        updateLabel(data.label, getLabelNode(el));
        (0, import_jquery6.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery6.default)(el).on("change.checkboxGroupInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery6.default)(el).off(".checkboxGroupInputBinding");
      }
    }]);
    return CheckboxGroupInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/number.ts
  var import_es_regexp_exec3 = __toESM(require_es_regexp_exec());
  var import_es_array_iterator6 = __toESM(require_es_array_iterator());
  var import_jquery8 = __toESM(require_jquery());

  // node_modules/core-js/modules/es.reflect.get.js
  var $26 = require_export();
  var call3 = require_function_call();
  var isObject4 = require_is_object();
  var anObject5 = require_an_object();
  var isDataDescriptor = require_is_data_descriptor();
  var getOwnPropertyDescriptorModule = require_object_get_own_property_descriptor();
  var getPrototypeOf2 = require_object_get_prototype_of();
  function get(target, propertyKey) {
    var receiver = arguments.length < 3 ? target : arguments[2];
    var descriptor, prototype;
    if (anObject5(target) === receiver)
      return target[propertyKey];
    descriptor = getOwnPropertyDescriptorModule.f(target, propertyKey);
    if (descriptor)
      return isDataDescriptor(descriptor) ? descriptor.value : descriptor.get === void 0 ? void 0 : call3(descriptor.get, receiver);
    if (isObject4(prototype = getPrototypeOf2(target)))
      return get(prototype, propertyKey, receiver);
  }
  $26({ target: "Reflect", stat: true }, {
    get: get
  });

  // node_modules/core-js/modules/es.object.get-own-property-descriptor.js
  var $27 = require_export();
  var fails9 = require_fails();
  var toIndexedObject2 = require_to_indexed_object();
  var nativeGetOwnPropertyDescriptor = require_object_get_own_property_descriptor().f;
  var DESCRIPTORS5 = require_descriptors();
  var FORCED6 = !DESCRIPTORS5 || fails9(function() {
    nativeGetOwnPropertyDescriptor(1);
  });
  $27({ target: "Object", stat: true, forced: FORCED6, sham: !DESCRIPTORS5 }, {
    getOwnPropertyDescriptor: function getOwnPropertyDescriptor2(it, key) {
      return nativeGetOwnPropertyDescriptor(toIndexedObject2(it), key);
    }
  });

  // srcts/src/bindings/input/text.ts
  var import_es_array_iterator5 = __toESM(require_es_array_iterator());
  var import_jquery7 = __toESM(require_jquery());
  function _typeof5(obj) {
    "@babel/helpers - typeof";
    return _typeof5 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof5(obj);
  }
  function _classCallCheck5(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties5(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey5(descriptor.key), descriptor);
    }
  }
  function _createClass5(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties5(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties5(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey5(arg) {
    var key = _toPrimitive5(arg, "string");
    return _typeof5(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive5(input, hint) {
    if (_typeof5(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof5(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _get() {
    if (typeof Reflect !== "undefined" && Reflect.get) {
      _get = Reflect.get.bind();
    } else {
      _get = function _get2(target, property, receiver) {
        var base = _superPropBase(target, property);
        if (!base)
          return;
        var desc = Object.getOwnPropertyDescriptor(base, property);
        if (desc.get) {
          return desc.get.call(arguments.length < 3 ? target : receiver);
        }
        return desc.value;
      };
    }
    return _get.apply(this, arguments);
  }
  function _superPropBase(object, property) {
    while (!Object.prototype.hasOwnProperty.call(object, property)) {
      object = _getPrototypeOf3(object);
      if (object === null)
        break;
    }
    return object;
  }
  function _inherits3(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf3(subClass, superClass);
  }
  function _setPrototypeOf3(o, p) {
    _setPrototypeOf3 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf3(o, p);
  }
  function _createSuper3(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct3();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf3(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf3(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn3(this, result);
    };
  }
  function _possibleConstructorReturn3(self2, call8) {
    if (call8 && (_typeof5(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized3(self2);
  }
  function _assertThisInitialized3(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct3() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf3(o) {
    _getPrototypeOf3 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf3(o);
  }
  function getLabelNode2(el) {
    return (0, import_jquery7.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  var TextInputBindingBase = /* @__PURE__ */ function(_InputBinding) {
    _inherits3(TextInputBindingBase2, _InputBinding);
    var _super = _createSuper3(TextInputBindingBase2);
    function TextInputBindingBase2() {
      _classCallCheck5(this, TextInputBindingBase2);
      return _super.apply(this, arguments);
    }
    _createClass5(TextInputBindingBase2, [{
      key: "find",
      value: function find2(scope) {
        var $inputs = (0, import_jquery7.default)(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
        return $inputs.not('input[type="text"][id$="-selectized"]');
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return _get(_getPrototypeOf3(TextInputBindingBase2.prototype), "getId", this).call(this, el) || el.name;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        throw "not implemented";
        el;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        throw "not implemented";
        el;
        value;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery7.default)(el).on(
          "keyup.textInputBinding input.textInputBinding",
          function() {
            callback(true);
          }
        );
        (0, import_jquery7.default)(el).on(
          "change.textInputBinding",
          function() {
            callback(false);
          }
        );
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery7.default)(el).off(".textInputBinding");
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        throw "not implemented";
        el;
        data;
      }
    }, {
      key: "getState",
      value: function getState(el) {
        throw "not implemented";
        el;
      }
    }, {
      key: "getRatePolicy",
      value: function getRatePolicy(el) {
        return {
          policy: "debounce",
          delay: 250
        };
        el;
      }
    }]);
    return TextInputBindingBase2;
  }(InputBinding);
  var TextInputBinding = /* @__PURE__ */ function(_TextInputBindingBase) {
    _inherits3(TextInputBinding2, _TextInputBindingBase);
    var _super2 = _createSuper3(TextInputBinding2);
    function TextInputBinding2() {
      _classCallCheck5(this, TextInputBinding2);
      return _super2.apply(this, arguments);
    }
    _createClass5(TextInputBinding2, [{
      key: "setValue",
      value: function setValue(el, value) {
        el.value = value;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        return el.value;
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          label: getLabelNode2(el).text(),
          value: el.value,
          placeholder: el.placeholder
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        if (hasDefinedProperty(data, "value"))
          this.setValue(el, data.value);
        updateLabel(data.label, getLabelNode2(el));
        if (hasDefinedProperty(data, "placeholder"))
          el.placeholder = data.placeholder;
        (0, import_jquery7.default)(el).trigger("change");
      }
    }]);
    return TextInputBinding2;
  }(TextInputBindingBase);

  // srcts/src/bindings/input/number.ts
  function _typeof6(obj) {
    "@babel/helpers - typeof";
    return _typeof6 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof6(obj);
  }
  function _classCallCheck6(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties6(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey6(descriptor.key), descriptor);
    }
  }
  function _createClass6(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties6(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties6(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey6(arg) {
    var key = _toPrimitive6(arg, "string");
    return _typeof6(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive6(input, hint) {
    if (_typeof6(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof6(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits4(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf4(subClass, superClass);
  }
  function _setPrototypeOf4(o, p) {
    _setPrototypeOf4 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf4(o, p);
  }
  function _createSuper4(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct4();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf4(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf4(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn4(this, result);
    };
  }
  function _possibleConstructorReturn4(self2, call8) {
    if (call8 && (_typeof6(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized4(self2);
  }
  function _assertThisInitialized4(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct4() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf4(o) {
    _getPrototypeOf4 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf4(o);
  }
  function getLabelNode3(el) {
    return (0, import_jquery8.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  var NumberInputBinding = /* @__PURE__ */ function(_TextInputBindingBase) {
    _inherits4(NumberInputBinding2, _TextInputBindingBase);
    var _super = _createSuper4(NumberInputBinding2);
    function NumberInputBinding2() {
      _classCallCheck6(this, NumberInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass6(NumberInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery8.default)(scope).find('input[type="number"]');
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var numberVal = (0, import_jquery8.default)(el).val();
        if (typeof numberVal == "string") {
          if (/^\s*$/.test(numberVal))
            return null;
        }
        var numberValue = Number(numberVal);
        if (!isNaN(numberValue)) {
          return numberValue;
        }
        return numberVal;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        el.value = "" + value;
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return "shiny.number";
        el;
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var _data$value, _data$min, _data$max, _data$step;
        if (hasDefinedProperty(data, "value"))
          el.value = (_data$value = data.value) !== null && _data$value !== void 0 ? _data$value : "";
        if (hasDefinedProperty(data, "min"))
          el.min = (_data$min = data.min) !== null && _data$min !== void 0 ? _data$min : "";
        if (hasDefinedProperty(data, "max"))
          el.max = (_data$max = data.max) !== null && _data$max !== void 0 ? _data$max : "";
        if (hasDefinedProperty(data, "step"))
          el.step = (_data$step = data.step) !== null && _data$step !== void 0 ? _data$step : "";
        updateLabel(data.label, getLabelNode3(el));
        (0, import_jquery8.default)(el).trigger("change");
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          label: getLabelNode3(el).text(),
          value: this.getValue(el),
          min: Number(el.min),
          max: Number(el.max),
          step: Number(el.step)
        };
      }
    }]);
    return NumberInputBinding2;
  }(TextInputBindingBase);

  // srcts/src/bindings/input/password.ts
  var import_es_array_iterator7 = __toESM(require_es_array_iterator());
  var import_jquery9 = __toESM(require_jquery());
  function _typeof7(obj) {
    "@babel/helpers - typeof";
    return _typeof7 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof7(obj);
  }
  function _classCallCheck7(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties7(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey7(descriptor.key), descriptor);
    }
  }
  function _createClass7(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties7(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties7(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey7(arg) {
    var key = _toPrimitive7(arg, "string");
    return _typeof7(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive7(input, hint) {
    if (_typeof7(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof7(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits5(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf5(subClass, superClass);
  }
  function _setPrototypeOf5(o, p) {
    _setPrototypeOf5 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf5(o, p);
  }
  function _createSuper5(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct5();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf5(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf5(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn5(this, result);
    };
  }
  function _possibleConstructorReturn5(self2, call8) {
    if (call8 && (_typeof7(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized5(self2);
  }
  function _assertThisInitialized5(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct5() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf5(o) {
    _getPrototypeOf5 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf5(o);
  }
  var PasswordInputBinding = /* @__PURE__ */ function(_TextInputBinding) {
    _inherits5(PasswordInputBinding2, _TextInputBinding);
    var _super = _createSuper5(PasswordInputBinding2);
    function PasswordInputBinding2() {
      _classCallCheck7(this, PasswordInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass7(PasswordInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery9.default)(scope).find('input[type="password"]');
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return "shiny.password";
        el;
      }
    }]);
    return PasswordInputBinding2;
  }(TextInputBinding);

  // srcts/src/bindings/input/textarea.ts
  var import_es_array_iterator8 = __toESM(require_es_array_iterator());
  var import_jquery10 = __toESM(require_jquery());
  function _typeof8(obj) {
    "@babel/helpers - typeof";
    return _typeof8 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof8(obj);
  }
  function _classCallCheck8(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties8(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey8(descriptor.key), descriptor);
    }
  }
  function _createClass8(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties8(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties8(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey8(arg) {
    var key = _toPrimitive8(arg, "string");
    return _typeof8(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive8(input, hint) {
    if (_typeof8(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof8(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits6(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf6(subClass, superClass);
  }
  function _setPrototypeOf6(o, p) {
    _setPrototypeOf6 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf6(o, p);
  }
  function _createSuper6(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct6();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf6(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf6(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn6(this, result);
    };
  }
  function _possibleConstructorReturn6(self2, call8) {
    if (call8 && (_typeof8(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized6(self2);
  }
  function _assertThisInitialized6(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct6() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf6(o) {
    _getPrototypeOf6 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf6(o);
  }
  var TextareaInputBinding = /* @__PURE__ */ function(_TextInputBinding) {
    _inherits6(TextareaInputBinding2, _TextInputBinding);
    var _super = _createSuper6(TextareaInputBinding2);
    function TextareaInputBinding2() {
      _classCallCheck8(this, TextareaInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass8(TextareaInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery10.default)(scope).find("textarea");
      }
    }]);
    return TextareaInputBinding2;
  }(TextInputBinding);

  // srcts/src/bindings/input/radio.ts
  var import_es_array_iterator9 = __toESM(require_es_array_iterator());
  var import_jquery11 = __toESM(require_jquery());
  function _typeof9(obj) {
    "@babel/helpers - typeof";
    return _typeof9 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof9(obj);
  }
  function _classCallCheck9(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties9(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey9(descriptor.key), descriptor);
    }
  }
  function _createClass9(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties9(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties9(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey9(arg) {
    var key = _toPrimitive9(arg, "string");
    return _typeof9(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive9(input, hint) {
    if (_typeof9(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof9(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits7(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf7(subClass, superClass);
  }
  function _setPrototypeOf7(o, p) {
    _setPrototypeOf7 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf7(o, p);
  }
  function _createSuper7(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct7();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf7(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf7(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn7(this, result);
    };
  }
  function _possibleConstructorReturn7(self2, call8) {
    if (call8 && (_typeof9(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized7(self2);
  }
  function _assertThisInitialized7(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct7() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf7(o) {
    _getPrototypeOf7 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf7(o);
  }
  function getLabelNode4(el) {
    return (0, import_jquery11.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel2(obj) {
    var parentNode = obj.parentNode;
    if (parentNode.tagName === "LABEL") {
      return (0, import_jquery11.default)(parentNode).find("span").text().trim();
    }
    return null;
  }
  var RadioInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits7(RadioInputBinding2, _InputBinding);
    var _super = _createSuper7(RadioInputBinding2);
    function RadioInputBinding2() {
      _classCallCheck9(this, RadioInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass9(RadioInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery11.default)(scope).find(".shiny-input-radiogroup");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var checkedItems = (0, import_jquery11.default)('input:radio[name="' + $escape(el.id) + '"]:checked');
        if (checkedItems.length === 0) {
          return null;
        }
        return checkedItems.val();
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (Array.isArray(value) && value.length === 0) {
          (0, import_jquery11.default)('input:radio[name="' + $escape(el.id) + '"]').prop("checked", false);
        } else {
          (0, import_jquery11.default)('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $objs = (0, import_jquery11.default)('input:radio[name="' + $escape(el.id) + '"]');
        var options = new Array($objs.length);
        for (var i = 0; i < options.length; i++) {
          options[i] = {
            value: $objs[i].value,
            label: getLabel2($objs[i])
          };
        }
        return {
          label: getLabelNode4(el).text(),
          value: this.getValue(el),
          options: options
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery11.default)(el);
        if (hasDefinedProperty(data, "options")) {
          $el.find("div.shiny-options-group").remove();
          $el.find("label.radio").remove();
          $el.append(data.options);
        }
        if (hasDefinedProperty(data, "value")) {
          this.setValue(el, data.value);
        }
        updateLabel(data.label, getLabelNode4(el));
        (0, import_jquery11.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery11.default)(el).on("change.radioInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery11.default)(el).off(".radioInputBinding");
      }
    }]);
    return RadioInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/date.ts
  var import_es_array_iterator10 = __toESM(require_es_array_iterator());
  var import_jquery12 = __toESM(require_jquery());
  function _typeof10(obj) {
    "@babel/helpers - typeof";
    return _typeof10 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof10(obj);
  }
  function _classCallCheck10(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties10(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey10(descriptor.key), descriptor);
    }
  }
  function _createClass10(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties10(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties10(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey10(arg) {
    var key = _toPrimitive10(arg, "string");
    return _typeof10(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive10(input, hint) {
    if (_typeof10(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof10(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits8(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf8(subClass, superClass);
  }
  function _setPrototypeOf8(o, p) {
    _setPrototypeOf8 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf8(o, p);
  }
  function _createSuper8(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct8();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf8(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf8(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn8(this, result);
    };
  }
  function _possibleConstructorReturn8(self2, call8) {
    if (call8 && (_typeof10(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized8(self2);
  }
  function _assertThisInitialized8(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct8() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf8(o) {
    _getPrototypeOf8 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf8(o);
  }
  var DateInputBindingBase = /* @__PURE__ */ function(_InputBinding) {
    _inherits8(DateInputBindingBase2, _InputBinding);
    var _super = _createSuper8(DateInputBindingBase2);
    function DateInputBindingBase2() {
      _classCallCheck10(this, DateInputBindingBase2);
      return _super.apply(this, arguments);
    }
    _createClass10(DateInputBindingBase2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery12.default)(scope).find(".shiny-date-input");
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return "shiny.date";
        el;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery12.default)(el).on(
          "keyup.dateInputBinding input.dateInputBinding",
          function() {
            callback(true);
          }
        );
        (0, import_jquery12.default)(el).on(
          "changeDate.dateInputBinding change.dateInputBinding",
          function() {
            callback(false);
          }
        );
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery12.default)(el).off(".dateInputBinding");
      }
    }, {
      key: "getRatePolicy",
      value: function getRatePolicy() {
        return {
          policy: "debounce",
          delay: 250
        };
      }
    }, {
      key: "setValue",
      value: function setValue(el, data) {
        throw "not implemented";
        el;
        data;
      }
    }, {
      key: "initialize",
      value: function initialize(el) {
        var $input = (0, import_jquery12.default)(el).find("input");
        var date = $input.data("initial-date");
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
    }, {
      key: "_getLabelNode",
      value: function _getLabelNode(el) {
        return (0, import_jquery12.default)(el).find('label[for="' + $escape(el.id) + '"]');
      }
    }, {
      key: "_formatToString",
      value: function _formatToString(format) {
        var str = "";
        var i;
        for (i = 0; i < format.parts.length; i++) {
          str += format.separators[i] + format.parts[i];
        }
        str += format.separators[i];
        return str;
      }
    }, {
      key: "_setMin",
      value: function _setMin(el, date) {
        if (date === null) {
          (0, import_jquery12.default)(el).bsDatepicker("setStartDate", null);
          return;
        }
        var parsedDate = this._newDate(date);
        if (parsedDate === null)
          return;
        date = parsedDate;
        if (isNaN(date.valueOf()))
          return;
        var curValue = (0, import_jquery12.default)(el).bsDatepicker("getUTCDate");
        (0, import_jquery12.default)(el).bsDatepicker("setStartDate", this._utcDateAsLocal(date));
        if (date && curValue && date.getTime() > curValue.getTime()) {
          (0, import_jquery12.default)(el).bsDatepicker("clearDates");
        } else {
          (0, import_jquery12.default)(el).bsDatepicker("setUTCDate", curValue);
        }
      }
    }, {
      key: "_setMax",
      value: function _setMax(el, date) {
        if (date === null) {
          (0, import_jquery12.default)(el).bsDatepicker("setEndDate", null);
          return;
        }
        var parsedDate = this._newDate(date);
        if (parsedDate === null)
          return;
        date = parsedDate;
        if (isNaN(date.valueOf()))
          return;
        var curValue = (0, import_jquery12.default)(el).bsDatepicker("getUTCDate");
        (0, import_jquery12.default)(el).bsDatepicker("setEndDate", this._utcDateAsLocal(date));
        if (date && curValue && date.getTime() < curValue.getTime()) {
          (0, import_jquery12.default)(el).bsDatepicker("clearDates");
        } else {
          (0, import_jquery12.default)(el).bsDatepicker("setUTCDate", curValue);
        }
      }
    }, {
      key: "_newDate",
      value: function _newDate(date) {
        if (date instanceof Date)
          return date;
        if (!date)
          return null;
        var d = parseDate(date);
        if (isNaN(d.valueOf()))
          return null;
        return d;
      }
    }, {
      key: "_floorDateTime",
      value: function _floorDateTime(date) {
        date = new Date(date.getTime());
        date.setUTCHours(0, 0, 0, 0);
        return date;
      }
    }, {
      key: "_dateAsUTC",
      value: function _dateAsUTC(date) {
        return new Date(date.getTime() - date.getTimezoneOffset() * 6e4);
      }
    }, {
      key: "_utcDateAsLocal",
      value: function _utcDateAsLocal(date) {
        return new Date(date.getTime() + date.getTimezoneOffset() * 6e4);
      }
    }]);
    return DateInputBindingBase2;
  }(InputBinding);
  var DateInputBinding = /* @__PURE__ */ function(_DateInputBindingBase) {
    _inherits8(DateInputBinding2, _DateInputBindingBase);
    var _super2 = _createSuper8(DateInputBinding2);
    function DateInputBinding2() {
      _classCallCheck10(this, DateInputBinding2);
      return _super2.apply(this, arguments);
    }
    _createClass10(DateInputBinding2, [{
      key: "getValue",
      value: function getValue(el) {
        var date = (0, import_jquery12.default)(el).find("input").bsDatepicker("getUTCDate");
        return formatDateUTC(date);
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (value === null) {
          (0, import_jquery12.default)(el).find("input").val("").bsDatepicker("update");
          return;
        }
        var date = this._newDate(value);
        if (date === null) {
          return;
        }
        if (isNaN(date.valueOf()))
          return;
        (0, import_jquery12.default)(el).find("input").bsDatepicker("setUTCDate", date);
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $el = (0, import_jquery12.default)(el);
        var $input = $el.find("input");
        var min4 = $input.data("datepicker").startDate;
        var max4 = $input.data("datepicker").endDate;
        min4 = min4 === -Infinity ? null : formatDateUTC(min4);
        max4 = max4 === Infinity ? null : formatDateUTC(max4);
        var startview = $input.data("datepicker").startViewMode;
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
          min: min4,
          max: max4,
          language: $input.data("datepicker").language,
          weekstart: $input.data("datepicker").weekStart,
          format: this._formatToString($input.data("datepicker").format),
          startview: startview
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $input = (0, import_jquery12.default)(el).find("input");
        updateLabel(data.label, this._getLabelNode(el));
        if (hasDefinedProperty(data, "min"))
          this._setMin($input[0], data.min);
        if (hasDefinedProperty(data, "max"))
          this._setMax($input[0], data.max);
        if (hasDefinedProperty(data, "value"))
          this.setValue(el, data.value);
        (0, import_jquery12.default)(el).trigger("change");
      }
    }]);
    return DateInputBinding2;
  }(DateInputBindingBase);

  // srcts/src/bindings/input/slider.ts
  var import_es_regexp_exec4 = __toESM(require_es_regexp_exec());
  var import_es_array_iterator11 = __toESM(require_es_array_iterator());
  var import_jquery13 = __toESM(require_jquery());
  function _typeof11(obj) {
    "@babel/helpers - typeof";
    return _typeof11 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof11(obj);
  }
  function _classCallCheck11(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties11(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey11(descriptor.key), descriptor);
    }
  }
  function _createClass11(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties11(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties11(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey11(arg) {
    var key = _toPrimitive11(arg, "string");
    return _typeof11(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive11(input, hint) {
    if (_typeof11(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof11(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits9(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf9(subClass, superClass);
  }
  function _setPrototypeOf9(o, p) {
    _setPrototypeOf9 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf9(o, p);
  }
  function _createSuper9(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct9();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf9(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf9(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn9(this, result);
    };
  }
  function _possibleConstructorReturn9(self2, call8) {
    if (call8 && (_typeof11(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized9(self2);
  }
  function _assertThisInitialized9(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct9() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf9(o) {
    _getPrototypeOf9 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf9(o);
  }
  function forceIonSliderUpdate(slider) {
    if (slider.$cache && slider.$cache.input)
      slider.$cache.input.trigger("change");
    else
      console.log("Couldn't force ion slider to update");
  }
  function getTypePrettifyer(dataType, timeFormat, timezone) {
    var timeFormatter;
    var prettify;
    if (dataType === "date") {
      timeFormatter = window.strftime.utc();
      prettify = function prettify2(num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else if (dataType === "datetime") {
      if (timezone)
        timeFormatter = window.strftime.timezone(timezone);
      else
        timeFormatter = window.strftime;
      prettify = function prettify2(num) {
        return timeFormatter(timeFormat, new Date(num));
      };
    } else {
      prettify = function prettify2(num) {
        return formatNumber(num, this.prettify_separator);
      };
    }
    return prettify;
  }
  function getLabelNode5(el) {
    return (0, import_jquery13.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function numValues(el) {
    if ((0, import_jquery13.default)(el).data("ionRangeSlider").options.type === "double")
      return 2;
    else
      return 1;
  }
  var SliderInputBinding = /* @__PURE__ */ function(_TextInputBindingBase) {
    _inherits9(SliderInputBinding2, _TextInputBindingBase);
    var _super = _createSuper9(SliderInputBinding2);
    function SliderInputBinding2() {
      _classCallCheck11(this, SliderInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass11(SliderInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        if (!import_jquery13.default.fn.ionRangeSlider) {
          return (0, import_jquery13.default)();
        }
        return (0, import_jquery13.default)(scope).find("input.js-range-slider");
      }
    }, {
      key: "getType",
      value: function getType(el) {
        var dataType = (0, import_jquery13.default)(el).data("data-type");
        if (dataType === "date")
          return "shiny.date";
        else if (dataType === "datetime")
          return "shiny.datetime";
        else
          return null;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $el = (0, import_jquery13.default)(el);
        var result = (0, import_jquery13.default)(el).data("ionRangeSlider").result;
        var convert;
        var dataType = $el.data("data-type");
        if (dataType === "date") {
          convert = function convert2(val) {
            return formatDateUTC(new Date(Number(val)));
          };
        } else if (dataType === "datetime") {
          convert = function convert2(val) {
            return Number(val) / 1e3;
          };
        } else {
          convert = function convert2(val) {
            return Number(val);
          };
        }
        if (numValues(el) === 2) {
          return [convert(result.from), convert(result.to)];
        } else {
          return convert(result.from);
        }
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        var $el = (0, import_jquery13.default)(el);
        var slider = $el.data("ionRangeSlider");
        $el.data("immediate", true);
        try {
          if (numValues(el) === 2 && value instanceof Array) {
            slider.update({
              from: value[0],
              to: value[1]
            });
          } else {
            slider.update({
              from: value
            });
          }
          forceIonSliderUpdate(slider);
        } finally {
          $el.data("immediate", false);
        }
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery13.default)(el).on("change.sliderInputBinding", function() {
          callback(!(0, import_jquery13.default)(el).data("immediate") && !(0, import_jquery13.default)(el).data("animating"));
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery13.default)(el).off(".sliderInputBinding");
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery13.default)(el);
        var slider = $el.data("ionRangeSlider");
        var msg = {};
        if (hasDefinedProperty(data, "value")) {
          if (numValues(el) === 2 && data.value instanceof Array) {
            msg.from = data.value[0];
            msg.to = data.value[1];
          } else {
            if (Array.isArray(data.value)) {
              var errorReason = ["an empty array.", "a single-value array.", "an array with more than two values."];
              throw "Slider requires two values to update with an array, but message value was " + errorReason[Math.min(data.value.length, 2)];
            }
            msg.from = data.value;
          }
        }
        var sliderFeatures = ["min", "max", "step"];
        for (var i = 0; i < sliderFeatures.length; i++) {
          var feats = sliderFeatures[i];
          if (hasDefinedProperty(data, feats)) {
            msg[feats] = data[feats];
          }
        }
        updateLabel(data.label, getLabelNode5(el));
        var domElements = ["data-type", "time-format", "timezone"];
        for (var _i = 0; _i < domElements.length; _i++) {
          var elem = domElements[_i];
          if (hasDefinedProperty(data, elem)) {
            $el.data(elem, data[elem]);
          }
        }
        var dataType = $el.data("data-type");
        var timeFormat = $el.data("time-format");
        var timezone = $el.data("timezone");
        msg.prettify = getTypePrettifyer(dataType, timeFormat, timezone);
        $el.data("immediate", true);
        try {
          slider.update(msg);
          forceIonSliderUpdate(slider);
        } finally {
          $el.data("immediate", false);
        }
      }
    }, {
      key: "getRatePolicy",
      value: function getRatePolicy(el) {
        return {
          policy: "debounce",
          delay: 250
        };
        el;
      }
    }, {
      key: "getState",
      value: function getState(el) {
        el;
      }
    }, {
      key: "initialize",
      value: function initialize(el) {
        var $el = (0, import_jquery13.default)(el);
        var dataType = $el.data("data-type");
        var timeFormat = $el.data("time-format");
        var timezone = $el.data("timezone");
        var opts = {
          prettify: getTypePrettifyer(dataType, timeFormat, timezone)
        };
        $el.ionRangeSlider(opts);
      }
    }]);
    return SliderInputBinding2;
  }(TextInputBindingBase);
  function formatNumber(num) {
    var thousandSep = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : ",";
    var decimalSep = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : ".";
    var parts = num.toString().split(".");
    parts[0] = parts[0].replace(/(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g, "$1" + thousandSep);
    if (parts.length === 1)
      return parts[0];
    else if (parts.length === 2)
      return parts[0] + decimalSep + parts[1];
    else
      return "";
  }
  (0, import_jquery13.default)(document).on("click", ".slider-animate-button", function(evt) {
    evt.preventDefault();
    var self2 = (0, import_jquery13.default)(this);
    var target = (0, import_jquery13.default)("#" + $escape(self2.attr("data-target-id")));
    var startLabel = "Play";
    var stopLabel = "Pause";
    var loop = self2.attr("data-loop") !== void 0 && !/^\s*false\s*$/i.test(self2.attr("data-loop"));
    var animInterval = self2.attr("data-interval");
    if (isNaN(animInterval))
      animInterval = 1500;
    else
      animInterval = Number(animInterval);
    if (!target.data("animTimer")) {
      var timer;
      if (target.hasClass("jslider")) {
        var slider = target.slider();
        if (!slider.canStepNext())
          slider.resetToStart();
        timer = setInterval(function() {
          if (loop && !slider.canStepNext()) {
            slider.resetToStart();
          } else {
            slider.stepNext();
            if (!loop && !slider.canStepNext()) {
              self2.click();
            }
          }
        }, animInterval);
      } else {
        var _slider = target.data("ionRangeSlider");
        var sliderCanStep = function sliderCanStep2() {
          if (_slider.options.type === "double")
            return _slider.result.to < _slider.result.max;
          else
            return _slider.result.from < _slider.result.max;
        };
        var sliderReset = function sliderReset2() {
          var val = {
            from: _slider.result.min
          };
          if (_slider.options.type === "double")
            val.to = val.from + (_slider.result.to - _slider.result.from);
          _slider.update(val);
          forceIonSliderUpdate(_slider);
        };
        var sliderStep = function sliderStep2() {
          var val = {
            from: Math.min(_slider.result.max, _slider.result.from + _slider.options.step)
          };
          if (_slider.options.type === "double")
            val.to = Math.min(_slider.result.max, _slider.result.to + _slider.options.step);
          _slider.update(val);
          forceIonSliderUpdate(_slider);
        };
        if (!sliderCanStep())
          sliderReset();
        timer = setInterval(function() {
          if (loop && !sliderCanStep()) {
            sliderReset();
          } else {
            sliderStep();
            if (!loop && !sliderCanStep()) {
              self2.click();
            }
          }
        }, animInterval);
      }
      target.data("animTimer", timer);
      self2.attr("title", stopLabel);
      self2.addClass("playing");
      target.data("animating", true);
    } else {
      clearTimeout(target.data("animTimer"));
      target.removeData("animTimer");
      self2.attr("title", startLabel);
      self2.removeClass("playing");
      target.removeData("animating");
    }
  });

  // srcts/src/bindings/input/daterange.ts
  var import_es_array_iterator12 = __toESM(require_es_array_iterator());
  var import_jquery14 = __toESM(require_jquery());
  function _typeof12(obj) {
    "@babel/helpers - typeof";
    return _typeof12 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof12(obj);
  }
  function _classCallCheck12(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties12(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey12(descriptor.key), descriptor);
    }
  }
  function _createClass12(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties12(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties12(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey12(arg) {
    var key = _toPrimitive12(arg, "string");
    return _typeof12(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive12(input, hint) {
    if (_typeof12(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof12(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits10(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf10(subClass, superClass);
  }
  function _setPrototypeOf10(o, p) {
    _setPrototypeOf10 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf10(o, p);
  }
  function _createSuper10(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct10();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf10(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf10(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn10(this, result);
    };
  }
  function _possibleConstructorReturn10(self2, call8) {
    if (call8 && (_typeof12(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized10(self2);
  }
  function _assertThisInitialized10(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct10() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf10(o) {
    _getPrototypeOf10 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf10(o);
  }
  function getLabelNode6(el) {
    return (0, import_jquery14.default)(el).find('label[for="' + $escape(el.id) + '"]');
  }
  var DateRangeInputBinding = /* @__PURE__ */ function(_DateInputBindingBase) {
    _inherits10(DateRangeInputBinding2, _DateInputBindingBase);
    var _super = _createSuper10(DateRangeInputBinding2);
    function DateRangeInputBinding2() {
      _classCallCheck12(this, DateRangeInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass12(DateRangeInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery14.default)(scope).find(".shiny-date-range-input");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $inputs = (0, import_jquery14.default)(el).find("input");
        var start = $inputs.eq(0).bsDatepicker("getUTCDate");
        var end = $inputs.eq(1).bsDatepicker("getUTCDate");
        return [formatDateUTC(start), formatDateUTC(end)];
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (!(value instanceof Object)) {
          return;
        }
        var $inputs = (0, import_jquery14.default)(el).find("input");
        if (value.start !== void 0) {
          if (value.start === null) {
            $inputs.eq(0).val("").bsDatepicker("update");
          } else {
            var start = this._newDate(value.start);
            $inputs.eq(0).bsDatepicker("setUTCDate", start);
          }
        }
        if (value.end !== void 0) {
          if (value.end === null) {
            $inputs.eq(1).val("").bsDatepicker("update");
          } else {
            var end = this._newDate(value.end);
            $inputs.eq(1).bsDatepicker("setUTCDate", end);
          }
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $el = (0, import_jquery14.default)(el);
        var $inputs = $el.find("input");
        var $startinput = $inputs.eq(0);
        var $endinput = $inputs.eq(1);
        var min4 = $startinput.bsDatepicker("getStartDate");
        var max4 = $startinput.bsDatepicker("getEndDate");
        var minStr = min4 === -Infinity ? null : formatDateUTC(min4);
        var maxStr = max4 === Infinity ? null : formatDateUTC(max4);
        var startview = $startinput.data("datepicker").startView;
        if (startview === 2)
          startview = "decade";
        else if (startview === 1)
          startview = "year";
        else if (startview === 0)
          startview = "month";
        return {
          label: getLabelNode6(el).text(),
          value: this.getValue(el),
          valueString: [$startinput.val(), $endinput.val()],
          min: minStr,
          max: maxStr,
          weekstart: $startinput.data("datepicker").weekStart,
          format: this._formatToString($startinput.data("datepicker").format),
          language: $startinput.data("datepicker").language,
          startview: startview
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery14.default)(el);
        var $inputs = $el.find("input");
        var $startinput = $inputs.eq(0);
        var $endinput = $inputs.eq(1);
        updateLabel(data.label, getLabelNode6(el));
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
    }, {
      key: "initialize",
      value: function initialize(el) {
        var $el = (0, import_jquery14.default)(el);
        var $inputs = $el.find("input");
        var $startinput = $inputs.eq(0);
        var $endinput = $inputs.eq(1);
        var start = $startinput.data("initial-date");
        var end = $endinput.data("initial-date");
        if (start === void 0 || start === null)
          start = this._dateAsUTC(new Date());
        if (end === void 0 || end === null)
          end = this._dateAsUTC(new Date());
        this.setValue(el, {
          start: start,
          end: end
        });
        this._setMin($startinput[0], $startinput.data("min-date"));
        this._setMin($endinput[0], $startinput.data("min-date"));
        this._setMax($startinput[0], $endinput.data("max-date"));
        this._setMax($endinput[0], $endinput.data("max-date"));
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery14.default)(el).on(
          "keyup.dateRangeInputBinding input.dateRangeInputBinding",
          function() {
            callback(true);
          }
        );
        (0, import_jquery14.default)(el).on(
          "changeDate.dateRangeInputBinding change.dateRangeInputBinding",
          function() {
            callback(false);
          }
        );
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery14.default)(el).off(".dateRangeInputBinding");
      }
    }]);
    return DateRangeInputBinding2;
  }(DateInputBindingBase);

  // srcts/src/bindings/input/selectInput.ts
  var import_es_json_stringify = __toESM(require_es_json_stringify());
  var import_es_array_iterator13 = __toESM(require_es_array_iterator());
  var import_jquery15 = __toESM(require_jquery());

  // srcts/src/utils/eval.ts
  var indirectEval = eval;

  // srcts/src/bindings/input/selectInput.ts
  function _typeof13(obj) {
    "@babel/helpers - typeof";
    return _typeof13 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof13(obj);
  }
  function _classCallCheck13(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties13(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey13(descriptor.key), descriptor);
    }
  }
  function _createClass13(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties13(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties13(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey13(arg) {
    var key = _toPrimitive13(arg, "string");
    return _typeof13(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive13(input, hint) {
    if (_typeof13(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof13(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits11(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf11(subClass, superClass);
  }
  function _setPrototypeOf11(o, p) {
    _setPrototypeOf11 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf11(o, p);
  }
  function _createSuper11(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct11();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf11(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf11(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn11(this, result);
    };
  }
  function _possibleConstructorReturn11(self2, call8) {
    if (call8 && (_typeof13(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized11(self2);
  }
  function _assertThisInitialized11(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct11() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf11(o) {
    _getPrototypeOf11 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf11(o);
  }
  function getLabelNode7(el) {
    var escapedId = $escape(el.id);
    if (isSelectize(el)) {
      escapedId += "-selectized";
    }
    return (0, import_jquery15.default)(el).parent().parent().find('label[for="' + escapedId + '"]');
  }
  function isSelectize(el) {
    var config = (0, import_jquery15.default)(el).parent().find('script[data-for="' + $escape(el.id) + '"]');
    return config.length > 0;
  }
  var SelectInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits11(SelectInputBinding2, _InputBinding);
    var _super = _createSuper11(SelectInputBinding2);
    function SelectInputBinding2() {
      _classCallCheck13(this, SelectInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass13(SelectInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery15.default)(scope).find("select");
      }
    }, {
      key: "getType",
      value: function getType(el) {
        var $el = (0, import_jquery15.default)(el);
        if (!$el.hasClass("symbol")) {
          return null;
        }
        if ($el.attr("multiple") === "multiple") {
          return "shiny.symbolList";
        } else {
          return "shiny.symbol";
        }
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return InputBinding.prototype.getId.call(this, el) || el.name;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        return (0, import_jquery15.default)(el).val();
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (!isSelectize(el)) {
          (0, import_jquery15.default)(el).val(value);
        } else {
          var selectize = this._selectize(el);
          selectize === null || selectize === void 0 ? void 0 : selectize.setValue(value);
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var options = new Array(el.length);
        for (var i = 0; i < el.length; i++) {
          options[i] = {
            value: el[i].value,
            label: el[i].label
          };
        }
        return {
          label: getLabelNode7(el),
          value: this.getValue(el),
          options: options
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery15.default)(el);
        if (hasDefinedProperty(data, "options")) {
          var selectize = this._selectize(el);
          selectize === null || selectize === void 0 ? void 0 : selectize.destroy();
          $el.empty().append(data.options);
          this._selectize(el);
        }
        if (hasDefinedProperty(data, "config")) {
          $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
          this._selectize(el, true);
        }
        if (hasDefinedProperty(data, "url")) {
          var _selectize2 = this._selectize(el);
          _selectize2.clearOptions();
          var loaded = false;
          _selectize2.settings.load = function(query, callback) {
            var settings = _selectize2.settings;
            import_jquery15.default.ajax({
              url: data.url,
              data: {
                query: query,
                field: JSON.stringify([settings.searchField]),
                value: settings.valueField,
                conju: settings.searchConjunction,
                maxop: settings.maxOptions
              },
              type: "GET",
              error: function error() {
                callback();
              },
              success: function success(res) {
                import_jquery15.default.each(res, function(index, elem) {
                  var optgroupId = elem[settings.optgroupField || "optgroup"];
                  var optgroup = {};
                  optgroup[settings.optgroupLabelField || "label"] = optgroupId;
                  optgroup[settings.optgroupValueField || "value"] = optgroupId;
                  _selectize2.addOptionGroup(optgroupId, optgroup);
                });
                callback(res);
                if (!loaded) {
                  if (hasDefinedProperty(data, "value")) {
                    _selectize2.setValue(data.value);
                  } else if (settings.maxItems === 1) {
                    _selectize2.setValue(res[0].value);
                  }
                }
                loaded = true;
              }
            });
          };
          _selectize2.load(function(callback) {
            _selectize2.settings.load.apply(_selectize2, ["", callback]);
          });
        } else if (hasDefinedProperty(data, "value")) {
          this.setValue(el, data.value);
        }
        updateLabel(data.label, getLabelNode7(el));
        (0, import_jquery15.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        var _this = this;
        (0, import_jquery15.default)(el).on(
          "change.selectInputBinding",
          function() {
            if (el.nonempty && _this.getValue(el) === "") {
              return;
            }
            callback(false);
          }
        );
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery15.default)(el).off(".selectInputBinding");
      }
    }, {
      key: "initialize",
      value: function initialize(el) {
        this._selectize(el);
      }
    }, {
      key: "_selectize",
      value: function _selectize(el) {
        var update = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
        if (!import_jquery15.default.fn.selectize)
          return void 0;
        var $el = (0, import_jquery15.default)(el);
        var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
        if (config.length === 0)
          return void 0;
        var options = import_jquery15.default.extend({
          labelField: "label",
          valueField: "value",
          searchField: ["label"]
        }, JSON.parse(config.html()));
        if (typeof config.data("nonempty") !== "undefined") {
          el.nonempty = true;
          options = import_jquery15.default.extend(options, {
            onItemRemove: function onItemRemove(value) {
              if (this.getValue() === "")
                (0, import_jquery15.default)("select#" + $escape(el.id)).empty().append((0, import_jquery15.default)("<option/>", {
                  value: value,
                  selected: true
                })).trigger("change");
            },
            onDropdownClose: function onDropdownClose() {
              if (this.getValue() === "") {
                this.setValue((0, import_jquery15.default)("select#" + $escape(el.id)).val());
              }
            }
          });
        } else {
          el.nonempty = false;
        }
        if (config.data("eval") instanceof Array)
          import_jquery15.default.each(config.data("eval"), function(i, x) {
            options[x] = indirectEval("(" + options[x] + ")");
          });
        var control = $el.selectize(options)[0].selectize;
        if (update) {
          var settings = import_jquery15.default.extend(control.settings, options);
          control.destroy();
          control = $el.selectize(settings)[0].selectize;
        }
        return control;
      }
    }]);
    return SelectInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/actionbutton.ts
  var import_es_array_iterator14 = __toESM(require_es_array_iterator());
  var import_jquery16 = __toESM(require_jquery());
  function _typeof14(obj) {
    "@babel/helpers - typeof";
    return _typeof14 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof14(obj);
  }
  function _classCallCheck14(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties14(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey14(descriptor.key), descriptor);
    }
  }
  function _createClass14(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties14(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties14(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey14(arg) {
    var key = _toPrimitive14(arg, "string");
    return _typeof14(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive14(input, hint) {
    if (_typeof14(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof14(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits12(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf12(subClass, superClass);
  }
  function _setPrototypeOf12(o, p) {
    _setPrototypeOf12 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf12(o, p);
  }
  function _createSuper12(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct12();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf12(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf12(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn12(this, result);
    };
  }
  function _possibleConstructorReturn12(self2, call8) {
    if (call8 && (_typeof14(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized12(self2);
  }
  function _assertThisInitialized12(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct12() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf12(o) {
    _getPrototypeOf12 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf12(o);
  }
  var ActionButtonInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits12(ActionButtonInputBinding2, _InputBinding);
    var _super = _createSuper12(ActionButtonInputBinding2);
    function ActionButtonInputBinding2() {
      _classCallCheck14(this, ActionButtonInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass14(ActionButtonInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery16.default)(scope).find(".action-button");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        return (0, import_jquery16.default)(el).data("val") || 0;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        (0, import_jquery16.default)(el).data("val", value);
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return "shiny.action";
        el;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery16.default)(el).on(
          "click.actionButtonInputBinding",
          function() {
            var $el = (0, import_jquery16.default)(this);
            var val = $el.data("val") || 0;
            $el.data("val", val + 1);
            callback(false);
          }
        );
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          value: this.getValue(el)
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery16.default)(el);
        var label = $el.text();
        var icon = "";
        if ($el.find("i[class]").length > 0) {
          var iconHtml = $el.find("i[class]")[0];
          if (iconHtml === $el.children()[0]) {
            icon = (0, import_jquery16.default)(iconHtml).prop("outerHTML");
          }
        }
        if (hasDefinedProperty(data, "label")) {
          label = data.label;
        }
        if (hasDefinedProperty(data, "icon")) {
          var _data$icon;
          icon = Array.isArray(data.icon) ? "" : (_data$icon = data.icon) !== null && _data$icon !== void 0 ? _data$icon : "";
        }
        $el.html(icon + " " + label);
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery16.default)(el).off(".actionButtonInputBinding");
      }
    }]);
    return ActionButtonInputBinding2;
  }(InputBinding);
  (0, import_jquery16.default)(document).on("click", "a.action-button", function(e) {
    e.preventDefault();
  });

  // srcts/src/bindings/input/tabinput.ts
  var import_es_array_iterator15 = __toESM(require_es_array_iterator());
  var import_jquery17 = __toESM(require_jquery());
  function _typeof15(obj) {
    "@babel/helpers - typeof";
    return _typeof15 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof15(obj);
  }
  function _classCallCheck15(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties15(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey15(descriptor.key), descriptor);
    }
  }
  function _createClass15(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties15(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties15(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey15(arg) {
    var key = _toPrimitive15(arg, "string");
    return _typeof15(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive15(input, hint) {
    if (_typeof15(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof15(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits13(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf13(subClass, superClass);
  }
  function _setPrototypeOf13(o, p) {
    _setPrototypeOf13 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf13(o, p);
  }
  function _createSuper13(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct13();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf13(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf13(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn13(this, result);
    };
  }
  function _possibleConstructorReturn13(self2, call8) {
    if (call8 && (_typeof15(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized13(self2);
  }
  function _assertThisInitialized13(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct13() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf13(o) {
    _getPrototypeOf13 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf13(o);
  }
  function getTabName(anchor) {
    return anchor.attr("data-value") || anchor.text();
  }
  var BootstrapTabInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits13(BootstrapTabInputBinding2, _InputBinding);
    var _super = _createSuper13(BootstrapTabInputBinding2);
    function BootstrapTabInputBinding2() {
      _classCallCheck15(this, BootstrapTabInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass15(BootstrapTabInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery17.default)(scope).find("ul.nav.shiny-tab-input");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var anchor = isBS3() ? (0, import_jquery17.default)(el).find("li:not(.dropdown).active > a") : (0, import_jquery17.default)(el).find(".nav-link:not(.dropdown-toggle).active, .dropdown-menu .dropdown-item.active");
        if (anchor.length === 1)
          return getTabName(anchor);
        return null;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        var success = false;
        if (value) {
          var anchors = isBS3() ? (0, import_jquery17.default)(el).find("li:not(.dropdown) > a") : (0, import_jquery17.default)(el).find(".nav-link:not(.dropdown-toggle), .dropdown-menu .dropdown-item");
          anchors.each(function() {
            if (getTabName((0, import_jquery17.default)(this)) === value) {
              (0, import_jquery17.default)(this).tab("show");
              success = true;
              return false;
            }
            return;
          });
        }
        if (!success) {
          (0, import_jquery17.default)(el).trigger("change");
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          value: this.getValue(el)
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        if (hasDefinedProperty(data, "value"))
          this.setValue(el, data.value);
        (0, import_jquery17.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery17.default)(el).on(
          "change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding",
          function() {
            callback(false);
          }
        );
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery17.default)(el).off(".bootstrapTabInputBinding");
      }
    }]);
    return BootstrapTabInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/fileinput.ts
  var import_es_array_iterator17 = __toESM(require_es_array_iterator());
  var import_jquery20 = __toESM(require_jquery());

  // node_modules/core-js/modules/es.array.from.js
  var $39 = require_export();
  var from = require_array_from();
  var checkCorrectnessOfIteration = require_check_correctness_of_iteration();
  var INCORRECT_ITERATION = !checkCorrectnessOfIteration(function(iterable) {
    Array.from(iterable);
  });
  $39({ target: "Array", stat: true, forced: INCORRECT_ITERATION }, {
    from: from
  });

  // node_modules/core-js/modules/es.array.map.js
  var $40 = require_export();
  var $map = require_array_iteration().map;
  var arrayMethodHasSpeciesSupport4 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT3 = arrayMethodHasSpeciesSupport4("map");
  $40({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT3 }, {
    map: function map(callbackfn) {
      return $map(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/file/fileProcessor.ts
  var import_es_array_iterator16 = __toESM(require_es_array_iterator());
  var import_jquery19 = __toESM(require_jquery());

  // srcts/src/events/inputChanged.ts
  var import_jquery18 = __toESM(require_jquery());
  function triggerFileInputChanged(name, value, binding, el, inputType, onEl) {
    var evt = import_jquery18.default.Event("shiny:inputchanged");
    evt.name = name;
    evt.value = value;
    evt.binding = binding;
    evt.el = el;
    evt.inputType = inputType;
    (0, import_jquery18.default)(onEl).trigger(evt);
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
  function shinyBindAll(scope) {
    validateShinyHasBeenSet().bindAll(scope);
  }
  function shinyUnbindAll(scope) {
    var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
    validateShinyHasBeenSet().unbindAll(scope, includeSelf);
  }
  function shinyInitializeInputs(scope) {
    validateShinyHasBeenSet().initializeInputs(scope);
  }
  function shinyAppBindOutput(id, binding) {
    shinyShinyApp().bindOutput(id, binding);
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
  function _typeof16(obj) {
    "@babel/helpers - typeof";
    return _typeof16 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof16(obj);
  }
  function _inherits14(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf14(subClass, superClass);
  }
  function _setPrototypeOf14(o, p) {
    _setPrototypeOf14 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf14(o, p);
  }
  function _createSuper14(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct14();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf14(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf14(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn14(this, result);
    };
  }
  function _possibleConstructorReturn14(self2, call8) {
    if (call8 && (_typeof16(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized14(self2);
  }
  function _assertThisInitialized14(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct14() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf14(o) {
    _getPrototypeOf14 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf14(o);
  }
  function _classCallCheck16(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties16(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey16(descriptor.key), descriptor);
    }
  }
  function _createClass16(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties16(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties16(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty3(obj, key, value) {
    key = _toPropertyKey16(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey16(arg) {
    var key = _toPrimitive16(arg, "string");
    return _typeof16(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive16(input, hint) {
    if (_typeof16(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof16(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var FileProcessor = /* @__PURE__ */ function() {
    function FileProcessor2(files) {
      var exec$run = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : true;
      _classCallCheck16(this, FileProcessor2);
      _defineProperty3(this, "files", void 0);
      _defineProperty3(this, "fileIndex", -1);
      _defineProperty3(this, "aborted", false);
      _defineProperty3(this, "completed", false);
      this.files = Array.from(files);
      if (exec$run) {
        this.$run();
      }
    }
    _createClass16(FileProcessor2, [{
      key: "onBegin",
      value: function onBegin(files, cont) {
        files;
        setTimeout(cont, 0);
      }
    }, {
      key: "onFile",
      value: function onFile(file, cont) {
        file;
        setTimeout(cont, 0);
      }
    }, {
      key: "onComplete",
      value: function onComplete() {
        return;
      }
    }, {
      key: "onAbort",
      value: function onAbort() {
        return;
      }
    }, {
      key: "abort",
      value: function abort() {
        if (this.completed || this.aborted)
          return;
        this.aborted = true;
        this.onAbort();
      }
    }, {
      key: "$getRun",
      value: function $getRun() {
        var _this = this;
        var called = false;
        return function() {
          if (called)
            return;
          called = true;
          _this.$run();
        };
      }
    }, {
      key: "$run",
      value: function $run() {
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
        var file = this.files[this.fileIndex++];
        this.onFile(file, this.$getRun());
      }
    }]);
    return FileProcessor2;
  }();
  var FileUploader = /* @__PURE__ */ function(_FileProcessor) {
    _inherits14(FileUploader2, _FileProcessor);
    var _super = _createSuper14(FileUploader2);
    function FileUploader2(shinyapp, id, files, el) {
      var _this2;
      _classCallCheck16(this, FileUploader2);
      _this2 = _super.call(this, files, false);
      _defineProperty3(_assertThisInitialized14(_this2), "shinyapp", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "id", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "el", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "jobId", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "uploadUrl", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "progressBytes", void 0);
      _defineProperty3(_assertThisInitialized14(_this2), "totalBytes", void 0);
      _this2.shinyapp = shinyapp;
      _this2.id = id;
      _this2.el = el;
      _this2.$run();
      return _this2;
    }
    _createClass16(FileUploader2, [{
      key: "makeRequest",
      value: function makeRequest(method, args, onSuccess, onFailure, blobs) {
        this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
      }
    }, {
      key: "onBegin",
      value: function onBegin(files, cont) {
        var _this3 = this;
        this.$setError(null);
        this.$setActive(true);
        this.$setVisible(true);
        this.onProgress(null, 0);
        this.totalBytes = 0;
        this.progressBytes = 0;
        import_jquery19.default.each(files, function(i, file) {
          _this3.totalBytes += file.size;
        });
        var fileInfo = import_jquery19.default.map(files, function(file) {
          return {
            name: file.name,
            size: file.size,
            type: file.type
          };
        });
        this.makeRequest("uploadInit", [fileInfo], function(response) {
          _this3.jobId = response.jobId;
          _this3.uploadUrl = response.uploadUrl;
          cont();
        }, function(error) {
          _this3.onError(error);
        }, void 0);
      }
    }, {
      key: "onFile",
      value: function onFile(file, cont) {
        var _this4 = this;
        this.onProgress(file, 0);
        import_jquery19.default.ajax(this.uploadUrl, {
          type: "POST",
          cache: false,
          xhr: function xhr() {
            if (typeof import_jquery19.default.ajaxSettings.xhr !== "function")
              throw "jQuery's XHR is not a function";
            var xhrVal = import_jquery19.default.ajaxSettings.xhr();
            if (xhrVal.upload) {
              xhrVal.upload.onprogress = function(e) {
                if (e.lengthComputable) {
                  _this4.onProgress(file, (_this4.progressBytes + e.loaded) / _this4.totalBytes);
                }
              };
            }
            return xhrVal;
          },
          data: file,
          contentType: "application/octet-stream",
          processData: false,
          success: function success() {
            _this4.progressBytes += file.size;
            cont();
          },
          error: function error(jqXHR, textStatus, errorThrown) {
            errorThrown;
            _this4.onError(jqXHR.responseText || textStatus);
          }
        });
      }
    }, {
      key: "onComplete",
      value: function onComplete() {
        var _this5 = this;
        var fileInfo = import_jquery19.default.map(this.files, function(file, i) {
          return {
            name: file.name,
            size: file.size,
            type: file.type
          };
          i;
        });
        var evt = triggerFileInputChanged(this.id, fileInfo, getFileInputBinding(), this.el, "shiny.fileupload", document);
        this.makeRequest("uploadEnd", [this.jobId, this.id], function() {
          _this5.$setActive(false);
          _this5.onProgress(null, 1);
          _this5.$bar().text("Upload complete");
          (0, import_jquery19.default)(evt.el).val("");
        }, function(error) {
          _this5.onError(error);
        }, void 0);
        this.$bar().text("Finishing upload");
      }
    }, {
      key: "onError",
      value: function onError(message) {
        this.$setError(message || "");
        this.$setActive(false);
      }
    }, {
      key: "onAbort",
      value: function onAbort() {
        this.$setVisible(false);
      }
    }, {
      key: "onProgress",
      value: function onProgress(file, completed) {
        this.$bar().width(Math.round(completed * 100) + "%");
        this.$bar().text(file ? file.name : "");
      }
    }, {
      key: "$container",
      value: function $container() {
        return (0, import_jquery19.default)("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
      }
    }, {
      key: "$bar",
      value: function $bar() {
        return (0, import_jquery19.default)("#" + $escape(this.id) + "_progress.shiny-file-input-progress .progress-bar");
      }
    }, {
      key: "$setVisible",
      value: function $setVisible(visible) {
        this.$container().css("visibility", visible ? "visible" : "hidden");
      }
    }, {
      key: "$setError",
      value: function $setError(error) {
        this.$bar().toggleClass("progress-bar-danger", error !== null);
        if (error !== null) {
          this.onProgress(null, 1);
          this.$bar().text(error);
        }
      }
    }, {
      key: "$setActive",
      value: function $setActive(active) {
        this.$container().toggleClass("active", !!active);
      }
    }]);
    return FileUploader2;
  }(FileProcessor);

  // srcts/src/bindings/input/fileinput.ts
  function _typeof17(obj) {
    "@babel/helpers - typeof";
    return _typeof17 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof17(obj);
  }
  function _classCallCheck17(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties17(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey17(descriptor.key), descriptor);
    }
  }
  function _createClass17(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties17(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties17(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey17(arg) {
    var key = _toPrimitive17(arg, "string");
    return _typeof17(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive17(input, hint) {
    if (_typeof17(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof17(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits15(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf15(subClass, superClass);
  }
  function _setPrototypeOf15(o, p) {
    _setPrototypeOf15 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf15(o, p);
  }
  function _createSuper15(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct15();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf15(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf15(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn15(this, result);
    };
  }
  function _possibleConstructorReturn15(self2, call8) {
    if (call8 && (_typeof17(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized15(self2);
  }
  function _assertThisInitialized15(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct15() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf15(o) {
    _getPrototypeOf15 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf15(o);
  }
  var zoneActive = "shiny-file-input-active";
  var zoneOver = "shiny-file-input-over";
  function zoneOf(el) {
    return (0, import_jquery20.default)(el).closest("div.input-group");
  }
  function enableDraghover(el) {
    var $el = (0, import_jquery20.default)(el);
    var childCounter = 0;
    $el.on({
      "dragenter.draghover": function dragenterDraghover(e) {
        if (childCounter++ === 0) {
          $el.trigger("draghover:enter", e);
        }
      },
      "dragleave.draghover": function dragleaveDraghover(e) {
        if (--childCounter === 0) {
          $el.trigger("draghover:leave", e);
        }
        if (childCounter < 0) {
          console.error("draghover childCounter is negative somehow");
        }
      },
      "dragover.draghover": function dragoverDraghover(e) {
        e.preventDefault();
      },
      "drop.draghover": function dropDraghover(e) {
        childCounter = 0;
        $el.trigger("draghover:drop", e);
        e.preventDefault();
      }
    });
    return $el;
  }
  function disableDraghover(el) {
    return (0, import_jquery20.default)(el).off(".draghover");
  }
  function enableDocumentEvents() {
    var $doc = (0, import_jquery20.default)("html");
    enableDraghover($doc).on({
      "draghover:enter.draghover": function draghoverEnterDraghover() {
        zoneOf($fileInputs).addClass(zoneActive);
      },
      "draghover:leave.draghover": function draghoverLeaveDraghover() {
        zoneOf($fileInputs).removeClass(zoneActive);
      },
      "draghover:drop.draghover": function draghoverDropDraghover() {
        zoneOf($fileInputs).removeClass(zoneOver).removeClass(zoneActive);
      }
    });
  }
  function disableDocumentEvents() {
    var $doc = (0, import_jquery20.default)("html");
    $doc.off(".draghover");
    disableDraghover($doc);
  }
  function canSetFiles(fileList) {
    var testEl = document.createElement("input");
    testEl.type = "file";
    try {
      testEl.files = fileList;
    } catch (e) {
      return false;
    }
    return true;
  }
  function handleDrop(e, el) {
    var _e$originalEvent, _e$originalEvent$data;
    var files = (_e$originalEvent = e.originalEvent) === null || _e$originalEvent === void 0 ? void 0 : (_e$originalEvent$data = _e$originalEvent.dataTransfer) === null || _e$originalEvent$data === void 0 ? void 0 : _e$originalEvent$data.files, $el = (0, import_jquery20.default)(el);
    if (files === void 0 || files === null) {
      console.log("Dropping files is not supported on this browser. (no FileList)");
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
    var $fileText = $el.closest("div.input-group").find("input[type=text]");
    if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + " files");
    }
  }
  function abortCurrentUpload($el) {
    var uploader = $el.data("currentUploader");
    if (uploader)
      uploader.abort();
    $el.removeAttr("data-restore");
  }
  function uploadDroppedFilesIE10Plus(el, files) {
    var $el = (0, import_jquery20.default)(el);
    abortCurrentUpload($el);
    setFileText($el, files);
    $el.data("currentUploader", new FileUploader(shinyShinyApp(), fileInputBindingGetId(el), files, el));
  }
  function uploadFiles(evt) {
    var $el = (0, import_jquery20.default)(evt.target);
    abortCurrentUpload($el);
    var files = evt.target.files;
    var id = fileInputBindingGetId(evt.target);
    if (files.length === 0)
      return;
    setFileText($el, files);
    $el.data("currentUploader", new FileUploader(shinyShinyApp(), id, files, evt.target));
  }
  var $fileInputs = (0, import_jquery20.default)();
  function fileInputBindingGetId(el) {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  }
  var FileInputBinding = /* @__PURE__ */ function(_InputBinding) {
    _inherits15(FileInputBinding2, _InputBinding);
    var _super = _createSuper15(FileInputBinding2);
    function FileInputBinding2() {
      _classCallCheck17(this, FileInputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass17(FileInputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery20.default)(scope).find('input[type="file"]');
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return fileInputBindingGetId(el);
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var data = (0, import_jquery20.default)(el).attr("data-restore");
        if (data) {
          var dataParsed = JSON.parse(data);
          var $fileText = (0, import_jquery20.default)(el).closest("div.input-group").find("input[type=text]");
          if (dataParsed.name.length === 1) {
            $fileText.val(dataParsed.name[0]);
          } else {
            $fileText.val(dataParsed.name.length + " files");
          }
          var $progress = (0, import_jquery20.default)(el).closest("div.form-group").find(".progress");
          var $bar = $progress.find(".progress-bar");
          $progress.removeClass("active");
          $bar.width("100%");
          $bar.css("visibility", "visible");
          return dataParsed;
        } else {
          return null;
        }
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        el;
        value;
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return "shiny.file";
        el;
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        callback;
        (0, import_jquery20.default)(el).on("change.fileInputBinding", uploadFiles);
        if ($fileInputs.length === 0)
          enableDocumentEvents();
        $fileInputs = $fileInputs.add(el);
        var $zone = zoneOf(el);
        enableDraghover($zone).on({
          "draghover:enter.draghover": function draghoverEnterDraghover(e) {
            e;
            $zone.addClass(zoneOver);
          },
          "draghover:leave.draghover": function draghoverLeaveDraghover(e) {
            $zone.removeClass(zoneOver);
            e.stopPropagation();
          },
          "draghover:drop.draghover": function draghoverDropDraghover(e, dropEvent) {
            e;
            handleDrop(dropEvent, el);
          }
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        var $el = (0, import_jquery20.default)(el), $zone = zoneOf(el);
        $zone.removeClass(zoneOver).removeClass(zoneActive);
        disableDraghover($zone);
        $el.off(".fileInputBinding");
        $zone.off(".draghover");
        $fileInputs = $fileInputs.not(el);
        if ($fileInputs.length === 0)
          disableDocumentEvents();
      }
    }]);
    return FileInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/index.ts
  function initInputBindings() {
    var inputBindings = new BindingRegistry();
    inputBindings.register(new TextInputBinding(), "shiny.textInput");
    inputBindings.register(new TextareaInputBinding(), "shiny.textareaInput");
    inputBindings.register(new PasswordInputBinding(), "shiny.passwordInput");
    inputBindings.register(new NumberInputBinding(), "shiny.numberInput");
    inputBindings.register(new CheckboxInputBinding(), "shiny.checkboxInput");
    inputBindings.register(new CheckboxGroupInputBinding(), "shiny.checkboxGroupInput");
    inputBindings.register(new RadioInputBinding(), "shiny.radioInput");
    inputBindings.register(new SliderInputBinding(), "shiny.sliderInput");
    inputBindings.register(new DateInputBinding(), "shiny.dateInput");
    inputBindings.register(new DateRangeInputBinding(), "shiny.dateRangeInput");
    inputBindings.register(new SelectInputBinding(), "shiny.selectInput");
    inputBindings.register(new ActionButtonInputBinding(), "shiny.actionButtonInput");
    inputBindings.register(new BootstrapTabInputBinding(), "shiny.bootstrapTabInput");
    var fileInputBinding2 = new FileInputBinding();
    inputBindings.register(fileInputBinding2, "shiny.fileInputBinding");
    return {
      inputBindings: inputBindings,
      fileInputBinding: fileInputBinding2
    };
  }

  // srcts/src/bindings/output/text.ts
  var import_es_array_iterator19 = __toESM(require_es_array_iterator());
  var import_jquery22 = __toESM(require_jquery());

  // node_modules/core-js/modules/es.array.join.js
  var $44 = require_export();
  var uncurryThis7 = require_function_uncurry_this();
  var IndexedObject = require_indexed_object();
  var toIndexedObject3 = require_to_indexed_object();
  var arrayMethodIsStrict2 = require_array_method_is_strict();
  var nativeJoin = uncurryThis7([].join);
  var ES3_STRINGS = IndexedObject != Object;
  var FORCED7 = ES3_STRINGS || !arrayMethodIsStrict2("join", ",");
  $44({ target: "Array", proto: true, forced: FORCED7 }, {
    join: function join(separator) {
      return nativeJoin(toIndexedObject3(this), separator === void 0 ? "," : separator);
    }
  });

  // srcts/src/bindings/output/outputBinding.ts
  var import_es_regexp_exec5 = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.promise.js
  require_es_promise_constructor();
  require_es_promise_all();
  require_es_promise_catch();
  require_es_promise_race();
  require_es_promise_reject();
  require_es_promise_resolve();

  // srcts/src/bindings/output/outputBinding.ts
  var import_es_array_iterator18 = __toESM(require_es_array_iterator());

  // node_modules/core-js/modules/es.symbol.async-iterator.js
  var defineWellKnownSymbol3 = require_well_known_symbol_define();
  defineWellKnownSymbol3("asyncIterator");

  // node_modules/core-js/modules/es.symbol.to-string-tag.js
  var getBuiltIn2 = require_get_built_in();
  var defineWellKnownSymbol4 = require_well_known_symbol_define();
  var setToStringTag2 = require_set_to_string_tag();
  defineWellKnownSymbol4("toStringTag");
  setToStringTag2(getBuiltIn2("Symbol"), "Symbol");

  // node_modules/core-js/modules/es.json.to-string-tag.js
  var global7 = require_global();
  var setToStringTag3 = require_set_to_string_tag();
  setToStringTag3(global7.JSON, "JSON", true);

  // node_modules/core-js/modules/es.math.to-string-tag.js
  var setToStringTag4 = require_set_to_string_tag();
  setToStringTag4(Math, "Math", true);

  // node_modules/core-js/modules/es.array.reverse.js
  var $45 = require_export();
  var uncurryThis8 = require_function_uncurry_this();
  var isArray3 = require_is_array();
  var nativeReverse = uncurryThis8([].reverse);
  var test = [1, 2];
  $45({ target: "Array", proto: true, forced: String(test) === String(test.reverse()) }, {
    reverse: function reverse() {
      if (isArray3(this))
        this.length = this.length;
      return nativeReverse(this);
    }
  });

  // srcts/src/bindings/output/outputBinding.ts
  var import_jquery21 = __toESM(require_jquery());
  function _typeof18(obj) {
    "@babel/helpers - typeof";
    return _typeof18 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof18(obj);
  }
  function _regeneratorRuntime() {
    "use strict";
    _regeneratorRuntime = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof18(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function _classCallCheck18(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties18(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey18(descriptor.key), descriptor);
    }
  }
  function _createClass18(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties18(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties18(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty4(obj, key, value) {
    key = _toPropertyKey18(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey18(arg) {
    var key = _toPrimitive18(arg, "string");
    return _typeof18(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive18(input, hint) {
    if (_typeof18(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof18(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var OutputBinding = /* @__PURE__ */ function() {
    function OutputBinding2() {
      _classCallCheck18(this, OutputBinding2);
      _defineProperty4(this, "name", void 0);
    }
    _createClass18(OutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        throw "Not implemented";
        scope;
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        throw "Not implemented";
        el;
        data;
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return el.getAttribute("data-input-id") || el.id;
      }
    }, {
      key: "onValueChange",
      value: function() {
        var _onValueChange = _asyncToGenerator(/* @__PURE__ */ _regeneratorRuntime().mark(function _callee(el, data) {
          return _regeneratorRuntime().wrap(function _callee$(_context) {
            while (1)
              switch (_context.prev = _context.next) {
                case 0:
                  this.clearError(el);
                  _context.next = 3;
                  return this.renderValue(el, data);
                case 3:
                case "end":
                  return _context.stop();
              }
          }, _callee, this);
        }));
        function onValueChange(_x, _x2) {
          return _onValueChange.apply(this, arguments);
        }
        return onValueChange;
      }()
    }, {
      key: "onValueError",
      value: function onValueError(el, err) {
        this.renderError(el, err);
      }
    }, {
      key: "renderError",
      value: function renderError(el, err) {
        this.clearError(el);
        if (err.message === "") {
          (0, import_jquery21.default)(el).empty();
          return;
        }
        var errClass = "shiny-output-error";
        if (err.type !== null) {
          errClass = errClass + " " + import_jquery21.default.map(asArray(err.type), function(type) {
            return errClass + "-" + type;
          }).join(" ");
        }
        (0, import_jquery21.default)(el).addClass(errClass).text(err.message);
      }
    }, {
      key: "clearError",
      value: function clearError(el) {
        (0, import_jquery21.default)(el).attr("class", function(i, c) {
          return c.replace(/(^|\s)shiny-output-error\S*/g, "");
        });
      }
    }, {
      key: "showProgress",
      value: function showProgress(el, show3) {
        var recalcClass = "recalculating";
        if (show3)
          (0, import_jquery21.default)(el).addClass(recalcClass);
        else
          (0, import_jquery21.default)(el).removeClass(recalcClass);
      }
    }]);
    return OutputBinding2;
  }();

  // srcts/src/bindings/output/text.ts
  function _typeof19(obj) {
    "@babel/helpers - typeof";
    return _typeof19 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof19(obj);
  }
  function _classCallCheck19(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties19(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey19(descriptor.key), descriptor);
    }
  }
  function _createClass19(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties19(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties19(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey19(arg) {
    var key = _toPrimitive19(arg, "string");
    return _typeof19(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive19(input, hint) {
    if (_typeof19(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof19(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits16(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf16(subClass, superClass);
  }
  function _setPrototypeOf16(o, p) {
    _setPrototypeOf16 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf16(o, p);
  }
  function _createSuper16(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct16();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf16(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf16(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn16(this, result);
    };
  }
  function _possibleConstructorReturn16(self2, call8) {
    if (call8 && (_typeof19(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized16(self2);
  }
  function _assertThisInitialized16(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct16() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf16(o) {
    _getPrototypeOf16 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf16(o);
  }
  var TextOutputBinding = /* @__PURE__ */ function(_OutputBinding) {
    _inherits16(TextOutputBinding2, _OutputBinding);
    var _super = _createSuper16(TextOutputBinding2);
    function TextOutputBinding2() {
      _classCallCheck19(this, TextOutputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass19(TextOutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery22.default)(scope).find(".shiny-text-output");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        (0, import_jquery22.default)(el).text(data);
      }
    }]);
    return TextOutputBinding2;
  }(OutputBinding);

  // srcts/src/bindings/output/downloadlink.ts
  var import_es_array_iterator20 = __toESM(require_es_array_iterator());
  var import_jquery23 = __toESM(require_jquery());
  function _typeof20(obj) {
    "@babel/helpers - typeof";
    return _typeof20 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof20(obj);
  }
  function _classCallCheck20(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties20(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey20(descriptor.key), descriptor);
    }
  }
  function _createClass20(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties20(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties20(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey20(arg) {
    var key = _toPrimitive20(arg, "string");
    return _typeof20(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive20(input, hint) {
    if (_typeof20(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof20(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits17(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf17(subClass, superClass);
  }
  function _setPrototypeOf17(o, p) {
    _setPrototypeOf17 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf17(o, p);
  }
  function _createSuper17(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct17();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf17(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf17(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn17(this, result);
    };
  }
  function _possibleConstructorReturn17(self2, call8) {
    if (call8 && (_typeof20(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized17(self2);
  }
  function _assertThisInitialized17(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct17() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf17(o) {
    _getPrototypeOf17 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf17(o);
  }
  var DownloadLinkOutputBinding = /* @__PURE__ */ function(_OutputBinding) {
    _inherits17(DownloadLinkOutputBinding2, _OutputBinding);
    var _super = _createSuper17(DownloadLinkOutputBinding2);
    function DownloadLinkOutputBinding2() {
      _classCallCheck20(this, DownloadLinkOutputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass20(DownloadLinkOutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery23.default)(scope).find("a.shiny-download-link");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        (0, import_jquery23.default)(el).attr("href", data);
      }
    }]);
    return DownloadLinkOutputBinding2;
  }(OutputBinding);
  (0, import_jquery23.default)(document).on("click.shinyDownloadLink", "a.shiny-download-link", function(e) {
    e;
    var evt = import_jquery23.default.Event("shiny:filedownload");
    evt.name = this.id;
    evt.href = this.href;
    (0, import_jquery23.default)(document).trigger(evt);
  });

  // srcts/src/bindings/output/datatable.ts
  var import_es_regexp_exec6 = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.search.js
  var call4 = require_function_call();
  var fixRegExpWellKnownSymbolLogic2 = require_fix_regexp_well_known_symbol_logic();
  var anObject6 = require_an_object();
  var isNullOrUndefined2 = require_is_null_or_undefined();
  var requireObjectCoercible2 = require_require_object_coercible();
  var sameValue = require_same_value();
  var toString6 = require_to_string();
  var getMethod2 = require_get_method();
  var regExpExec3 = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic2("search", function(SEARCH, nativeSearch, maybeCallNative) {
    return [
      function search(regexp) {
        var O = requireObjectCoercible2(this);
        var searcher = isNullOrUndefined2(regexp) ? void 0 : getMethod2(regexp, SEARCH);
        return searcher ? call4(searcher, regexp, O) : new RegExp(regexp)[SEARCH](toString6(O));
      },
      function(string) {
        var rx = anObject6(this);
        var S = toString6(string);
        var res = maybeCallNative(nativeSearch, rx, S);
        if (res.done)
          return res.value;
        var previousLastIndex = rx.lastIndex;
        if (!sameValue(previousLastIndex, 0))
          rx.lastIndex = 0;
        var result = regExpExec3(rx, S);
        if (!sameValue(rx.lastIndex, previousLastIndex))
          rx.lastIndex = previousLastIndex;
        return result === null ? -1 : result.index;
      }
    ];
  });

  // srcts/src/bindings/output/datatable.ts
  var import_es_array_iterator24 = __toESM(require_es_array_iterator());
  var import_jquery24 = __toESM(require_jquery());

  // srcts/src/time/debounce.ts
  var import_es_array_iterator21 = __toESM(require_es_array_iterator());
  function _typeof21(obj) {
    "@babel/helpers - typeof";
    return _typeof21 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof21(obj);
  }
  function _classCallCheck21(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties21(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey21(descriptor.key), descriptor);
    }
  }
  function _createClass21(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties21(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties21(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty5(obj, key, value) {
    key = _toPropertyKey21(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey21(arg) {
    var key = _toPrimitive21(arg, "string");
    return _typeof21(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive21(input, hint) {
    if (_typeof21(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof21(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var Debouncer = /* @__PURE__ */ function() {
    function Debouncer2(target, func, delayMs) {
      _classCallCheck21(this, Debouncer2);
      _defineProperty5(this, "target", void 0);
      _defineProperty5(this, "func", void 0);
      _defineProperty5(this, "delayMs", void 0);
      _defineProperty5(this, "timerId", void 0);
      _defineProperty5(this, "args", void 0);
      this.target = target;
      this.func = func;
      this.delayMs = delayMs;
      this.timerId = null;
      this.args = null;
    }
    _createClass21(Debouncer2, [{
      key: "normalCall",
      value: function normalCall() {
        var _this = this;
        this.$clearTimer();
        for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }
        this.args = args;
        this.timerId = setTimeout(function() {
          if (_this.timerId === null)
            return;
          _this.$clearTimer();
          _this.$invoke();
        }, this.delayMs);
      }
    }, {
      key: "immediateCall",
      value: function immediateCall() {
        this.$clearTimer();
        for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
          args[_key2] = arguments[_key2];
        }
        this.args = args;
        this.$invoke();
      }
    }, {
      key: "isPending",
      value: function isPending() {
        return this.timerId !== null;
      }
    }, {
      key: "$clearTimer",
      value: function $clearTimer() {
        if (this.timerId !== null) {
          clearTimeout(this.timerId);
          this.timerId = null;
        }
      }
    }, {
      key: "$invoke",
      value: function $invoke() {
        if (this.args && this.args.length > 0) {
          this.func.apply(this.target, this.args);
        } else {
          this.func.apply(this.target);
        }
        this.args = null;
      }
    }]);
    return Debouncer2;
  }();
  function debounce(threshold, func) {
    var timerId = null;
    return function thisFunc() {
      for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
        args[_key3] = arguments[_key3];
      }
      if (timerId !== null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function() {
        if (timerId === null)
          return;
        timerId = null;
        func.apply(thisFunc, args);
      }, threshold);
    };
  }

  // srcts/src/time/invoke.ts
  var import_es_array_iterator22 = __toESM(require_es_array_iterator());
  function _typeof22(obj) {
    "@babel/helpers - typeof";
    return _typeof22 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof22(obj);
  }
  function _classCallCheck22(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties22(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey22(descriptor.key), descriptor);
    }
  }
  function _createClass22(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties22(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties22(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty6(obj, key, value) {
    key = _toPropertyKey22(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey22(arg) {
    var key = _toPrimitive22(arg, "string");
    return _typeof22(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive22(input, hint) {
    if (_typeof22(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof22(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var Invoker = /* @__PURE__ */ function() {
    function Invoker2(target, func) {
      _classCallCheck22(this, Invoker2);
      _defineProperty6(this, "target", void 0);
      _defineProperty6(this, "func", void 0);
      this.target = target;
      this.func = func;
    }
    _createClass22(Invoker2, [{
      key: "normalCall",
      value: function normalCall() {
        for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }
        this.func.apply(this.target, args);
      }
    }, {
      key: "immediateCall",
      value: function immediateCall() {
        for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
          args[_key2] = arguments[_key2];
        }
        this.func.apply(this.target, args);
      }
    }]);
    return Invoker2;
  }();

  // srcts/src/time/throttle.ts
  var import_es_array_iterator23 = __toESM(require_es_array_iterator());
  function _typeof23(obj) {
    "@babel/helpers - typeof";
    return _typeof23 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof23(obj);
  }
  function _classCallCheck23(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties23(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey23(descriptor.key), descriptor);
    }
  }
  function _createClass23(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties23(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties23(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty7(obj, key, value) {
    key = _toPropertyKey23(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey23(arg) {
    var key = _toPrimitive23(arg, "string");
    return _typeof23(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive23(input, hint) {
    if (_typeof23(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof23(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var Throttler = /* @__PURE__ */ function() {
    function Throttler2(target, func, delayMs) {
      _classCallCheck23(this, Throttler2);
      _defineProperty7(this, "target", void 0);
      _defineProperty7(this, "func", void 0);
      _defineProperty7(this, "delayMs", void 0);
      _defineProperty7(this, "timerId", void 0);
      _defineProperty7(this, "args", void 0);
      this.target = target;
      this.func = func;
      this.delayMs = delayMs;
      this.timerId = null;
      this.args = null;
    }
    _createClass23(Throttler2, [{
      key: "normalCall",
      value: function normalCall() {
        for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }
        this.args = args;
        if (this.timerId === null) {
          this.$invoke();
        }
      }
    }, {
      key: "immediateCall",
      value: function immediateCall() {
        this.$clearTimer();
        for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
          args[_key2] = arguments[_key2];
        }
        this.args = args;
        this.$invoke();
      }
    }, {
      key: "isPending",
      value: function isPending() {
        return this.args !== null;
      }
    }, {
      key: "$clearTimer",
      value: function $clearTimer() {
        if (this.timerId !== null) {
          clearTimeout(this.timerId);
          this.timerId = null;
        }
      }
    }, {
      key: "$invoke",
      value: function $invoke() {
        var _this = this;
        if (this.args === null) {
          return;
        }
        this.func.apply(this.target, this.args);
        this.args = null;
        this.timerId = setTimeout(function() {
          if (_this.timerId === null)
            return;
          _this.$clearTimer();
          if (_this.isPending()) {
            _this.$invoke();
          }
        }, this.delayMs);
      }
    }]);
    return Throttler2;
  }();

  // srcts/src/bindings/output/datatable.ts
  function _typeof24(obj) {
    "@babel/helpers - typeof";
    return _typeof24 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof24(obj);
  }
  function _classCallCheck24(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties24(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey24(descriptor.key), descriptor);
    }
  }
  function _createClass24(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties24(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties24(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey24(arg) {
    var key = _toPrimitive24(arg, "string");
    return _typeof24(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive24(input, hint) {
    if (_typeof24(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof24(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits18(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf18(subClass, superClass);
  }
  function _setPrototypeOf18(o, p) {
    _setPrototypeOf18 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf18(o, p);
  }
  function _createSuper18(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct18();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf18(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf18(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn18(this, result);
    };
  }
  function _possibleConstructorReturn18(self2, call8) {
    if (call8 && (_typeof24(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized18(self2);
  }
  function _assertThisInitialized18(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct18() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf18(o) {
    _getPrototypeOf18 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf18(o);
  }
  var DatatableOutputBinding = /* @__PURE__ */ function(_OutputBinding) {
    _inherits18(DatatableOutputBinding2, _OutputBinding);
    var _super = _createSuper18(DatatableOutputBinding2);
    function DatatableOutputBinding2() {
      _classCallCheck24(this, DatatableOutputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass24(DatatableOutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery24.default)(scope).find(".shiny-datatable-output");
      }
    }, {
      key: "onValueError",
      value: function onValueError(el, err) {
        shinyUnbindAll(el);
        this.renderError(el, err);
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, _data) {
        var _data$options$searchi, _data$options, _data$options2, _data$options2$search;
        var $el = (0, import_jquery24.default)(el).empty();
        if (!_data || !_data.colnames)
          return;
        var colnames = import_jquery24.default.makeArray(_data.colnames);
        var header = import_jquery24.default.map(colnames, function(x) {
          return "<th>" + x + "</th>";
        }).join("");
        header = "<thead><tr>" + header + "</tr></thead>";
        var footer = "";
        if ((_data$options$searchi = (_data$options = _data.options) === null || _data$options === void 0 ? void 0 : _data$options.searching) !== null && _data$options$searchi !== void 0 ? _data$options$searchi : true) {
          footer = import_jquery24.default.map(colnames, function(x) {
            return '<th><input type="text" placeholder="' + escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) + '" /></th>';
          }).join("");
          footer = "<tfoot>" + footer + "</tfoot>";
        }
        var content = '<table class="table table-striped table-hover">' + header + footer + "</table>";
        $el.append(content);
        if (_data.evalOptions) {
          import_jquery24.default.each(_data.evalOptions, function(i, x) {
            _data.options[x] = indirectEval("(" + _data.options[x] + ")");
          });
        }
        var searchCI = ((_data$options2 = _data.options) === null || _data$options2 === void 0 ? void 0 : (_data$options2$search = _data$options2.search) === null || _data$options2$search === void 0 ? void 0 : _data$options2$search.caseInsensitive) !== false;
        var oTable = (0, import_jquery24.default)(el).children("table").DataTable(import_jquery24.default.extend({
          processing: true,
          serverSide: true,
          order: [],
          orderClasses: false,
          pageLength: 25,
          ajax: {
            url: _data.action,
            type: "POST",
            data: function data(d) {
              d.search || (d.search = {});
              d.search.caseInsensitive = searchCI;
              d.escape = _data.escape;
            }
          }
        }, _data.options));
        if (typeof _data.callback === "string") {
          var callback = indirectEval("(" + _data.callback + ")");
          if (typeof callback === "function")
            callback(oTable);
        }
        $el.find("label input").first().unbind("keyup").keyup(debounce(_data.searchDelay, function() {
          oTable.search(this.value).draw();
        }));
        var searchInputs = $el.find("tfoot input");
        if (searchInputs.length > 0) {
          import_jquery24.default.each(oTable.settings()[0].aoColumns, function(i, x) {
            if (!x.bSearchable)
              searchInputs.eq(i).hide();
          });
          searchInputs.keyup(debounce(_data.searchDelay, function() {
            oTable.column(searchInputs.index(this)).search(this.value).draw();
          }));
        }
        $el.parents(".tab-content").css("overflow", "visible");
      }
    }]);
    return DatatableOutputBinding2;
  }(OutputBinding);

  // srcts/src/bindings/output/html.ts
  var import_es_array_iterator27 = __toESM(require_es_array_iterator());
  var import_jquery27 = __toESM(require_jquery());

  // srcts/src/shiny/render.ts
  var import_es_regexp_exec8 = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.object.entries.js
  var $50 = require_export();
  var $entries = require_object_to_array().entries;
  $50({ target: "Object", stat: true }, {
    entries: function entries(O) {
      return $entries(O);
    }
  });

  // srcts/src/shiny/render.ts
  var import_es_array_iterator26 = __toESM(require_es_array_iterator());

  // node_modules/core-js/modules/es.promise.all-settled.js
  var $51 = require_export();
  var call5 = require_function_call();
  var aCallable = require_a_callable();
  var newPromiseCapabilityModule = require_new_promise_capability();
  var perform = require_perform();
  var iterate = require_iterate();
  var PROMISE_STATICS_INCORRECT_ITERATION = require_promise_statics_incorrect_iteration();
  $51({ target: "Promise", stat: true, forced: PROMISE_STATICS_INCORRECT_ITERATION }, {
    allSettled: function allSettled(iterable) {
      var C = this;
      var capability = newPromiseCapabilityModule.f(C);
      var resolve = capability.resolve;
      var reject = capability.reject;
      var result = perform(function() {
        var promiseResolve = aCallable(C.resolve);
        var values2 = [];
        var counter = 0;
        var remaining = 1;
        iterate(iterable, function(promise) {
          var index = counter++;
          var alreadyCalled = false;
          remaining++;
          call5(promiseResolve, C, promise).then(function(value) {
            if (alreadyCalled)
              return;
            alreadyCalled = true;
            values2[index] = { status: "fulfilled", value: value };
            --remaining || resolve(values2);
          }, function(error) {
            if (alreadyCalled)
              return;
            alreadyCalled = true;
            values2[index] = { status: "rejected", reason: error };
            --remaining || resolve(values2);
          });
        });
        --remaining || resolve(values2);
      });
      if (result.error)
        reject(result.value);
      return capability.promise;
    }
  });

  // srcts/src/shiny/render.ts
  var import_jquery26 = __toESM(require_jquery());

  // srcts/src/shiny/sendImageSize.ts
  var import_es_array_iterator25 = __toESM(require_es_array_iterator());
  function _typeof25(obj) {
    "@babel/helpers - typeof";
    return _typeof25 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof25(obj);
  }
  function _classCallCheck25(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties25(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey25(descriptor.key), descriptor);
    }
  }
  function _createClass25(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties25(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties25(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty8(obj, key, value) {
    key = _toPropertyKey25(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey25(arg) {
    var key = _toPrimitive25(arg, "string");
    return _typeof25(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive25(input, hint) {
    if (_typeof25(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof25(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var SendImageSize = /* @__PURE__ */ function() {
    function SendImageSize2() {
      _classCallCheck25(this, SendImageSize2);
      _defineProperty8(this, "regular", void 0);
      _defineProperty8(this, "transitioned", void 0);
    }
    _createClass25(SendImageSize2, [{
      key: "setImageSend",
      value: function setImageSend(inputBatchSender, doSendImageSize) {
        var sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
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
    }]);
    return SendImageSize2;
  }();
  var sendImageSizeFns = new SendImageSize();

  // srcts/src/shiny/singletons.ts
  var import_es_regexp_exec7 = __toESM(require_es_regexp_exec());
  var import_jquery25 = __toESM(require_jquery());
  var reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
  var reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;
  var knownSingletons = {};
  function renderHtml(html, el, where) {
    var processed = processHtml(html);
    addToHead(processed.head);
    register(processed.singletons);
    switch (where.toLowerCase()) {
      case "replace":
        (0, import_jquery25.default)(el).html(processed.html);
        break;
      case "beforebegin":
        (0, import_jquery25.default)(el).before(processed.html);
        break;
      case "afterbegin":
        (0, import_jquery25.default)(el).prepend(processed.html);
        break;
      case "beforeend":
        (0, import_jquery25.default)(el).append(processed.html);
        break;
      case "afterend":
        (0, import_jquery25.default)(el).after(processed.html);
        break;
      default:
        throw new Error("Unknown where position: " + where);
    }
    return processed;
  }
  function register(s) {
    import_jquery25.default.extend(knownSingletons, s);
  }
  function registerNames(s) {
    if (typeof s === "string") {
      knownSingletons[s] = true;
    } else if (s instanceof Array) {
      for (var i = 0; i < s.length; i++) {
        knownSingletons[s[i]] = true;
      }
    }
  }
  function addToHead(head) {
    if (head.length > 0) {
      var tempDiv = (0, import_jquery25.default)("<div>" + head + "</div>").get(0);
      var $head = (0, import_jquery25.default)("head");
      while (tempDiv.hasChildNodes()) {
        $head.append(tempDiv.firstChild);
      }
    }
  }
  function processHtml(val) {
    var newSingletons = {};
    var newVal;
    var findNewPayload = function findNewPayload2(match, p1, sig, payload) {
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
    var heads = [];
    var headAddPayload = function headAddPayload2(match, payload) {
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
  function _regeneratorRuntime2() {
    "use strict";
    _regeneratorRuntime2 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof26(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function _slicedToArray(arr, i) {
    return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
  }
  function _nonIterableRest() {
    throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
  }
  function _iterableToArrayLimit(arr, i) {
    var _i = null == arr ? null : "undefined" != typeof Symbol && arr[Symbol.iterator] || arr["@@iterator"];
    if (null != _i) {
      var _s, _e, _x, _r, _arr = [], _n = true, _d = false;
      try {
        if (_x = (_i = _i.call(arr)).next, 0 === i) {
          if (Object(_i) !== _i)
            return;
          _n = false;
        } else
          for (; !(_n = (_s = _x.call(_i)).done) && (_arr.push(_s.value), _arr.length !== i); _n = true)
            ;
      } catch (err) {
        _d = true, _e = err;
      } finally {
        try {
          if (!_n && null != _i.return && (_r = _i.return(), Object(_r) !== _r))
            return;
        } finally {
          if (_d)
            throw _e;
        }
      }
      return _arr;
    }
  }
  function _arrayWithHoles(arr) {
    if (Array.isArray(arr))
      return arr;
  }
  function _createForOfIteratorHelper(o, allowArrayLike) {
    var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"];
    if (!it) {
      if (Array.isArray(o) || (it = _unsupportedIterableToArray(o)) || allowArrayLike && o && typeof o.length === "number") {
        if (it)
          o = it;
        var i = 0;
        var F = function F2() {
        };
        return { s: F, n: function n() {
          if (i >= o.length)
            return { done: true };
          return { done: false, value: o[i++] };
        }, e: function e(_e2) {
          throw _e2;
        }, f: F };
      }
      throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
    }
    var normalCompletion = true, didErr = false, err;
    return { s: function s() {
      it = it.call(o);
    }, n: function n() {
      var step = it.next();
      normalCompletion = step.done;
      return step;
    }, e: function e(_e3) {
      didErr = true;
      err = _e3;
    }, f: function f() {
      try {
        if (!normalCompletion && it.return != null)
          it.return();
      } finally {
        if (didErr)
          throw err;
      }
    } };
  }
  function _unsupportedIterableToArray(o, minLen) {
    if (!o)
      return;
    if (typeof o === "string")
      return _arrayLikeToArray(o, minLen);
    var n = Object.prototype.toString.call(o).slice(8, -1);
    if (n === "Object" && o.constructor)
      n = o.constructor.name;
    if (n === "Map" || n === "Set")
      return Array.from(o);
    if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
      return _arrayLikeToArray(o, minLen);
  }
  function _arrayLikeToArray(arr, len) {
    if (len == null || len > arr.length)
      len = arr.length;
    for (var i = 0, arr2 = new Array(len); i < len; i++)
      arr2[i] = arr[i];
    return arr2;
  }
  function _typeof26(obj) {
    "@babel/helpers - typeof";
    return _typeof26 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof26(obj);
  }
  function asyncGeneratorStep2(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator2(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep2(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep2(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function renderContentAsync(_x, _x2) {
    return _renderContentAsync.apply(this, arguments);
  }
  function _renderContentAsync() {
    _renderContentAsync = _asyncToGenerator2(/* @__PURE__ */ _regeneratorRuntime2().mark(function _callee(el, content) {
      var where, html, dependencies, scope, $parent, $grandparent, _args = arguments;
      return _regeneratorRuntime2().wrap(function _callee$(_context) {
        while (1)
          switch (_context.prev = _context.next) {
            case 0:
              where = _args.length > 2 && _args[2] !== void 0 ? _args[2] : "replace";
              if (where === "replace") {
                shinyUnbindAll(el);
              }
              html = "";
              dependencies = [];
              if (content === null) {
                html = "";
              } else if (typeof content === "string") {
                html = content;
              } else if (_typeof26(content) === "object") {
                html = content.html;
                dependencies = content.deps || [];
              }
              _context.next = 7;
              return renderHtmlAsync(html, el, dependencies, where);
            case 7:
              scope = el;
              if (where === "replace") {
                shinyInitializeInputs(el);
                shinyBindAll(el);
              } else {
                $parent = (0, import_jquery26.default)(el).parent();
                if ($parent.length > 0) {
                  scope = $parent;
                  if (where === "beforeBegin" || where === "afterEnd") {
                    $grandparent = $parent.parent();
                    if ($grandparent.length > 0)
                      scope = $grandparent;
                  }
                }
                shinyInitializeInputs(scope);
                shinyBindAll(scope);
              }
            case 9:
            case "end":
              return _context.stop();
          }
      }, _callee);
    }));
    return _renderContentAsync.apply(this, arguments);
  }
  function renderContent(el, content) {
    var where = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : "replace";
    if (where === "replace") {
      shinyUnbindAll(el);
    }
    var html = "";
    var dependencies = [];
    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (_typeof26(content) === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }
    renderHtml2(html, el, dependencies, where);
    var scope = el;
    if (where === "replace") {
      shinyInitializeInputs(el);
      shinyBindAll(el);
    } else {
      var $parent = (0, import_jquery26.default)(el).parent();
      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          var $grandparent = $parent.parent();
          if ($grandparent.length > 0)
            scope = $grandparent;
        }
      }
      shinyInitializeInputs(scope);
      shinyBindAll(scope);
    }
  }
  function renderHtmlAsync(_x3, _x4, _x5) {
    return _renderHtmlAsync.apply(this, arguments);
  }
  function _renderHtmlAsync() {
    _renderHtmlAsync = _asyncToGenerator2(/* @__PURE__ */ _regeneratorRuntime2().mark(function _callee2(html, el, dependencies) {
      var where, _args2 = arguments;
      return _regeneratorRuntime2().wrap(function _callee2$(_context2) {
        while (1)
          switch (_context2.prev = _context2.next) {
            case 0:
              where = _args2.length > 3 && _args2[3] !== void 0 ? _args2[3] : "replace";
              _context2.next = 3;
              return renderDependenciesAsync(dependencies);
            case 3:
              return _context2.abrupt("return", renderHtml(html, el, where));
            case 4:
            case "end":
              return _context2.stop();
          }
      }, _callee2);
    }));
    return _renderHtmlAsync.apply(this, arguments);
  }
  function renderHtml2(html, el, dependencies) {
    var where = arguments.length > 3 && arguments[3] !== void 0 ? arguments[3] : "replace";
    renderDependencies(dependencies);
    return renderHtml(html, el, where);
  }
  function renderDependenciesAsync(_x6) {
    return _renderDependenciesAsync.apply(this, arguments);
  }
  function _renderDependenciesAsync() {
    _renderDependenciesAsync = _asyncToGenerator2(/* @__PURE__ */ _regeneratorRuntime2().mark(function _callee3(dependencies) {
      var _iterator2, _step2, dep;
      return _regeneratorRuntime2().wrap(function _callee3$(_context3) {
        while (1)
          switch (_context3.prev = _context3.next) {
            case 0:
              if (!dependencies) {
                _context3.next = 18;
                break;
              }
              _iterator2 = _createForOfIteratorHelper(dependencies);
              _context3.prev = 2;
              _iterator2.s();
            case 4:
              if ((_step2 = _iterator2.n()).done) {
                _context3.next = 10;
                break;
              }
              dep = _step2.value;
              _context3.next = 8;
              return renderDependencyAsync(dep);
            case 8:
              _context3.next = 4;
              break;
            case 10:
              _context3.next = 15;
              break;
            case 12:
              _context3.prev = 12;
              _context3.t0 = _context3["catch"](2);
              _iterator2.e(_context3.t0);
            case 15:
              _context3.prev = 15;
              _iterator2.f();
              return _context3.finish(15);
            case 18:
            case "end":
              return _context3.stop();
          }
      }, _callee3, null, [[2, 12, 15, 18]]);
    }));
    return _renderDependenciesAsync.apply(this, arguments);
  }
  function renderDependencies(dependencies) {
    if (dependencies) {
      var _iterator = _createForOfIteratorHelper(dependencies), _step;
      try {
        for (_iterator.s(); !(_step = _iterator.n()).done; ) {
          var dep = _step.value;
          renderDependency(dep);
        }
      } catch (err) {
        _iterator.e(err);
      } finally {
        _iterator.f();
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
    var names = Object.keys(htmlDependencies);
    var idx = names.indexOf(dep.name);
    if (idx === -1) {
      return false;
    }
    return htmlDependencies[names[idx]] === dep.version;
  }
  function addStylesheetsAndRestyle(links) {
    var $head = (0, import_jquery26.default)("head").first();
    var refreshStyle = function refreshStyle2(href, oldSheet) {
      var xhr = new XMLHttpRequest();
      xhr.open("GET", href);
      xhr.onload = function() {
        var id = "shiny_restyle_" + href.split("?restyle")[0].replace(/\W/g, "_");
        var oldStyle = $head.find("style#" + id);
        var newStyle = (0, import_jquery26.default)("<style>").attr("id", id).html(xhr.responseText);
        $head.append(newStyle);
        oldStyle.remove();
        removeSheet(oldSheet);
        sendImageSizeFns.transitioned();
      };
      xhr.send();
    };
    var findSheet = function findSheet2(href) {
      if (!href)
        return null;
      for (var i = 0; i < document.styleSheets.length; i++) {
        var sheet = document.styleSheets[i];
        if (typeof sheet.href === "string" && sheet.href.indexOf(href) > -1) {
          return sheet;
        }
      }
      return null;
    };
    var removeSheet = function removeSheet2(sheet) {
      if (!sheet)
        return;
      sheet.disabled = true;
      if (isIE())
        sheet.cssText = "";
      if (sheet.ownerNode instanceof Element) {
        (0, import_jquery26.default)(sheet.ownerNode).remove();
      }
    };
    links.map(function(link) {
      var $link = (0, import_jquery26.default)(link);
      var oldSheet = findSheet($link.attr("href"));
      var href = $link.attr("href") + "?restyle=" + new Date().getTime();
      if (isIE()) {
        refreshStyle(href, oldSheet);
      } else {
        $link.attr("href", href);
        $link.attr("onload", function() {
          var $dummyEl = (0, import_jquery26.default)("<div>").css("transition", "0.1s all").css("position", "absolute").css("top", "-1000px").css("left", "0");
          $dummyEl.one("transitionend", function() {
            $dummyEl.remove();
            removeSheet(oldSheet);
            sendImageSizeFns.transitioned();
          });
          (0, import_jquery26.default)(document.body).append($dummyEl);
          var color = "#" + Math.floor(Math.random() * 16777215).toString(16);
          setTimeout(function() {
            return $dummyEl.css("color", color);
          }, 10);
        });
        $head.append(link);
      }
    });
  }
  function getStylesheetLinkTags(dep) {
    return dep.stylesheet.map(function(x) {
      if (!hasDefinedProperty(x, "rel"))
        x.rel = "stylesheet";
      if (!hasDefinedProperty(x, "type"))
        x.type = "text/css";
      var link = document.createElement("link");
      Object.entries(x).forEach(function(_ref) {
        var _ref2 = _slicedToArray(_ref, 2), attr = _ref2[0], val = _ref2[1];
        if (attr === "href") {
          val = encodeURI(val);
        }
        link.setAttribute(attr, val ? val : "");
      });
      return link;
    });
  }
  function appendStylesheetLinkTags(dep, $head) {
    var stylesheetLinks = getStylesheetLinkTags(dep);
    if (stylesheetLinks.length !== 0) {
      $head.append(stylesheetLinks);
    }
  }
  function appendScriptTags(dep, $head) {
    dep.script.forEach(function(x) {
      var script = document.createElement("script");
      Object.entries(x).forEach(function(_ref3) {
        var _ref4 = _slicedToArray(_ref3, 2), attr = _ref4[0], val = _ref4[1];
        if (attr === "src") {
          val = encodeURI(val);
        }
        script.setAttribute(attr, val ? val : "");
      });
      $head.append(script);
    });
  }
  function appendScriptTagsAsync(_x7) {
    return _appendScriptTagsAsync.apply(this, arguments);
  }
  function _appendScriptTagsAsync() {
    _appendScriptTagsAsync = _asyncToGenerator2(/* @__PURE__ */ _regeneratorRuntime2().mark(function _callee4(dep) {
      var scriptPromises;
      return _regeneratorRuntime2().wrap(function _callee4$(_context4) {
        while (1)
          switch (_context4.prev = _context4.next) {
            case 0:
              scriptPromises = [];
              dep.script.forEach(function(x) {
                var script = document.createElement("script");
                if (!hasDefinedProperty(x, "async")) {
                  script.async = false;
                }
                Object.entries(x).forEach(function(_ref9) {
                  var _ref10 = _slicedToArray(_ref9, 2), attr = _ref10[0], val = _ref10[1];
                  if (attr === "src") {
                    val = encodeURI(val);
                  }
                  script.setAttribute(attr, val ? val : "");
                });
                var p = new Promise(function(resolve, reject) {
                  script.onload = function(e) {
                    resolve(null);
                  };
                  script.onerror = function(e) {
                    reject(e);
                  };
                });
                scriptPromises.push(p);
                document.head.append(script);
              });
              _context4.next = 4;
              return Promise.allSettled(scriptPromises);
            case 4:
            case "end":
              return _context4.stop();
          }
      }, _callee4);
    }));
    return _appendScriptTagsAsync.apply(this, arguments);
  }
  function appendMetaTags(dep, $head) {
    dep.meta.forEach(function(x) {
      var meta = document.createElement("meta");
      for (var _i2 = 0, _Object$entries = Object.entries(x); _i2 < _Object$entries.length; _i2++) {
        var _Object$entries$_i = _slicedToArray(_Object$entries[_i2], 2), attr = _Object$entries$_i[0], val = _Object$entries$_i[1];
        meta.setAttribute(attr, val);
      }
      $head.append(meta);
    });
  }
  function appendAttachmentLinkTags(dep, $head) {
    dep.attachment.forEach(function(x) {
      var link = (0, import_jquery26.default)("<link rel='attachment'>").attr("id", dep.name + "-" + x.key + "-attachment").attr("href", encodeURI(x.href));
      $head.append(link);
    });
  }
  function appendExtraHeadContent(dep, $head) {
    if (dep.head) {
      var $newHead = (0, import_jquery26.default)("<head></head>");
      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
  }
  function renderDependencyAsync(_x8) {
    return _renderDependencyAsync.apply(this, arguments);
  }
  function _renderDependencyAsync() {
    _renderDependencyAsync = _asyncToGenerator2(/* @__PURE__ */ _regeneratorRuntime2().mark(function _callee5(dep_) {
      var dep, $head;
      return _regeneratorRuntime2().wrap(function _callee5$(_context5) {
        while (1)
          switch (_context5.prev = _context5.next) {
            case 0:
              dep = normalizeHtmlDependency(dep_);
              if (!needsRestyle(dep)) {
                _context5.next = 4;
                break;
              }
              addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
              return _context5.abrupt("return", true);
            case 4:
              if (!hasDefinedProperty(htmlDependencies, dep.name)) {
                _context5.next = 6;
                break;
              }
              return _context5.abrupt("return", false);
            case 6:
              registerDependency(dep.name, dep.version);
              $head = (0, import_jquery26.default)("head").first();
              appendMetaTags(dep, $head);
              appendStylesheetLinkTags(dep, $head);
              _context5.next = 12;
              return appendScriptTagsAsync(dep);
            case 12:
              appendAttachmentLinkTags(dep, $head);
              appendExtraHeadContent(dep, $head);
              return _context5.abrupt("return", true);
            case 15:
            case "end":
              return _context5.stop();
          }
      }, _callee5);
    }));
    return _renderDependencyAsync.apply(this, arguments);
  }
  function renderDependency(dep_) {
    var dep = normalizeHtmlDependency(dep_);
    if (needsRestyle(dep)) {
      addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
      return true;
    }
    if (hasDefinedProperty(htmlDependencies, dep.name))
      return false;
    registerDependency(dep.name, dep.version);
    var $head = (0, import_jquery26.default)("head").first();
    appendMetaTags(dep, $head);
    appendStylesheetLinkTags(dep, $head);
    appendScriptTags(dep, $head);
    appendAttachmentLinkTags(dep, $head);
    appendExtraHeadContent(dep, $head);
    return true;
  }
  function normalizeHtmlDependency(dep) {
    var _dep$src;
    var hrefPrefix = (_dep$src = dep.src) === null || _dep$src === void 0 ? void 0 : _dep$src.href;
    var result = {
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
        result.meta = Object.entries(dep.meta).map(function(_ref5) {
          var _ref6 = _slicedToArray(_ref5, 2), attr = _ref6[0], val = _ref6[1];
          return {
            name: attr,
            content: val
          };
        });
      }
    }
    result.stylesheet = asArray(dep.stylesheet).map(function(s) {
      if (typeof s === "string") {
        s = {
          href: s
        };
      }
      if (hrefPrefix) {
        s.href = hrefPrefix + "/" + s.href;
      }
      return s;
    });
    result.script = asArray(dep.script).map(function(s) {
      if (typeof s === "string") {
        s = {
          src: s
        };
      }
      if (hrefPrefix) {
        s.src = hrefPrefix + "/" + s.src;
      }
      return s;
    });
    var attachments = dep.attachment;
    if (!attachments)
      attachments = [];
    if (typeof attachments === "string")
      attachments = [attachments];
    if (Array.isArray(attachments)) {
      var tmp = attachments;
      attachments = tmp.map(function(attachment, index) {
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
      attachments = Object.entries(attachments).map(function(_ref7) {
        var _ref8 = _slicedToArray(_ref7, 2), attr = _ref8[0], val = _ref8[1];
        return {
          key: attr,
          href: val
        };
      });
    }
    result.attachment = attachments.map(function(s) {
      if (hrefPrefix) {
        s.href = hrefPrefix + "/" + s.href;
      }
      return s;
    });
    return result;
  }

  // srcts/src/bindings/output/html.ts
  function _typeof27(obj) {
    "@babel/helpers - typeof";
    return _typeof27 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof27(obj);
  }
  function _regeneratorRuntime3() {
    "use strict";
    _regeneratorRuntime3 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof27(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep3(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator3(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep3(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep3(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function _classCallCheck26(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties26(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey26(descriptor.key), descriptor);
    }
  }
  function _createClass26(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties26(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties26(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey26(arg) {
    var key = _toPrimitive26(arg, "string");
    return _typeof27(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive26(input, hint) {
    if (_typeof27(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof27(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits19(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf19(subClass, superClass);
  }
  function _setPrototypeOf19(o, p) {
    _setPrototypeOf19 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf19(o, p);
  }
  function _createSuper19(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct19();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf19(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf19(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn19(this, result);
    };
  }
  function _possibleConstructorReturn19(self2, call8) {
    if (call8 && (_typeof27(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized19(self2);
  }
  function _assertThisInitialized19(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct19() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf19(o) {
    _getPrototypeOf19 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf19(o);
  }
  var HtmlOutputBinding = /* @__PURE__ */ function(_OutputBinding) {
    _inherits19(HtmlOutputBinding2, _OutputBinding);
    var _super = _createSuper19(HtmlOutputBinding2);
    function HtmlOutputBinding2() {
      _classCallCheck26(this, HtmlOutputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass26(HtmlOutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery27.default)(scope).find(".shiny-html-output");
      }
    }, {
      key: "onValueError",
      value: function onValueError(el, err) {
        shinyUnbindAll(el);
        this.renderError(el, err);
      }
    }, {
      key: "renderValue",
      value: function() {
        var _renderValue = _asyncToGenerator3(/* @__PURE__ */ _regeneratorRuntime3().mark(function _callee(el, data) {
          return _regeneratorRuntime3().wrap(function _callee$(_context) {
            while (1)
              switch (_context.prev = _context.next) {
                case 0:
                  _context.next = 2;
                  return renderContentAsync(el, data);
                case 2:
                case "end":
                  return _context.stop();
              }
          }, _callee);
        }));
        function renderValue(_x, _x2) {
          return _renderValue.apply(this, arguments);
        }
        return renderValue;
      }()
    }]);
    return HtmlOutputBinding2;
  }(OutputBinding);

  // node_modules/core-js/modules/es.array.filter.js
  var $55 = require_export();
  var $filter = require_array_iteration().filter;
  var arrayMethodHasSpeciesSupport5 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT4 = arrayMethodHasSpeciesSupport5("filter");
  $55({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT4 }, {
    filter: function filter(callbackfn) {
      return $filter(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/bindings/output/image.ts
  var import_es_array_iterator29 = __toESM(require_es_array_iterator());
  var import_jquery32 = __toESM(require_jquery());

  // node_modules/core-js/modules/es.array.some.js
  var $56 = require_export();
  var $some = require_array_iteration().some;
  var arrayMethodIsStrict3 = require_array_method_is_strict();
  var STRICT_METHOD = arrayMethodIsStrict3("some");
  $56({ target: "Array", proto: true, forced: !STRICT_METHOD }, {
    some: function some(callbackfn) {
      return $some(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.object.values.js
  var $57 = require_export();
  var $values = require_object_to_array().values;
  $57({ target: "Object", stat: true }, {
    values: function values(O) {
      return $values(O);
    }
  });

  // node_modules/core-js/modules/es.object.get-own-property-descriptors.js
  var $58 = require_export();
  var DESCRIPTORS6 = require_descriptors();
  var ownKeys = require_own_keys();
  var toIndexedObject4 = require_to_indexed_object();
  var getOwnPropertyDescriptorModule2 = require_object_get_own_property_descriptor();
  var createProperty4 = require_create_property();
  $58({ target: "Object", stat: true, sham: !DESCRIPTORS6 }, {
    getOwnPropertyDescriptors: function getOwnPropertyDescriptors(object) {
      var O = toIndexedObject4(object);
      var getOwnPropertyDescriptor3 = getOwnPropertyDescriptorModule2.f;
      var keys2 = ownKeys(O);
      var result = {};
      var index = 0;
      var key, descriptor;
      while (keys2.length > index) {
        descriptor = getOwnPropertyDescriptor3(O, key = keys2[index++]);
        if (descriptor !== void 0)
          createProperty4(result, key, descriptor);
      }
      return result;
    }
  });

  // node_modules/core-js/modules/es.object.define-properties.js
  var $59 = require_export();
  var DESCRIPTORS7 = require_descriptors();
  var defineProperties = require_object_define_properties().f;
  $59({ target: "Object", stat: true, forced: Object.defineProperties !== defineProperties, sham: !DESCRIPTORS7 }, {
    defineProperties: defineProperties
  });

  // srcts/src/imageutils/createBrush.ts
  var import_es_array_iterator28 = __toESM(require_es_array_iterator());
  var import_jquery29 = __toESM(require_jquery());

  // srcts/src/imageutils/initCoordmap.ts
  var import_jquery28 = __toESM(require_jquery());

  // srcts/src/imageutils/initPanelScales.ts
  function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax) {
    var clip = arguments.length > 5 && arguments[5] !== void 0 ? arguments[5] : true;
    clip = clip || true;
    var factor = (rangeMax - rangeMin) / (domainMax - domainMin);
    var val = x - domainMin;
    var newval = val * factor + rangeMin;
    if (clip) {
      var max4 = Math.max(rangeMax, rangeMin);
      var min4 = Math.min(rangeMax, rangeMin);
      if (newval > max4)
        newval = max4;
      else if (newval < min4)
        newval = min4;
    }
    return newval;
  }
  function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
    return {
      scale: function scale(val, clip) {
        if (logbase)
          val = Math.log(val) / Math.log(logbase);
        return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
      },
      scaleInv: function scaleInv(val, clip) {
        var res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);
        if (logbase)
          res = Math.pow(logbase, res);
        return res;
      }
    };
  }
  function addScaleFuns(panel_) {
    var panel = panel_;
    var d = panel.domain;
    var r = panel.range;
    var xlog = panel.log && panel.log.x ? panel.log.x : null;
    var ylog = panel.log && panel.log.y ? panel.log.y : null;
    var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
    var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);
    function scaleDataToImg(val, clip) {
      return mapValues(val, function(value, key) {
        var prefix = key.substring(0, 1);
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
      return mapValues(val, function(value, key) {
        var prefix = key.substring(0, 1);
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
      var newOffset = {
        x: offsetImg.x,
        y: offsetImg.y
      };
      var bounds = panel.range;
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
    return panels.map(function(panel) {
      return addScaleFuns(panel);
    });
  }

  // srcts/src/imageutils/initCoordmap.ts
  function findScalingRatio($el) {
    var boundingRect = $el[0].getBoundingClientRect();
    return {
      x: boundingRect.width / $el.outerWidth(),
      y: boundingRect.height / $el.outerHeight()
    };
  }
  function findOrigin($el) {
    var offset = $el.offset();
    var scalingRatio = findScalingRatio($el);
    var paddingBorder = {
      left: parseInt($el.css("border-left-width")) + parseInt($el.css("padding-left")),
      top: parseInt($el.css("border-top-width")) + parseInt($el.css("padding-top"))
    };
    return {
      x: offset.left + scalingRatio.x * paddingBorder.left,
      y: offset.top + scalingRatio.y * paddingBorder.top
    };
  }
  function findDims($el) {
    var contentRatio = {
      x: $el.width() / $el.outerWidth(),
      y: $el.height() / $el.outerHeight()
    };
    var boundingRect = $el[0].getBoundingClientRect();
    return {
      x: contentRatio.x * boundingRect.width,
      y: contentRatio.y * boundingRect.height
    };
  }
  function initCoordmap($el, coordmap_) {
    var $img = $el.find("img");
    var img = $img[0];
    if (coordmap_.panels.length === 0) {
      var bounds = {
        top: 0,
        left: 0,
        right: img.clientWidth - 1,
        bottom: img.clientHeight - 1
      };
      coordmap_.panels[0] = {
        domain: bounds,
        range: bounds,
        mapping: {}
      };
    }
    var coordmap = coordmap_;
    coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
    coordmap.dims.width = coordmap.dims.width || img.naturalWidth;
    coordmap.panels = initPanelScales(coordmap_.panels);
    coordmap.mouseOffsetCss = function(mouseEvent) {
      var imgOrigin = findOrigin($img);
      return {
        x: mouseEvent.pageX - imgOrigin.x,
        y: mouseEvent.pageY - imgOrigin.y
      };
    };
    function scaleCssToImg(offsetCss) {
      var pixelScaling = coordmap.imgToCssScalingRatio();
      var result = mapValues(offsetCss, function(value, key) {
        var prefix = key.substring(0, 1);
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
      var pixelScaling = coordmap.imgToCssScalingRatio();
      var result = mapValues(offsetImg, function(value, key) {
        var prefix = key.substring(0, 1);
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
      var imgDims = findDims($img);
      return {
        x: imgDims.x / coordmap.dims.width,
        y: imgDims.y / coordmap.dims.height
      };
    };
    coordmap.cssToImgScalingRatio = function() {
      var res = coordmap.imgToCssScalingRatio();
      return {
        x: 1 / res.x,
        y: 1 / res.y
      };
    };
    coordmap.getPanelCss = function(offsetCss) {
      var expand = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 0;
      var offsetImg = coordmap.scaleCssToImg(offsetCss);
      var x = offsetImg.x;
      var y = offsetImg.y;
      var cssToImgRatio = coordmap.cssToImgScalingRatio();
      var expandImg = {
        x: expand * cssToImgRatio.x,
        y: expand * cssToImgRatio.y
      };
      var matches = [];
      var dists = [];
      var b;
      var i;
      for (i = 0; i < coordmap.panels.length; i++) {
        b = coordmap.panels[i].range;
        if (x <= b.right + expandImg.x && x >= b.left - expandImg.x && y <= b.bottom + expandImg.y && y >= b.top - expandImg.y) {
          matches.push(coordmap.panels[i]);
          var xdist = 0;
          var ydist = 0;
          if (x > b.right && x <= b.right + expandImg.x) {
            xdist = x - b.right;
          } else if (x < b.left && x >= b.left - expandImg.x) {
            xdist = x - b.left;
          }
          if (y > b.bottom && y <= b.bottom + expandImg.y) {
            ydist = y - b.bottom;
          } else if (y < b.top && y >= b.top - expandImg.y) {
            ydist = y - b.top;
          }
          dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
        }
      }
      if (matches.length) {
        var minDist = Math.min.apply(null, dists);
        for (i = 0; i < matches.length; i++) {
          if (dists[i] === minDist) {
            return matches[i];
          }
        }
      }
      return null;
    };
    coordmap.isInPanelCss = function(offsetCss) {
      var expand = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 0;
      if (coordmap.getPanelCss(offsetCss, expand))
        return true;
      return false;
    };
    coordmap.mouseCoordinateSender = function(inputId) {
      var clip = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : true;
      var nullOutside = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : false;
      return function(e) {
        if (e === null) {
          shinySetInputValue(inputId, null);
          return;
        }
        var coordsCss = coordmap.mouseOffsetCss(e);
        if (!coordmap.isInPanelCss(coordsCss)) {
          if (nullOutside) {
            shinySetInputValue(inputId, null);
            return;
          }
          if (clip)
            return;
          var _coords = {
            coords_css: coordsCss,
            coords_img: coordmap.scaleCssToImg(coordsCss)
          };
          shinySetInputValue(inputId, _coords, {
            priority: "event"
          });
          return;
        }
        var panel = coordmap.getPanelCss(coordsCss);
        var coordsImg = coordmap.scaleCssToImg(coordsCss);
        var coordsData = panel.scaleImgToData(coordsImg);
        var coords = {
          x: coordsData === null || coordsData === void 0 ? void 0 : coordsData.x,
          y: coordsData === null || coordsData === void 0 ? void 0 : coordsData.y,
          coords_css: coordsCss,
          coords_img: coordsImg,
          img_css_ratio: coordmap.cssToImgScalingRatio()
        };
        import_jquery28.default.extend(coords, panel.panel_vars);
        coords.mapping = panel.mapping;
        coords.domain = panel.domain;
        coords.range = panel.range;
        coords.log = panel.log;
        shinySetInputValue(inputId, coords, {
          priority: "event"
        });
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
  function shiftToRange(vals, min4, max4) {
    if (!(vals instanceof Array))
      vals = [vals];
    var maxval = Math.max.apply(null, vals);
    var minval = Math.min.apply(null, vals);
    var shiftAmount = 0;
    if (maxval > max4) {
      shiftAmount = max4 - maxval;
    } else if (minval < min4) {
      shiftAmount = min4 - minval;
    }
    var newvals = [];
    for (var i = 0; i < vals.length; i++) {
      newvals[i] = vals[i] + shiftAmount;
    }
    return newvals;
  }

  // srcts/src/imageutils/createBrush.ts
  function _typeof28(obj) {
    "@babel/helpers - typeof";
    return _typeof28 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof28(obj);
  }
  function ownKeys2(object, enumerableOnly) {
    var keys2 = Object.keys(object);
    if (Object.getOwnPropertySymbols) {
      var symbols = Object.getOwnPropertySymbols(object);
      enumerableOnly && (symbols = symbols.filter(function(sym) {
        return Object.getOwnPropertyDescriptor(object, sym).enumerable;
      })), keys2.push.apply(keys2, symbols);
    }
    return keys2;
  }
  function _objectSpread(target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = null != arguments[i] ? arguments[i] : {};
      i % 2 ? ownKeys2(Object(source), true).forEach(function(key) {
        _defineProperty9(target, key, source[key]);
      }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)) : ownKeys2(Object(source)).forEach(function(key) {
        Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
      });
    }
    return target;
  }
  function _defineProperty9(obj, key, value) {
    key = _toPropertyKey27(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey27(arg) {
    var key = _toPrimitive27(arg, "string");
    return _typeof28(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive27(input, hint) {
    if (_typeof28(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof28(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function createBrush($el, opts, coordmap, expandPixels) {
    var resizeExpand = 10;
    var el = $el[0];
    var $div = null;
    var state = {};
    var cssToImg = coordmap.scaleCssToImg;
    var imgToCss = coordmap.scaleImgToCss;
    reset();
    function reset() {
      state.brushing = false;
      state.dragging = false;
      state.resizing = false;
      state.down = {
        x: NaN,
        y: NaN
      };
      state.up = {
        x: NaN,
        y: NaN
      };
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
      var oldDiv = $el.find("#" + el.id + "_brush");
      return oldDiv.length > 0;
    }
    function importOldBrush() {
      var oldDiv = $el.find("#" + el.id + "_brush");
      if (oldDiv.length === 0)
        return;
      var oldBoundsData = oldDiv.data("bounds-data");
      var oldPanel = oldDiv.data("panel");
      if (!oldBoundsData || !oldPanel)
        return;
      for (var i = 0; i < coordmap.panels.length; i++) {
        var curPanel = coordmap.panels[i];
        if (equal(oldPanel.mapping, curPanel.mapping) && equal(oldPanel.panel_vars, curPanel.panel_vars)) {
          state.panel = coordmap.panels[i];
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
      var boundsDataVal = boundsData();
      if (Object.values(boundsDataVal).some(isnan))
        return;
      boundsData(boundsDataVal);
      updateDiv();
    }
    function isInsideBrush(offsetCss) {
      var bounds = state.boundsCss;
      return offsetCss.x <= bounds.xmax && offsetCss.x >= bounds.xmin && offsetCss.y <= bounds.ymax && offsetCss.y >= bounds.ymin;
    }
    function isInResizeArea(offsetCss) {
      var sides = whichResizeSides(offsetCss);
      return sides.left || sides.right || sides.top || sides.bottom;
    }
    function whichResizeSides(offsetCss) {
      var b = state.boundsCss;
      var e = {
        xmin: b.xmin - resizeExpand,
        xmax: b.xmax + resizeExpand,
        ymin: b.ymin - resizeExpand,
        ymax: b.ymax + resizeExpand
      };
      var res = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };
      if ((opts.brushDirection === "xy" || opts.brushDirection === "x") && offsetCss.y <= e.ymax && offsetCss.y >= e.ymin) {
        if (offsetCss.x < b.xmin && offsetCss.x >= e.xmin)
          res.left = true;
        else if (offsetCss.x > b.xmax && offsetCss.x <= e.xmax)
          res.right = true;
      }
      if ((opts.brushDirection === "xy" || opts.brushDirection === "y") && offsetCss.x <= e.xmax && offsetCss.x >= e.xmin) {
        if (offsetCss.y < b.ymin && offsetCss.y >= e.ymin)
          res.top = true;
        else if (offsetCss.y > b.ymax && offsetCss.y <= e.ymax)
          res.bottom = true;
      }
      return res;
    }
    function boundsCss(boxCss) {
      if (boxCss === void 0) {
        return _objectSpread({}, state.boundsCss);
      }
      var minCss = {
        x: boxCss.xmin,
        y: boxCss.ymin
      };
      var maxCss = {
        x: boxCss.xmax,
        y: boxCss.ymax
      };
      var panel = state.panel;
      var panelBoundsImg = panel.range;
      if (opts.brushClip) {
        minCss = imgToCss(panel.clipImg(cssToImg(minCss)));
        maxCss = imgToCss(panel.clipImg(cssToImg(maxCss)));
      }
      if (opts.brushDirection === "xy") {
      } else if (opts.brushDirection === "x") {
        minCss.y = imgToCss({
          y: panelBoundsImg.top
        }).y;
        maxCss.y = imgToCss({
          y: panelBoundsImg.bottom
        }).y;
      } else if (opts.brushDirection === "y") {
        minCss.x = imgToCss({
          x: panelBoundsImg.left
        }).x;
        maxCss.x = imgToCss({
          x: panelBoundsImg.right
        }).x;
      }
      state.boundsCss = {
        xmin: minCss.x,
        xmax: maxCss.x,
        ymin: minCss.y,
        ymax: maxCss.y
      };
      var minData = panel.scaleImgToData(cssToImg(minCss));
      var maxData = panel.scaleImgToData(cssToImg(maxCss));
      state.boundsData = findBox(minData, maxData);
      state.boundsData = mapValues(state.boundsData, function(val) {
        return roundSignif(val, 14);
      });
      $div.data("bounds-data", state.boundsData);
      $div.data("panel", state.panel);
      return void 0;
    }
    function boundsData(boxData) {
      if (typeof boxData === "undefined") {
        return _objectSpread({}, state.boundsData);
      }
      var boxCss = imgToCss(state.panel.scaleDataToImg(boxData));
      boxCss = mapValues(boxCss, function(val) {
        return roundSignif(val, 13);
      });
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
      $div = (0, import_jquery29.default)(document.createElement("div")).attr("id", el.id + "_brush").css({
        "background-color": opts.brushFill,
        opacity: opts.brushOpacity,
        "pointer-events": "none",
        position: "absolute"
      }).hide();
      var borderStyle = "1px solid " + opts.brushStroke;
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
        {
          x: 0,
          y: 0
        }
      ).width(0).outerHeight(0);
    }
    function updateDiv() {
      var imgOffsetCss = findOrigin($el.find("img"));
      var b = state.boundsCss;
      $div.offset({
        top: imgOffsetCss.y + b.ymin,
        left: imgOffsetCss.x + b.xmin
      }).outerWidth(b.xmax - b.xmin + 1).outerHeight(b.ymax - b.ymin + 1);
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
      state.changeStartBounds = _objectSpread({}, state.boundsCss);
    }
    function dragTo(offsetCss) {
      var dx = offsetCss.x - state.down.x;
      var dy = offsetCss.y - state.down.y;
      var start = state.changeStartBounds;
      var newBoundsCss = {
        xmin: start.xmin + dx,
        xmax: start.xmax + dx,
        ymin: start.ymin + dy,
        ymax: start.ymax + dy
      };
      if (opts.brushClip) {
        var panel = state.panel;
        var panelBoundsImg = panel.range;
        var newBoundsImg = cssToImg(newBoundsCss);
        var xvalsImg = [newBoundsImg.xmin, newBoundsImg.xmax];
        var yvalsImg = [newBoundsImg.ymin, newBoundsImg.ymax];
        xvalsImg = shiftToRange(xvalsImg, panelBoundsImg.left, panelBoundsImg.right);
        yvalsImg = shiftToRange(yvalsImg, panelBoundsImg.top, panelBoundsImg.bottom);
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
      state.changeStartBounds = _objectSpread({}, state.boundsCss);
      state.resizeSides = whichResizeSides(state.down);
    }
    function resizeTo(offsetCss) {
      var dCss = {
        x: offsetCss.x - state.down.x,
        y: offsetCss.y - state.down.y
      };
      var dImg = cssToImg(dCss);
      var bImg = cssToImg(state.changeStartBounds);
      var panel = state.panel;
      var panelBoundsImg = panel.range;
      if (state.resizeSides.left) {
        var xminImg = shiftToRange(bImg.xmin + dImg.x, panelBoundsImg.left, bImg.xmax)[0];
        bImg.xmin = xminImg;
      } else if (state.resizeSides.right) {
        var xmaxImg = shiftToRange(bImg.xmax + dImg.x, bImg.xmin, panelBoundsImg.right)[0];
        bImg.xmax = xmaxImg;
      }
      if (state.resizeSides.top) {
        var yminImg = shiftToRange(bImg.ymin + dImg.y, panelBoundsImg.top, bImg.ymax)[0];
        bImg.ymin = yminImg;
      } else if (state.resizeSides.bottom) {
        var ymaxImg = shiftToRange(bImg.ymax + dImg.y, bImg.ymin, panelBoundsImg.bottom)[0];
        bImg.ymax = ymaxImg;
      }
      boundsCss(imgToCss(bImg));
      updateDiv();
    }
    function stopResizing() {
      state.resizing = false;
    }
    return {
      reset: reset,
      hasOldBrush: hasOldBrush,
      importOldBrush: importOldBrush,
      isInsideBrush: isInsideBrush,
      isInResizeArea: isInResizeArea,
      whichResizeSides: whichResizeSides,
      onResize: onResize,
      boundsCss: boundsCss,
      boundsData: boundsData,
      getPanel: getPanel2,
      down: down,
      up: up,
      isBrushing: isBrushing,
      startBrushing: startBrushing,
      brushTo: brushTo,
      stopBrushing: stopBrushing,
      isDragging: isDragging,
      startDragging: startDragging,
      dragTo: dragTo,
      stopDragging: stopDragging,
      isResizing: isResizing,
      startResizing: startResizing,
      resizeTo: resizeTo,
      stopResizing: stopResizing
    };
  }

  // srcts/src/imageutils/createClickInfo.ts
  var import_jquery30 = __toESM(require_jquery());
  function createClickInfo($el, dblclickId, dblclickDelay) {
    var clickTimer = void 0;
    var pendingE = null;
    function triggerEvent(newEventType, e) {
      var e2 = import_jquery30.default.Event(newEventType, {
        which: e.which,
        pageX: e.pageX,
        pageY: e.pageY
      });
      $el.trigger(e2);
    }
    function triggerPendingMousedown2() {
      if (pendingE) {
        triggerEvent("mousedown2", pendingE);
        pendingE = null;
      }
    }
    function scheduleMousedown2(e) {
      pendingE = e;
      clickTimer = window.setTimeout(function() {
        triggerPendingMousedown2();
      }, dblclickDelay);
    }
    function mousedown(e) {
      if (e.which !== 1)
        return;
      if (!dblclickId) {
        triggerEvent("mousedown2", e);
        return;
      }
      if (pendingE === null) {
        scheduleMousedown2(e);
      } else {
        clearTimeout(clickTimer);
        if (pendingE && Math.abs(pendingE.pageX - e.pageX) > 2 || Math.abs(pendingE.pageY - e.pageY) > 2) {
          triggerPendingMousedown2();
          scheduleMousedown2(e);
        } else {
          pendingE = null;
          triggerEvent("dblclick2", e);
        }
      }
    }
    function dblclickIE8(e) {
      e.which = 1;
      triggerEvent("dblclick2", e);
    }
    return {
      mousedown: mousedown,
      dblclickIE8: dblclickIE8
    };
  }

  // srcts/src/imageutils/createHandlers.ts
  var import_jquery31 = __toESM(require_jquery());
  function createClickHandler(inputId, clip, coordmap) {
    var clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);
    clickInfoSender(null);
    return {
      mousedown: function mousedown(e) {
        if (e.which !== 1)
          return;
        clickInfoSender(e);
      },
      onResetImg: function onResetImg() {
        clickInfoSender(null);
      },
      onResize: null
    };
  }
  function createHoverHandler(inputId, delay, delayType, clip, nullOutside, coordmap) {
    var sendHoverInfo = coordmap.mouseCoordinateSender(inputId, clip, nullOutside);
    var hoverInfoSender;
    if (delayType === "throttle")
      hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
    else
      hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);
    hoverInfoSender.immediateCall(null);
    var mouseout;
    if (nullOutside)
      mouseout = function mouseout2() {
        hoverInfoSender.normalCall(null);
      };
    else
      mouseout = function mouseout2() {
      };
    return {
      mousemove: function mousemove(e) {
        hoverInfoSender.normalCall(e);
      },
      mouseout: mouseout,
      onResetImg: function onResetImg() {
        hoverInfoSender.immediateCall(null);
      },
      onResize: null
    };
  }
  function createBrushHandler(inputId, $el, opts, coordmap, outputId) {
    var expandPixels = 20;
    var brush = createBrush($el, opts, coordmap, expandPixels);
    $el.on("shiny-internal:brushed.image_output", function(e, coords) {
      if (coords.brushId === inputId && coords.outputId !== outputId) {
        $el.data("mostRecentBrush", false);
        brush.reset();
      }
    });
    function setCursorStyle(style) {
      $el.removeClass("crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize");
      if (style)
        $el.addClass(style);
    }
    function sendBrushInfo() {
      var coords = brush.boundsData();
      if (isNaN(coords.xmin)) {
        shinySetInputValue(inputId, null);
        imageOutputBinding.find(document.documentElement).trigger("shiny-internal:brushed", {
          brushId: inputId,
          outputId: null
        });
        return;
      }
      var panel = brush.getPanel();
      import_jquery31.default.extend(coords, panel.panel_vars);
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
    var brushInfoSender;
    if (opts.brushDelayType === "throttle") {
      brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
    } else {
      brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
    }
    if (!brush.hasOldBrush()) {
      brushInfoSender.immediateCall();
    }
    function mousedown(e) {
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing())
        return;
      if (e.which !== 1)
        return;
      var offsetCss = coordmap.mouseOffsetCss(e);
      if (opts.brushClip && !coordmap.isInPanelCss(offsetCss, expandPixels))
        return;
      brush.up({
        x: NaN,
        y: NaN
      });
      brush.down(offsetCss);
      if (brush.isInResizeArea(offsetCss)) {
        brush.startResizing(offsetCss);
        (0, import_jquery31.default)(document).on("mousemove.image_brush", mousemoveResizing).on("mouseup.image_brush", mouseupResizing);
      } else if (brush.isInsideBrush(offsetCss)) {
        brush.startDragging(offsetCss);
        setCursorStyle("grabbing");
        (0, import_jquery31.default)(document).on("mousemove.image_brush", mousemoveDragging).on("mouseup.image_brush", mouseupDragging);
      } else {
        var panel = coordmap.getPanelCss(offsetCss, expandPixels);
        brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offsetCss)));
        (0, import_jquery31.default)(document).on("mousemove.image_brush", mousemoveBrushing).on("mouseup.image_brush", mouseupBrushing);
      }
    }
    function mousemove(e) {
      var offsetCss = coordmap.mouseOffsetCss(e);
      if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
        if (brush.isInResizeArea(offsetCss)) {
          var r = brush.whichResizeSides(offsetCss);
          if (r.left && r.top || r.right && r.bottom) {
            setCursorStyle("nwse-resize");
          } else if (r.left && r.bottom || r.right && r.top) {
            setCursorStyle("nesw-resize");
          } else if (r.left || r.right) {
            setCursorStyle("ew-resize");
          } else if (r.top || r.bottom) {
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
    function mousemoveBrushing(e) {
      brush.brushTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }
    function mousemoveDragging(e) {
      brush.dragTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }
    function mousemoveResizing(e) {
      brush.resizeTo(coordmap.mouseOffsetCss(e));
      brushInfoSender.normalCall();
    }
    function mouseupBrushing(e) {
      if (e.which !== 1)
        return;
      (0, import_jquery31.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e));
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
    function mouseupDragging(e) {
      if (e.which !== 1)
        return;
      (0, import_jquery31.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e));
      brush.stopDragging();
      setCursorStyle("grabbable");
      if (brushInfoSender.isPending())
        brushInfoSender.immediateCall();
    }
    function mouseupResizing(e) {
      if (e.which !== 1)
        return;
      (0, import_jquery31.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e));
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
      mousedown: mousedown,
      mousemove: mousemove,
      onResetImg: onResetImg,
      onResize: onResize
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
  function _typeof29(obj) {
    "@babel/helpers - typeof";
    return _typeof29 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof29(obj);
  }
  function _classCallCheck27(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties27(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey28(descriptor.key), descriptor);
    }
  }
  function _createClass27(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties27(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties27(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _toPropertyKey28(arg) {
    var key = _toPrimitive28(arg, "string");
    return _typeof29(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive28(input, hint) {
    if (_typeof29(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof29(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function _inherits20(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    Object.defineProperty(subClass, "prototype", { writable: false });
    if (superClass)
      _setPrototypeOf20(subClass, superClass);
  }
  function _setPrototypeOf20(o, p) {
    _setPrototypeOf20 = Object.setPrototypeOf ? Object.setPrototypeOf.bind() : function _setPrototypeOf21(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf20(o, p);
  }
  function _createSuper20(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct20();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf20(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf20(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn20(this, result);
    };
  }
  function _possibleConstructorReturn20(self2, call8) {
    if (call8 && (_typeof29(call8) === "object" || typeof call8 === "function")) {
      return call8;
    } else if (call8 !== void 0) {
      throw new TypeError("Derived constructors may only return object or undefined");
    }
    return _assertThisInitialized20(self2);
  }
  function _assertThisInitialized20(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct20() {
    if (typeof Reflect === "undefined" || !Reflect.construct)
      return false;
    if (Reflect.construct.sham)
      return false;
    if (typeof Proxy === "function")
      return true;
    try {
      Boolean.prototype.valueOf.call(Reflect.construct(Boolean, [], function() {
      }));
      return true;
    } catch (e) {
      return false;
    }
  }
  function _getPrototypeOf20(o) {
    _getPrototypeOf20 = Object.setPrototypeOf ? Object.getPrototypeOf.bind() : function _getPrototypeOf21(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf20(o);
  }
  var ImageOutputBinding = /* @__PURE__ */ function(_OutputBinding) {
    _inherits20(ImageOutputBinding2, _OutputBinding);
    var _super = _createSuper20(ImageOutputBinding2);
    function ImageOutputBinding2() {
      _classCallCheck27(this, ImageOutputBinding2);
      return _super.apply(this, arguments);
    }
    _createClass27(ImageOutputBinding2, [{
      key: "find",
      value: function find2(scope) {
        return (0, import_jquery32.default)(scope).find(".shiny-image-output, .shiny-plot-output");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        var outputId = this.getId(el);
        var $el = (0, import_jquery32.default)(el);
        var img;
        var $img = $el.find("img");
        if ($img.length === 0) {
          img = document.createElement("img");
          $el.append(img);
          $img = (0, import_jquery32.default)(img);
        } else {
          img = $img[0];
          $img.trigger("reset");
        }
        if (!data) {
          $el.empty();
          return;
        }
        var opts = {
          clickId: $el.data("click-id"),
          clickClip: ifUndefined(strToBool($el.data("click-clip")), true),
          dblclickId: $el.data("dblclick-id"),
          dblclickClip: ifUndefined(strToBool($el.data("dblclick-clip")), true),
          dblclickDelay: ifUndefined($el.data("dblclick-delay"), 400),
          hoverId: $el.data("hover-id"),
          hoverClip: ifUndefined(strToBool($el.data("hover-clip")), true),
          hoverDelayType: ifUndefined($el.data("hover-delay-type"), "debounce"),
          hoverDelay: ifUndefined($el.data("hover-delay"), 300),
          hoverNullOutside: ifUndefined(strToBool($el.data("hover-null-outside")), false),
          brushId: $el.data("brush-id"),
          brushClip: ifUndefined(strToBool($el.data("brush-clip")), true),
          brushDelayType: ifUndefined($el.data("brush-delay-type"), "debounce"),
          brushDelay: ifUndefined($el.data("brush-delay"), 300),
          brushFill: ifUndefined($el.data("brush-fill"), "#666"),
          brushStroke: ifUndefined($el.data("brush-stroke"), "#000"),
          brushOpacity: ifUndefined($el.data("brush-opacity"), 0.3),
          brushDirection: ifUndefined($el.data("brush-direction"), "xy"),
          brushResetOnNew: ifUndefined(strToBool($el.data("brush-reset-on-new")), false),
          coordmap: data.coordmap
        };
        if (opts.brushFill === "auto") {
          opts.brushFill = getComputedLinkColor($el[0]);
        }
        if (opts.brushStroke === "auto") {
          opts.brushStroke = getStyle($el[0], "color");
        }
        import_jquery32.default.each(data, function(key, value) {
          if (value === null || key === "coordmap") {
            return;
          }
          if (key === "src" && value === img.getAttribute("src")) {
            img.removeAttribute("src");
          }
          img.setAttribute(key, value);
        });
        for (var i = 0; i < img.attributes.length; i++) {
          var attrib = img.attributes[i];
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
          var optsCoordmap = opts.coordmap = initCoordmap($el, opts.coordmap);
          var clickInfo = createClickInfo($el, opts.dblclickId, opts.dblclickDelay);
          $el.on("mousedown.image_output", clickInfo.mousedown);
          if (isIE() && IEVersion() === 8) {
            $el.on("dblclick.image_output", clickInfo.dblclickIE8);
          }
          if (opts.clickId) {
            disableDrag($el, $img);
            var clickHandler = createClickHandler(opts.clickId, opts.clickClip, optsCoordmap);
            $el.on("mousedown2.image_output", clickHandler.mousedown);
            $el.on("resize.image_output", clickHandler.onResize);
            $img.on("reset.image_output", clickHandler.onResetImg);
          }
          if (opts.dblclickId) {
            disableDrag($el, $img);
            var dblclickHandler = createClickHandler(opts.dblclickId, opts.clickClip, optsCoordmap);
            $el.on("dblclick2.image_output", dblclickHandler.mousedown);
            $el.on("resize.image_output", dblclickHandler.onResize);
            $img.on("reset.image_output", dblclickHandler.onResetImg);
          }
          if (opts.hoverId) {
            disableDrag($el, $img);
            var hoverHandler = createHoverHandler(opts.hoverId, opts.hoverDelay, opts.hoverDelayType, opts.hoverClip, opts.hoverNullOutside, optsCoordmap);
            $el.on("mousemove.image_output", hoverHandler.mousemove);
            $el.on("mouseout.image_output", hoverHandler.mouseout);
            $el.on("resize.image_output", hoverHandler.onResize);
            $img.on("reset.image_output", hoverHandler.onResetImg);
          }
          if (opts.brushId) {
            disableDrag($el, $img);
            var brushHandler = createBrushHandler(opts.brushId, $el, opts, optsCoordmap, outputId);
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
    }, {
      key: "renderError",
      value: function renderError(el, err) {
        (0, import_jquery32.default)(el).find("img").trigger("reset");
        OutputBinding.prototype.renderError.call(this, el, err);
      }
    }, {
      key: "clearError",
      value: function clearError(el) {
        (0, import_jquery32.default)(el).contents().filter(function() {
          return !(this instanceof HTMLElement && (this.tagName === "IMG" || this.id === el.id + "_brush"));
        }).remove();
        OutputBinding.prototype.clearError.call(this, el);
      }
    }, {
      key: "resize",
      value: function resize(el, width, height) {
        (0, import_jquery32.default)(el).find("img").trigger("resize");
        return;
        width;
        height;
      }
    }]);
    return ImageOutputBinding2;
  }(OutputBinding);
  var imageOutputBinding = new ImageOutputBinding();

  // srcts/src/bindings/output/index.ts
  function initOutputBindings() {
    var outputBindings = new BindingRegistry();
    outputBindings.register(new TextOutputBinding(), "shiny.textOutput");
    outputBindings.register(new DownloadLinkOutputBinding(), "shiny.downloadLink");
    outputBindings.register(new DatatableOutputBinding(), "shiny.datatableOutput");
    outputBindings.register(new HtmlOutputBinding(), "shiny.htmlOutput");
    outputBindings.register(imageOutputBinding, "shiny.imageOutput");
    return {
      outputBindings: outputBindings
    };
  }

  // srcts/src/imageutils/resetBrush.ts
  function resetBrush(brushId) {
    shinySetInputValue(brushId, null);
    imageOutputBinding.find(document.documentElement).trigger("shiny-internal:brushed", {
      brushId: brushId,
      outputId: null
    });
  }

  // srcts/src/shiny/notifications.ts
  var import_es_regexp_exec9 = __toESM(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.split.js
  var apply3 = require_function_apply();
  var call6 = require_function_call();
  var uncurryThis9 = require_function_uncurry_this();
  var fixRegExpWellKnownSymbolLogic3 = require_fix_regexp_well_known_symbol_logic();
  var anObject7 = require_an_object();
  var isNullOrUndefined3 = require_is_null_or_undefined();
  var isRegExp = require_is_regexp();
  var requireObjectCoercible3 = require_require_object_coercible();
  var speciesConstructor = require_species_constructor();
  var advanceStringIndex2 = require_advance_string_index();
  var toLength2 = require_to_length();
  var toString7 = require_to_string();
  var getMethod3 = require_get_method();
  var arraySlice = require_array_slice_simple();
  var callRegExpExec = require_regexp_exec_abstract();
  var regexpExec = require_regexp_exec();
  var stickyHelpers = require_regexp_sticky_helpers();
  var fails10 = require_fails();
  var UNSUPPORTED_Y = stickyHelpers.UNSUPPORTED_Y;
  var MAX_UINT32 = 4294967295;
  var min3 = Math.min;
  var $push = [].push;
  var exec = uncurryThis9(/./.exec);
  var push3 = uncurryThis9($push);
  var stringSlice3 = uncurryThis9("".slice);
  var SPLIT_WORKS_WITH_OVERWRITTEN_EXEC = !fails10(function() {
    var re = /(?:)/;
    var originalExec = re.exec;
    re.exec = function() {
      return originalExec.apply(this, arguments);
    };
    var result = "ab".split(re);
    return result.length !== 2 || result[0] !== "a" || result[1] !== "b";
  });
  fixRegExpWellKnownSymbolLogic3("split", function(SPLIT, nativeSplit, maybeCallNative) {
    var internalSplit;
    if ("abbc".split(/(b)*/)[1] == "c" || "test".split(/(?:)/, -1).length != 4 || "ab".split(/(?:ab)*/).length != 2 || ".".split(/(.?)(.?)/).length != 4 || ".".split(/()()/).length > 1 || "".split(/.?/).length) {
      internalSplit = function(separator, limit) {
        var string = toString7(requireObjectCoercible3(this));
        var lim = limit === void 0 ? MAX_UINT32 : limit >>> 0;
        if (lim === 0)
          return [];
        if (separator === void 0)
          return [string];
        if (!isRegExp(separator)) {
          return call6(nativeSplit, string, separator, lim);
        }
        var output = [];
        var flags = (separator.ignoreCase ? "i" : "") + (separator.multiline ? "m" : "") + (separator.unicode ? "u" : "") + (separator.sticky ? "y" : "");
        var lastLastIndex = 0;
        var separatorCopy = new RegExp(separator.source, flags + "g");
        var match, lastIndex, lastLength;
        while (match = call6(regexpExec, separatorCopy, string)) {
          lastIndex = separatorCopy.lastIndex;
          if (lastIndex > lastLastIndex) {
            push3(output, stringSlice3(string, lastLastIndex, match.index));
            if (match.length > 1 && match.index < string.length)
              apply3($push, output, arraySlice(match, 1));
            lastLength = match[0].length;
            lastLastIndex = lastIndex;
            if (output.length >= lim)
              break;
          }
          if (separatorCopy.lastIndex === match.index)
            separatorCopy.lastIndex++;
        }
        if (lastLastIndex === string.length) {
          if (lastLength || !exec(separatorCopy, ""))
            push3(output, "");
        } else
          push3(output, stringSlice3(string, lastLastIndex));
        return output.length > lim ? arraySlice(output, 0, lim) : output;
      };
    } else if ("0".split(void 0, 0).length) {
      internalSplit = function(separator, limit) {
        return separator === void 0 && limit === 0 ? [] : call6(nativeSplit, this, separator, limit);
      };
    } else
      internalSplit = nativeSplit;
    return [
      function split(separator, limit) {
        var O = requireObjectCoercible3(this);
        var splitter = isNullOrUndefined3(separator) ? void 0 : getMethod3(separator, SPLIT);
        return splitter ? call6(splitter, separator, O, limit) : call6(internalSplit, toString7(O), separator, limit);
      },
      function(string, limit) {
        var rx = anObject7(this);
        var S = toString7(string);
        var res = maybeCallNative(internalSplit, rx, S, limit, internalSplit !== nativeSplit);
        if (res.done)
          return res.value;
        var C = speciesConstructor(rx, RegExp);
        var unicodeMatching = rx.unicode;
        var flags = (rx.ignoreCase ? "i" : "") + (rx.multiline ? "m" : "") + (rx.unicode ? "u" : "") + (UNSUPPORTED_Y ? "g" : "y");
        var splitter = new C(UNSUPPORTED_Y ? "^(?:" + rx.source + ")" : rx, flags);
        var lim = limit === void 0 ? MAX_UINT32 : limit >>> 0;
        if (lim === 0)
          return [];
        if (S.length === 0)
          return callRegExpExec(splitter, S) === null ? [S] : [];
        var p = 0;
        var q = 0;
        var A = [];
        while (q < S.length) {
          splitter.lastIndex = UNSUPPORTED_Y ? 0 : q;
          var z = callRegExpExec(splitter, UNSUPPORTED_Y ? stringSlice3(S, q) : S);
          var e;
          if (z === null || (e = min3(toLength2(splitter.lastIndex + (UNSUPPORTED_Y ? q : 0)), S.length)) === p) {
            q = advanceStringIndex2(S, q, unicodeMatching);
          } else {
            push3(A, stringSlice3(S, p, q));
            if (A.length === lim)
              return A;
            for (var i = 1; i <= z.length - 1; i++) {
              push3(A, z[i]);
              if (A.length === lim)
                return A;
            }
            q = p = e;
          }
        }
        push3(A, stringSlice3(S, p));
        return A;
      }
    ];
  }, !SPLIT_WORKS_WITH_OVERWRITTEN_EXEC, UNSUPPORTED_Y);

  // node_modules/core-js/modules/es.string.match.js
  var call7 = require_function_call();
  var fixRegExpWellKnownSymbolLogic4 = require_fix_regexp_well_known_symbol_logic();
  var anObject8 = require_an_object();
  var isNullOrUndefined4 = require_is_null_or_undefined();
  var toLength3 = require_to_length();
  var toString8 = require_to_string();
  var requireObjectCoercible4 = require_require_object_coercible();
  var getMethod4 = require_get_method();
  var advanceStringIndex3 = require_advance_string_index();
  var regExpExec4 = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic4("match", function(MATCH, nativeMatch, maybeCallNative) {
    return [
      function match(regexp) {
        var O = requireObjectCoercible4(this);
        var matcher = isNullOrUndefined4(regexp) ? void 0 : getMethod4(regexp, MATCH);
        return matcher ? call7(matcher, regexp, O) : new RegExp(regexp)[MATCH](toString8(O));
      },
      function(string) {
        var rx = anObject8(this);
        var S = toString8(string);
        var res = maybeCallNative(nativeMatch, rx, S);
        if (res.done)
          return res.value;
        if (!rx.global)
          return regExpExec4(rx, S);
        var fullUnicode = rx.unicode;
        rx.lastIndex = 0;
        var A = [];
        var n = 0;
        var result;
        while ((result = regExpExec4(rx, S)) !== null) {
          var matchStr = toString8(result[0]);
          A[n] = matchStr;
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex3(S, toLength3(rx.lastIndex), fullUnicode);
          n++;
        }
        return n === 0 ? null : A;
      }
    ];
  });

  // srcts/src/shiny/notifications.ts
  var import_es_array_iterator30 = __toESM(require_es_array_iterator());
  var import_jquery33 = __toESM(require_jquery());
  function _typeof30(obj) {
    "@babel/helpers - typeof";
    return _typeof30 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof30(obj);
  }
  function _regeneratorRuntime4() {
    "use strict";
    _regeneratorRuntime4 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof30(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep4(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator4(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep4(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep4(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  var fadeDuration = 250;
  function show() {
    return _show.apply(this, arguments);
  }
  function _show() {
    _show = _asyncToGenerator4(/* @__PURE__ */ _regeneratorRuntime4().mark(function _callee() {
      var _$notificationInit;
      var _ref, _ref$html, html, _ref$action, action, _ref$deps, deps, _ref$duration, duration, _ref$id, id, _ref$closeButton, closeButton, _ref$type, type, $notificationInit, $notification, newHtml, $content, classes, classVal, $close, _args = arguments;
      return _regeneratorRuntime4().wrap(function _callee$(_context) {
        while (1)
          switch (_context.prev = _context.next) {
            case 0:
              _ref = _args.length > 0 && _args[0] !== void 0 ? _args[0] : {}, _ref$html = _ref.html, html = _ref$html === void 0 ? "" : _ref$html, _ref$action = _ref.action, action = _ref$action === void 0 ? "" : _ref$action, _ref$deps = _ref.deps, deps = _ref$deps === void 0 ? [] : _ref$deps, _ref$duration = _ref.duration, duration = _ref$duration === void 0 ? 5e3 : _ref$duration, _ref$id = _ref.id, id = _ref$id === void 0 ? null : _ref$id, _ref$closeButton = _ref.closeButton, closeButton = _ref$closeButton === void 0 ? true : _ref$closeButton, _ref$type = _ref.type, type = _ref$type === void 0 ? null : _ref$type;
              if (!id)
                id = randomId();
              _context.next = 4;
              return renderDependenciesAsync(deps);
            case 4:
              createPanel();
              $notificationInit = get2(id);
              if (((_$notificationInit = $notificationInit) === null || _$notificationInit === void 0 ? void 0 : _$notificationInit.length) === 0)
                $notificationInit = create2(id);
              $notification = $notificationInit;
              newHtml = '<div class="shiny-notification-content-text">'.concat(html, "</div>") + '<div class="shiny-notification-content-action">'.concat(action, "</div>");
              $content = $notification.find(".shiny-notification-content");
              _context.next = 12;
              return renderContentAsync($content, {
                html: newHtml
              });
            case 12:
              classes = $notification === null || $notification === void 0 ? void 0 : $notification.attr("class");
              if (classes) {
                classVal = classes.split(/\s+/).filter(function(cls) {
                  return cls.match(/^shiny-notification-/);
                }).join(" ");
                $notification.removeClass(classVal);
              }
              if (type && type !== "default")
                $notification.addClass("shiny-notification-" + type);
              $close = $notification.find(".shiny-notification-close");
              if (closeButton && $close.length === 0) {
                $notification.append('<div class="shiny-notification-close">&times;</div>');
              } else if (!closeButton && $close.length !== 0) {
                $close.remove();
              }
              if (duration)
                addRemovalCallback(id, duration);
              else
                clearRemovalCallback(id);
              return _context.abrupt("return", id);
            case 19:
            case "end":
              return _context.stop();
          }
      }, _callee);
    }));
    return _show.apply(this, arguments);
  }
  function remove(id) {
    var _get2;
    (_get2 = get2(id)) === null || _get2 === void 0 ? void 0 : _get2.fadeOut(fadeDuration, function() {
      shinyUnbindAll(this);
      (0, import_jquery33.default)(this).remove();
      if (ids().length === 0) {
        getPanel().remove();
      }
    });
  }
  function get2(id) {
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
    return (0, import_jquery33.default)("#shiny-notification-panel");
  }
  function createPanel() {
    var $panel = getPanel();
    if ($panel.length > 0)
      return $panel;
    (0, import_jquery33.default)(document.body).append('<div id="shiny-notification-panel">');
    return $panel;
  }
  function create2(id) {
    var _$notification;
    var $notification = get2(id);
    if (((_$notification = $notification) === null || _$notification === void 0 ? void 0 : _$notification.length) === 0) {
      $notification = (0, import_jquery33.default)('<div id="shiny-notification-'.concat(id, '" class="shiny-notification">') + '<div class="shiny-notification-close">&times;</div><div class="shiny-notification-content"></div></div>');
      $notification.find(".shiny-notification-close").on("click", function(e) {
        e.preventDefault();
        e.stopPropagation();
        remove(id);
      });
      getPanel().append($notification);
    }
    return $notification;
  }
  function addRemovalCallback(id, delay) {
    var _get2;
    clearRemovalCallback(id);
    var removalCallback = setTimeout(function() {
      remove(id);
    }, delay);
    (_get2 = get2(id)) === null || _get2 === void 0 ? void 0 : _get2.data("removalCallback", removalCallback);
  }
  function clearRemovalCallback(id) {
    var $notification = get2(id);
    var oldRemovalCallback = $notification === null || $notification === void 0 ? void 0 : $notification.data("removalCallback");
    if (oldRemovalCallback) {
      clearTimeout(oldRemovalCallback);
    }
  }

  // srcts/src/shiny/modal.ts
  var import_es_array_iterator31 = __toESM(require_es_array_iterator());
  var import_jquery34 = __toESM(require_jquery());
  function _typeof31(obj) {
    "@babel/helpers - typeof";
    return _typeof31 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof31(obj);
  }
  function _regeneratorRuntime5() {
    "use strict";
    _regeneratorRuntime5 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof31(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep5(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator5(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep5(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep5(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function show2() {
    return _show2.apply(this, arguments);
  }
  function _show2() {
    _show2 = _asyncToGenerator5(/* @__PURE__ */ _regeneratorRuntime5().mark(function _callee() {
      var _ref, _ref$html, html, _ref$deps, deps, $modal, _args = arguments;
      return _regeneratorRuntime5().wrap(function _callee$(_context) {
        while (1)
          switch (_context.prev = _context.next) {
            case 0:
              _ref = _args.length > 0 && _args[0] !== void 0 ? _args[0] : {}, _ref$html = _ref.html, html = _ref$html === void 0 ? "" : _ref$html, _ref$deps = _ref.deps, deps = _ref$deps === void 0 ? [] : _ref$deps;
              _context.next = 3;
              return renderDependenciesAsync(deps);
            case 3:
              (0, import_jquery34.default)(".modal-backdrop").remove();
              $modal = (0, import_jquery34.default)("#shiny-modal-wrapper");
              if ($modal.length === 0) {
                $modal = (0, import_jquery34.default)('<div id="shiny-modal-wrapper"></div>');
                (0, import_jquery34.default)(document.body).append($modal);
                $modal.on("hidden.bs.modal", function(e) {
                  if (e.target === (0, import_jquery34.default)("#shiny-modal")[0]) {
                    shinyUnbindAll($modal);
                    $modal.remove();
                  }
                });
              }
              $modal.on("keydown.shinymodal", function(e) {
                if ((0, import_jquery34.default)("#shiny-modal").data("keyboard") === false)
                  return;
                if (e.keyCode === 27) {
                  e.stopPropagation();
                  e.preventDefault();
                }
              });
              _context.next = 9;
              return renderContentAsync($modal, {
                html: html
              });
            case 9:
            case "end":
              return _context.stop();
          }
      }, _callee);
    }));
    return _show2.apply(this, arguments);
  }
  function remove2() {
    var $modal = (0, import_jquery34.default)("#shiny-modal-wrapper");
    $modal.off("keydown.shinymodal");
    if ($modal.find(".modal").length > 0) {
      $modal.find(".modal").modal("hide");
    } else {
      shinyUnbindAll($modal);
      $modal.remove();
    }
  }

  // srcts/src/shiny/reconnectDialog.ts
  var import_jquery35 = __toESM(require_jquery());
  function updateTime(reconnectTime) {
    var $time = (0, import_jquery35.default)("#shiny-reconnect-time");
    if ($time.length === 0)
      return;
    var seconds = Math.floor((reconnectTime - new Date().getTime()) / 1e3);
    if (seconds > 0) {
      $time.text(" in " + seconds + "s");
    } else {
      $time.text("...");
    }
    setTimeout(function() {
      updateTime(reconnectTime);
    }, 1e3);
  }
  function showReconnectDialog(delay) {
    var reconnectTime = new Date().getTime() + delay;
    if ((0, import_jquery35.default)("#shiny-reconnect-text").length > 0)
      return;
    var html = '<span id="shiny-reconnect-text">Attempting to reconnect</span><span id="shiny-reconnect-time"></span>';
    var action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';
    show({
      id: "reconnect",
      html: html,
      action: action,
      duration: null,
      closeButton: false,
      type: "warning"
    });
    updateTime(reconnectTime);
  }
  function hideReconnectDialog() {
    remove("reconnect");
  }

  // srcts/src/shiny/init.ts
  var import_es_regexp_exec12 = __toESM(require_es_regexp_exec());
  var import_jquery39 = __toESM(require_jquery());

  // srcts/src/inputPolicies/inputBatchSender.ts
  var import_es_array_iterator32 = __toESM(require_es_array_iterator());
  function _typeof32(obj) {
    "@babel/helpers - typeof";
    return _typeof32 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof32(obj);
  }
  function _classCallCheck28(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties28(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey29(descriptor.key), descriptor);
    }
  }
  function _createClass28(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties28(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties28(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty10(obj, key, value) {
    key = _toPropertyKey29(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey29(arg) {
    var key = _toPrimitive29(arg, "string");
    return _typeof32(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive29(input, hint) {
    if (_typeof32(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof32(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputBatchSender = /* @__PURE__ */ function() {
    function InputBatchSender2(shinyapp) {
      _classCallCheck28(this, InputBatchSender2);
      _defineProperty10(this, "target", void 0);
      _defineProperty10(this, "shinyapp", void 0);
      _defineProperty10(this, "pendingData", {});
      _defineProperty10(this, "reentrant", false);
      _defineProperty10(this, "sendIsEnqueued", false);
      _defineProperty10(this, "lastChanceCallback", []);
      this.shinyapp = shinyapp;
    }
    _createClass28(InputBatchSender2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        var _this = this;
        this.pendingData[nameType] = value;
        if (!this.reentrant) {
          if (opts.priority === "event") {
            this._sendNow();
          } else if (!this.sendIsEnqueued) {
            this.shinyapp.taskQueue.enqueue(function() {
              _this.sendIsEnqueued = false;
              _this._sendNow();
            });
          }
        }
      }
    }, {
      key: "_sendNow",
      value: function _sendNow() {
        if (this.reentrant) {
          console.trace("Unexpected reentrancy in InputBatchSender!");
        }
        this.reentrant = true;
        try {
          this.lastChanceCallback.forEach(function(callback) {
            return callback();
          });
          var currentData = this.pendingData;
          this.pendingData = {};
          this.shinyapp.sendInput(currentData);
        } finally {
          this.reentrant = false;
        }
      }
    }]);
    return InputBatchSender2;
  }();

  // srcts/src/inputPolicies/inputNoResendDecorator.ts
  var import_es_json_stringify2 = __toESM(require_es_json_stringify());
  var import_es_array_iterator33 = __toESM(require_es_array_iterator());

  // srcts/src/inputPolicies/splitInputNameType.ts
  function splitInputNameType(nameType) {
    var name2 = nameType.split(":");
    return {
      name: name2[0],
      inputType: name2.length > 1 ? name2[1] : ""
    };
  }

  // srcts/src/inputPolicies/inputNoResendDecorator.ts
  function _typeof33(obj) {
    "@babel/helpers - typeof";
    return _typeof33 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof33(obj);
  }
  function _classCallCheck29(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties29(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey30(descriptor.key), descriptor);
    }
  }
  function _createClass29(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties29(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties29(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty11(obj, key, value) {
    key = _toPropertyKey30(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey30(arg) {
    var key = _toPrimitive30(arg, "string");
    return _typeof33(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive30(input, hint) {
    if (_typeof33(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof33(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputNoResendDecorator = /* @__PURE__ */ function() {
    function InputNoResendDecorator2(target) {
      var initialValues = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : {};
      _classCallCheck29(this, InputNoResendDecorator2);
      _defineProperty11(this, "target", void 0);
      _defineProperty11(this, "lastSentValues", {});
      this.target = target;
      this.reset(initialValues);
    }
    _createClass29(InputNoResendDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        var _splitInputNameType = splitInputNameType(nameType), inputName = _splitInputNameType.name, inputType = _splitInputNameType.inputType;
        var jsonValue = JSON.stringify(value);
        if (opts.priority !== "event" && this.lastSentValues[inputName] && this.lastSentValues[inputName].jsonValue === jsonValue && this.lastSentValues[inputName].inputType === inputType) {
          return;
        }
        this.lastSentValues[inputName] = {
          jsonValue: jsonValue,
          inputType: inputType
        };
        this.target.setInput(nameType, value, opts);
      }
    }, {
      key: "reset",
      value: function reset() {
        var values2 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {};
        var cacheValues = {};
        for (var inputName in values2) {
          if (hasDefinedProperty(values2, inputName)) {
            var _splitInputNameType2 = splitInputNameType(inputName), name = _splitInputNameType2.name, inputType = _splitInputNameType2.inputType;
            cacheValues[name] = {
              jsonValue: JSON.stringify(values2[inputName]),
              inputType: inputType
            };
          }
        }
        this.lastSentValues = cacheValues;
      }
    }, {
      key: "forget",
      value: function forget(name) {
        delete this.lastSentValues[name];
      }
    }]);
    return InputNoResendDecorator2;
  }();

  // srcts/src/inputPolicies/inputEventDecorator.ts
  var import_es_array_iterator34 = __toESM(require_es_array_iterator());
  var import_jquery36 = __toESM(require_jquery());
  function _typeof34(obj) {
    "@babel/helpers - typeof";
    return _typeof34 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof34(obj);
  }
  function _classCallCheck30(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties30(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey31(descriptor.key), descriptor);
    }
  }
  function _createClass30(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties30(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties30(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty12(obj, key, value) {
    key = _toPropertyKey31(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey31(arg) {
    var key = _toPrimitive31(arg, "string");
    return _typeof34(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive31(input, hint) {
    if (_typeof34(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof34(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputEventDecorator = /* @__PURE__ */ function() {
    function InputEventDecorator2(target) {
      _classCallCheck30(this, InputEventDecorator2);
      _defineProperty12(this, "target", void 0);
      this.target = target;
    }
    _createClass30(InputEventDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        var evt = import_jquery36.default.Event("shiny:inputchanged");
        var input = splitInputNameType(nameType);
        evt.name = input.name;
        evt.inputType = input.inputType;
        evt.value = value;
        evt.binding = opts.binding || null;
        evt.el = opts.el || null;
        evt.priority = opts.priority;
        (0, import_jquery36.default)(opts.el || window.document).trigger(evt);
        if (!evt.isDefaultPrevented()) {
          var name = evt.name;
          if (evt.inputType !== "")
            name += ":" + evt.inputType;
          this.target.setInput(name, evt.value, {
            priority: opts.priority
          });
        }
      }
    }]);
    return InputEventDecorator2;
  }();

  // srcts/src/inputPolicies/inputRateDecorator.ts
  var import_es_array_iterator35 = __toESM(require_es_array_iterator());
  function _typeof35(obj) {
    "@babel/helpers - typeof";
    return _typeof35 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof35(obj);
  }
  function _classCallCheck31(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties31(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey32(descriptor.key), descriptor);
    }
  }
  function _createClass31(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties31(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties31(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty13(obj, key, value) {
    key = _toPropertyKey32(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey32(arg) {
    var key = _toPrimitive32(arg, "string");
    return _typeof35(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive32(input, hint) {
    if (_typeof35(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof35(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputRateDecorator = /* @__PURE__ */ function() {
    function InputRateDecorator2(target) {
      _classCallCheck31(this, InputRateDecorator2);
      _defineProperty13(this, "target", void 0);
      _defineProperty13(this, "inputRatePolicies", {});
      this.target = target;
    }
    _createClass31(InputRateDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        var _splitInputNameType = splitInputNameType(nameType), inputName = _splitInputNameType.name;
        this._ensureInit(inputName);
        if (opts.priority !== "deferred")
          this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
        else
          this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
      }
    }, {
      key: "setRatePolicy",
      value: function setRatePolicy(nameType, mode, millis) {
        var _splitInputNameType2 = splitInputNameType(nameType), inputName = _splitInputNameType2.name;
        if (mode === "direct") {
          this.inputRatePolicies[inputName] = new Invoker(this, this._doSetInput);
        } else if (mode === "debounce") {
          this.inputRatePolicies[inputName] = new Debouncer(this, this._doSetInput, millis);
        } else if (mode === "throttle") {
          this.inputRatePolicies[inputName] = new Throttler(this, this._doSetInput, millis);
        }
      }
    }, {
      key: "_ensureInit",
      value: function _ensureInit(name) {
        if (!(name in this.inputRatePolicies))
          this.setRatePolicy(name, "direct");
      }
    }, {
      key: "_doSetInput",
      value: function _doSetInput(nameType, value, opts) {
        this.target.setInput(nameType, value, opts);
      }
    }]);
    return InputRateDecorator2;
  }();

  // srcts/src/inputPolicies/inputDeferDecorator.ts
  var import_es_regexp_exec10 = __toESM(require_es_regexp_exec());
  var import_es_array_iterator36 = __toESM(require_es_array_iterator());
  function _typeof36(obj) {
    "@babel/helpers - typeof";
    return _typeof36 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof36(obj);
  }
  function _classCallCheck32(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties32(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey33(descriptor.key), descriptor);
    }
  }
  function _createClass32(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties32(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties32(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty14(obj, key, value) {
    key = _toPropertyKey33(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey33(arg) {
    var key = _toPrimitive33(arg, "string");
    return _typeof36(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive33(input, hint) {
    if (_typeof36(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof36(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var InputDeferDecorator = /* @__PURE__ */ function() {
    function InputDeferDecorator2(target) {
      _classCallCheck32(this, InputDeferDecorator2);
      _defineProperty14(this, "pendingInput", {});
      _defineProperty14(this, "target", void 0);
      this.target = target;
    }
    _createClass32(InputDeferDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        if (/^\./.test(nameType))
          this.target.setInput(nameType, value, opts);
        else
          this.pendingInput[nameType] = {
            value: value,
            opts: opts
          };
      }
    }, {
      key: "submit",
      value: function submit() {
        for (var nameType in this.pendingInput) {
          if (hasDefinedProperty(this.pendingInput, nameType)) {
            var _this$pendingInput$na = this.pendingInput[nameType], value = _this$pendingInput$na.value, opts = _this$pendingInput$na.opts;
            this.target.setInput(nameType, value, opts);
          }
        }
      }
    }]);
    return InputDeferDecorator2;
  }();

  // srcts/src/inputPolicies/inputValidateDecorator.ts
  var import_es_array_iterator37 = __toESM(require_es_array_iterator());
  function _typeof37(obj) {
    "@babel/helpers - typeof";
    return _typeof37 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof37(obj);
  }
  function _classCallCheck33(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties33(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey34(descriptor.key), descriptor);
    }
  }
  function _createClass33(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties33(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties33(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function ownKeys3(object, enumerableOnly) {
    var keys2 = Object.keys(object);
    if (Object.getOwnPropertySymbols) {
      var symbols = Object.getOwnPropertySymbols(object);
      enumerableOnly && (symbols = symbols.filter(function(sym) {
        return Object.getOwnPropertyDescriptor(object, sym).enumerable;
      })), keys2.push.apply(keys2, symbols);
    }
    return keys2;
  }
  function _objectSpread2(target) {
    for (var i = 1; i < arguments.length; i++) {
      var source = null != arguments[i] ? arguments[i] : {};
      i % 2 ? ownKeys3(Object(source), true).forEach(function(key) {
        _defineProperty15(target, key, source[key]);
      }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(target, Object.getOwnPropertyDescriptors(source)) : ownKeys3(Object(source)).forEach(function(key) {
        Object.defineProperty(target, key, Object.getOwnPropertyDescriptor(source, key));
      });
    }
    return target;
  }
  function _defineProperty15(obj, key, value) {
    key = _toPropertyKey34(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey34(arg) {
    var key = _toPrimitive34(arg, "string");
    return _typeof37(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive34(input, hint) {
    if (_typeof37(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof37(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  function addDefaultInputOpts(opts) {
    var newOpts = _objectSpread2({
      priority: "immediate"
    }, opts);
    switch (newOpts.priority) {
      case "deferred":
      case "immediate":
      case "event":
        break;
      default:
        throw new Error("Unexpected input value mode: '" + newOpts.priority + "'");
    }
    return newOpts;
  }
  var InputValidateDecorator = /* @__PURE__ */ function() {
    function InputValidateDecorator2(target) {
      _classCallCheck33(this, InputValidateDecorator2);
      _defineProperty15(this, "target", void 0);
      this.target = target;
    }
    _createClass33(InputValidateDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value) {
        var opts = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : {};
        if (!nameType)
          throw "Can't set input with empty name.";
        var newOpts = addDefaultInputOpts(opts);
        this.target.setInput(nameType, value, newOpts);
      }
    }]);
    return InputValidateDecorator2;
  }();

  // srcts/src/shiny/bind.ts
  var import_jquery37 = __toESM(require_jquery());

  // srcts/src/bindings/outputAdapter.ts
  var import_es_array_iterator38 = __toESM(require_es_array_iterator());
  function _typeof38(obj) {
    "@babel/helpers - typeof";
    return _typeof38 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof38(obj);
  }
  function _regeneratorRuntime6() {
    "use strict";
    _regeneratorRuntime6 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof38(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep6(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator6(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep6(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep6(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function _classCallCheck34(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties34(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey35(descriptor.key), descriptor);
    }
  }
  function _createClass34(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties34(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties34(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty16(obj, key, value) {
    key = _toPropertyKey35(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey35(arg) {
    var key = _toPrimitive35(arg, "string");
    return _typeof38(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive35(input, hint) {
    if (_typeof38(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof38(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var OutputBindingAdapter = /* @__PURE__ */ function() {
    function OutputBindingAdapter2(el, binding) {
      _classCallCheck34(this, OutputBindingAdapter2);
      _defineProperty16(this, "el", void 0);
      _defineProperty16(this, "binding", void 0);
      this.el = el;
      this.binding = binding;
      if (binding.resize) {
        this.onResize = makeResizeFilter(el, function(width, height) {
          binding.resize(el, width, height);
        });
      }
    }
    _createClass34(OutputBindingAdapter2, [{
      key: "getId",
      value: function getId() {
        return this.binding.getId(this.el);
      }
    }, {
      key: "onValueChange",
      value: function() {
        var _onValueChange = _asyncToGenerator6(/* @__PURE__ */ _regeneratorRuntime6().mark(function _callee(data) {
          return _regeneratorRuntime6().wrap(function _callee$(_context) {
            while (1)
              switch (_context.prev = _context.next) {
                case 0:
                  _context.next = 2;
                  return this.binding.onValueChange(this.el, data);
                case 2:
                case "end":
                  return _context.stop();
              }
          }, _callee, this);
        }));
        function onValueChange(_x) {
          return _onValueChange.apply(this, arguments);
        }
        return onValueChange;
      }()
    }, {
      key: "onValueError",
      value: function onValueError(err) {
        this.binding.onValueError(this.el, err);
      }
    }, {
      key: "showProgress",
      value: function showProgress(show3) {
        this.binding.showProgress(this.el, show3);
      }
    }, {
      key: "onResize",
      value: function onResize() {
      }
    }]);
    return OutputBindingAdapter2;
  }();

  // srcts/src/shiny/bind.ts
  var boundInputs = {};
  function valueChangeCallback(inputs, binding, el, allowDeferred) {
    var id = binding.getId(el);
    if (id) {
      var value = binding.getValue(el);
      var type = binding.getType(el);
      if (type)
        id = id + ":" + type;
      var opts = {
        priority: allowDeferred ? "deferred" : "immediate",
        binding: binding,
        el: el
      };
      inputs.setInput(id, value, opts);
    }
  }
  function bindInputs(shinyCtx) {
    var scope = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : document.documentElement;
    var inputs = shinyCtx.inputs, inputsRate = shinyCtx.inputsRate, inputBindings = shinyCtx.inputBindings;
    var bindings = inputBindings.getBindings();
    var inputItems = {};
    var _loop = function _loop2() {
      var binding = bindings[i].binding;
      var matches = binding.find(scope) || [];
      var _loop22 = function _loop23() {
        var el = matches[j];
        var id = binding.getId(el);
        if (!id || boundInputs[id])
          return "continue";
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
        var thisCallback = function() {
          var thisBinding = binding;
          var thisEl = el;
          return function(allowDeferred) {
            valueChangeCallback(inputs, thisBinding, thisEl, allowDeferred);
          };
        }();
        binding.subscribe(el, thisCallback);
        (0, import_jquery37.default)(el).data("shiny-input-binding", binding);
        (0, import_jquery37.default)(el).addClass("shiny-bound-input");
        var ratePolicy = binding.getRatePolicy(el);
        if (ratePolicy !== null) {
          inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
        }
        boundInputs[id] = {
          binding: binding,
          node: el
        };
        (0, import_jquery37.default)(el).trigger({
          type: "shiny:bound",
          binding: binding,
          bindingType: "input"
        });
      };
      for (var j = 0; j < matches.length; j++) {
        var _ret = _loop22();
        if (_ret === "continue")
          continue;
      }
    };
    for (var i = 0; i < bindings.length; i++) {
      _loop();
    }
    return inputItems;
  }
  function bindOutputs(_ref) {
    var sendOutputHiddenState = _ref.sendOutputHiddenState, maybeAddThemeObserver = _ref.maybeAddThemeObserver, outputBindings = _ref.outputBindings;
    var scope = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : document.documentElement;
    var $scope = (0, import_jquery37.default)(scope);
    var bindings = outputBindings.getBindings();
    for (var i = 0; i < bindings.length; i++) {
      var binding = bindings[i].binding;
      var matches = binding.find($scope) || [];
      for (var j = 0; j < matches.length; j++) {
        var _el = matches[j];
        var id = binding.getId(_el);
        if (!id)
          continue;
        if (!import_jquery37.default.contains(document.documentElement, _el))
          continue;
        var $el = (0, import_jquery37.default)(_el);
        if ($el.hasClass("shiny-bound-output")) {
          continue;
        }
        maybeAddThemeObserver(_el);
        var bindingAdapter = new OutputBindingAdapter(_el, binding);
        shinyAppBindOutput(id, bindingAdapter);
        $el.data("shiny-output-binding", bindingAdapter);
        $el.addClass("shiny-bound-output");
        if (!$el.attr("aria-live"))
          $el.attr("aria-live", "polite");
        $el.trigger({
          type: "shiny:bound",
          binding: binding,
          bindingType: "output"
        });
      }
    }
    setTimeout(sendImageSizeFns.regular, 0);
    setTimeout(sendOutputHiddenState, 0);
  }
  function unbindInputs() {
    var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document.documentElement;
    var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
    var inputs = (0, import_jquery37.default)(scope).find(".shiny-bound-input").toArray();
    if (includeSelf && (0, import_jquery37.default)(scope).hasClass("shiny-bound-input")) {
      inputs.push(scope);
    }
    for (var i = 0; i < inputs.length; i++) {
      var _el2 = inputs[i];
      var binding = (0, import_jquery37.default)(_el2).data("shiny-input-binding");
      if (!binding)
        continue;
      var id = binding.getId(_el2);
      (0, import_jquery37.default)(_el2).removeClass("shiny-bound-input");
      delete boundInputs[id];
      binding.unsubscribe(_el2);
      (0, import_jquery37.default)(_el2).trigger({
        type: "shiny:unbound",
        binding: binding,
        bindingType: "input"
      });
    }
  }
  function unbindOutputs(_ref2) {
    var sendOutputHiddenState = _ref2.sendOutputHiddenState;
    var scope = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : document.documentElement;
    var includeSelf = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : false;
    var outputs = (0, import_jquery37.default)(scope).find(".shiny-bound-output").toArray();
    if (includeSelf && (0, import_jquery37.default)(scope).hasClass("shiny-bound-output")) {
      outputs.push(scope);
    }
    for (var i = 0; i < outputs.length; i++) {
      var $el = (0, import_jquery37.default)(outputs[i]);
      var bindingAdapter = $el.data("shiny-output-binding");
      if (!bindingAdapter)
        continue;
      var id = bindingAdapter.binding.getId(outputs[i]);
      shinyAppUnbindOutput(id, bindingAdapter);
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
  function _bindAll(shinyCtx, scope) {
    bindOutputs(shinyCtx, scope);
    return bindInputs(shinyCtx, scope);
  }
  function unbindAll(shinyCtx, scope) {
    var includeSelf = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : false;
    unbindInputs(scope, includeSelf);
    unbindOutputs(shinyCtx, scope, includeSelf);
  }
  function bindAll(shinyCtx, scope) {
    var currentInputItems = _bindAll(shinyCtx, scope);
    var inputs = shinyCtx.inputs;
    import_jquery37.default.each(currentInputItems, function(name, item) {
      inputs.setInput(name, item.value, item.opts);
    });
    shinyCtx.initDeferredIframes();
  }

  // srcts/src/shiny/shinyapp.ts
  var import_es_regexp_exec11 = __toESM(require_es_regexp_exec());
  var import_es_json_stringify3 = __toESM(require_es_json_stringify());

  // node_modules/core-js/modules/es.array-buffer.constructor.js
  var $70 = require_export();
  var global8 = require_global();
  var arrayBufferModule = require_array_buffer();
  var setSpecies = require_set_species();
  var ARRAY_BUFFER = "ArrayBuffer";
  var ArrayBuffer2 = arrayBufferModule[ARRAY_BUFFER];
  var NativeArrayBuffer = global8[ARRAY_BUFFER];
  $70({ global: true, constructor: true, forced: NativeArrayBuffer !== ArrayBuffer2 }, {
    ArrayBuffer: ArrayBuffer2
  });
  setSpecies(ARRAY_BUFFER);

  // node_modules/core-js/modules/es.array-buffer.slice.js
  var $71 = require_export();
  var uncurryThis10 = require_function_uncurry_this_clause();
  var fails11 = require_fails();
  var ArrayBufferModule = require_array_buffer();
  var anObject9 = require_an_object();
  var toAbsoluteIndex3 = require_to_absolute_index();
  var toLength4 = require_to_length();
  var speciesConstructor2 = require_species_constructor();
  var ArrayBuffer3 = ArrayBufferModule.ArrayBuffer;
  var DataView2 = ArrayBufferModule.DataView;
  var DataViewPrototype = DataView2.prototype;
  var nativeArrayBufferSlice = uncurryThis10(ArrayBuffer3.prototype.slice);
  var getUint8 = uncurryThis10(DataViewPrototype.getUint8);
  var setUint8 = uncurryThis10(DataViewPrototype.setUint8);
  var INCORRECT_SLICE = fails11(function() {
    return !new ArrayBuffer3(2).slice(1, void 0).byteLength;
  });
  $71({ target: "ArrayBuffer", proto: true, unsafe: true, forced: INCORRECT_SLICE }, {
    slice: function slice2(start, end) {
      if (nativeArrayBufferSlice && end === void 0) {
        return nativeArrayBufferSlice(anObject9(this), start);
      }
      var length = anObject9(this).byteLength;
      var first = toAbsoluteIndex3(start, length);
      var fin = toAbsoluteIndex3(end === void 0 ? length : end, length);
      var result = new (speciesConstructor2(this, ArrayBuffer3))(toLength4(fin - first));
      var viewSource = new DataView2(this);
      var viewTarget = new DataView2(result);
      var index = 0;
      while (first < fin) {
        setUint8(viewTarget, index++, getUint8(viewSource, first++));
      }
      return result;
    }
  });

  // node_modules/core-js/modules/es.data-view.js
  require_es_data_view_constructor();

  // node_modules/core-js/modules/es.array.reduce.js
  var $72 = require_export();
  var $reduce = require_array_reduce().left;
  var arrayMethodIsStrict4 = require_array_method_is_strict();
  var CHROME_VERSION = require_engine_v8_version();
  var IS_NODE = require_engine_is_node();
  var CHROME_BUG = !IS_NODE && CHROME_VERSION > 79 && CHROME_VERSION < 83;
  var FORCED8 = CHROME_BUG || !arrayMethodIsStrict4("reduce");
  $72({ target: "Array", proto: true, forced: FORCED8 }, {
    reduce: function reduce(callbackfn) {
      var length = arguments.length;
      return $reduce(this, callbackfn, length, length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/shiny/shinyapp.ts
  var import_es_array_iterator40 = __toESM(require_es_array_iterator());
  var import_jquery38 = __toESM(require_jquery());

  // srcts/src/utils/asyncQueue.ts
  var import_es_array_iterator39 = __toESM(require_es_array_iterator());
  function _typeof39(obj) {
    "@babel/helpers - typeof";
    return _typeof39 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof39(obj);
  }
  function _regeneratorRuntime7() {
    "use strict";
    _regeneratorRuntime7 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof39(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep7(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator7(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep7(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep7(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function _classCallCheck35(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties35(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey36(descriptor.key), descriptor);
    }
  }
  function _createClass35(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties35(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties35(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty17(obj, key, value) {
    key = _toPropertyKey36(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey36(arg) {
    var key = _toPrimitive36(arg, "string");
    return _typeof39(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive36(input, hint) {
    if (_typeof39(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof39(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
  var AsyncQueue = /* @__PURE__ */ function() {
    function AsyncQueue2() {
      _classCallCheck35(this, AsyncQueue2);
      _defineProperty17(this, "$promises", []);
      _defineProperty17(this, "$resolvers", []);
    }
    _createClass35(AsyncQueue2, [{
      key: "_add",
      value: function _add() {
        var _this = this;
        var p = new Promise(function(resolve) {
          _this.$resolvers.push(resolve);
        });
        this.$promises.push(p);
      }
    }, {
      key: "enqueue",
      value: function enqueue(x) {
        if (!this.$resolvers.length)
          this._add();
        var resolve = this.$resolvers.shift();
        resolve(x);
      }
    }, {
      key: "dequeue",
      value: function() {
        var _dequeue = _asyncToGenerator7(/* @__PURE__ */ _regeneratorRuntime7().mark(function _callee() {
          var promise;
          return _regeneratorRuntime7().wrap(function _callee$(_context) {
            while (1)
              switch (_context.prev = _context.next) {
                case 0:
                  if (!this.$promises.length)
                    this._add();
                  promise = this.$promises.shift();
                  return _context.abrupt("return", promise);
                case 3:
                case "end":
                  return _context.stop();
              }
          }, _callee, this);
        }));
        function dequeue() {
          return _dequeue.apply(this, arguments);
        }
        return dequeue;
      }()
    }, {
      key: "isEmpty",
      value: function isEmpty() {
        return !this.$promises.length;
      }
    }, {
      key: "isBlocked",
      value: function isBlocked() {
        return !!this.$resolvers.length;
      }
    }, {
      key: "length",
      get: function get3() {
        return this.$promises.length - this.$resolvers.length;
      }
    }]);
    return AsyncQueue2;
  }();

  // srcts/src/shiny/shinyapp.ts
  function _typeof40(obj) {
    "@babel/helpers - typeof";
    return _typeof40 = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function(obj2) {
      return typeof obj2;
    } : function(obj2) {
      return obj2 && "function" == typeof Symbol && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
    }, _typeof40(obj);
  }
  function _createForOfIteratorHelper2(o, allowArrayLike) {
    var it = typeof Symbol !== "undefined" && o[Symbol.iterator] || o["@@iterator"];
    if (!it) {
      if (Array.isArray(o) || (it = _unsupportedIterableToArray2(o)) || allowArrayLike && o && typeof o.length === "number") {
        if (it)
          o = it;
        var i = 0;
        var F = function F2() {
        };
        return { s: F, n: function n() {
          if (i >= o.length)
            return { done: true };
          return { done: false, value: o[i++] };
        }, e: function e(_e) {
          throw _e;
        }, f: F };
      }
      throw new TypeError("Invalid attempt to iterate non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
    }
    var normalCompletion = true, didErr = false, err;
    return { s: function s() {
      it = it.call(o);
    }, n: function n() {
      var step = it.next();
      normalCompletion = step.done;
      return step;
    }, e: function e(_e2) {
      didErr = true;
      err = _e2;
    }, f: function f() {
      try {
        if (!normalCompletion && it.return != null)
          it.return();
      } finally {
        if (didErr)
          throw err;
      }
    } };
  }
  function _unsupportedIterableToArray2(o, minLen) {
    if (!o)
      return;
    if (typeof o === "string")
      return _arrayLikeToArray2(o, minLen);
    var n = Object.prototype.toString.call(o).slice(8, -1);
    if (n === "Object" && o.constructor)
      n = o.constructor.name;
    if (n === "Map" || n === "Set")
      return Array.from(o);
    if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n))
      return _arrayLikeToArray2(o, minLen);
  }
  function _arrayLikeToArray2(arr, len) {
    if (len == null || len > arr.length)
      len = arr.length;
    for (var i = 0, arr2 = new Array(len); i < len; i++)
      arr2[i] = arr[i];
    return arr2;
  }
  function _regeneratorRuntime8() {
    "use strict";
    _regeneratorRuntime8 = function _regeneratorRuntime9() {
      return exports;
    };
    var exports = {}, Op = Object.prototype, hasOwn4 = Op.hasOwnProperty, defineProperty3 = Object.defineProperty || function(obj, key, desc) {
      obj[key] = desc.value;
    }, $Symbol = "function" == typeof Symbol ? Symbol : {}, iteratorSymbol = $Symbol.iterator || "@@iterator", asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator", toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
    function define(obj, key, value) {
      return Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }), obj[key];
    }
    try {
      define({}, "");
    } catch (err) {
      define = function define2(obj, key, value) {
        return obj[key] = value;
      };
    }
    function wrap(innerFn, outerFn, self2, tryLocsList) {
      var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator, generator = Object.create(protoGenerator.prototype), context = new Context(tryLocsList || []);
      return defineProperty3(generator, "_invoke", { value: makeInvokeMethod(innerFn, self2, context) }), generator;
    }
    function tryCatch(fn, obj, arg) {
      try {
        return { type: "normal", arg: fn.call(obj, arg) };
      } catch (err) {
        return { type: "throw", arg: err };
      }
    }
    exports.wrap = wrap;
    var ContinueSentinel = {};
    function Generator() {
    }
    function GeneratorFunction() {
    }
    function GeneratorFunctionPrototype() {
    }
    var IteratorPrototype = {};
    define(IteratorPrototype, iteratorSymbol, function() {
      return this;
    });
    var getProto = Object.getPrototypeOf, NativeIteratorPrototype = getProto && getProto(getProto(values2([])));
    NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn4.call(NativeIteratorPrototype, iteratorSymbol) && (IteratorPrototype = NativeIteratorPrototype);
    var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
    function defineIteratorMethods(prototype) {
      ["next", "throw", "return"].forEach(function(method) {
        define(prototype, method, function(arg) {
          return this._invoke(method, arg);
        });
      });
    }
    function AsyncIterator(generator, PromiseImpl) {
      function invoke(method, arg, resolve, reject) {
        var record = tryCatch(generator[method], generator, arg);
        if ("throw" !== record.type) {
          var result = record.arg, value = result.value;
          return value && "object" == _typeof40(value) && hasOwn4.call(value, "__await") ? PromiseImpl.resolve(value.__await).then(function(value2) {
            invoke("next", value2, resolve, reject);
          }, function(err) {
            invoke("throw", err, resolve, reject);
          }) : PromiseImpl.resolve(value).then(function(unwrapped) {
            result.value = unwrapped, resolve(result);
          }, function(error) {
            return invoke("throw", error, resolve, reject);
          });
        }
        reject(record.arg);
      }
      var previousPromise;
      defineProperty3(this, "_invoke", { value: function value(method, arg) {
        function callInvokeWithMethodAndArg() {
          return new PromiseImpl(function(resolve, reject) {
            invoke(method, arg, resolve, reject);
          });
        }
        return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      } });
    }
    function makeInvokeMethod(innerFn, self2, context) {
      var state = "suspendedStart";
      return function(method, arg) {
        if ("executing" === state)
          throw new Error("Generator is already running");
        if ("completed" === state) {
          if ("throw" === method)
            throw arg;
          return doneResult();
        }
        for (context.method = method, context.arg = arg; ; ) {
          var delegate = context.delegate;
          if (delegate) {
            var delegateResult = maybeInvokeDelegate(delegate, context);
            if (delegateResult) {
              if (delegateResult === ContinueSentinel)
                continue;
              return delegateResult;
            }
          }
          if ("next" === context.method)
            context.sent = context._sent = context.arg;
          else if ("throw" === context.method) {
            if ("suspendedStart" === state)
              throw state = "completed", context.arg;
            context.dispatchException(context.arg);
          } else
            "return" === context.method && context.abrupt("return", context.arg);
          state = "executing";
          var record = tryCatch(innerFn, self2, context);
          if ("normal" === record.type) {
            if (state = context.done ? "completed" : "suspendedYield", record.arg === ContinueSentinel)
              continue;
            return { value: record.arg, done: context.done };
          }
          "throw" === record.type && (state = "completed", context.method = "throw", context.arg = record.arg);
        }
      };
    }
    function maybeInvokeDelegate(delegate, context) {
      var methodName = context.method, method = delegate.iterator[methodName];
      if (void 0 === method)
        return context.delegate = null, "throw" === methodName && delegate.iterator.return && (context.method = "return", context.arg = void 0, maybeInvokeDelegate(delegate, context), "throw" === context.method) || "return" !== methodName && (context.method = "throw", context.arg = new TypeError("The iterator does not provide a '" + methodName + "' method")), ContinueSentinel;
      var record = tryCatch(method, delegate.iterator, context.arg);
      if ("throw" === record.type)
        return context.method = "throw", context.arg = record.arg, context.delegate = null, ContinueSentinel;
      var info = record.arg;
      return info ? info.done ? (context[delegate.resultName] = info.value, context.next = delegate.nextLoc, "return" !== context.method && (context.method = "next", context.arg = void 0), context.delegate = null, ContinueSentinel) : info : (context.method = "throw", context.arg = new TypeError("iterator result is not an object"), context.delegate = null, ContinueSentinel);
    }
    function pushTryEntry(locs) {
      var entry = { tryLoc: locs[0] };
      1 in locs && (entry.catchLoc = locs[1]), 2 in locs && (entry.finallyLoc = locs[2], entry.afterLoc = locs[3]), this.tryEntries.push(entry);
    }
    function resetTryEntry(entry) {
      var record = entry.completion || {};
      record.type = "normal", delete record.arg, entry.completion = record;
    }
    function Context(tryLocsList) {
      this.tryEntries = [{ tryLoc: "root" }], tryLocsList.forEach(pushTryEntry, this), this.reset(true);
    }
    function values2(iterable) {
      if (iterable) {
        var iteratorMethod = iterable[iteratorSymbol];
        if (iteratorMethod)
          return iteratorMethod.call(iterable);
        if ("function" == typeof iterable.next)
          return iterable;
        if (!isNaN(iterable.length)) {
          var i = -1, next2 = function next3() {
            for (; ++i < iterable.length; )
              if (hasOwn4.call(iterable, i))
                return next3.value = iterable[i], next3.done = false, next3;
            return next3.value = void 0, next3.done = true, next3;
          };
          return next2.next = next2;
        }
      }
      return { next: doneResult };
    }
    function doneResult() {
      return { value: void 0, done: true };
    }
    return GeneratorFunction.prototype = GeneratorFunctionPrototype, defineProperty3(Gp, "constructor", { value: GeneratorFunctionPrototype, configurable: true }), defineProperty3(GeneratorFunctionPrototype, "constructor", { value: GeneratorFunction, configurable: true }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, toStringTagSymbol, "GeneratorFunction"), exports.isGeneratorFunction = function(genFun) {
      var ctor = "function" == typeof genFun && genFun.constructor;
      return !!ctor && (ctor === GeneratorFunction || "GeneratorFunction" === (ctor.displayName || ctor.name));
    }, exports.mark = function(genFun) {
      return Object.setPrototypeOf ? Object.setPrototypeOf(genFun, GeneratorFunctionPrototype) : (genFun.__proto__ = GeneratorFunctionPrototype, define(genFun, toStringTagSymbol, "GeneratorFunction")), genFun.prototype = Object.create(Gp), genFun;
    }, exports.awrap = function(arg) {
      return { __await: arg };
    }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, asyncIteratorSymbol, function() {
      return this;
    }), exports.AsyncIterator = AsyncIterator, exports.async = function(innerFn, outerFn, self2, tryLocsList, PromiseImpl) {
      void 0 === PromiseImpl && (PromiseImpl = Promise);
      var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList), PromiseImpl);
      return exports.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
        return result.done ? result.value : iter.next();
      });
    }, defineIteratorMethods(Gp), define(Gp, toStringTagSymbol, "Generator"), define(Gp, iteratorSymbol, function() {
      return this;
    }), define(Gp, "toString", function() {
      return "[object Generator]";
    }), exports.keys = function(val) {
      var object = Object(val), keys2 = [];
      for (var key in object)
        keys2.push(key);
      return keys2.reverse(), function next2() {
        for (; keys2.length; ) {
          var key2 = keys2.pop();
          if (key2 in object)
            return next2.value = key2, next2.done = false, next2;
        }
        return next2.done = true, next2;
      };
    }, exports.values = values2, Context.prototype = { constructor: Context, reset: function reset(skipTempReset) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = void 0, this.done = false, this.delegate = null, this.method = "next", this.arg = void 0, this.tryEntries.forEach(resetTryEntry), !skipTempReset)
        for (var name in this)
          "t" === name.charAt(0) && hasOwn4.call(this, name) && !isNaN(+name.slice(1)) && (this[name] = void 0);
    }, stop: function stop() {
      this.done = true;
      var rootRecord = this.tryEntries[0].completion;
      if ("throw" === rootRecord.type)
        throw rootRecord.arg;
      return this.rval;
    }, dispatchException: function dispatchException(exception) {
      if (this.done)
        throw exception;
      var context = this;
      function handle(loc, caught) {
        return record.type = "throw", record.arg = exception, context.next = loc, caught && (context.method = "next", context.arg = void 0), !!caught;
      }
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i], record = entry.completion;
        if ("root" === entry.tryLoc)
          return handle("end");
        if (entry.tryLoc <= this.prev) {
          var hasCatch = hasOwn4.call(entry, "catchLoc"), hasFinally = hasOwn4.call(entry, "finallyLoc");
          if (hasCatch && hasFinally) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          } else if (hasCatch) {
            if (this.prev < entry.catchLoc)
              return handle(entry.catchLoc, true);
          } else {
            if (!hasFinally)
              throw new Error("try statement without catch or finally");
            if (this.prev < entry.finallyLoc)
              return handle(entry.finallyLoc);
          }
        }
      }
    }, abrupt: function abrupt(type, arg) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc <= this.prev && hasOwn4.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
          var finallyEntry = entry;
          break;
        }
      }
      finallyEntry && ("break" === type || "continue" === type) && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc && (finallyEntry = null);
      var record = finallyEntry ? finallyEntry.completion : {};
      return record.type = type, record.arg = arg, finallyEntry ? (this.method = "next", this.next = finallyEntry.finallyLoc, ContinueSentinel) : this.complete(record);
    }, complete: function complete(record, afterLoc) {
      if ("throw" === record.type)
        throw record.arg;
      return "break" === record.type || "continue" === record.type ? this.next = record.arg : "return" === record.type ? (this.rval = this.arg = record.arg, this.method = "return", this.next = "end") : "normal" === record.type && afterLoc && (this.next = afterLoc), ContinueSentinel;
    }, finish: function finish(finallyLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.finallyLoc === finallyLoc)
          return this.complete(entry.completion, entry.afterLoc), resetTryEntry(entry), ContinueSentinel;
      }
    }, catch: function _catch(tryLoc) {
      for (var i = this.tryEntries.length - 1; i >= 0; --i) {
        var entry = this.tryEntries[i];
        if (entry.tryLoc === tryLoc) {
          var record = entry.completion;
          if ("throw" === record.type) {
            var thrown = record.arg;
            resetTryEntry(entry);
          }
          return thrown;
        }
      }
      throw new Error("illegal catch attempt");
    }, delegateYield: function delegateYield(iterable, resultName, nextLoc) {
      return this.delegate = { iterator: values2(iterable), resultName: resultName, nextLoc: nextLoc }, "next" === this.method && (this.arg = void 0), ContinueSentinel;
    } }, exports;
  }
  function asyncGeneratorStep8(gen, resolve, reject, _next, _throw, key, arg) {
    try {
      var info = gen[key](arg);
      var value = info.value;
    } catch (error) {
      reject(error);
      return;
    }
    if (info.done) {
      resolve(value);
    } else {
      Promise.resolve(value).then(_next, _throw);
    }
  }
  function _asyncToGenerator8(fn) {
    return function() {
      var self2 = this, args = arguments;
      return new Promise(function(resolve, reject) {
        var gen = fn.apply(self2, args);
        function _next(value) {
          asyncGeneratorStep8(gen, resolve, reject, _next, _throw, "next", value);
        }
        function _throw(err) {
          asyncGeneratorStep8(gen, resolve, reject, _next, _throw, "throw", err);
        }
        _next(void 0);
      });
    };
  }
  function _classCallCheck36(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperties36(target, props) {
    for (var i = 0; i < props.length; i++) {
      var descriptor = props[i];
      descriptor.enumerable = descriptor.enumerable || false;
      descriptor.configurable = true;
      if ("value" in descriptor)
        descriptor.writable = true;
      Object.defineProperty(target, _toPropertyKey37(descriptor.key), descriptor);
    }
  }
  function _createClass36(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties36(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties36(Constructor, staticProps);
    Object.defineProperty(Constructor, "prototype", { writable: false });
    return Constructor;
  }
  function _defineProperty18(obj, key, value) {
    key = _toPropertyKey37(key);
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function _toPropertyKey37(arg) {
    var key = _toPrimitive37(arg, "string");
    return _typeof40(key) === "symbol" ? key : String(key);
  }
  function _toPrimitive37(input, hint) {
    if (_typeof40(input) !== "object" || input === null)
      return input;
    var prim = input[Symbol.toPrimitive];
    if (prim !== void 0) {
      var res = prim.call(input, hint || "default");
      if (_typeof40(res) !== "object")
        return res;
      throw new TypeError("@@toPrimitive must return a primitive value.");
    }
    return (hint === "string" ? String : Number)(input);
  }
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
      var typeIdx = customMessageHandlerOrder.indexOf(type);
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
  var ShinyApp = /* @__PURE__ */ function() {
    function ShinyApp2() {
      _classCallCheck36(this, ShinyApp2);
      _defineProperty18(this, "$socket", null);
      _defineProperty18(this, "taskQueue", new AsyncQueue());
      _defineProperty18(this, "config", null);
      _defineProperty18(this, "$inputValues", {});
      _defineProperty18(this, "$initialInput", null);
      _defineProperty18(this, "$bindings", {});
      _defineProperty18(this, "$values", {});
      _defineProperty18(this, "$errors", {});
      _defineProperty18(this, "$conditionals", {});
      _defineProperty18(this, "$pendingMessages", []);
      _defineProperty18(this, "$activeRequests", {});
      _defineProperty18(this, "$nextRequestId", 0);
      _defineProperty18(this, "$allowReconnect", false);
      _defineProperty18(this, "scheduledReconnect", void 0);
      _defineProperty18(this, "reconnectDelay", function() {
        var attempts = 0;
        var delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];
        return {
          next: function next2() {
            var i = attempts;
            if (i >= delays.length) {
              i = delays.length - 1;
            }
            attempts++;
            return delays[i];
          },
          reset: function reset() {
            attempts = 0;
          }
        };
      }());
      _defineProperty18(this, "progressHandlers", {
        binding: function binding(message) {
          var key = message.id;
          var binding2 = this.$bindings[key];
          if (binding2) {
            (0, import_jquery38.default)(binding2.el).trigger({
              type: "shiny:outputinvalidated",
              binding: binding2,
              name: key
            });
            if (binding2.showProgress)
              binding2.showProgress(true);
          }
        },
        open: function() {
          var _open = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee(message) {
            var $container, depth, $progress, $progressBar, $progressText;
            return _regeneratorRuntime8().wrap(function _callee$(_context) {
              while (1)
                switch (_context.prev = _context.next) {
                  case 0:
                    if (!(message.style === "notification")) {
                      _context.next = 5;
                      break;
                    }
                    _context.next = 3;
                    return show({
                      html: '<div id="shiny-progress-'.concat(message.id, '" class="shiny-progress-notification">') + '<div class="progress active" style="display: none;"><div class="progress-bar"></div></div><div class="progress-text"><span class="progress-message">message</span> <span class="progress-detail"></span></div></div>',
                      id: message.id,
                      duration: null
                    });
                  case 3:
                    _context.next = 6;
                    break;
                  case 5:
                    if (message.style === "old") {
                      $container = (0, import_jquery38.default)(".shiny-progress-container");
                      if ($container.length === 0) {
                        $container = (0, import_jquery38.default)('<div class="shiny-progress-container"></div>');
                        (0, import_jquery38.default)(document.body).append($container);
                      }
                      depth = (0, import_jquery38.default)(".shiny-progress.open").length;
                      $progress = (0, import_jquery38.default)('<div class="shiny-progress open"><div class="progress active"><div class="progress-bar bar"></div></div><div class="progress-text"><span class="progress-message">message</span><span class="progress-detail"></span></div></div>');
                      $progress.attr("id", message.id);
                      $container.append($progress);
                      $progressBar = $progress.find(".progress");
                      if ($progressBar) {
                        $progressBar.css("top", depth * $progressBar.height() + "px");
                        $progressText = $progress.find(".progress-text");
                        $progressText.css("top", 3 * $progressBar.height() + depth * $progressText.outerHeight() + "px");
                        $progress.hide();
                      }
                    }
                  case 6:
                  case "end":
                    return _context.stop();
                }
            }, _callee);
          }));
          function open(_x) {
            return _open.apply(this, arguments);
          }
          return open;
        }(),
        update: function update(message) {
          if (message.style === "notification") {
            var $progress = (0, import_jquery38.default)("#shiny-progress-" + message.id);
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
            var _$progress = (0, import_jquery38.default)("#" + message.id + ".shiny-progress");
            if (typeof message.message !== "undefined") {
              _$progress.find(".progress-message").text(message.message);
            }
            if (typeof message.detail !== "undefined") {
              _$progress.find(".progress-detail").text(message.detail);
            }
            if (typeof message.value !== "undefined" && message.value !== null) {
              _$progress.find(".progress").show();
              _$progress.find(".bar").width(message.value * 100 + "%");
            }
            _$progress.fadeIn();
          }
        },
        close: function close(message) {
          if (message.style === "notification") {
            remove(message.id);
          } else if (message.style === "old") {
            var $progress = (0, import_jquery38.default)("#" + message.id + ".shiny-progress");
            $progress.removeClass("open");
            $progress.fadeOut({
              complete: function complete() {
                $progress.remove();
                if ((0, import_jquery38.default)(".shiny-progress").length === 0)
                  (0, import_jquery38.default)(".shiny-progress-container").remove();
              }
            });
          }
        }
      });
      this._init();
    }
    _createClass36(ShinyApp2, [{
      key: "connect",
      value: function connect(initialInput) {
        if (this.$socket)
          throw "Connect was already called on this application object";
        this.$socket = this.createSocket();
        this.$initialInput = initialInput;
        import_jquery38.default.extend(this.$inputValues, initialInput);
        this.$updateConditionals();
      }
    }, {
      key: "isConnected",
      value: function isConnected() {
        return !!this.$socket;
      }
    }, {
      key: "reconnect",
      value: function reconnect() {
        clearTimeout(this.scheduledReconnect);
        if (this.isConnected())
          throw "Attempted to reconnect, but already connected.";
        this.$socket = this.createSocket();
        this.$initialInput = import_jquery38.default.extend({}, this.$inputValues);
        this.$updateConditionals();
      }
    }, {
      key: "createSocket",
      value: function createSocket() {
        var _this = this;
        var createSocketFunc = getShinyCreateWebsocket() || function() {
          var protocol = "ws:";
          if (window.location.protocol === "https:")
            protocol = "wss:";
          var defaultPath = window.location.pathname;
          if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
            defaultPath = encodeURI(defaultPath);
            if (isQt()) {
              defaultPath = encodeURI(defaultPath);
            }
          }
          if (!/\/$/.test(defaultPath))
            defaultPath += "/";
          defaultPath += "websocket/";
          var ws = new WebSocket(protocol + "//" + window.location.host + defaultPath);
          ws.binaryType = "arraybuffer";
          return ws;
        };
        var socket = createSocketFunc();
        var hasOpened = false;
        socket.onopen = function() {
          hasOpened = true;
          (0, import_jquery38.default)(document).trigger({
            type: "shiny:connected",
            socket: socket
          });
          _this.onConnected();
          socket.send(JSON.stringify({
            method: "init",
            data: _this.$initialInput
          }));
          while (_this.$pendingMessages.length) {
            var msg = _this.$pendingMessages.shift();
            socket.send(msg);
          }
          _this.startActionQueueLoop();
        };
        socket.onmessage = function(e) {
          _this.taskQueue.enqueue(/* @__PURE__ */ _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee2() {
            return _regeneratorRuntime8().wrap(function _callee2$(_context2) {
              while (1)
                switch (_context2.prev = _context2.next) {
                  case 0:
                    _context2.next = 2;
                    return _this.dispatchMessage(e.data);
                  case 2:
                    return _context2.abrupt("return", _context2.sent);
                  case 3:
                  case "end":
                    return _context2.stop();
                }
            }, _callee2);
          })));
        };
        socket.onclose = function() {
          if (hasOpened) {
            (0, import_jquery38.default)(document).trigger({
              type: "shiny:disconnected",
              socket: socket
            });
            _this.$notifyDisconnected();
          }
          _this.onDisconnected();
          _this.$removeSocket();
        };
        return socket;
      }
    }, {
      key: "startActionQueueLoop",
      value: function() {
        var _startActionQueueLoop = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee3() {
          var action;
          return _regeneratorRuntime8().wrap(function _callee3$(_context3) {
            while (1)
              switch (_context3.prev = _context3.next) {
                case 0:
                  if (false) {
                    _context3.next = 14;
                    break;
                  }
                  _context3.next = 3;
                  return this.taskQueue.dequeue();
                case 3:
                  action = _context3.sent;
                  _context3.prev = 4;
                  _context3.next = 7;
                  return action();
                case 7:
                  _context3.next = 12;
                  break;
                case 9:
                  _context3.prev = 9;
                  _context3.t0 = _context3["catch"](4);
                  console.error(_context3.t0);
                case 12:
                  _context3.next = 0;
                  break;
                case 14:
                case "end":
                  return _context3.stop();
              }
          }, _callee3, this, [[4, 9]]);
        }));
        function startActionQueueLoop() {
          return _startActionQueueLoop.apply(this, arguments);
        }
        return startActionQueueLoop;
      }()
    }, {
      key: "sendInput",
      value: function sendInput(values2) {
        var msg = JSON.stringify({
          method: "update",
          data: values2
        });
        this.$sendMsg(msg);
        import_jquery38.default.extend(this.$inputValues, values2);
        this.$updateConditionals();
      }
    }, {
      key: "$notifyDisconnected",
      value: function $notifyDisconnected() {
        if (window.parent) {
          window.parent.postMessage("disconnected", "*");
        }
      }
    }, {
      key: "$removeSocket",
      value: function $removeSocket() {
        this.$socket = null;
      }
    }, {
      key: "$scheduleReconnect",
      value: function $scheduleReconnect(delay) {
        var _this2 = this;
        this.scheduledReconnect = window.setTimeout(function() {
          _this2.reconnect();
        }, delay);
      }
    }, {
      key: "onDisconnected",
      value: function onDisconnected() {
        var $overlay = (0, import_jquery38.default)("#shiny-disconnected-overlay");
        if ($overlay.length === 0) {
          (0, import_jquery38.default)(document.body).append('<div id="shiny-disconnected-overlay"></div>');
        }
        if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
          var delay = this.reconnectDelay.next();
          showReconnectDialog(delay);
          this.$scheduleReconnect(delay);
        }
      }
    }, {
      key: "onConnected",
      value: function onConnected() {
        (0, import_jquery38.default)("#shiny-disconnected-overlay").remove();
        hideReconnectDialog();
        this.reconnectDelay.reset();
      }
    }, {
      key: "makeRequest",
      value: function makeRequest(method, args, onSuccess, onError, blobs) {
        var requestId = this.$nextRequestId;
        while (this.$activeRequests[requestId]) {
          requestId = (requestId + 1) % 1e9;
        }
        this.$nextRequestId = requestId + 1;
        this.$activeRequests[requestId] = {
          onSuccess: onSuccess,
          onError: onError
        };
        var msg = JSON.stringify({
          method: method,
          args: args,
          tag: requestId
        });
        if (blobs) {
          var uint32ToBuf = function uint32ToBuf2(val) {
            var buffer = new ArrayBuffer(4);
            var view = new DataView(buffer);
            view.setUint32(0, val, true);
            return buffer;
          };
          var payload = [];
          payload.push(uint32ToBuf(16908802));
          var jsonBuf = new Blob([msg]);
          payload.push(uint32ToBuf(jsonBuf.size));
          payload.push(jsonBuf);
          for (var i = 0; i < blobs.length; i++) {
            var _blob = blobs[i];
            payload.push(uint32ToBuf(_blob.byteLength || _blob.size || 0));
            payload.push(_blob);
          }
          var blob = new Blob(payload);
          msg = blob;
        }
        this.$sendMsg(msg);
      }
    }, {
      key: "$sendMsg",
      value: function $sendMsg(msg) {
        if (!this.$socket.readyState) {
          this.$pendingMessages.push(msg);
        } else {
          this.$socket.send(msg);
        }
      }
    }, {
      key: "receiveError",
      value: function receiveError(name, error) {
        if (this.$errors[name] === error)
          return;
        this.$errors[name] = error;
        delete this.$values[name];
        var binding = this.$bindings[name];
        var evt = import_jquery38.default.Event("shiny:error");
        evt.name = name;
        evt.error = error;
        evt.binding = binding;
        (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
        if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
          binding.onValueError(evt.error);
        }
      }
    }, {
      key: "receiveOutput",
      value: function() {
        var _receiveOutput = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee4(name, value) {
          var binding, evt;
          return _regeneratorRuntime8().wrap(function _callee4$(_context4) {
            while (1)
              switch (_context4.prev = _context4.next) {
                case 0:
                  binding = this.$bindings[name];
                  evt = import_jquery38.default.Event("shiny:value");
                  evt.name = name;
                  evt.value = value;
                  evt.binding = binding;
                  if (!(this.$values[name] === value)) {
                    _context4.next = 8;
                    break;
                  }
                  (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
                  return _context4.abrupt("return", void 0);
                case 8:
                  this.$values[name] = value;
                  delete this.$errors[name];
                  (0, import_jquery38.default)(binding ? binding.el : document).trigger(evt);
                  if (!(!evt.isDefaultPrevented() && binding)) {
                    _context4.next = 14;
                    break;
                  }
                  _context4.next = 14;
                  return binding.onValueChange(evt.value);
                case 14:
                  return _context4.abrupt("return", value);
                case 15:
                case "end":
                  return _context4.stop();
              }
          }, _callee4, this);
        }));
        function receiveOutput(_x2, _x3) {
          return _receiveOutput.apply(this, arguments);
        }
        return receiveOutput;
      }()
    }, {
      key: "bindOutput",
      value: function bindOutput(id, binding) {
        if (!id)
          throw "Can't bind an element with no ID";
        if (this.$bindings[id])
          throw "Duplicate binding for ID " + id;
        this.$bindings[id] = binding;
        if (this.$values[id] !== void 0)
          binding.onValueChange(this.$values[id]);
        else if (this.$errors[id] !== void 0)
          binding.onValueError(this.$errors[id]);
        return binding;
      }
    }, {
      key: "unbindOutput",
      value: function unbindOutput(id, binding) {
        if (this.$bindings[id] === binding) {
          delete this.$bindings[id];
          return true;
        } else {
          return false;
        }
      }
    }, {
      key: "_narrowScopeComponent",
      value: function _narrowScopeComponent(scopeComponent, nsPrefix) {
        return Object.keys(scopeComponent).filter(function(k) {
          return k.indexOf(nsPrefix) === 0;
        }).map(function(k) {
          return _defineProperty18({}, k.substring(nsPrefix.length), scopeComponent[k]);
        }).reduce(function(obj, pair) {
          return import_jquery38.default.extend(obj, pair);
        }, {});
      }
    }, {
      key: "_narrowScope",
      value: function _narrowScope(scope, nsPrefix) {
        if (nsPrefix) {
          return {
            input: this._narrowScopeComponent(scope.input, nsPrefix),
            output: this._narrowScopeComponent(scope.output, nsPrefix)
          };
        }
        return scope;
      }
    }, {
      key: "$updateConditionals",
      value: function $updateConditionals() {
        (0, import_jquery38.default)(document).trigger({
          type: "shiny:conditional"
        });
        var inputs = {};
        for (var name in this.$inputValues) {
          if (hasOwnProperty(this.$inputValues, name)) {
            var shortName = name.replace(/:.*/, "");
            inputs[shortName] = this.$inputValues[name];
          }
        }
        var scope = {
          input: inputs,
          output: this.$values
        };
        var conditionals = (0, import_jquery38.default)(document).find("[data-display-if]");
        for (var i = 0; i < conditionals.length; i++) {
          var el = (0, import_jquery38.default)(conditionals[i]);
          var condFunc = el.data("data-display-if-func");
          if (!condFunc) {
            var condExpr = el.attr("data-display-if");
            condFunc = scopeExprToFunc(condExpr);
            el.data("data-display-if-func", condFunc);
          }
          var nsPrefix = el.attr("data-ns-prefix");
          var nsScope = this._narrowScope(scope, nsPrefix);
          var show3 = condFunc(nsScope);
          var showing = el.css("display") !== "none";
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
    }, {
      key: "dispatchMessage",
      value: function() {
        var _dispatchMessage = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee5(data) {
          var msgObj, len, typedv, typebuf, i, type, evt;
          return _regeneratorRuntime8().wrap(function _callee5$(_context5) {
            while (1)
              switch (_context5.prev = _context5.next) {
                case 0:
                  msgObj = {};
                  if (typeof data === "string") {
                    msgObj = JSON.parse(data);
                  } else {
                    len = new DataView(data, 0, 1).getUint8(0);
                    typedv = new DataView(data, 1, len);
                    typebuf = [];
                    for (i = 0; i < len; i++) {
                      typebuf.push(String.fromCharCode(typedv.getUint8(i)));
                    }
                    type = typebuf.join("");
                    data = data.slice(len + 1);
                    msgObj.custom = {};
                    msgObj.custom[type] = data;
                  }
                  evt = import_jquery38.default.Event("shiny:message");
                  evt.message = msgObj;
                  (0, import_jquery38.default)(document).trigger(evt);
                  if (!evt.isDefaultPrevented()) {
                    _context5.next = 7;
                    break;
                  }
                  return _context5.abrupt("return");
                case 7:
                  _context5.next = 9;
                  return this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);
                case 9:
                  this.$updateConditionals();
                case 10:
                case "end":
                  return _context5.stop();
              }
          }, _callee5, this);
        }));
        function dispatchMessage(_x4) {
          return _dispatchMessage.apply(this, arguments);
        }
        return dispatchMessage;
      }()
    }, {
      key: "_sendMessagesToHandlers",
      value: function() {
        var _sendMessagesToHandlers2 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee6(msgObj, handlers, handlerOrder) {
          var i, msgType;
          return _regeneratorRuntime8().wrap(function _callee6$(_context6) {
            while (1)
              switch (_context6.prev = _context6.next) {
                case 0:
                  i = 0;
                case 1:
                  if (!(i < handlerOrder.length)) {
                    _context6.next = 9;
                    break;
                  }
                  msgType = handlerOrder[i];
                  if (!hasOwnProperty(msgObj, msgType)) {
                    _context6.next = 6;
                    break;
                  }
                  _context6.next = 6;
                  return handlers[msgType].call(this, msgObj[msgType]);
                case 6:
                  i++;
                  _context6.next = 1;
                  break;
                case 9:
                case "end":
                  return _context6.stop();
              }
          }, _callee6, this);
        }));
        function _sendMessagesToHandlers(_x5, _x6, _x7) {
          return _sendMessagesToHandlers2.apply(this, arguments);
        }
        return _sendMessagesToHandlers;
      }()
    }, {
      key: "_init",
      value: function _init() {
        var _this3 = this;
        addMessageHandler("values", /* @__PURE__ */ function() {
          var _ref3 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee7(message) {
            var name, _key;
            return _regeneratorRuntime8().wrap(function _callee7$(_context7) {
              while (1)
                switch (_context7.prev = _context7.next) {
                  case 0:
                    for (name in _this3.$bindings) {
                      if (hasOwnProperty(_this3.$bindings, name))
                        _this3.$bindings[name].showProgress(false);
                    }
                    _context7.t0 = _regeneratorRuntime8().keys(message);
                  case 2:
                    if ((_context7.t1 = _context7.t0()).done) {
                      _context7.next = 9;
                      break;
                    }
                    _key = _context7.t1.value;
                    if (!hasOwnProperty(message, _key)) {
                      _context7.next = 7;
                      break;
                    }
                    _context7.next = 7;
                    return _this3.receiveOutput(_key, message[_key]);
                  case 7:
                    _context7.next = 2;
                    break;
                  case 9:
                  case "end":
                    return _context7.stop();
                }
            }, _callee7);
          }));
          return function(_x8) {
            return _ref3.apply(this, arguments);
          };
        }());
        addMessageHandler("errors", function(message) {
          for (var _key2 in message) {
            if (hasOwnProperty(message, _key2))
              _this3.receiveError(_key2, message[_key2]);
          }
        });
        addMessageHandler("inputMessages", function(message) {
          for (var i = 0; i < message.length; i++) {
            var $obj = (0, import_jquery38.default)(".shiny-bound-input#" + $escape(message[i].id));
            var inputBinding = $obj.data("shiny-input-binding");
            if ($obj.length > 0) {
              if (!$obj.attr("aria-live"))
                $obj.attr("aria-live", "polite");
              var el = $obj[0];
              var evt = import_jquery38.default.Event("shiny:updateinput");
              evt.message = message[i].message;
              evt.binding = inputBinding;
              (0, import_jquery38.default)(el).trigger(evt);
              if (!evt.isDefaultPrevented()) {
                try {
                  inputBinding.receiveMessage(el, evt.message);
                } catch (error) {
                  console.error("[shiny] Error in inputBinding.receiveMessage()", {
                    error: error,
                    binding: inputBinding,
                    message: evt.message
                  });
                }
              }
            }
          }
        });
        addMessageHandler("javascript", function(message) {
          indirectEval(message);
        });
        addMessageHandler("console", function(message) {
          for (var i = 0; i < message.length; i++) {
            if (console.log)
              console.log(message[i]);
          }
        });
        addMessageHandler("progress", /* @__PURE__ */ function() {
          var _ref4 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee8(message) {
            var handler;
            return _regeneratorRuntime8().wrap(function _callee8$(_context8) {
              while (1)
                switch (_context8.prev = _context8.next) {
                  case 0:
                    if (!(message.type && message.message)) {
                      _context8.next = 5;
                      break;
                    }
                    _context8.next = 3;
                    return _this3.progressHandlers[message.type];
                  case 3:
                    handler = _context8.sent;
                    if (handler)
                      handler.call(_this3, message.message);
                  case 5:
                  case "end":
                    return _context8.stop();
                }
            }, _callee8);
          }));
          return function(_x9) {
            return _ref4.apply(this, arguments);
          };
        }());
        addMessageHandler("notification", /* @__PURE__ */ function() {
          var _ref5 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee9(message) {
            return _regeneratorRuntime8().wrap(function _callee9$(_context9) {
              while (1)
                switch (_context9.prev = _context9.next) {
                  case 0:
                    if (!(message.type === "show")) {
                      _context9.next = 5;
                      break;
                    }
                    _context9.next = 3;
                    return show(message.message);
                  case 3:
                    _context9.next = 10;
                    break;
                  case 5:
                    if (!(message.type === "remove")) {
                      _context9.next = 9;
                      break;
                    }
                    remove(message.message);
                    _context9.next = 10;
                    break;
                  case 9:
                    throw "Unkown notification type: " + message.type;
                  case 10:
                  case "end":
                    return _context9.stop();
                }
            }, _callee9);
          }));
          return function(_x10) {
            return _ref5.apply(this, arguments);
          };
        }());
        addMessageHandler("modal", /* @__PURE__ */ function() {
          var _ref6 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee10(message) {
            return _regeneratorRuntime8().wrap(function _callee10$(_context10) {
              while (1)
                switch (_context10.prev = _context10.next) {
                  case 0:
                    if (!(message.type === "show")) {
                      _context10.next = 5;
                      break;
                    }
                    _context10.next = 3;
                    return show2(message.message);
                  case 3:
                    _context10.next = 10;
                    break;
                  case 5:
                    if (!(message.type === "remove")) {
                      _context10.next = 9;
                      break;
                    }
                    remove2();
                    _context10.next = 10;
                    break;
                  case 9:
                    throw "Unkown modal type: " + message.type;
                  case 10:
                  case "end":
                    return _context10.stop();
                }
            }, _callee10);
          }));
          return function(_x11) {
            return _ref6.apply(this, arguments);
          };
        }());
        addMessageHandler("response", function(message) {
          var requestId = message.tag;
          var request = _this3.$activeRequests[requestId];
          if (request) {
            delete _this3.$activeRequests[requestId];
            if ("value" in message)
              request.onSuccess(message.value);
            else
              request.onError(message.error);
          }
        });
        addMessageHandler("allowReconnect", function(message) {
          switch (message) {
            case true:
            case false:
            case "force":
              _this3.$allowReconnect = message;
              break;
            default:
              throw "Invalid value for allowReconnect: " + message;
          }
        });
        addMessageHandler("custom", function(message) {
          var shinyOnCustomMessage = getShinyOnCustomMessage();
          if (shinyOnCustomMessage)
            shinyOnCustomMessage(message);
          _this3._sendMessagesToHandlers(message, customMessageHandlers, customMessageHandlerOrder);
        });
        addMessageHandler("config", function(message) {
          _this3.config = {
            workerId: message.workerId,
            sessionId: message.sessionId
          };
          if (message.user)
            setShinyUser(message.user);
          (0, import_jquery38.default)(document).trigger("shiny:sessioninitialized");
        });
        addMessageHandler("busy", function(message) {
          if (message === "busy") {
            (0, import_jquery38.default)(document.documentElement).addClass("shiny-busy");
            (0, import_jquery38.default)(document).trigger("shiny:busy");
          } else if (message === "idle") {
            (0, import_jquery38.default)(document.documentElement).removeClass("shiny-busy");
            (0, import_jquery38.default)(document).trigger("shiny:idle");
          }
        });
        addMessageHandler("recalculating", function(message) {
          if (hasOwnProperty(message, "name") && hasOwnProperty(message, "status")) {
            var binding = _this3.$bindings[message.name];
            if (binding) {
              (0, import_jquery38.default)(binding.el).trigger("shiny:" + message.status);
            } else {
              (0, import_jquery38.default)().trigger("shiny:" + message.status);
            }
          }
        });
        addMessageHandler("reload", function(message) {
          window.location.reload();
          return;
          message;
        });
        addMessageHandler("shiny-insert-ui", /* @__PURE__ */ function() {
          var _ref7 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee11(message) {
            var targets, _iterator, _step, target;
            return _regeneratorRuntime8().wrap(function _callee11$(_context11) {
              while (1)
                switch (_context11.prev = _context11.next) {
                  case 0:
                    targets = (0, import_jquery38.default)(message.selector);
                    if (!(targets.length === 0)) {
                      _context11.next = 7;
                      break;
                    }
                    console.warn('The selector you chose ("' + message.selector + '") could not be found in the DOM.');
                    _context11.next = 5;
                    return renderHtmlAsync(message.content.html, (0, import_jquery38.default)([]), message.content.deps);
                  case 5:
                    _context11.next = 26;
                    break;
                  case 7:
                    _iterator = _createForOfIteratorHelper2(targets);
                    _context11.prev = 8;
                    _iterator.s();
                  case 10:
                    if ((_step = _iterator.n()).done) {
                      _context11.next = 18;
                      break;
                    }
                    target = _step.value;
                    _context11.next = 14;
                    return renderContentAsync(target, message.content, message.where);
                  case 14:
                    if (!(message.multiple === false)) {
                      _context11.next = 16;
                      break;
                    }
                    return _context11.abrupt("break", 18);
                  case 16:
                    _context11.next = 10;
                    break;
                  case 18:
                    _context11.next = 23;
                    break;
                  case 20:
                    _context11.prev = 20;
                    _context11.t0 = _context11["catch"](8);
                    _iterator.e(_context11.t0);
                  case 23:
                    _context11.prev = 23;
                    _iterator.f();
                    return _context11.finish(23);
                  case 26:
                  case "end":
                    return _context11.stop();
                }
            }, _callee11, null, [[8, 20, 23, 26]]);
          }));
          return function(_x12) {
            return _ref7.apply(this, arguments);
          };
        }());
        addMessageHandler("shiny-remove-ui", function(message) {
          var els = (0, import_jquery38.default)(message.selector);
          els.each(function(i, el) {
            shinyUnbindAll(el, true);
            (0, import_jquery38.default)(el).remove();
            return message.multiple === false ? false : void 0;
          });
        });
        addMessageHandler("frozen", function(message) {
          for (var i = 0; i < message.ids.length; i++) {
            shinyForgetLastInputValue(message.ids[i]);
          }
        });
        function getTabset(id) {
          var $tabset = (0, import_jquery38.default)("#" + $escape(id));
          if ($tabset.length === 0)
            throw "There is no tabsetPanel (or navbarPage or navlistPanel) with id equal to '" + id + "'";
          return $tabset;
        }
        function getTabContent($tabset) {
          var tabsetId = $tabset.attr("data-tabsetid");
          var $tabContent = (0, import_jquery38.default)("div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']");
          return $tabContent;
        }
        function getTargetTabs($tabset, $tabContent, target) {
          var dataValue = "[data-value='" + $escape(target) + "']";
          var $aTag = $tabset.find("a" + dataValue);
          var $liTag = $aTag.parent();
          if ($liTag.length === 0) {
            throw "There is no tabPanel (or navbarMenu) with value (or menuName) equal to '" + target + "'";
          }
          var $liTags = [];
          var $divTags = [];
          if ($aTag.attr("data-toggle") === "dropdown") {
            var $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
            var dropdownId = $dropdownTabset.attr("data-tabsetid");
            var $dropdownLiTags = $dropdownTabset.find("a[data-toggle='tab']").parent("li");
            $dropdownLiTags.each(function(i, el) {
              $liTags.push((0, import_jquery38.default)(el));
            });
            var selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
            var $dropdownDivs = $tabContent.find(selector);
            $dropdownDivs.each(function(i, el) {
              $divTags.push((0, import_jquery38.default)(el));
            });
          } else {
            $divTags.push($tabContent.find("div" + dataValue));
          }
          return {
            $liTag: $liTag,
            $liTags: $liTags,
            $divTags: $divTags
          };
        }
        addMessageHandler("shiny-insert-tab", /* @__PURE__ */ function() {
          var _ref8 = _asyncToGenerator8(/* @__PURE__ */ _regeneratorRuntime8().mark(function _callee12(message) {
            var $parentTabset, $tabset, $tabContent, tabsetId, $divTag, $liTag, $aTag, $targetLiTag, targetInfo, dropdown, index, tabId, _iterator2, _step2, el, getTabIndex, getDropdown;
            return _regeneratorRuntime8().wrap(function _callee12$(_context12) {
              while (1)
                switch (_context12.prev = _context12.next) {
                  case 0:
                    getDropdown = function _getDropdown() {
                      if (message.menuName !== null) {
                        var $dropdownATag = (0, import_jquery38.default)("a.dropdown-toggle[data-value='" + $escape(message.menuName) + "']");
                        if ($dropdownATag.length === 0) {
                          throw "There is no navbarMenu with menuName equal to '" + message.menuName + "'";
                        }
                        var $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
                        var dropdownId = $dropdownTabset.attr("data-tabsetid");
                        return {
                          $tabset: $dropdownTabset,
                          id: dropdownId
                        };
                      } else if (message.target !== null && $targetLiTag !== null) {
                        var $uncleTabset = $targetLiTag.parent("ul");
                        if ($uncleTabset.hasClass("dropdown-menu")) {
                          var uncleId = $uncleTabset.attr("data-tabsetid");
                          return {
                            $tabset: $uncleTabset,
                            id: uncleId
                          };
                        }
                      }
                      return null;
                    };
                    getTabIndex = function _getTabIndex($tabset2, tabsetId2) {
                      var existingTabIds = [0];
                      $tabset2.find("> li").each(function() {
                        var $tab = (0, import_jquery38.default)(this).find("> a[data-toggle='tab']");
                        if ($tab.length > 0) {
                          var href = $tab.attr("href").replace(/.*(?=#[^\s]+$)/, "");
                          var _index = href.replace("#tab-" + tabsetId2 + "-", "");
                          existingTabIds.push(Number(_index));
                        }
                      });
                      return Math.max.apply(null, existingTabIds) + 1;
                    };
                    $parentTabset = getTabset(message.inputId);
                    $tabset = $parentTabset;
                    $tabContent = getTabContent($tabset);
                    tabsetId = $parentTabset.attr("data-tabsetid");
                    $divTag = (0, import_jquery38.default)(message.divTag.html);
                    $liTag = (0, import_jquery38.default)(message.liTag.html);
                    $aTag = $liTag.find("> a");
                    $targetLiTag = null;
                    if (message.target !== null) {
                      targetInfo = getTargetTabs($tabset, $tabContent, message.target);
                      $targetLiTag = targetInfo.$liTag;
                    }
                    dropdown = getDropdown();
                    if (!(dropdown !== null)) {
                      _context12.next = 18;
                      break;
                    }
                    if (!($aTag.attr("data-toggle") === "dropdown")) {
                      _context12.next = 15;
                      break;
                    }
                    throw "Cannot insert a navbarMenu inside another one";
                  case 15:
                    $tabset = dropdown.$tabset;
                    tabsetId = dropdown.id;
                    $liTag.removeClass("nav-item").find(".nav-link").removeClass("nav-link").addClass("dropdown-item");
                  case 18:
                    if ($aTag.attr("data-toggle") === "tab") {
                      index = getTabIndex($tabset, tabsetId);
                      tabId = "tab-" + tabsetId + "-" + index;
                      $liTag.find("> a").attr("href", "#" + tabId);
                      $divTag.attr("id", tabId);
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
                    _context12.next = 22;
                    return renderContentAsync($liTag[0], {
                      html: $liTag.html(),
                      deps: message.liTag.deps
                    });
                  case 22:
                    _context12.next = 24;
                    return renderContentAsync(
                      $tabContent[0],
                      {
                        html: "",
                        deps: message.divTag.deps
                      },
                      "beforeend"
                    );
                  case 24:
                    _iterator2 = _createForOfIteratorHelper2($divTag.get());
                    _context12.prev = 25;
                    _iterator2.s();
                  case 27:
                    if ((_step2 = _iterator2.n()).done) {
                      _context12.next = 34;
                      break;
                    }
                    el = _step2.value;
                    $tabContent[0].appendChild(el);
                    _context12.next = 32;
                    return renderContentAsync(el, el.innerHTML || el.textContent);
                  case 32:
                    _context12.next = 27;
                    break;
                  case 34:
                    _context12.next = 39;
                    break;
                  case 36:
                    _context12.prev = 36;
                    _context12.t0 = _context12["catch"](25);
                    _iterator2.e(_context12.t0);
                  case 39:
                    _context12.prev = 39;
                    _iterator2.f();
                    return _context12.finish(39);
                  case 42:
                    if (message.select) {
                      $liTag.find("a").tab("show");
                    }
                  case 43:
                  case "end":
                    return _context12.stop();
                }
            }, _callee12, null, [[25, 36, 39, 42]]);
          }));
          return function(_x13) {
            return _ref8.apply(this, arguments);
          };
        }());
        function ensureTabsetHasVisibleTab($tabset) {
          var inputBinding = $tabset.data("shiny-input-binding");
          if (!inputBinding.getValue($tabset)) {
            var destTabValue = getFirstTab($tabset);
            var evt = import_jquery38.default.Event("shiny:updateinput");
            evt.binding = inputBinding;
            $tabset.trigger(evt);
            inputBinding.setValue($tabset[0], destTabValue);
          }
        }
        function getFirstTab($ul) {
          return $ul.find("li:visible a[data-toggle='tab']").first().attr("data-value") || null;
        }
        function tabApplyFunction(target, func) {
          var liTags = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : false;
          import_jquery38.default.each(target, function(key, el) {
            if (key === "$liTag") {
              func(el);
            } else if (key === "$divTags") {
              import_jquery38.default.each(el, function(i, div) {
                func(div);
              });
            } else if (liTags && key === "$liTags") {
              import_jquery38.default.each(el, function(i, div) {
                func(div);
              });
            }
          });
        }
        addMessageHandler("shiny-remove-tab", function(message) {
          var $tabset = getTabset(message.inputId);
          var $tabContent = getTabContent($tabset);
          var target = getTargetTabs($tabset, $tabContent, message.target);
          tabApplyFunction(target, removeEl);
          ensureTabsetHasVisibleTab($tabset);
          function removeEl($el) {
            shinyUnbindAll($el, true);
            $el.remove();
          }
        });
        addMessageHandler("shiny-change-tab-visibility", function(message) {
          var $tabset = getTabset(message.inputId);
          var $tabContent = getTabContent($tabset);
          var target = getTargetTabs($tabset, $tabContent, message.target);
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
        });
        addMessageHandler("updateQueryString", function(message) {
          if (message.mode === "replace") {
            window.history.replaceState(null, null, message.queryString);
            return;
          }
          var what = null;
          if (message.queryString.charAt(0) === "#")
            what = "hash";
          else if (message.queryString.charAt(0) === "?")
            what = "query";
          else
            throw "The 'query' string must start with either '?' (to update the query string) or with '#' (to update the hash).";
          var path2 = window.location.pathname;
          var oldQS = window.location.search;
          var oldHash = window.location.hash;
          var relURL = path2;
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
        });
        addMessageHandler("resetBrush", function(message) {
          resetBrush(message.brushId);
        });
      }
    }, {
      key: "getTestSnapshotBaseUrl",
      value: function getTestSnapshotBaseUrl() {
        var _ref9 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref9$fullUrl = _ref9.fullUrl, fullUrl = _ref9$fullUrl === void 0 ? true : _ref9$fullUrl;
        var loc = window.location;
        var url = "";
        if (fullUrl) {
          url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
        }
        url += "/session/" + encodeURIComponent(this.config.sessionId) + "/dataobj/shinytest?w=" + encodeURIComponent(this.config.workerId) + "&nonce=" + randomId();
        return url;
      }
    }]);
    return ShinyApp2;
  }();

  // srcts/src/shiny/init.ts
  function initShiny(windowShiny3) {
    setShinyObj(windowShiny3);
    var shinyapp = windowShiny3.shinyapp = new ShinyApp();
    windowShiny3.progressHandlers = shinyapp.progressHandlers;
    var inputBatchSender = new InputBatchSender(shinyapp);
    var inputsNoResend = new InputNoResendDecorator(inputBatchSender);
    var inputsEvent = new InputEventDecorator(inputsNoResend);
    var inputsRate = new InputRateDecorator(inputsEvent);
    var inputsDefer = new InputDeferDecorator(inputsEvent);
    var target;
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
    var inputs = new InputValidateDecorator(target);
    windowShiny3.setInputValue = windowShiny3.onInputChange = function(name, value) {
      var opts = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : {};
      var newOpts = addDefaultInputOpts(opts);
      inputs.setInput(name, value, newOpts);
    };
    windowShiny3.forgetLastInputValue = function(name) {
      inputsNoResend.forget(name);
    };
    var inputBindings = windowShiny3.inputBindings;
    var outputBindings = windowShiny3.outputBindings;
    function shinyBindCtx() {
      return {
        inputs: inputs,
        inputsRate: inputsRate,
        sendOutputHiddenState: sendOutputHiddenState,
        maybeAddThemeObserver: maybeAddThemeObserver,
        inputBindings: inputBindings,
        outputBindings: outputBindings,
        initDeferredIframes: initDeferredIframes
      };
    }
    windowShiny3.bindAll = function(scope) {
      bindAll(shinyBindCtx(), scope);
    };
    windowShiny3.unbindAll = function(scope) {
      var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
      unbindAll(shinyBindCtx(), scope, includeSelf);
    };
    function initializeInputs() {
      var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document.documentElement;
      var bindings = inputBindings.getBindings();
      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var inputObjects = binding.find(scope);
        if (inputObjects) {
          for (var j = 0; j < inputObjects.length; j++) {
            var $inputObjectJ = (0, import_jquery39.default)(inputObjects[j]);
            if (!$inputObjectJ.data("_shiny_initialized")) {
              $inputObjectJ.data("_shiny_initialized", true);
              binding.initialize(inputObjects[j]);
            }
          }
        }
      }
    }
    windowShiny3.initializeInputs = initializeInputs;
    function getIdFromEl(el) {
      var $el = (0, import_jquery39.default)(el);
      var bindingAdapter = $el.data("shiny-output-binding");
      if (!bindingAdapter)
        return null;
      else
        return bindingAdapter.getId();
    }
    initializeInputs(document.documentElement);
    var initialValues = mapValues(_bindAll(shinyBindCtx(), document.documentElement), function(x) {
      return x.value;
    });
    (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
      var id = getIdFromEl(this), rect = this.getBoundingClientRect();
      if (rect.width !== 0 || rect.height !== 0) {
        initialValues[".clientdata_output_" + id + "_width"] = rect.width;
        initialValues[".clientdata_output_" + id + "_height"] = rect.height;
      }
    });
    function getComputedBgColor(el) {
      if (!el) {
        return null;
      }
      var bgColor = getStyle(el, "background-color");
      if (!bgColor)
        return bgColor;
      var m = bgColor.match(/^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/);
      if (bgColor === "transparent" || m && parseFloat(m[4]) === 0) {
        var bgImage = getStyle(el, "background-image");
        if (bgImage && bgImage !== "none") {
          return null;
        } else {
          return getComputedBgColor(el.parentElement);
        }
      }
      return bgColor;
    }
    function getComputedFont(el) {
      var fontFamily = getStyle(el, "font-family");
      var fontSize = getStyle(el, "font-size");
      return {
        families: fontFamily === null || fontFamily === void 0 ? void 0 : fontFamily.replace(/"/g, "").split(", "),
        size: fontSize
      };
    }
    (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
      var el = this;
      var id = getIdFromEl(el);
      initialValues[".clientdata_output_" + id + "_bg"] = getComputedBgColor(el);
      initialValues[".clientdata_output_" + id + "_fg"] = getStyle(el, "color");
      initialValues[".clientdata_output_" + id + "_accent"] = getComputedLinkColor(el);
      initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(el);
      maybeAddThemeObserver(el);
    });
    function maybeAddThemeObserver(el) {
      if (!window.MutationObserver) {
        return;
      }
      var cl = el.classList;
      var reportTheme = cl.contains("shiny-image-output") || cl.contains("shiny-plot-output") || cl.contains("shiny-report-theme");
      if (!reportTheme) {
        return;
      }
      var $el = (0, import_jquery39.default)(el);
      if ($el.data("shiny-theme-observer")) {
        return;
      }
      var observerCallback = new Debouncer(null, function() {
        return doSendTheme(el);
      }, 100);
      var observer = new MutationObserver(function() {
        return observerCallback.normalCall();
      });
      var config = {
        attributes: true,
        attributeFilter: ["style", "class"]
      };
      observer.observe(el, config);
      $el.data("shiny-theme-observer", observer);
    }
    function doSendTheme(el) {
      if (el.classList.contains("shiny-output-error")) {
        return;
      }
      var id = getIdFromEl(el);
      inputs.setInput(".clientdata_output_" + id + "_bg", getComputedBgColor(el));
      inputs.setInput(".clientdata_output_" + id + "_fg", getStyle(el, "color"));
      inputs.setInput(".clientdata_output_" + id + "_accent", getComputedLinkColor(el));
      inputs.setInput(".clientdata_output_" + id + "_font", getComputedFont(el));
    }
    function doSendImageSize() {
      (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
        var id = getIdFromEl(this), rect = this.getBoundingClientRect();
        if (rect.width !== 0 || rect.height !== 0) {
          inputs.setInput(".clientdata_output_" + id + "_width", rect.width);
          inputs.setInput(".clientdata_output_" + id + "_height", rect.height);
        }
      });
      (0, import_jquery39.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
        doSendTheme(this);
      });
      (0, import_jquery39.default)(".shiny-bound-output").each(function() {
        var $this = (0, import_jquery39.default)(this), binding = $this.data("shiny-output-binding");
        $this.trigger({
          type: "shiny:visualchange",
          visible: !isHidden(this),
          binding: binding
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
    var lastKnownVisibleOutputs = {};
    (0, import_jquery39.default)(".shiny-bound-output").each(function() {
      var id = getIdFromEl(this);
      if (isHidden(this)) {
        initialValues[".clientdata_output_" + id + "_hidden"] = true;
      } else {
        lastKnownVisibleOutputs[id] = true;
        initialValues[".clientdata_output_" + id + "_hidden"] = false;
      }
    });
    function doSendOutputHiddenState() {
      var visibleOutputs = {};
      (0, import_jquery39.default)(".shiny-bound-output").each(function() {
        var id = getIdFromEl(this);
        delete lastKnownVisibleOutputs[id];
        var hidden = isHidden(this), evt = {
          type: "shiny:visualchange",
          visible: !hidden
        };
        if (hidden) {
          inputs.setInput(".clientdata_output_" + id + "_hidden", true);
        } else {
          visibleOutputs[id] = true;
          inputs.setInput(".clientdata_output_" + id + "_hidden", false);
        }
        var $this = (0, import_jquery39.default)(this);
        evt.binding = $this.data("shiny-output-binding");
        $this.trigger(evt);
      });
      for (var name in lastKnownVisibleOutputs) {
        if (hasDefinedProperty(lastKnownVisibleOutputs, name))
          inputs.setInput(".clientdata_output_" + name + "_hidden", true);
      }
      lastKnownVisibleOutputs = visibleOutputs;
    }
    var sendOutputHiddenStateDebouncer = new Debouncer(null, doSendOutputHiddenState, 0);
    function sendOutputHiddenState() {
      sendOutputHiddenStateDebouncer.normalCall();
    }
    inputBatchSender.lastChanceCallback.push(function() {
      if (sendOutputHiddenStateDebouncer.isPending())
        sendOutputHiddenStateDebouncer.immediateCall();
    });
    function filterEventsByNamespace(namespace, handler) {
      for (var _len = arguments.length, args = new Array(_len > 2 ? _len - 2 : 0), _key = 2; _key < _len; _key++) {
        args[_key - 2] = arguments[_key];
      }
      var namespaceArr = namespace.split(".");
      return function(e) {
        var _e$namespace$split, _e$namespace;
        var eventNamespace = (_e$namespace$split = (_e$namespace = e.namespace) === null || _e$namespace === void 0 ? void 0 : _e$namespace.split(".")) !== null && _e$namespace$split !== void 0 ? _e$namespace$split : [];
        for (var i = 0; i < namespaceArr.length; i++) {
          if (eventNamespace.indexOf(namespaceArr[i]) === -1)
            return;
        }
        handler.apply(this, [namespaceArr, handler].concat(args));
      };
    }
    (0, import_jquery39.default)(window).resize(debounce(500, sendImageSizeFns.regular));
    var bs3classes = ["modal", "dropdown", "tab", "tooltip", "popover", "collapse"];
    import_jquery39.default.each(bs3classes, function(idx, classname) {
      (0, import_jquery39.default)(document.body).on("shown.bs." + classname + ".sendImageSize", "*", filterEventsByNamespace("bs", sendImageSizeFns.regular));
      (0, import_jquery39.default)(document.body).on("shown.bs." + classname + ".sendOutputHiddenState hidden.bs." + classname + ".sendOutputHiddenState", "*", filterEventsByNamespace("bs", sendOutputHiddenState));
    });
    (0, import_jquery39.default)(document.body).on("shown.sendImageSize", "*", sendImageSizeFns.regular);
    (0, import_jquery39.default)(document.body).on("shown.sendOutputHiddenState hidden.sendOutputHiddenState", "*", sendOutputHiddenState);
    initialValues[".clientdata_pixelratio"] = pixelRatio();
    (0, import_jquery39.default)(window).resize(function() {
      inputs.setInput(".clientdata_pixelratio", pixelRatio());
    });
    initialValues[".clientdata_url_protocol"] = window.location.protocol;
    initialValues[".clientdata_url_hostname"] = window.location.hostname;
    initialValues[".clientdata_url_port"] = window.location.port;
    initialValues[".clientdata_url_pathname"] = window.location.pathname;
    initialValues[".clientdata_url_search"] = window.location.search;
    (0, import_jquery39.default)(window).on("pushstate", function(e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
      return;
      e;
    });
    (0, import_jquery39.default)(window).on("popstate", function(e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
      return;
      e;
    });
    initialValues[".clientdata_url_hash_initial"] = window.location.hash;
    initialValues[".clientdata_url_hash"] = window.location.hash;
    (0, import_jquery39.default)(window).on("hashchange", function(e) {
      inputs.setInput(".clientdata_url_hash", window.location.hash);
      return;
      e;
    });
    var singletonText = initialValues[".clientdata_singletons"] = (0, import_jquery39.default)('script[type="application/shiny-singletons"]').text();
    registerNames(singletonText.split(/,/));
    var dependencyText = (0, import_jquery39.default)('script[type="application/html-dependencies"]').text();
    import_jquery39.default.each(dependencyText.split(/;/), function(i, depStr) {
      var match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
      if (match) {
        registerDependency(match[1], match[2]);
      }
    });
    inputsNoResend.reset(initialValues);
    shinyapp.connect(initialValues);
    (0, import_jquery39.default)(document).one("shiny:connected", function() {
      initDeferredIframes();
    });
  }
  function initDeferredIframes() {
    if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
      return;
    }
    (0, import_jquery39.default)(".shiny-frame-deferred").each(function(i, el) {
      var $el = (0, import_jquery39.default)(el);
      $el.removeClass("shiny-frame-deferred");
      $el.attr("src", $el.attr("data-deferred-src"));
      $el.attr("data-deferred-src", null);
    });
  }

  // srcts/src/shiny/index.ts
  var windowShiny2;
  function setShiny(windowShiny_) {
    windowShiny2 = windowShiny_;
    windowShiny2.version = "1.7.4.9002";
    var _initInputBindings = initInputBindings(), inputBindings = _initInputBindings.inputBindings, fileInputBinding2 = _initInputBindings.fileInputBinding;
    var _initOutputBindings = initOutputBindings(), outputBindings = _initOutputBindings.outputBindings;
    setFileInputBinding(fileInputBinding2);
    windowShiny2.$escape = $escape;
    windowShiny2.compareVersion = compareVersion;
    windowShiny2.inputBindings = inputBindings;
    windowShiny2.InputBinding = InputBinding;
    windowShiny2.outputBindings = outputBindings;
    windowShiny2.OutputBinding = OutputBinding;
    windowShiny2.resetBrush = resetBrush;
    windowShiny2.notifications = {
      show: show,
      remove: remove
    };
    windowShiny2.modal = {
      show: show2,
      remove: remove2
    };
    windowShiny2.addCustomMessageHandler = addCustomMessageHandler;
    windowShiny2.showReconnectDialog = showReconnectDialog;
    windowShiny2.hideReconnectDialog = hideReconnectDialog;
    windowShiny2.renderDependenciesAsync = renderDependenciesAsync;
    windowShiny2.renderDependencies = renderDependencies;
    windowShiny2.renderContentAsync = renderContentAsync;
    windowShiny2.renderContent = renderContent;
    windowShiny2.renderHtmlAsync = renderHtmlAsync;
    windowShiny2.renderHtml = renderHtml2;
    (0, import_jquery40.default)(function() {
      setTimeout(function() {
        initShiny(windowShiny2);
      }, 1);
    });
  }

  // srcts/src/window/userAgent.ts
  function windowUserAgent() {
    return window.navigator.userAgent;
  }

  // srcts/src/shiny/reactlog.ts
  var import_jquery41 = __toESM(require_jquery());
  function shinyAppConfig() {
    return shinyShinyApp().config;
  }
  function initReactlog() {
    (0, import_jquery41.default)(document).on("keydown", function(e) {
      if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey)
        return;
      var url = "reactlog?w=" + window.escape(shinyAppConfig().workerId) + "&s=" + window.escape(shinyAppConfig().sessionId);
      window.open(url);
      e.preventDefault();
    });
    (0, import_jquery41.default)(document).on("keydown", function(e) {
      if (!(e.which === 115 && (e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey || e.which === 114 && (e.ctrlKey || e.metaKey) && e.shiftKey && !e.altKey)) {
        return;
      }
      var url = "reactlog/mark?w=" + window.escape(shinyAppConfig().workerId) + "&s=" + window.escape(shinyAppConfig().sessionId);
      import_jquery41.default.get(url, function(result) {
        if (result !== "marked")
          return;
        var html = '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';
        show({
          html: html,
          closeButton: true
        });
      }).fail(function() {
        window.open(url);
      });
      e.preventDefault();
    });
  }

  // srcts/src/initialize/index.ts
  function init() {
    setShiny(windowShiny());
    setUserAgent(windowUserAgent());
    determineBrowserInfo();
    trackHistory();
    disableFormSubmission();
    initReactlog();
  }

  // srcts/src/index.ts
  init();
})();
/*! regenerator-runtime -- Copyright (c) 2014-present, Facebook, Inc. -- license (MIT): https://github.com/facebook/regenerator/blob/main/LICENSE */
//# sourceMappingURL=shiny.js.map
