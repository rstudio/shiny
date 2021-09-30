/*! shiny 1.7.0 | (c) 2012-2021 RStudio, PBC. | License: GPL-3 | file LICENSE */
(function() {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __markAsModule = function(target) {
    return __defProp(target, "__esModule", { value: true });
  };
  var __commonJS = function(cb, mod) {
    return function __require() {
      return mod || (0, cb[Object.keys(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
    };
  };
  var __reExport = function(target, module, desc) {
    if (module && typeof module === "object" || typeof module === "function")
      for (var keys2 = __getOwnPropNames(module), i = 0, n = keys2.length, key; i < n; i++) {
        key = keys2[i];
        if (!__hasOwnProp.call(target, key) && key !== "default")
          __defProp(target, key, { get: function(k) {
            return module[k];
          }.bind(null, key), enumerable: !(desc = __getOwnPropDesc(module, key)) || desc.enumerable });
      }
    return target;
  };
  var __toModule = function(module) {
    return __reExport(__markAsModule(__defProp(module != null ? __create(__getProtoOf(module)) : {}, "default", module && module.__esModule && "default" in module ? { get: function() {
      return module.default;
    }, enumerable: true } : { value: module, enumerable: true })), module);
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
      module.exports = function(exec) {
        try {
          return !!exec();
        } catch (error) {
          return true;
        }
      };
    }
  });

  // node_modules/core-js/internals/descriptors.js
  var require_descriptors = __commonJS({
    "node_modules/core-js/internals/descriptors.js": function(exports, module) {
      var fails11 = require_fails();
      module.exports = !fails11(function() {
        return Object.defineProperty({}, 1, { get: function() {
          return 7;
        } })[1] != 7;
      });
    }
  });

  // node_modules/core-js/internals/object-property-is-enumerable.js
  var require_object_property_is_enumerable = __commonJS({
    "node_modules/core-js/internals/object-property-is-enumerable.js": function(exports) {
      "use strict";
      var $propertyIsEnumerable2 = {}.propertyIsEnumerable;
      var getOwnPropertyDescriptor4 = Object.getOwnPropertyDescriptor;
      var NASHORN_BUG = getOwnPropertyDescriptor4 && !$propertyIsEnumerable2.call({ 1: 2 }, 1);
      exports.f = NASHORN_BUG ? function propertyIsEnumerable2(V) {
        var descriptor = getOwnPropertyDescriptor4(this, V);
        return !!descriptor && descriptor.enumerable;
      } : $propertyIsEnumerable2;
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

  // node_modules/core-js/internals/classof-raw.js
  var require_classof_raw = __commonJS({
    "node_modules/core-js/internals/classof-raw.js": function(exports, module) {
      var toString2 = {}.toString;
      module.exports = function(it) {
        return toString2.call(it).slice(8, -1);
      };
    }
  });

  // node_modules/core-js/internals/indexed-object.js
  var require_indexed_object = __commonJS({
    "node_modules/core-js/internals/indexed-object.js": function(exports, module) {
      var fails11 = require_fails();
      var classof2 = require_classof_raw();
      var split = "".split;
      module.exports = fails11(function() {
        return !Object("z").propertyIsEnumerable(0);
      }) ? function(it) {
        return classof2(it) == "String" ? split.call(it, "") : Object(it);
      } : Object;
    }
  });

  // node_modules/core-js/internals/require-object-coercible.js
  var require_require_object_coercible = __commonJS({
    "node_modules/core-js/internals/require-object-coercible.js": function(exports, module) {
      module.exports = function(it) {
        if (it == void 0)
          throw TypeError("Can't call method on " + it);
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

  // node_modules/core-js/internals/is-object.js
  var require_is_object = __commonJS({
    "node_modules/core-js/internals/is-object.js": function(exports, module) {
      module.exports = function(it) {
        return typeof it === "object" ? it !== null : typeof it === "function";
      };
    }
  });

  // node_modules/core-js/internals/to-primitive.js
  var require_to_primitive = __commonJS({
    "node_modules/core-js/internals/to-primitive.js": function(exports, module) {
      var isObject7 = require_is_object();
      module.exports = function(input, PREFERRED_STRING) {
        if (!isObject7(input))
          return input;
        var fn, val;
        if (PREFERRED_STRING && typeof (fn = input.toString) == "function" && !isObject7(val = fn.call(input)))
          return val;
        if (typeof (fn = input.valueOf) == "function" && !isObject7(val = fn.call(input)))
          return val;
        if (!PREFERRED_STRING && typeof (fn = input.toString) == "function" && !isObject7(val = fn.call(input)))
          return val;
        throw TypeError("Can't convert object to primitive value");
      };
    }
  });

  // node_modules/core-js/internals/to-object.js
  var require_to_object = __commonJS({
    "node_modules/core-js/internals/to-object.js": function(exports, module) {
      var requireObjectCoercible5 = require_require_object_coercible();
      module.exports = function(argument) {
        return Object(requireObjectCoercible5(argument));
      };
    }
  });

  // node_modules/core-js/internals/has.js
  var require_has = __commonJS({
    "node_modules/core-js/internals/has.js": function(exports, module) {
      var toObject6 = require_to_object();
      var hasOwnProperty2 = {}.hasOwnProperty;
      module.exports = Object.hasOwn || function hasOwn(it, key) {
        return hasOwnProperty2.call(toObject6(it), key);
      };
    }
  });

  // node_modules/core-js/internals/document-create-element.js
  var require_document_create_element = __commonJS({
    "node_modules/core-js/internals/document-create-element.js": function(exports, module) {
      var global8 = require_global();
      var isObject7 = require_is_object();
      var document2 = global8.document;
      var EXISTS = isObject7(document2) && isObject7(document2.createElement);
      module.exports = function(it) {
        return EXISTS ? document2.createElement(it) : {};
      };
    }
  });

  // node_modules/core-js/internals/ie8-dom-define.js
  var require_ie8_dom_define = __commonJS({
    "node_modules/core-js/internals/ie8-dom-define.js": function(exports, module) {
      var DESCRIPTORS6 = require_descriptors();
      var fails11 = require_fails();
      var createElement = require_document_create_element();
      module.exports = !DESCRIPTORS6 && !fails11(function() {
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
      var DESCRIPTORS6 = require_descriptors();
      var propertyIsEnumerableModule2 = require_object_property_is_enumerable();
      var createPropertyDescriptor2 = require_create_property_descriptor();
      var toIndexedObject5 = require_to_indexed_object();
      var toPrimitive3 = require_to_primitive();
      var has5 = require_has();
      var IE8_DOM_DEFINE = require_ie8_dom_define();
      var $getOwnPropertyDescriptor2 = Object.getOwnPropertyDescriptor;
      exports.f = DESCRIPTORS6 ? $getOwnPropertyDescriptor2 : function getOwnPropertyDescriptor4(O, P) {
        O = toIndexedObject5(O);
        P = toPrimitive3(P, true);
        if (IE8_DOM_DEFINE)
          try {
            return $getOwnPropertyDescriptor2(O, P);
          } catch (error) {
          }
        if (has5(O, P))
          return createPropertyDescriptor2(!propertyIsEnumerableModule2.f.call(O, P), O[P]);
      };
    }
  });

  // node_modules/core-js/internals/an-object.js
  var require_an_object = __commonJS({
    "node_modules/core-js/internals/an-object.js": function(exports, module) {
      var isObject7 = require_is_object();
      module.exports = function(it) {
        if (!isObject7(it)) {
          throw TypeError(String(it) + " is not an object");
        }
        return it;
      };
    }
  });

  // node_modules/core-js/internals/object-define-property.js
  var require_object_define_property = __commonJS({
    "node_modules/core-js/internals/object-define-property.js": function(exports) {
      var DESCRIPTORS6 = require_descriptors();
      var IE8_DOM_DEFINE = require_ie8_dom_define();
      var anObject10 = require_an_object();
      var toPrimitive3 = require_to_primitive();
      var $defineProperty2 = Object.defineProperty;
      exports.f = DESCRIPTORS6 ? $defineProperty2 : function defineProperty5(O, P, Attributes) {
        anObject10(O);
        P = toPrimitive3(P, true);
        anObject10(Attributes);
        if (IE8_DOM_DEFINE)
          try {
            return $defineProperty2(O, P, Attributes);
          } catch (error) {
          }
        if ("get" in Attributes || "set" in Attributes)
          throw TypeError("Accessors not supported");
        if ("value" in Attributes)
          O[P] = Attributes.value;
        return O;
      };
    }
  });

  // node_modules/core-js/internals/create-non-enumerable-property.js
  var require_create_non_enumerable_property = __commonJS({
    "node_modules/core-js/internals/create-non-enumerable-property.js": function(exports, module) {
      var DESCRIPTORS6 = require_descriptors();
      var definePropertyModule2 = require_object_define_property();
      var createPropertyDescriptor2 = require_create_property_descriptor();
      module.exports = DESCRIPTORS6 ? function(object, key, value) {
        return definePropertyModule2.f(object, key, createPropertyDescriptor2(1, value));
      } : function(object, key, value) {
        object[key] = value;
        return object;
      };
    }
  });

  // node_modules/core-js/internals/set-global.js
  var require_set_global = __commonJS({
    "node_modules/core-js/internals/set-global.js": function(exports, module) {
      var global8 = require_global();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      module.exports = function(key, value) {
        try {
          createNonEnumerableProperty4(global8, key, value);
        } catch (error) {
          global8[key] = value;
        }
        return value;
      };
    }
  });

  // node_modules/core-js/internals/shared-store.js
  var require_shared_store = __commonJS({
    "node_modules/core-js/internals/shared-store.js": function(exports, module) {
      var global8 = require_global();
      var setGlobal = require_set_global();
      var SHARED = "__core-js_shared__";
      var store = global8[SHARED] || setGlobal(SHARED, {});
      module.exports = store;
    }
  });

  // node_modules/core-js/internals/inspect-source.js
  var require_inspect_source = __commonJS({
    "node_modules/core-js/internals/inspect-source.js": function(exports, module) {
      var store = require_shared_store();
      var functionToString = Function.toString;
      if (typeof store.inspectSource != "function") {
        store.inspectSource = function(it) {
          return functionToString.call(it);
        };
      }
      module.exports = store.inspectSource;
    }
  });

  // node_modules/core-js/internals/native-weak-map.js
  var require_native_weak_map = __commonJS({
    "node_modules/core-js/internals/native-weak-map.js": function(exports, module) {
      var global8 = require_global();
      var inspectSource = require_inspect_source();
      var WeakMap = global8.WeakMap;
      module.exports = typeof WeakMap === "function" && /native code/.test(inspectSource(WeakMap));
    }
  });

  // node_modules/core-js/internals/is-pure.js
  var require_is_pure = __commonJS({
    "node_modules/core-js/internals/is-pure.js": function(exports, module) {
      module.exports = false;
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
        version: "3.14.0",
        mode: IS_PURE2 ? "pure" : "global",
        copyright: "\xA9 2021 Denis Pushkarev (zloirock.ru)"
      });
    }
  });

  // node_modules/core-js/internals/uid.js
  var require_uid = __commonJS({
    "node_modules/core-js/internals/uid.js": function(exports, module) {
      var id = 0;
      var postfix = Math.random();
      module.exports = function(key) {
        return "Symbol(" + String(key === void 0 ? "" : key) + ")_" + (++id + postfix).toString(36);
      };
    }
  });

  // node_modules/core-js/internals/shared-key.js
  var require_shared_key = __commonJS({
    "node_modules/core-js/internals/shared-key.js": function(exports, module) {
      var shared2 = require_shared();
      var uid2 = require_uid();
      var keys2 = shared2("keys");
      module.exports = function(key) {
        return keys2[key] || (keys2[key] = uid2(key));
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
      var NATIVE_WEAK_MAP = require_native_weak_map();
      var global8 = require_global();
      var isObject7 = require_is_object();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var objectHas = require_has();
      var shared2 = require_shared_store();
      var sharedKey2 = require_shared_key();
      var hiddenKeys2 = require_hidden_keys();
      var OBJECT_ALREADY_INITIALIZED = "Object already initialized";
      var WeakMap = global8.WeakMap;
      var set;
      var get3;
      var has5;
      var enforce = function(it) {
        return has5(it) ? get3(it) : set(it, {});
      };
      var getterFor = function(TYPE) {
        return function(it) {
          var state;
          if (!isObject7(it) || (state = get3(it)).type !== TYPE) {
            throw TypeError("Incompatible receiver, " + TYPE + " required");
          }
          return state;
        };
      };
      if (NATIVE_WEAK_MAP || shared2.state) {
        store = shared2.state || (shared2.state = new WeakMap());
        wmget = store.get;
        wmhas = store.has;
        wmset = store.set;
        set = function(it, metadata) {
          if (wmhas.call(store, it))
            throw new TypeError(OBJECT_ALREADY_INITIALIZED);
          metadata.facade = it;
          wmset.call(store, it, metadata);
          return metadata;
        };
        get3 = function(it) {
          return wmget.call(store, it) || {};
        };
        has5 = function(it) {
          return wmhas.call(store, it);
        };
      } else {
        STATE = sharedKey2("state");
        hiddenKeys2[STATE] = true;
        set = function(it, metadata) {
          if (objectHas(it, STATE))
            throw new TypeError(OBJECT_ALREADY_INITIALIZED);
          metadata.facade = it;
          createNonEnumerableProperty4(it, STATE, metadata);
          return metadata;
        };
        get3 = function(it) {
          return objectHas(it, STATE) ? it[STATE] : {};
        };
        has5 = function(it) {
          return objectHas(it, STATE);
        };
      }
      var store;
      var wmget;
      var wmhas;
      var wmset;
      var STATE;
      module.exports = {
        set: set,
        get: get3,
        has: has5,
        enforce: enforce,
        getterFor: getterFor
      };
    }
  });

  // node_modules/core-js/internals/redefine.js
  var require_redefine = __commonJS({
    "node_modules/core-js/internals/redefine.js": function(exports, module) {
      var global8 = require_global();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var has5 = require_has();
      var setGlobal = require_set_global();
      var inspectSource = require_inspect_source();
      var InternalStateModule3 = require_internal_state();
      var getInternalState3 = InternalStateModule3.get;
      var enforceInternalState = InternalStateModule3.enforce;
      var TEMPLATE = String(String).split("String");
      (module.exports = function(O, key, value, options) {
        var unsafe = options ? !!options.unsafe : false;
        var simple = options ? !!options.enumerable : false;
        var noTargetGet = options ? !!options.noTargetGet : false;
        var state;
        if (typeof value == "function") {
          if (typeof key == "string" && !has5(value, "name")) {
            createNonEnumerableProperty4(value, "name", key);
          }
          state = enforceInternalState(value);
          if (!state.source) {
            state.source = TEMPLATE.join(typeof key == "string" ? key : "");
          }
        }
        if (O === global8) {
          if (simple)
            O[key] = value;
          else
            setGlobal(key, value);
          return;
        } else if (!unsafe) {
          delete O[key];
        } else if (!noTargetGet && O[key]) {
          simple = true;
        }
        if (simple)
          O[key] = value;
        else
          createNonEnumerableProperty4(O, key, value);
      })(Function.prototype, "toString", function toString2() {
        return typeof this == "function" && getInternalState3(this).source || inspectSource(this);
      });
    }
  });

  // node_modules/core-js/internals/path.js
  var require_path = __commonJS({
    "node_modules/core-js/internals/path.js": function(exports, module) {
      var global8 = require_global();
      module.exports = global8;
    }
  });

  // node_modules/core-js/internals/get-built-in.js
  var require_get_built_in = __commonJS({
    "node_modules/core-js/internals/get-built-in.js": function(exports, module) {
      var path = require_path();
      var global8 = require_global();
      var aFunction2 = function(variable) {
        return typeof variable == "function" ? variable : void 0;
      };
      module.exports = function(namespace, method) {
        return arguments.length < 2 ? aFunction2(path[namespace]) || aFunction2(global8[namespace]) : path[namespace] && path[namespace][method] || global8[namespace] && global8[namespace][method];
      };
    }
  });

  // node_modules/core-js/internals/to-integer.js
  var require_to_integer = __commonJS({
    "node_modules/core-js/internals/to-integer.js": function(exports, module) {
      var ceil = Math.ceil;
      var floor = Math.floor;
      module.exports = function(argument) {
        return isNaN(argument = +argument) ? 0 : (argument > 0 ? floor : ceil)(argument);
      };
    }
  });

  // node_modules/core-js/internals/to-length.js
  var require_to_length = __commonJS({
    "node_modules/core-js/internals/to-length.js": function(exports, module) {
      var toInteger3 = require_to_integer();
      var min4 = Math.min;
      module.exports = function(argument) {
        return argument > 0 ? min4(toInteger3(argument), 9007199254740991) : 0;
      };
    }
  });

  // node_modules/core-js/internals/to-absolute-index.js
  var require_to_absolute_index = __commonJS({
    "node_modules/core-js/internals/to-absolute-index.js": function(exports, module) {
      var toInteger3 = require_to_integer();
      var max4 = Math.max;
      var min4 = Math.min;
      module.exports = function(index, length) {
        var integer = toInteger3(index);
        return integer < 0 ? max4(integer + length, 0) : min4(integer, length);
      };
    }
  });

  // node_modules/core-js/internals/array-includes.js
  var require_array_includes = __commonJS({
    "node_modules/core-js/internals/array-includes.js": function(exports, module) {
      var toIndexedObject5 = require_to_indexed_object();
      var toLength8 = require_to_length();
      var toAbsoluteIndex4 = require_to_absolute_index();
      var createMethod = function(IS_INCLUDES) {
        return function($this, el, fromIndex) {
          var O = toIndexedObject5($this);
          var length = toLength8(O.length);
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
      var has5 = require_has();
      var toIndexedObject5 = require_to_indexed_object();
      var indexOf2 = require_array_includes().indexOf;
      var hiddenKeys2 = require_hidden_keys();
      module.exports = function(object, names) {
        var O = toIndexedObject5(object);
        var i = 0;
        var result = [];
        var key;
        for (key in O)
          !has5(hiddenKeys2, key) && has5(O, key) && result.push(key);
        while (names.length > i)
          if (has5(O, key = names[i++])) {
            ~indexOf2(result, key) || result.push(key);
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
      var hiddenKeys2 = enumBugKeys.concat("length", "prototype");
      exports.f = Object.getOwnPropertyNames || function getOwnPropertyNames3(O) {
        return internalObjectKeys(O, hiddenKeys2);
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
      var getOwnPropertyNamesModule2 = require_object_get_own_property_names();
      var getOwnPropertySymbolsModule2 = require_object_get_own_property_symbols();
      var anObject10 = require_an_object();
      module.exports = getBuiltIn3("Reflect", "ownKeys") || function ownKeys(it) {
        var keys2 = getOwnPropertyNamesModule2.f(anObject10(it));
        var getOwnPropertySymbols3 = getOwnPropertySymbolsModule2.f;
        return getOwnPropertySymbols3 ? keys2.concat(getOwnPropertySymbols3(it)) : keys2;
      };
    }
  });

  // node_modules/core-js/internals/copy-constructor-properties.js
  var require_copy_constructor_properties = __commonJS({
    "node_modules/core-js/internals/copy-constructor-properties.js": function(exports, module) {
      var has5 = require_has();
      var ownKeys = require_own_keys();
      var getOwnPropertyDescriptorModule3 = require_object_get_own_property_descriptor();
      var definePropertyModule2 = require_object_define_property();
      module.exports = function(target, source) {
        var keys2 = ownKeys(source);
        var defineProperty5 = definePropertyModule2.f;
        var getOwnPropertyDescriptor4 = getOwnPropertyDescriptorModule3.f;
        for (var i = 0; i < keys2.length; i++) {
          var key = keys2[i];
          if (!has5(target, key))
            defineProperty5(target, key, getOwnPropertyDescriptor4(source, key));
        }
      };
    }
  });

  // node_modules/core-js/internals/is-forced.js
  var require_is_forced = __commonJS({
    "node_modules/core-js/internals/is-forced.js": function(exports, module) {
      var fails11 = require_fails();
      var replacement = /#|\.prototype\./;
      var isForced2 = function(feature, detection) {
        var value = data[normalize(feature)];
        return value == POLYFILL ? true : value == NATIVE ? false : typeof detection == "function" ? fails11(detection) : !!detection;
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
      var global8 = require_global();
      var getOwnPropertyDescriptor4 = require_object_get_own_property_descriptor().f;
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var redefine5 = require_redefine();
      var setGlobal = require_set_global();
      var copyConstructorProperties2 = require_copy_constructor_properties();
      var isForced2 = require_is_forced();
      module.exports = function(options, source) {
        var TARGET = options.target;
        var GLOBAL = options.global;
        var STATIC = options.stat;
        var FORCED5, target, key, targetProperty, sourceProperty, descriptor;
        if (GLOBAL) {
          target = global8;
        } else if (STATIC) {
          target = global8[TARGET] || setGlobal(TARGET, {});
        } else {
          target = (global8[TARGET] || {}).prototype;
        }
        if (target)
          for (key in source) {
            sourceProperty = source[key];
            if (options.noTargetGet) {
              descriptor = getOwnPropertyDescriptor4(target, key);
              targetProperty = descriptor && descriptor.value;
            } else
              targetProperty = target[key];
            FORCED5 = isForced2(GLOBAL ? key : TARGET + (STATIC ? "." : "#") + key, options.forced);
            if (!FORCED5 && targetProperty !== void 0) {
              if (typeof sourceProperty === typeof targetProperty)
                continue;
              copyConstructorProperties2(sourceProperty, targetProperty);
            }
            if (options.sham || targetProperty && targetProperty.sham) {
              createNonEnumerableProperty4(sourceProperty, "sham", true);
            }
            redefine5(target, key, sourceProperty, options);
          }
      };
    }
  });

  // node_modules/core-js/internals/array-method-is-strict.js
  var require_array_method_is_strict = __commonJS({
    "node_modules/core-js/internals/array-method-is-strict.js": function(exports, module) {
      "use strict";
      var fails11 = require_fails();
      module.exports = function(METHOD_NAME, argument) {
        var method = [][METHOD_NAME];
        return !!method && fails11(function() {
          method.call(null, argument || function() {
            throw 1;
          }, 1);
        });
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
      var requireObjectCoercible5 = require_require_object_coercible();
      var whitespaces = require_whitespaces();
      var whitespace = "[" + whitespaces + "]";
      var ltrim = RegExp("^" + whitespace + whitespace + "*");
      var rtrim = RegExp(whitespace + whitespace + "*$");
      var createMethod = function(TYPE) {
        return function($this) {
          var string = String(requireObjectCoercible5($this));
          if (TYPE & 1)
            string = string.replace(ltrim, "");
          if (TYPE & 2)
            string = string.replace(rtrim, "");
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
      var global8 = require_global();
      var trim3 = require_string_trim().trim;
      var whitespaces = require_whitespaces();
      var $parseInt = global8.parseInt;
      var hex = /^[+-]?0[Xx]/;
      var FORCED5 = $parseInt(whitespaces + "08") !== 8 || $parseInt(whitespaces + "0x16") !== 22;
      module.exports = FORCED5 ? function parseInt2(string, radix) {
        var S = trim3(String(string));
        return $parseInt(S, radix >>> 0 || (hex.test(S) ? 16 : 10));
      } : $parseInt;
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
        if (that.sticky)
          result += "y";
        return result;
      };
    }
  });

  // node_modules/core-js/internals/regexp-sticky-helpers.js
  var require_regexp_sticky_helpers = __commonJS({
    "node_modules/core-js/internals/regexp-sticky-helpers.js": function(exports) {
      "use strict";
      var fails11 = require_fails();
      function RE(s, f) {
        return RegExp(s, f);
      }
      exports.UNSUPPORTED_Y = fails11(function() {
        var re = RE("a", "y");
        re.lastIndex = 2;
        return re.exec("abcd") != null;
      });
      exports.BROKEN_CARET = fails11(function() {
        var re = RE("^r", "gy");
        re.lastIndex = 2;
        return re.exec("str") != null;
      });
    }
  });

  // node_modules/core-js/internals/regexp-exec.js
  var require_regexp_exec = __commonJS({
    "node_modules/core-js/internals/regexp-exec.js": function(exports, module) {
      "use strict";
      var regexpFlags = require_regexp_flags();
      var stickyHelpers2 = require_regexp_sticky_helpers();
      var shared2 = require_shared();
      var nativeExec = RegExp.prototype.exec;
      var nativeReplace = shared2("native-string-replace", String.prototype.replace);
      var patchedExec = nativeExec;
      var UPDATES_LAST_INDEX_WRONG = function() {
        var re1 = /a/;
        var re2 = /b*/g;
        nativeExec.call(re1, "a");
        nativeExec.call(re2, "a");
        return re1.lastIndex !== 0 || re2.lastIndex !== 0;
      }();
      var UNSUPPORTED_Y2 = stickyHelpers2.UNSUPPORTED_Y || stickyHelpers2.BROKEN_CARET;
      var NPCG_INCLUDED = /()??/.exec("")[1] !== void 0;
      var PATCH = UPDATES_LAST_INDEX_WRONG || NPCG_INCLUDED || UNSUPPORTED_Y2;
      if (PATCH) {
        patchedExec = function exec(str) {
          var re = this;
          var lastIndex, reCopy, match, i;
          var sticky = UNSUPPORTED_Y2 && re.sticky;
          var flags2 = regexpFlags.call(re);
          var source = re.source;
          var charsAdded = 0;
          var strCopy = str;
          if (sticky) {
            flags2 = flags2.replace("y", "");
            if (flags2.indexOf("g") === -1) {
              flags2 += "g";
            }
            strCopy = String(str).slice(re.lastIndex);
            if (re.lastIndex > 0 && (!re.multiline || re.multiline && str[re.lastIndex - 1] !== "\n")) {
              source = "(?: " + source + ")";
              strCopy = " " + strCopy;
              charsAdded++;
            }
            reCopy = new RegExp("^(?:" + source + ")", flags2);
          }
          if (NPCG_INCLUDED) {
            reCopy = new RegExp("^" + source + "$(?!\\s)", flags2);
          }
          if (UPDATES_LAST_INDEX_WRONG)
            lastIndex = re.lastIndex;
          match = nativeExec.call(sticky ? reCopy : re, strCopy);
          if (sticky) {
            if (match) {
              match.input = match.input.slice(charsAdded);
              match[0] = match[0].slice(charsAdded);
              match.index = re.lastIndex;
              re.lastIndex += match[0].length;
            } else
              re.lastIndex = 0;
          } else if (UPDATES_LAST_INDEX_WRONG && match) {
            re.lastIndex = re.global ? match.index + match[0].length : lastIndex;
          }
          if (NPCG_INCLUDED && match && match.length > 1) {
            nativeReplace.call(match[0], reCopy, function() {
              for (i = 1; i < arguments.length - 2; i++) {
                if (arguments[i] === void 0)
                  match[i] = void 0;
              }
            });
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
      var $72 = require_export();
      var exec = require_regexp_exec();
      $72({ target: "RegExp", proto: true, forced: /./.exec !== exec }, {
        exec: exec
      });
    }
  });

  // node_modules/core-js/internals/engine-user-agent.js
  var require_engine_user_agent = __commonJS({
    "node_modules/core-js/internals/engine-user-agent.js": function(exports, module) {
      var getBuiltIn3 = require_get_built_in();
      module.exports = getBuiltIn3("navigator", "userAgent") || "";
    }
  });

  // node_modules/core-js/internals/engine-v8-version.js
  var require_engine_v8_version = __commonJS({
    "node_modules/core-js/internals/engine-v8-version.js": function(exports, module) {
      var global8 = require_global();
      var userAgent2 = require_engine_user_agent();
      var process2 = global8.process;
      var versions = process2 && process2.versions;
      var v8 = versions && versions.v8;
      var match;
      var version;
      if (v8) {
        match = v8.split(".");
        version = match[0] < 4 ? 1 : match[0] + match[1];
      } else if (userAgent2) {
        match = userAgent2.match(/Edge\/(\d+)/);
        if (!match || match[1] >= 74) {
          match = userAgent2.match(/Chrome\/(\d+)/);
          if (match)
            version = match[1];
        }
      }
      module.exports = version && +version;
    }
  });

  // node_modules/core-js/internals/native-symbol.js
  var require_native_symbol = __commonJS({
    "node_modules/core-js/internals/native-symbol.js": function(exports, module) {
      var V8_VERSION2 = require_engine_v8_version();
      var fails11 = require_fails();
      module.exports = !!Object.getOwnPropertySymbols && !fails11(function() {
        var symbol = Symbol();
        return !String(symbol) || !(Object(symbol) instanceof Symbol) || !Symbol.sham && V8_VERSION2 && V8_VERSION2 < 41;
      });
    }
  });

  // node_modules/core-js/internals/use-symbol-as-uid.js
  var require_use_symbol_as_uid = __commonJS({
    "node_modules/core-js/internals/use-symbol-as-uid.js": function(exports, module) {
      var NATIVE_SYMBOL2 = require_native_symbol();
      module.exports = NATIVE_SYMBOL2 && !Symbol.sham && typeof Symbol.iterator == "symbol";
    }
  });

  // node_modules/core-js/internals/well-known-symbol.js
  var require_well_known_symbol = __commonJS({
    "node_modules/core-js/internals/well-known-symbol.js": function(exports, module) {
      var global8 = require_global();
      var shared2 = require_shared();
      var has5 = require_has();
      var uid2 = require_uid();
      var NATIVE_SYMBOL2 = require_native_symbol();
      var USE_SYMBOL_AS_UID2 = require_use_symbol_as_uid();
      var WellKnownSymbolsStore2 = shared2("wks");
      var Symbol2 = global8.Symbol;
      var createWellKnownSymbol = USE_SYMBOL_AS_UID2 ? Symbol2 : Symbol2 && Symbol2.withoutSetter || uid2;
      module.exports = function(name) {
        if (!has5(WellKnownSymbolsStore2, name) || !(NATIVE_SYMBOL2 || typeof WellKnownSymbolsStore2[name] == "string")) {
          if (NATIVE_SYMBOL2 && has5(Symbol2, name)) {
            WellKnownSymbolsStore2[name] = Symbol2[name];
          } else {
            WellKnownSymbolsStore2[name] = createWellKnownSymbol("Symbol." + name);
          }
        }
        return WellKnownSymbolsStore2[name];
      };
    }
  });

  // node_modules/core-js/internals/fix-regexp-well-known-symbol-logic.js
  var require_fix_regexp_well_known_symbol_logic = __commonJS({
    "node_modules/core-js/internals/fix-regexp-well-known-symbol-logic.js": function(exports, module) {
      "use strict";
      require_es_regexp_exec();
      var redefine5 = require_redefine();
      var regexpExec2 = require_regexp_exec();
      var fails11 = require_fails();
      var wellKnownSymbol5 = require_well_known_symbol();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var SPECIES2 = wellKnownSymbol5("species");
      var RegExpPrototype2 = RegExp.prototype;
      var REPLACE_SUPPORTS_NAMED_GROUPS = !fails11(function() {
        var re = /./;
        re.exec = function() {
          var result = [];
          result.groups = { a: "7" };
          return result;
        };
        return "".replace(re, "$<a>") !== "7";
      });
      var REPLACE_KEEPS_$0 = function() {
        return "a".replace(/./, "$0") === "$0";
      }();
      var REPLACE = wellKnownSymbol5("replace");
      var REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE = function() {
        if (/./[REPLACE]) {
          return /./[REPLACE]("a", "$0") === "";
        }
        return false;
      }();
      var SPLIT_WORKS_WITH_OVERWRITTEN_EXEC = !fails11(function() {
        var re = /(?:)/;
        var originalExec = re.exec;
        re.exec = function() {
          return originalExec.apply(this, arguments);
        };
        var result = "ab".split(re);
        return result.length !== 2 || result[0] !== "a" || result[1] !== "b";
      });
      module.exports = function(KEY, length, exec, sham) {
        var SYMBOL2 = wellKnownSymbol5(KEY);
        var DELEGATES_TO_SYMBOL = !fails11(function() {
          var O = {};
          O[SYMBOL2] = function() {
            return 7;
          };
          return ""[KEY](O) != 7;
        });
        var DELEGATES_TO_EXEC = DELEGATES_TO_SYMBOL && !fails11(function() {
          var execCalled = false;
          var re = /a/;
          if (KEY === "split") {
            re = {};
            re.constructor = {};
            re.constructor[SPECIES2] = function() {
              return re;
            };
            re.flags = "";
            re[SYMBOL2] = /./[SYMBOL2];
          }
          re.exec = function() {
            execCalled = true;
            return null;
          };
          re[SYMBOL2]("");
          return !execCalled;
        });
        if (!DELEGATES_TO_SYMBOL || !DELEGATES_TO_EXEC || KEY === "replace" && !(REPLACE_SUPPORTS_NAMED_GROUPS && REPLACE_KEEPS_$0 && !REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE) || KEY === "split" && !SPLIT_WORKS_WITH_OVERWRITTEN_EXEC) {
          var nativeRegExpMethod = /./[SYMBOL2];
          var methods = exec(SYMBOL2, ""[KEY], function(nativeMethod, regexp, str, arg2, forceStringMethod) {
            var $exec = regexp.exec;
            if ($exec === regexpExec2 || $exec === RegExpPrototype2.exec) {
              if (DELEGATES_TO_SYMBOL && !forceStringMethod) {
                return { done: true, value: nativeRegExpMethod.call(regexp, str, arg2) };
              }
              return { done: true, value: nativeMethod.call(str, regexp, arg2) };
            }
            return { done: false };
          }, {
            REPLACE_KEEPS_$0: REPLACE_KEEPS_$0,
            REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE: REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE
          });
          var stringMethod = methods[0];
          var regexMethod = methods[1];
          redefine5(String.prototype, KEY, stringMethod);
          redefine5(RegExpPrototype2, SYMBOL2, length == 2 ? function(string, arg) {
            return regexMethod.call(string, this, arg);
          } : function(string) {
            return regexMethod.call(string, this);
          });
        }
        if (sham)
          createNonEnumerableProperty4(RegExpPrototype2[SYMBOL2], "sham", true);
      };
    }
  });

  // node_modules/core-js/internals/string-multibyte.js
  var require_string_multibyte = __commonJS({
    "node_modules/core-js/internals/string-multibyte.js": function(exports, module) {
      var toInteger3 = require_to_integer();
      var requireObjectCoercible5 = require_require_object_coercible();
      var createMethod = function(CONVERT_TO_STRING) {
        return function($this, pos) {
          var S = String(requireObjectCoercible5($this));
          var position = toInteger3(pos);
          var size = S.length;
          var first, second;
          if (position < 0 || position >= size)
            return CONVERT_TO_STRING ? "" : void 0;
          first = S.charCodeAt(position);
          return first < 55296 || first > 56319 || position + 1 === size || (second = S.charCodeAt(position + 1)) < 56320 || second > 57343 ? CONVERT_TO_STRING ? S.charAt(position) : first : CONVERT_TO_STRING ? S.slice(position, position + 2) : (first - 55296 << 10) + (second - 56320) + 65536;
        };
      };
      module.exports = {
        codeAt: createMethod(false),
        charAt: createMethod(true)
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
      var toObject6 = require_to_object();
      var floor = Math.floor;
      var replace = "".replace;
      var SUBSTITUTION_SYMBOLS = /\$([$&'`]|\d{1,2}|<[^>]*>)/g;
      var SUBSTITUTION_SYMBOLS_NO_NAMED = /\$([$&'`]|\d{1,2})/g;
      module.exports = function(matched, str, position, captures, namedCaptures, replacement) {
        var tailPos = position + matched.length;
        var m = captures.length;
        var symbols = SUBSTITUTION_SYMBOLS_NO_NAMED;
        if (namedCaptures !== void 0) {
          namedCaptures = toObject6(namedCaptures);
          symbols = SUBSTITUTION_SYMBOLS;
        }
        return replace.call(replacement, symbols, function(match, ch) {
          var capture;
          switch (ch.charAt(0)) {
            case "$":
              return "$";
            case "&":
              return matched;
            case "`":
              return str.slice(0, position);
            case "'":
              return str.slice(tailPos);
            case "<":
              capture = namedCaptures[ch.slice(1, -1)];
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
                  return captures[f - 1] === void 0 ? ch.charAt(1) : captures[f - 1] + ch.charAt(1);
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
      var classof2 = require_classof_raw();
      var regexpExec2 = require_regexp_exec();
      module.exports = function(R, S) {
        var exec = R.exec;
        if (typeof exec === "function") {
          var result = exec.call(R, S);
          if (typeof result !== "object") {
            throw TypeError("RegExp exec method returned something other than an Object or null");
          }
          return result;
        }
        if (classof2(R) !== "RegExp") {
          throw TypeError("RegExp#exec called on incompatible receiver");
        }
        return regexpExec2.call(R, S);
      };
    }
  });

  // node_modules/core-js/internals/to-string-tag-support.js
  var require_to_string_tag_support = __commonJS({
    "node_modules/core-js/internals/to-string-tag-support.js": function(exports, module) {
      var wellKnownSymbol5 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol5("toStringTag");
      var test = {};
      test[TO_STRING_TAG2] = "z";
      module.exports = String(test) === "[object z]";
    }
  });

  // node_modules/core-js/internals/classof.js
  var require_classof = __commonJS({
    "node_modules/core-js/internals/classof.js": function(exports, module) {
      var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
      var classofRaw = require_classof_raw();
      var wellKnownSymbol5 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol5("toStringTag");
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
        return it === void 0 ? "Undefined" : it === null ? "Null" : typeof (tag = tryGet(O = Object(it), TO_STRING_TAG2)) == "string" ? tag : CORRECT_ARGUMENTS ? classofRaw(O) : (result = classofRaw(O)) == "Object" && typeof O.callee == "function" ? "Arguments" : result;
      };
    }
  });

  // node_modules/core-js/internals/object-to-string.js
  var require_object_to_string = __commonJS({
    "node_modules/core-js/internals/object-to-string.js": function(exports, module) {
      "use strict";
      var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
      var classof2 = require_classof();
      module.exports = TO_STRING_TAG_SUPPORT2 ? {}.toString : function toString2() {
        return "[object " + classof2(this) + "]";
      };
    }
  });

  // node_modules/core-js/internals/number-parse-float.js
  var require_number_parse_float = __commonJS({
    "node_modules/core-js/internals/number-parse-float.js": function(exports, module) {
      var global8 = require_global();
      var trim3 = require_string_trim().trim;
      var whitespaces = require_whitespaces();
      var $parseFloat = global8.parseFloat;
      var FORCED5 = 1 / $parseFloat(whitespaces + "-0") !== -Infinity;
      module.exports = FORCED5 ? function parseFloat2(string) {
        var trimmedString = trim3(String(string));
        var result = $parseFloat(trimmedString);
        return result === 0 && trimmedString.charAt(0) == "-" ? -0 : result;
      } : $parseFloat;
    }
  });

  // node_modules/core-js/internals/this-number-value.js
  var require_this_number_value = __commonJS({
    "node_modules/core-js/internals/this-number-value.js": function(exports, module) {
      var classof2 = require_classof_raw();
      module.exports = function(value) {
        if (typeof value != "number" && classof2(value) != "Number") {
          throw TypeError("Incorrect invocation");
        }
        return +value;
      };
    }
  });

  // node_modules/core-js/internals/is-array.js
  var require_is_array = __commonJS({
    "node_modules/core-js/internals/is-array.js": function(exports, module) {
      var classof2 = require_classof_raw();
      module.exports = Array.isArray || function isArray4(arg) {
        return classof2(arg) == "Array";
      };
    }
  });

  // node_modules/core-js/internals/create-property.js
  var require_create_property = __commonJS({
    "node_modules/core-js/internals/create-property.js": function(exports, module) {
      "use strict";
      var toPrimitive3 = require_to_primitive();
      var definePropertyModule2 = require_object_define_property();
      var createPropertyDescriptor2 = require_create_property_descriptor();
      module.exports = function(object, key, value) {
        var propertyKey = toPrimitive3(key);
        if (propertyKey in object)
          definePropertyModule2.f(object, propertyKey, createPropertyDescriptor2(0, value));
        else
          object[propertyKey] = value;
      };
    }
  });

  // node_modules/core-js/internals/array-species-create.js
  var require_array_species_create = __commonJS({
    "node_modules/core-js/internals/array-species-create.js": function(exports, module) {
      var isObject7 = require_is_object();
      var isArray4 = require_is_array();
      var wellKnownSymbol5 = require_well_known_symbol();
      var SPECIES2 = wellKnownSymbol5("species");
      module.exports = function(originalArray, length) {
        var C;
        if (isArray4(originalArray)) {
          C = originalArray.constructor;
          if (typeof C == "function" && (C === Array || isArray4(C.prototype)))
            C = void 0;
          else if (isObject7(C)) {
            C = C[SPECIES2];
            if (C === null)
              C = void 0;
          }
        }
        return new (C === void 0 ? Array : C)(length === 0 ? 0 : length);
      };
    }
  });

  // node_modules/core-js/internals/array-method-has-species-support.js
  var require_array_method_has_species_support = __commonJS({
    "node_modules/core-js/internals/array-method-has-species-support.js": function(exports, module) {
      var fails11 = require_fails();
      var wellKnownSymbol5 = require_well_known_symbol();
      var V8_VERSION2 = require_engine_v8_version();
      var SPECIES2 = wellKnownSymbol5("species");
      module.exports = function(METHOD_NAME) {
        return V8_VERSION2 >= 51 || !fails11(function() {
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

  // node_modules/core-js/internals/is-regexp.js
  var require_is_regexp = __commonJS({
    "node_modules/core-js/internals/is-regexp.js": function(exports, module) {
      var isObject7 = require_is_object();
      var classof2 = require_classof_raw();
      var wellKnownSymbol5 = require_well_known_symbol();
      var MATCH = wellKnownSymbol5("match");
      module.exports = function(it) {
        var isRegExp2;
        return isObject7(it) && ((isRegExp2 = it[MATCH]) !== void 0 ? !!isRegExp2 : classof2(it) == "RegExp");
      };
    }
  });

  // node_modules/core-js/internals/a-function.js
  var require_a_function = __commonJS({
    "node_modules/core-js/internals/a-function.js": function(exports, module) {
      module.exports = function(it) {
        if (typeof it != "function") {
          throw TypeError(String(it) + " is not a function");
        }
        return it;
      };
    }
  });

  // node_modules/core-js/internals/species-constructor.js
  var require_species_constructor = __commonJS({
    "node_modules/core-js/internals/species-constructor.js": function(exports, module) {
      var anObject10 = require_an_object();
      var aFunction2 = require_a_function();
      var wellKnownSymbol5 = require_well_known_symbol();
      var SPECIES2 = wellKnownSymbol5("species");
      module.exports = function(O, defaultConstructor) {
        var C = anObject10(O).constructor;
        var S;
        return C === void 0 || (S = anObject10(C)[SPECIES2]) == void 0 ? defaultConstructor : aFunction2(S);
      };
    }
  });

  // node_modules/core-js/internals/function-bind-context.js
  var require_function_bind_context = __commonJS({
    "node_modules/core-js/internals/function-bind-context.js": function(exports, module) {
      var aFunction2 = require_a_function();
      module.exports = function(fn, that, length) {
        aFunction2(fn);
        if (that === void 0)
          return fn;
        switch (length) {
          case 0:
            return function() {
              return fn.call(that);
            };
          case 1:
            return function(a) {
              return fn.call(that, a);
            };
          case 2:
            return function(a, b) {
              return fn.call(that, a, b);
            };
          case 3:
            return function(a, b, c) {
              return fn.call(that, a, b, c);
            };
        }
        return function() {
          return fn.apply(that, arguments);
        };
      };
    }
  });

  // node_modules/core-js/internals/array-iteration.js
  var require_array_iteration = __commonJS({
    "node_modules/core-js/internals/array-iteration.js": function(exports, module) {
      var bind2 = require_function_bind_context();
      var IndexedObject2 = require_indexed_object();
      var toObject6 = require_to_object();
      var toLength8 = require_to_length();
      var arraySpeciesCreate3 = require_array_species_create();
      var push = [].push;
      var createMethod = function(TYPE) {
        var IS_MAP = TYPE == 1;
        var IS_FILTER = TYPE == 2;
        var IS_SOME = TYPE == 3;
        var IS_EVERY = TYPE == 4;
        var IS_FIND_INDEX = TYPE == 6;
        var IS_FILTER_OUT = TYPE == 7;
        var NO_HOLES = TYPE == 5 || IS_FIND_INDEX;
        return function($this, callbackfn, that, specificCreate) {
          var O = toObject6($this);
          var self2 = IndexedObject2(O);
          var boundFunction = bind2(callbackfn, that, 3);
          var length = toLength8(self2.length);
          var index = 0;
          var create5 = specificCreate || arraySpeciesCreate3;
          var target = IS_MAP ? create5($this, length) : IS_FILTER || IS_FILTER_OUT ? create5($this, 0) : void 0;
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
                      push.call(target, value);
                  }
                else
                  switch (TYPE) {
                    case 4:
                      return false;
                    case 7:
                      push.call(target, value);
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
        filterOut: createMethod(7)
      };
    }
  });

  // node_modules/core-js/internals/object-define-properties.js
  var require_object_define_properties = __commonJS({
    "node_modules/core-js/internals/object-define-properties.js": function(exports, module) {
      var DESCRIPTORS6 = require_descriptors();
      var definePropertyModule2 = require_object_define_property();
      var anObject10 = require_an_object();
      var objectKeys2 = require_object_keys();
      module.exports = DESCRIPTORS6 ? Object.defineProperties : function defineProperties2(O, Properties) {
        anObject10(O);
        var keys2 = objectKeys2(Properties);
        var length = keys2.length;
        var index = 0;
        var key;
        while (length > index)
          definePropertyModule2.f(O, key = keys2[index++], Properties[key]);
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
      var defineProperties2 = require_object_define_properties();
      var enumBugKeys = require_enum_bug_keys();
      var hiddenKeys2 = require_hidden_keys();
      var html = require_html();
      var documentCreateElement = require_document_create_element();
      var sharedKey2 = require_shared_key();
      var GT = ">";
      var LT = "<";
      var PROTOTYPE2 = "prototype";
      var SCRIPT = "script";
      var IE_PROTO = sharedKey2("IE_PROTO");
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
          activeXDocument = document.domain && new ActiveXObject("htmlfile");
        } catch (error) {
        }
        NullProtoObject = activeXDocument ? NullProtoObjectViaActiveX(activeXDocument) : NullProtoObjectViaIFrame();
        var length = enumBugKeys.length;
        while (length--)
          delete NullProtoObject[PROTOTYPE2][enumBugKeys[length]];
        return NullProtoObject();
      };
      hiddenKeys2[IE_PROTO] = true;
      module.exports = Object.create || function create5(O, Properties) {
        var result;
        if (O !== null) {
          EmptyConstructor[PROTOTYPE2] = anObject10(O);
          result = new EmptyConstructor();
          EmptyConstructor[PROTOTYPE2] = null;
          result[IE_PROTO] = O;
        } else
          result = NullProtoObject();
        return Properties === void 0 ? result : defineProperties2(result, Properties);
      };
    }
  });

  // node_modules/core-js/internals/add-to-unscopables.js
  var require_add_to_unscopables = __commonJS({
    "node_modules/core-js/internals/add-to-unscopables.js": function(exports, module) {
      var wellKnownSymbol5 = require_well_known_symbol();
      var create5 = require_object_create();
      var definePropertyModule2 = require_object_define_property();
      var UNSCOPABLES = wellKnownSymbol5("unscopables");
      var ArrayPrototype = Array.prototype;
      if (ArrayPrototype[UNSCOPABLES] == void 0) {
        definePropertyModule2.f(ArrayPrototype, UNSCOPABLES, {
          configurable: true,
          value: create5(null)
        });
      }
      module.exports = function(key) {
        ArrayPrototype[UNSCOPABLES][key] = true;
      };
    }
  });

  // node_modules/core-js/internals/a-possible-prototype.js
  var require_a_possible_prototype = __commonJS({
    "node_modules/core-js/internals/a-possible-prototype.js": function(exports, module) {
      var isObject7 = require_is_object();
      module.exports = function(it) {
        if (!isObject7(it) && it !== null) {
          throw TypeError("Can't set " + String(it) + " as a prototype");
        }
        return it;
      };
    }
  });

  // node_modules/core-js/internals/object-set-prototype-of.js
  var require_object_set_prototype_of = __commonJS({
    "node_modules/core-js/internals/object-set-prototype-of.js": function(exports, module) {
      var anObject10 = require_an_object();
      var aPossiblePrototype = require_a_possible_prototype();
      module.exports = Object.setPrototypeOf || ("__proto__" in {} ? function() {
        var CORRECT_SETTER = false;
        var test = {};
        var setter;
        try {
          setter = Object.getOwnPropertyDescriptor(Object.prototype, "__proto__").set;
          setter.call(test, []);
          CORRECT_SETTER = test instanceof Array;
        } catch (error) {
        }
        return function setPrototypeOf2(O, proto) {
          anObject10(O);
          aPossiblePrototype(proto);
          if (CORRECT_SETTER)
            setter.call(O, proto);
          else
            O.__proto__ = proto;
          return O;
        };
      }() : void 0);
    }
  });

  // node_modules/core-js/internals/correct-prototype-getter.js
  var require_correct_prototype_getter = __commonJS({
    "node_modules/core-js/internals/correct-prototype-getter.js": function(exports, module) {
      var fails11 = require_fails();
      module.exports = !fails11(function() {
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
      var has5 = require_has();
      var toObject6 = require_to_object();
      var sharedKey2 = require_shared_key();
      var CORRECT_PROTOTYPE_GETTER2 = require_correct_prototype_getter();
      var IE_PROTO = sharedKey2("IE_PROTO");
      var ObjectPrototype2 = Object.prototype;
      module.exports = CORRECT_PROTOTYPE_GETTER2 ? Object.getPrototypeOf : function(O) {
        O = toObject6(O);
        if (has5(O, IE_PROTO))
          return O[IE_PROTO];
        if (typeof O.constructor == "function" && O instanceof O.constructor) {
          return O.constructor.prototype;
        }
        return O instanceof Object ? ObjectPrototype2 : null;
      };
    }
  });

  // node_modules/core-js/internals/function-bind.js
  var require_function_bind = __commonJS({
    "node_modules/core-js/internals/function-bind.js": function(exports, module) {
      "use strict";
      var aFunction2 = require_a_function();
      var isObject7 = require_is_object();
      var slice3 = [].slice;
      var factories = {};
      var construct2 = function(C, argsLength, args) {
        if (!(argsLength in factories)) {
          for (var list = [], i = 0; i < argsLength; i++)
            list[i] = "a[" + i + "]";
          factories[argsLength] = Function("C,a", "return new C(" + list.join(",") + ")");
        }
        return factories[argsLength](C, args);
      };
      module.exports = Function.bind || function bind2(that) {
        var fn = aFunction2(this);
        var partArgs = slice3.call(arguments, 1);
        var boundFunction = function bound() {
          var args = partArgs.concat(slice3.call(arguments));
          return this instanceof boundFunction ? construct2(fn, args.length, args) : fn.apply(that, args);
        };
        if (isObject7(fn.prototype))
          boundFunction.prototype = fn.prototype;
        return boundFunction;
      };
    }
  });

  // node_modules/core-js/internals/object-get-own-property-names-external.js
  var require_object_get_own_property_names_external = __commonJS({
    "node_modules/core-js/internals/object-get-own-property-names-external.js": function(exports, module) {
      var toIndexedObject5 = require_to_indexed_object();
      var $getOwnPropertyNames2 = require_object_get_own_property_names().f;
      var toString2 = {}.toString;
      var windowNames = typeof window == "object" && window && Object.getOwnPropertyNames ? Object.getOwnPropertyNames(window) : [];
      var getWindowNames = function(it) {
        try {
          return $getOwnPropertyNames2(it);
        } catch (error) {
          return windowNames.slice();
        }
      };
      module.exports.f = function getOwnPropertyNames3(it) {
        return windowNames && toString2.call(it) == "[object Window]" ? getWindowNames(it) : $getOwnPropertyNames2(toIndexedObject5(it));
      };
    }
  });

  // node_modules/core-js/internals/well-known-symbol-wrapped.js
  var require_well_known_symbol_wrapped = __commonJS({
    "node_modules/core-js/internals/well-known-symbol-wrapped.js": function(exports) {
      var wellKnownSymbol5 = require_well_known_symbol();
      exports.f = wellKnownSymbol5;
    }
  });

  // node_modules/core-js/internals/define-well-known-symbol.js
  var require_define_well_known_symbol = __commonJS({
    "node_modules/core-js/internals/define-well-known-symbol.js": function(exports, module) {
      var path = require_path();
      var has5 = require_has();
      var wrappedWellKnownSymbolModule2 = require_well_known_symbol_wrapped();
      var defineProperty5 = require_object_define_property().f;
      module.exports = function(NAME2) {
        var Symbol2 = path.Symbol || (path.Symbol = {});
        if (!has5(Symbol2, NAME2))
          defineProperty5(Symbol2, NAME2, {
            value: wrappedWellKnownSymbolModule2.f(NAME2)
          });
      };
    }
  });

  // node_modules/core-js/internals/set-to-string-tag.js
  var require_set_to_string_tag = __commonJS({
    "node_modules/core-js/internals/set-to-string-tag.js": function(exports, module) {
      var defineProperty5 = require_object_define_property().f;
      var has5 = require_has();
      var wellKnownSymbol5 = require_well_known_symbol();
      var TO_STRING_TAG2 = wellKnownSymbol5("toStringTag");
      module.exports = function(it, TAG, STATIC) {
        if (it && !has5(it = STATIC ? it : it.prototype, TO_STRING_TAG2)) {
          defineProperty5(it, TO_STRING_TAG2, { configurable: true, value: TAG });
        }
      };
    }
  });

  // node_modules/core-js/internals/iterators.js
  var require_iterators = __commonJS({
    "node_modules/core-js/internals/iterators.js": function(exports, module) {
      module.exports = {};
    }
  });

  // node_modules/core-js/internals/iterators-core.js
  var require_iterators_core = __commonJS({
    "node_modules/core-js/internals/iterators-core.js": function(exports, module) {
      "use strict";
      var fails11 = require_fails();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var has5 = require_has();
      var wellKnownSymbol5 = require_well_known_symbol();
      var IS_PURE2 = require_is_pure();
      var ITERATOR2 = wellKnownSymbol5("iterator");
      var BUGGY_SAFARI_ITERATORS = false;
      var returnThis = function() {
        return this;
      };
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
      var NEW_ITERATOR_PROTOTYPE = IteratorPrototype == void 0 || fails11(function() {
        var test = {};
        return IteratorPrototype[ITERATOR2].call(test) !== test;
      });
      if (NEW_ITERATOR_PROTOTYPE)
        IteratorPrototype = {};
      if ((!IS_PURE2 || NEW_ITERATOR_PROTOTYPE) && !has5(IteratorPrototype, ITERATOR2)) {
        createNonEnumerableProperty4(IteratorPrototype, ITERATOR2, returnThis);
      }
      module.exports = {
        IteratorPrototype: IteratorPrototype,
        BUGGY_SAFARI_ITERATORS: BUGGY_SAFARI_ITERATORS
      };
    }
  });

  // node_modules/core-js/internals/create-iterator-constructor.js
  var require_create_iterator_constructor = __commonJS({
    "node_modules/core-js/internals/create-iterator-constructor.js": function(exports, module) {
      "use strict";
      var IteratorPrototype = require_iterators_core().IteratorPrototype;
      var create5 = require_object_create();
      var createPropertyDescriptor2 = require_create_property_descriptor();
      var setToStringTag2 = require_set_to_string_tag();
      var Iterators = require_iterators();
      var returnThis = function() {
        return this;
      };
      module.exports = function(IteratorConstructor, NAME2, next2) {
        var TO_STRING_TAG2 = NAME2 + " Iterator";
        IteratorConstructor.prototype = create5(IteratorPrototype, { next: createPropertyDescriptor2(1, next2) });
        setToStringTag2(IteratorConstructor, TO_STRING_TAG2, false, true);
        Iterators[TO_STRING_TAG2] = returnThis;
        return IteratorConstructor;
      };
    }
  });

  // node_modules/core-js/internals/define-iterator.js
  var require_define_iterator = __commonJS({
    "node_modules/core-js/internals/define-iterator.js": function(exports, module) {
      "use strict";
      var $72 = require_export();
      var createIteratorConstructor = require_create_iterator_constructor();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var setPrototypeOf2 = require_object_set_prototype_of();
      var setToStringTag2 = require_set_to_string_tag();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var redefine5 = require_redefine();
      var wellKnownSymbol5 = require_well_known_symbol();
      var IS_PURE2 = require_is_pure();
      var Iterators = require_iterators();
      var IteratorsCore = require_iterators_core();
      var IteratorPrototype = IteratorsCore.IteratorPrototype;
      var BUGGY_SAFARI_ITERATORS = IteratorsCore.BUGGY_SAFARI_ITERATORS;
      var ITERATOR2 = wellKnownSymbol5("iterator");
      var KEYS = "keys";
      var VALUES = "values";
      var ENTRIES = "entries";
      var returnThis = function() {
        return this;
      };
      module.exports = function(Iterable, NAME2, IteratorConstructor, next2, DEFAULT, IS_SET, FORCED5) {
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
              return function values() {
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
          if (IteratorPrototype !== Object.prototype && CurrentIteratorPrototype.next) {
            if (!IS_PURE2 && getPrototypeOf3(CurrentIteratorPrototype) !== IteratorPrototype) {
              if (setPrototypeOf2) {
                setPrototypeOf2(CurrentIteratorPrototype, IteratorPrototype);
              } else if (typeof CurrentIteratorPrototype[ITERATOR2] != "function") {
                createNonEnumerableProperty4(CurrentIteratorPrototype, ITERATOR2, returnThis);
              }
            }
            setToStringTag2(CurrentIteratorPrototype, TO_STRING_TAG2, true, true);
            if (IS_PURE2)
              Iterators[TO_STRING_TAG2] = returnThis;
          }
        }
        if (DEFAULT == VALUES && nativeIterator && nativeIterator.name !== VALUES) {
          INCORRECT_VALUES_NAME = true;
          defaultIterator = function values() {
            return nativeIterator.call(this);
          };
        }
        if ((!IS_PURE2 || FORCED5) && IterablePrototype[ITERATOR2] !== defaultIterator) {
          createNonEnumerableProperty4(IterablePrototype, ITERATOR2, defaultIterator);
        }
        Iterators[NAME2] = defaultIterator;
        if (DEFAULT) {
          methods = {
            values: getIterationMethod(VALUES),
            keys: IS_SET ? defaultIterator : getIterationMethod(KEYS),
            entries: getIterationMethod(ENTRIES)
          };
          if (FORCED5)
            for (KEY in methods) {
              if (BUGGY_SAFARI_ITERATORS || INCORRECT_VALUES_NAME || !(KEY in IterablePrototype)) {
                redefine5(IterablePrototype, KEY, methods[KEY]);
              }
            }
          else
            $72({ target: NAME2, proto: true, forced: BUGGY_SAFARI_ITERATORS || INCORRECT_VALUES_NAME }, methods);
        }
        return methods;
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
      var InternalStateModule3 = require_internal_state();
      var defineIterator2 = require_define_iterator();
      var ARRAY_ITERATOR = "Array Iterator";
      var setInternalState3 = InternalStateModule3.set;
      var getInternalState3 = InternalStateModule3.getterFor(ARRAY_ITERATOR);
      module.exports = defineIterator2(Array, "Array", function(iterated, kind) {
        setInternalState3(this, {
          type: ARRAY_ITERATOR,
          target: toIndexedObject5(iterated),
          index: 0,
          kind: kind
        });
      }, function() {
        var state = getInternalState3(this);
        var target = state.target;
        var kind = state.kind;
        var index = state.index++;
        if (!target || index >= target.length) {
          state.target = void 0;
          return { value: void 0, done: true };
        }
        if (kind == "keys")
          return { value: index, done: false };
        if (kind == "values")
          return { value: target[index], done: false };
        return { value: [index, target[index]], done: false };
      }, "values");
      Iterators.Arguments = Iterators.Array;
      addToUnscopables2("keys");
      addToUnscopables2("values");
      addToUnscopables2("entries");
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

  // node_modules/core-js/internals/string-trim-forced.js
  var require_string_trim_forced = __commonJS({
    "node_modules/core-js/internals/string-trim-forced.js": function(exports, module) {
      var fails11 = require_fails();
      var whitespaces = require_whitespaces();
      var non = "\u200B\x85\u180E";
      module.exports = function(METHOD_NAME) {
        return fails11(function() {
          return !!whitespaces[METHOD_NAME]() || non[METHOD_NAME]() != non || whitespaces[METHOD_NAME].name !== METHOD_NAME;
        });
      };
    }
  });

  // node_modules/core-js/internals/inherit-if-required.js
  var require_inherit_if_required = __commonJS({
    "node_modules/core-js/internals/inherit-if-required.js": function(exports, module) {
      var isObject7 = require_is_object();
      var setPrototypeOf2 = require_object_set_prototype_of();
      module.exports = function($this, dummy, Wrapper) {
        var NewTarget, NewTargetPrototype;
        if (setPrototypeOf2 && typeof (NewTarget = dummy.constructor) == "function" && NewTarget !== Wrapper && isObject7(NewTargetPrototype = NewTarget.prototype) && NewTargetPrototype !== Wrapper.prototype)
          setPrototypeOf2($this, NewTargetPrototype);
        return $this;
      };
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
      var DESCRIPTORS6 = require_descriptors();
      var objectKeys2 = require_object_keys();
      var toIndexedObject5 = require_to_indexed_object();
      var propertyIsEnumerable2 = require_object_property_is_enumerable().f;
      var createMethod = function(TO_ENTRIES) {
        return function(it) {
          var O = toIndexedObject5(it);
          var keys2 = objectKeys2(O);
          var length = keys2.length;
          var i = 0;
          var result = [];
          var key;
          while (length > i) {
            key = keys2[i++];
            if (!DESCRIPTORS6 || propertyIsEnumerable2.call(O, key)) {
              result.push(TO_ENTRIES ? [key, O[key]] : O[key]);
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

  // node_modules/core-js/internals/iterator-close.js
  var require_iterator_close = __commonJS({
    "node_modules/core-js/internals/iterator-close.js": function(exports, module) {
      var anObject10 = require_an_object();
      module.exports = function(iterator) {
        var returnMethod = iterator["return"];
        if (returnMethod !== void 0) {
          return anObject10(returnMethod.call(iterator)).value;
        }
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
          iteratorClose(iterator);
          throw error;
        }
      };
    }
  });

  // node_modules/core-js/internals/is-array-iterator-method.js
  var require_is_array_iterator_method = __commonJS({
    "node_modules/core-js/internals/is-array-iterator-method.js": function(exports, module) {
      var wellKnownSymbol5 = require_well_known_symbol();
      var Iterators = require_iterators();
      var ITERATOR2 = wellKnownSymbol5("iterator");
      var ArrayPrototype = Array.prototype;
      module.exports = function(it) {
        return it !== void 0 && (Iterators.Array === it || ArrayPrototype[ITERATOR2] === it);
      };
    }
  });

  // node_modules/core-js/internals/get-iterator-method.js
  var require_get_iterator_method = __commonJS({
    "node_modules/core-js/internals/get-iterator-method.js": function(exports, module) {
      var classof2 = require_classof();
      var Iterators = require_iterators();
      var wellKnownSymbol5 = require_well_known_symbol();
      var ITERATOR2 = wellKnownSymbol5("iterator");
      module.exports = function(it) {
        if (it != void 0)
          return it[ITERATOR2] || it["@@iterator"] || Iterators[classof2(it)];
      };
    }
  });

  // node_modules/core-js/internals/array-from.js
  var require_array_from = __commonJS({
    "node_modules/core-js/internals/array-from.js": function(exports, module) {
      "use strict";
      var bind2 = require_function_bind_context();
      var toObject6 = require_to_object();
      var callWithSafeIterationClosing = require_call_with_safe_iteration_closing();
      var isArrayIteratorMethod = require_is_array_iterator_method();
      var toLength8 = require_to_length();
      var createProperty4 = require_create_property();
      var getIteratorMethod = require_get_iterator_method();
      module.exports = function from2(arrayLike) {
        var O = toObject6(arrayLike);
        var C = typeof this == "function" ? this : Array;
        var argumentsLength = arguments.length;
        var mapfn = argumentsLength > 1 ? arguments[1] : void 0;
        var mapping = mapfn !== void 0;
        var iteratorMethod = getIteratorMethod(O);
        var index = 0;
        var length, result, step, iterator, next2, value;
        if (mapping)
          mapfn = bind2(mapfn, argumentsLength > 2 ? arguments[2] : void 0, 2);
        if (iteratorMethod != void 0 && !(C == Array && isArrayIteratorMethod(iteratorMethod))) {
          iterator = iteratorMethod.call(O);
          next2 = iterator.next;
          result = new C();
          for (; !(step = next2.call(iterator)).done; index++) {
            value = mapping ? callWithSafeIterationClosing(iterator, mapfn, [step.value, index], true) : step.value;
            createProperty4(result, index, value);
          }
        } else {
          length = toLength8(O.length);
          result = new C(length);
          for (; length > index; index++) {
            value = mapping ? mapfn(O[index], index) : O[index];
            createProperty4(result, index, value);
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
      var wellKnownSymbol5 = require_well_known_symbol();
      var ITERATOR2 = wellKnownSymbol5("iterator");
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
      module.exports = function(exec, SKIP_CLOSING) {
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
          exec(object);
        } catch (error) {
        }
        return ITERATION_SUPPORT;
      };
    }
  });

  // node_modules/core-js/internals/array-buffer-native.js
  var require_array_buffer_native = __commonJS({
    "node_modules/core-js/internals/array-buffer-native.js": function(exports, module) {
      module.exports = typeof ArrayBuffer !== "undefined" && typeof DataView !== "undefined";
    }
  });

  // node_modules/core-js/internals/redefine-all.js
  var require_redefine_all = __commonJS({
    "node_modules/core-js/internals/redefine-all.js": function(exports, module) {
      var redefine5 = require_redefine();
      module.exports = function(target, src, options) {
        for (var key in src)
          redefine5(target, key, src[key], options);
        return target;
      };
    }
  });

  // node_modules/core-js/internals/an-instance.js
  var require_an_instance = __commonJS({
    "node_modules/core-js/internals/an-instance.js": function(exports, module) {
      module.exports = function(it, Constructor, name) {
        if (!(it instanceof Constructor)) {
          throw TypeError("Incorrect " + (name ? name + " " : "") + "invocation");
        }
        return it;
      };
    }
  });

  // node_modules/core-js/internals/to-index.js
  var require_to_index = __commonJS({
    "node_modules/core-js/internals/to-index.js": function(exports, module) {
      var toInteger3 = require_to_integer();
      var toLength8 = require_to_length();
      module.exports = function(it) {
        if (it === void 0)
          return 0;
        var number = toInteger3(it);
        var length = toLength8(number);
        if (number !== length)
          throw RangeError("Wrong length or index");
        return length;
      };
    }
  });

  // node_modules/core-js/internals/ieee754.js
  var require_ieee754 = __commonJS({
    "node_modules/core-js/internals/ieee754.js": function(exports, module) {
      var abs = Math.abs;
      var pow = Math.pow;
      var floor = Math.floor;
      var log = Math.log;
      var LN2 = Math.LN2;
      var pack = function(number, mantissaLength, bytes) {
        var buffer = new Array(bytes);
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
          if (number * (c = pow(2, -exponent)) < 1) {
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
        for (; mantissaLength >= 8; buffer[index++] = mantissa & 255, mantissa /= 256, mantissaLength -= 8)
          ;
        exponent = exponent << mantissaLength | mantissa;
        exponentLength += mantissaLength;
        for (; exponentLength > 0; buffer[index++] = exponent & 255, exponent /= 256, exponentLength -= 8)
          ;
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
        for (; nBits > 0; exponent = exponent * 256 + buffer[index], index--, nBits -= 8)
          ;
        mantissa = exponent & (1 << -nBits) - 1;
        exponent >>= -nBits;
        nBits += mantissaLength;
        for (; nBits > 0; mantissa = mantissa * 256 + buffer[index], index--, nBits -= 8)
          ;
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
      var toObject6 = require_to_object();
      var toAbsoluteIndex4 = require_to_absolute_index();
      var toLength8 = require_to_length();
      module.exports = function fill(value) {
        var O = toObject6(this);
        var length = toLength8(O.length);
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
      var global8 = require_global();
      var DESCRIPTORS6 = require_descriptors();
      var NATIVE_ARRAY_BUFFER2 = require_array_buffer_native();
      var createNonEnumerableProperty4 = require_create_non_enumerable_property();
      var redefineAll = require_redefine_all();
      var fails11 = require_fails();
      var anInstance = require_an_instance();
      var toInteger3 = require_to_integer();
      var toLength8 = require_to_length();
      var toIndex = require_to_index();
      var IEEE754 = require_ieee754();
      var getPrototypeOf3 = require_object_get_prototype_of();
      var setPrototypeOf2 = require_object_set_prototype_of();
      var getOwnPropertyNames3 = require_object_get_own_property_names().f;
      var defineProperty5 = require_object_define_property().f;
      var arrayFill = require_array_fill();
      var setToStringTag2 = require_set_to_string_tag();
      var InternalStateModule3 = require_internal_state();
      var getInternalState3 = InternalStateModule3.get;
      var setInternalState3 = InternalStateModule3.set;
      var ARRAY_BUFFER2 = "ArrayBuffer";
      var DATA_VIEW = "DataView";
      var PROTOTYPE2 = "prototype";
      var WRONG_LENGTH = "Wrong length";
      var WRONG_INDEX = "Wrong index";
      var NativeArrayBuffer2 = global8[ARRAY_BUFFER2];
      var $ArrayBuffer = NativeArrayBuffer2;
      var $DataView = global8[DATA_VIEW];
      var $DataViewPrototype = $DataView && $DataView[PROTOTYPE2];
      var ObjectPrototype2 = Object.prototype;
      var RangeError2 = global8.RangeError;
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
      var addGetter = function(Constructor, key2) {
        defineProperty5(Constructor[PROTOTYPE2], key2, { get: function() {
          return getInternalState3(this)[key2];
        } });
      };
      var get3 = function(view, count, index, isLittleEndian) {
        var intIndex = toIndex(index);
        var store = getInternalState3(view);
        if (intIndex + count > store.byteLength)
          throw RangeError2(WRONG_INDEX);
        var bytes = getInternalState3(store.buffer).bytes;
        var start = intIndex + store.byteOffset;
        var pack = bytes.slice(start, start + count);
        return isLittleEndian ? pack : pack.reverse();
      };
      var set = function(view, count, index, conversion, value, isLittleEndian) {
        var intIndex = toIndex(index);
        var store = getInternalState3(view);
        if (intIndex + count > store.byteLength)
          throw RangeError2(WRONG_INDEX);
        var bytes = getInternalState3(store.buffer).bytes;
        var start = intIndex + store.byteOffset;
        var pack = conversion(+value);
        for (var i = 0; i < count; i++)
          bytes[start + i] = pack[isLittleEndian ? i : count - i - 1];
      };
      if (!NATIVE_ARRAY_BUFFER2) {
        $ArrayBuffer = function ArrayBuffer4(length) {
          anInstance(this, $ArrayBuffer, ARRAY_BUFFER2);
          var byteLength = toIndex(length);
          setInternalState3(this, {
            bytes: arrayFill.call(new Array(byteLength), 0),
            byteLength: byteLength
          });
          if (!DESCRIPTORS6)
            this.byteLength = byteLength;
        };
        $DataView = function DataView3(buffer, byteOffset, byteLength) {
          anInstance(this, $DataView, DATA_VIEW);
          anInstance(buffer, $ArrayBuffer, DATA_VIEW);
          var bufferLength = getInternalState3(buffer).byteLength;
          var offset = toInteger3(byteOffset);
          if (offset < 0 || offset > bufferLength)
            throw RangeError2("Wrong offset");
          byteLength = byteLength === void 0 ? bufferLength - offset : toLength8(byteLength);
          if (offset + byteLength > bufferLength)
            throw RangeError2(WRONG_LENGTH);
          setInternalState3(this, {
            buffer: buffer,
            byteLength: byteLength,
            byteOffset: offset
          });
          if (!DESCRIPTORS6) {
            this.buffer = buffer;
            this.byteLength = byteLength;
            this.byteOffset = offset;
          }
        };
        if (DESCRIPTORS6) {
          addGetter($ArrayBuffer, "byteLength");
          addGetter($DataView, "buffer");
          addGetter($DataView, "byteLength");
          addGetter($DataView, "byteOffset");
        }
        redefineAll($DataView[PROTOTYPE2], {
          getInt8: function getInt8(byteOffset) {
            return get3(this, 1, byteOffset)[0] << 24 >> 24;
          },
          getUint8: function getUint8(byteOffset) {
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
          setUint8: function setUint8(byteOffset, value) {
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
        if (!fails11(function() {
          NativeArrayBuffer2(1);
        }) || !fails11(function() {
          new NativeArrayBuffer2(-1);
        }) || fails11(function() {
          new NativeArrayBuffer2();
          new NativeArrayBuffer2(1.5);
          new NativeArrayBuffer2(NaN);
          return NativeArrayBuffer2.name != ARRAY_BUFFER2;
        })) {
          $ArrayBuffer = function ArrayBuffer4(length) {
            anInstance(this, $ArrayBuffer);
            return new NativeArrayBuffer2(toIndex(length));
          };
          ArrayBufferPrototype = $ArrayBuffer[PROTOTYPE2] = NativeArrayBuffer2[PROTOTYPE2];
          for (keys2 = getOwnPropertyNames3(NativeArrayBuffer2), j = 0; keys2.length > j; ) {
            if (!((key = keys2[j++]) in $ArrayBuffer)) {
              createNonEnumerableProperty4($ArrayBuffer, key, NativeArrayBuffer2[key]);
            }
          }
          ArrayBufferPrototype.constructor = $ArrayBuffer;
        }
        if (setPrototypeOf2 && getPrototypeOf3($DataViewPrototype) !== ObjectPrototype2) {
          setPrototypeOf2($DataViewPrototype, ObjectPrototype2);
        }
        testView = new $DataView(new $ArrayBuffer(2));
        $setInt8 = $DataViewPrototype.setInt8;
        testView.setInt8(0, 2147483648);
        testView.setInt8(1, 2147483649);
        if (testView.getInt8(0) || !testView.getInt8(1))
          redefineAll($DataViewPrototype, {
            setInt8: function setInt8(byteOffset, value) {
              $setInt8.call(this, byteOffset, value << 24 >> 24);
            },
            setUint8: function setUint8(byteOffset, value) {
              $setInt8.call(this, byteOffset, value << 24 >> 24);
            }
          }, { unsafe: true });
      }
      var ArrayBufferPrototype;
      var keys2;
      var j;
      var key;
      var testView;
      var $setInt8;
      setToStringTag2($ArrayBuffer, ARRAY_BUFFER2);
      setToStringTag2($DataView, DATA_VIEW);
      module.exports = {
        ArrayBuffer: $ArrayBuffer,
        DataView: $DataView
      };
    }
  });

  // node_modules/core-js/internals/set-species.js
  var require_set_species = __commonJS({
    "node_modules/core-js/internals/set-species.js": function(exports, module) {
      "use strict";
      var getBuiltIn3 = require_get_built_in();
      var definePropertyModule2 = require_object_define_property();
      var wellKnownSymbol5 = require_well_known_symbol();
      var DESCRIPTORS6 = require_descriptors();
      var SPECIES2 = wellKnownSymbol5("species");
      module.exports = function(CONSTRUCTOR_NAME) {
        var Constructor = getBuiltIn3(CONSTRUCTOR_NAME);
        var defineProperty5 = definePropertyModule2.f;
        if (DESCRIPTORS6 && Constructor && !Constructor[SPECIES2]) {
          defineProperty5(Constructor, SPECIES2, {
            configurable: true,
            get: function() {
              return this;
            }
          });
        }
      };
    }
  });

  // node_modules/core-js/internals/array-reduce.js
  var require_array_reduce = __commonJS({
    "node_modules/core-js/internals/array-reduce.js": function(exports, module) {
      var aFunction2 = require_a_function();
      var toObject6 = require_to_object();
      var IndexedObject2 = require_indexed_object();
      var toLength8 = require_to_length();
      var createMethod = function(IS_RIGHT) {
        return function(that, callbackfn, argumentsLength, memo) {
          aFunction2(callbackfn);
          var O = toObject6(that);
          var self2 = IndexedObject2(O);
          var length = toLength8(O.length);
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
                throw TypeError("Reduce of empty array with no initial value");
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

  // node_modules/core-js/internals/engine-is-node.js
  var require_engine_is_node = __commonJS({
    "node_modules/core-js/internals/engine-is-node.js": function(exports, module) {
      var classof2 = require_classof_raw();
      var global8 = require_global();
      module.exports = classof2(global8.process) == "process";
    }
  });

  // node_modules/core-js/internals/array-for-each.js
  var require_array_for_each = __commonJS({
    "node_modules/core-js/internals/array-for-each.js": function(exports, module) {
      "use strict";
      var $forEach2 = require_array_iteration().forEach;
      var arrayMethodIsStrict4 = require_array_method_is_strict();
      var STRICT_METHOD4 = arrayMethodIsStrict4("forEach");
      module.exports = !STRICT_METHOD4 ? function forEach3(callbackfn) {
        return $forEach2(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
      } : [].forEach;
    }
  });

  // srcts/src/initialize/disableForm.ts
  var import_jquery = __toModule(require_jquery());
  function disableFormSubmission() {
    (0, import_jquery.default)(document).on("submit", "form:not([action])", function(e) {
      e.preventDefault();
    });
  }

  // srcts/src/initialize/history.ts
  var import_jquery2 = __toModule(require_jquery());
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
  "use strict";
  var $3 = require_export();
  var $indexOf = require_array_includes().indexOf;
  var arrayMethodIsStrict = require_array_method_is_strict();
  var nativeIndexOf = [].indexOf;
  var NEGATIVE_ZERO = !!nativeIndexOf && 1 / [1].indexOf(1, -0) < 0;
  var STRICT_METHOD = arrayMethodIsStrict("indexOf");
  $3({ target: "Array", proto: true, forced: NEGATIVE_ZERO || !STRICT_METHOD }, {
    indexOf: function indexOf(searchElement) {
      return NEGATIVE_ZERO ? nativeIndexOf.apply(this, arguments) || 0 : $indexOf(this, searchElement, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.parse-int.js
  var $4 = require_export();
  var parseIntImplementation = require_number_parse_int();
  $4({ global: true, forced: parseInt != parseIntImplementation }, {
    parseInt: parseIntImplementation
  });

  // srcts/src/initialize/browser.ts
  var import_jquery3 = __toModule(require_jquery());

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
  var import_jquery43 = __toModule(require_jquery());

  // node_modules/core-js/modules/es.function.name.js
  var DESCRIPTORS = require_descriptors();
  var defineProperty = require_object_define_property().f;
  var FunctionPrototype = Function.prototype;
  var FunctionPrototypeToString = FunctionPrototype.toString;
  var nameRE = /^\s*function ([^ (]*)/;
  var NAME = "name";
  if (DESCRIPTORS && !(NAME in FunctionPrototype)) {
    defineProperty(FunctionPrototype, NAME, {
      configurable: true,
      get: function() {
        try {
          return FunctionPrototypeToString.call(this).match(nameRE)[1];
        } catch (error) {
          return "";
        }
      }
    });
  }

  // srcts/src/utils/index.ts
  var import_es_regexp_exec = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.replace.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic = require_fix_regexp_well_known_symbol_logic();
  var anObject = require_an_object();
  var toLength = require_to_length();
  var toInteger = require_to_integer();
  var requireObjectCoercible = require_require_object_coercible();
  var advanceStringIndex = require_advance_string_index();
  var getSubstitution = require_get_substitution();
  var regExpExec = require_regexp_exec_abstract();
  var max = Math.max;
  var min = Math.min;
  var maybeToString = function(it) {
    return it === void 0 ? it : String(it);
  };
  fixRegExpWellKnownSymbolLogic("replace", 2, function(REPLACE, nativeReplace, maybeCallNative, reason) {
    var REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE = reason.REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE;
    var REPLACE_KEEPS_$0 = reason.REPLACE_KEEPS_$0;
    var UNSAFE_SUBSTITUTE = REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE ? "$" : "$0";
    return [
      function replace(searchValue, replaceValue) {
        var O = requireObjectCoercible(this);
        var replacer = searchValue == void 0 ? void 0 : searchValue[REPLACE];
        return replacer !== void 0 ? replacer.call(searchValue, O, replaceValue) : nativeReplace.call(String(O), searchValue, replaceValue);
      },
      function(regexp, replaceValue) {
        if (!REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE && REPLACE_KEEPS_$0 || typeof replaceValue === "string" && replaceValue.indexOf(UNSAFE_SUBSTITUTE) === -1) {
          var res = maybeCallNative(nativeReplace, regexp, this, replaceValue);
          if (res.done)
            return res.value;
        }
        var rx = anObject(regexp);
        var S = String(this);
        var functionalReplace = typeof replaceValue === "function";
        if (!functionalReplace)
          replaceValue = String(replaceValue);
        var global8 = rx.global;
        if (global8) {
          var fullUnicode = rx.unicode;
          rx.lastIndex = 0;
        }
        var results = [];
        while (true) {
          var result = regExpExec(rx, S);
          if (result === null)
            break;
          results.push(result);
          if (!global8)
            break;
          var matchStr = String(result[0]);
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex(S, toLength(rx.lastIndex), fullUnicode);
        }
        var accumulatedResult = "";
        var nextSourcePosition = 0;
        for (var i = 0; i < results.length; i++) {
          result = results[i];
          var matched = String(result[0]);
          var position = max(min(toInteger(result.index), S.length), 0);
          var captures = [];
          for (var j = 1; j < result.length; j++)
            captures.push(maybeToString(result[j]));
          var namedCaptures = result.groups;
          if (functionalReplace) {
            var replacerArgs = [matched].concat(captures, position, S);
            if (namedCaptures !== void 0)
              replacerArgs.push(namedCaptures);
            var replacement = String(replaceValue.apply(void 0, replacerArgs));
          } else {
            replacement = getSubstitution(matched, S, position, captures, namedCaptures, replaceValue);
          }
          if (position >= nextSourcePosition) {
            accumulatedResult += S.slice(nextSourcePosition, position) + replacement;
            nextSourcePosition = position + matched.length;
          }
        }
        return accumulatedResult + S.slice(nextSourcePosition);
      }
    ];
  });

  // node_modules/core-js/modules/es.object.to-string.js
  var TO_STRING_TAG_SUPPORT = require_to_string_tag_support();
  var redefine = require_redefine();
  var toString = require_object_to_string();
  if (!TO_STRING_TAG_SUPPORT) {
    redefine(Object.prototype, "toString", toString, { unsafe: true });
  }

  // node_modules/core-js/modules/es.regexp.to-string.js
  "use strict";
  var redefine2 = require_redefine();
  var anObject2 = require_an_object();
  var fails = require_fails();
  var flags = require_regexp_flags();
  var TO_STRING = "toString";
  var RegExpPrototype = RegExp.prototype;
  var nativeToString = RegExpPrototype[TO_STRING];
  var NOT_GENERIC = fails(function() {
    return nativeToString.call({ source: "a", flags: "b" }) != "/a/b";
  });
  var INCORRECT_NAME = nativeToString.name != TO_STRING;
  if (NOT_GENERIC || INCORRECT_NAME) {
    redefine2(RegExp.prototype, TO_STRING, function toString2() {
      var R = anObject2(this);
      var p = String(R.source);
      var rf = R.flags;
      var f = String(rf === void 0 && R instanceof RegExp && !("flags" in RegExpPrototype) ? flags.call(R) : rf);
      return "/" + p + "/" + f;
    }, { unsafe: true });
  }

  // node_modules/core-js/modules/es.parse-float.js
  var $6 = require_export();
  var parseFloatImplementation = require_number_parse_float();
  $6({ global: true, forced: parseFloat != parseFloatImplementation }, {
    parseFloat: parseFloatImplementation
  });

  // node_modules/core-js/modules/es.number.to-precision.js
  "use strict";
  var $7 = require_export();
  var fails2 = require_fails();
  var thisNumberValue = require_this_number_value();
  var nativeToPrecision = 1 .toPrecision;
  var FORCED = fails2(function() {
    return nativeToPrecision.call(1, void 0) !== "1";
  }) || !fails2(function() {
    nativeToPrecision.call({});
  });
  $7({ target: "Number", proto: true, forced: FORCED }, {
    toPrecision: function toPrecision(precision) {
      return precision === void 0 ? nativeToPrecision.call(thisNumberValue(this)) : nativeToPrecision.call(thisNumberValue(this), precision);
    }
  });

  // node_modules/core-js/modules/es.array.concat.js
  "use strict";
  var $8 = require_export();
  var fails3 = require_fails();
  var isArray = require_is_array();
  var isObject = require_is_object();
  var toObject = require_to_object();
  var toLength2 = require_to_length();
  var createProperty = require_create_property();
  var arraySpeciesCreate = require_array_species_create();
  var arrayMethodHasSpeciesSupport = require_array_method_has_species_support();
  var wellKnownSymbol = require_well_known_symbol();
  var V8_VERSION = require_engine_v8_version();
  var IS_CONCAT_SPREADABLE = wellKnownSymbol("isConcatSpreadable");
  var MAX_SAFE_INTEGER = 9007199254740991;
  var MAXIMUM_ALLOWED_INDEX_EXCEEDED = "Maximum allowed index exceeded";
  var IS_CONCAT_SPREADABLE_SUPPORT = V8_VERSION >= 51 || !fails3(function() {
    var array = [];
    array[IS_CONCAT_SPREADABLE] = false;
    return array.concat()[0] !== array;
  });
  var SPECIES_SUPPORT = arrayMethodHasSpeciesSupport("concat");
  var isConcatSpreadable = function(O) {
    if (!isObject(O))
      return false;
    var spreadable = O[IS_CONCAT_SPREADABLE];
    return spreadable !== void 0 ? !!spreadable : isArray(O);
  };
  var FORCED2 = !IS_CONCAT_SPREADABLE_SUPPORT || !SPECIES_SUPPORT;
  $8({ target: "Array", proto: true, forced: FORCED2 }, {
    concat: function concat(arg) {
      var O = toObject(this);
      var A = arraySpeciesCreate(O, 0);
      var n = 0;
      var i, k, length, len, E;
      for (i = -1, length = arguments.length; i < length; i++) {
        E = i === -1 ? O : arguments[i];
        if (isConcatSpreadable(E)) {
          len = toLength2(E.length);
          if (n + len > MAX_SAFE_INTEGER)
            throw TypeError(MAXIMUM_ALLOWED_INDEX_EXCEEDED);
          for (k = 0; k < len; k++, n++)
            if (k in E)
              createProperty(A, n, E[k]);
        } else {
          if (n >= MAX_SAFE_INTEGER)
            throw TypeError(MAXIMUM_ALLOWED_INDEX_EXCEEDED);
          createProperty(A, n++, E);
        }
      }
      A.length = n;
      return A;
    }
  });

  // node_modules/core-js/modules/es.array.slice.js
  "use strict";
  var $9 = require_export();
  var isObject2 = require_is_object();
  var isArray2 = require_is_array();
  var toAbsoluteIndex = require_to_absolute_index();
  var toLength3 = require_to_length();
  var toIndexedObject = require_to_indexed_object();
  var createProperty2 = require_create_property();
  var wellKnownSymbol2 = require_well_known_symbol();
  var arrayMethodHasSpeciesSupport2 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT = arrayMethodHasSpeciesSupport2("slice");
  var SPECIES = wellKnownSymbol2("species");
  var nativeSlice = [].slice;
  var max2 = Math.max;
  $9({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT }, {
    slice: function slice(start, end) {
      var O = toIndexedObject(this);
      var length = toLength3(O.length);
      var k = toAbsoluteIndex(start, length);
      var fin = toAbsoluteIndex(end === void 0 ? length : end, length);
      var Constructor, result, n;
      if (isArray2(O)) {
        Constructor = O.constructor;
        if (typeof Constructor == "function" && (Constructor === Array || isArray2(Constructor.prototype))) {
          Constructor = void 0;
        } else if (isObject2(Constructor)) {
          Constructor = Constructor[SPECIES];
          if (Constructor === null)
            Constructor = void 0;
        }
        if (Constructor === Array || Constructor === void 0) {
          return nativeSlice.call(O, k, fin);
        }
      }
      result = new (Constructor === void 0 ? Array : Constructor)(max2(fin - k, 0));
      for (n = 0; k < fin; k++, n++)
        if (k in O)
          createProperty2(result, n, O[k]);
      result.length = n;
      return result;
    }
  });

  // node_modules/core-js/modules/es.array.splice.js
  "use strict";
  var $10 = require_export();
  var toAbsoluteIndex2 = require_to_absolute_index();
  var toInteger2 = require_to_integer();
  var toLength4 = require_to_length();
  var toObject2 = require_to_object();
  var arraySpeciesCreate2 = require_array_species_create();
  var createProperty3 = require_create_property();
  var arrayMethodHasSpeciesSupport3 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT2 = arrayMethodHasSpeciesSupport3("splice");
  var max3 = Math.max;
  var min2 = Math.min;
  var MAX_SAFE_INTEGER2 = 9007199254740991;
  var MAXIMUM_ALLOWED_LENGTH_EXCEEDED = "Maximum allowed length exceeded";
  $10({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT2 }, {
    splice: function splice(start, deleteCount) {
      var O = toObject2(this);
      var len = toLength4(O.length);
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
        actualDeleteCount = min2(max3(toInteger2(deleteCount), 0), len - actualStart);
      }
      if (len + insertCount - actualDeleteCount > MAX_SAFE_INTEGER2) {
        throw TypeError(MAXIMUM_ALLOWED_LENGTH_EXCEEDED);
      }
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
            delete O[to];
        }
        for (k = len; k > len - actualDeleteCount + insertCount; k--)
          delete O[k - 1];
      } else if (insertCount > actualDeleteCount) {
        for (k = len - actualDeleteCount; k > actualStart; k--) {
          from2 = k + actualDeleteCount - 1;
          to = k + insertCount - 1;
          if (from2 in O)
            O[to] = O[from2];
          else
            delete O[to];
        }
      }
      for (k = 0; k < insertCount; k++) {
        O[k + actualStart] = arguments[k + 2];
      }
      O.length = len - actualDeleteCount + insertCount;
      return A;
    }
  });

  // node_modules/core-js/modules/es.object.keys.js
  var $11 = require_export();
  var toObject3 = require_to_object();
  var nativeKeys = require_object_keys();
  var fails4 = require_fails();
  var FAILS_ON_PRIMITIVES = fails4(function() {
    nativeKeys(1);
  });
  $11({ target: "Object", stat: true, forced: FAILS_ON_PRIMITIVES }, {
    keys: function keys(it) {
      return nativeKeys(toObject3(it));
    }
  });

  // node_modules/core-js/modules/es.string.split.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic2 = require_fix_regexp_well_known_symbol_logic();
  var isRegExp = require_is_regexp();
  var anObject3 = require_an_object();
  var requireObjectCoercible2 = require_require_object_coercible();
  var speciesConstructor = require_species_constructor();
  var advanceStringIndex2 = require_advance_string_index();
  var toLength5 = require_to_length();
  var callRegExpExec = require_regexp_exec_abstract();
  var regexpExec = require_regexp_exec();
  var stickyHelpers = require_regexp_sticky_helpers();
  var UNSUPPORTED_Y = stickyHelpers.UNSUPPORTED_Y;
  var arrayPush = [].push;
  var min3 = Math.min;
  var MAX_UINT32 = 4294967295;
  fixRegExpWellKnownSymbolLogic2("split", 2, function(SPLIT, nativeSplit, maybeCallNative) {
    var internalSplit;
    if ("abbc".split(/(b)*/)[1] == "c" || "test".split(/(?:)/, -1).length != 4 || "ab".split(/(?:ab)*/).length != 2 || ".".split(/(.?)(.?)/).length != 4 || ".".split(/()()/).length > 1 || "".split(/.?/).length) {
      internalSplit = function(separator, limit) {
        var string = String(requireObjectCoercible2(this));
        var lim = limit === void 0 ? MAX_UINT32 : limit >>> 0;
        if (lim === 0)
          return [];
        if (separator === void 0)
          return [string];
        if (!isRegExp(separator)) {
          return nativeSplit.call(string, separator, lim);
        }
        var output = [];
        var flags2 = (separator.ignoreCase ? "i" : "") + (separator.multiline ? "m" : "") + (separator.unicode ? "u" : "") + (separator.sticky ? "y" : "");
        var lastLastIndex = 0;
        var separatorCopy = new RegExp(separator.source, flags2 + "g");
        var match, lastIndex, lastLength;
        while (match = regexpExec.call(separatorCopy, string)) {
          lastIndex = separatorCopy.lastIndex;
          if (lastIndex > lastLastIndex) {
            output.push(string.slice(lastLastIndex, match.index));
            if (match.length > 1 && match.index < string.length)
              arrayPush.apply(output, match.slice(1));
            lastLength = match[0].length;
            lastLastIndex = lastIndex;
            if (output.length >= lim)
              break;
          }
          if (separatorCopy.lastIndex === match.index)
            separatorCopy.lastIndex++;
        }
        if (lastLastIndex === string.length) {
          if (lastLength || !separatorCopy.test(""))
            output.push("");
        } else
          output.push(string.slice(lastLastIndex));
        return output.length > lim ? output.slice(0, lim) : output;
      };
    } else if ("0".split(void 0, 0).length) {
      internalSplit = function(separator, limit) {
        return separator === void 0 && limit === 0 ? [] : nativeSplit.call(this, separator, limit);
      };
    } else
      internalSplit = nativeSplit;
    return [
      function split(separator, limit) {
        var O = requireObjectCoercible2(this);
        var splitter = separator == void 0 ? void 0 : separator[SPLIT];
        return splitter !== void 0 ? splitter.call(separator, O, limit) : internalSplit.call(String(O), separator, limit);
      },
      function(regexp, limit) {
        var res = maybeCallNative(internalSplit, regexp, this, limit, internalSplit !== nativeSplit);
        if (res.done)
          return res.value;
        var rx = anObject3(regexp);
        var S = String(this);
        var C = speciesConstructor(rx, RegExp);
        var unicodeMatching = rx.unicode;
        var flags2 = (rx.ignoreCase ? "i" : "") + (rx.multiline ? "m" : "") + (rx.unicode ? "u" : "") + (UNSUPPORTED_Y ? "g" : "y");
        var splitter = new C(UNSUPPORTED_Y ? "^(?:" + rx.source + ")" : rx, flags2);
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
          var z = callRegExpExec(splitter, UNSUPPORTED_Y ? S.slice(q) : S);
          var e;
          if (z === null || (e = min3(toLength5(splitter.lastIndex + (UNSUPPORTED_Y ? q : 0)), S.length)) === p) {
            q = advanceStringIndex2(S, q, unicodeMatching);
          } else {
            A.push(S.slice(p, q));
            if (A.length === lim)
              return A;
            for (var i = 1; i <= z.length - 1; i++) {
              A.push(z[i]);
              if (A.length === lim)
                return A;
            }
            q = p = e;
          }
        }
        A.push(S.slice(p));
        return A;
      }
    ];
  }, UNSUPPORTED_Y);

  // srcts/src/utils/index.ts
  var import_jquery5 = __toModule(require_jquery());

  // srcts/src/window/pixelRatio.ts
  function windowDevicePixelRatio() {
    return window.devicePixelRatio;
  }

  // srcts/src/utils/blob.ts
  var import_jquery4 = __toModule(require_jquery());
  var blobBuilderClass;
  function setBlobBuilder(blobBuilderClass_) {
    blobBuilderClass = blobBuilderClass_;
    return;
  }
  function makeBlob(parts) {
    try {
      return new Blob(parts);
    } catch (e) {
      var blobBuilder = new blobBuilderClass();
      import_jquery4.default.each(parts, function(i, part) {
        blobBuilder.append(part);
      });
      return blobBuilder.getBlob();
    }
  }

  // srcts/src/utils/object.ts
  function hasOwnProperty(x, y) {
    return Object.prototype.hasOwnProperty.call(x, y);
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
    if (el.currentStyle)
      x = el.currentStyle[styleProp];
    else if (window.getComputedStyle) {
      var style = document.defaultView.getComputedStyle(el, null);
      if (style)
        x = style.getPropertyValue(styleProp);
    }
    return x;
  }
  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits) {
      str = "0" + str;
    }
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
      var size = {
        w: el.offsetWidth,
        h: el.offsetHeight
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
    function merge(sortfunc2, a, b) {
      var ia = 0;
      var ib = 0;
      var sorted = [];
      while (ia < a.length && ib < b.length) {
        if (sortfunc2(a[ia], b[ib]) <= 0) {
          sorted.push(a[ia++]);
        } else {
          sorted.push(b[ib++]);
        }
      }
      while (ia < a.length) {
        sorted.push(a[ia++]);
      }
      while (ib < b.length) {
        sorted.push(b[ib++]);
      }
      return sorted;
    }
    list = list.slice(0);
    for (var chunkSize = 1; chunkSize < list.length; chunkSize *= 2) {
      for (var i = 0; i < list.length; i += chunkSize * 2) {
        var listA = list.slice(i, i + chunkSize);
        var listB = list.slice(i + chunkSize, i + chunkSize * 2);
        var merged = merge(sortfunc, listA, listB);
        var args = [i, merged.length];
        Array.prototype.push.apply(args, merged);
        Array.prototype.splice.apply(list, args);
      }
    }
    return list;
  }
  var $escape = function $escape2(val) {
    return val.replace(/([!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~])/g, "\\$1");
  };
  function mapValues(obj, f) {
    var newObj = {};
    for (var _key in obj) {
      if (hasOwnProperty(obj, _key))
        newObj[_key] = f(obj[_key], _key, obj);
    }
    return newObj;
  }
  function isnan(x) {
    return typeof x === "number" && isNaN(x);
  }
  function _equal(x, y) {
    if (import_jquery5.default.type(x) === "object" && import_jquery5.default.type(y) === "object") {
      var xo = x;
      var yo = y;
      if (Object.keys(xo).length !== Object.keys(yo).length)
        return false;
      for (var prop in xo) {
        if (!hasOwnProperty(yo, prop) || !_equal(xo[prop], yo[prop]))
          return false;
      }
      return true;
    } else if (import_jquery5.default.type(x) === "array" && import_jquery5.default.type(y) === "array") {
      var xa = x;
      var ya = y;
      if (xa.length !== ya.length)
        return false;
      for (var i = 0; i < xa.length; i++) {
        if (!_equal(xa[i], ya[i]))
          return false;
      }
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
      a2 = versionParts(a2);
      b2 = versionParts(b2);
      var len = Math.min(a2.length, b2.length);
      var cmp;
      for (var i = 0; i < len; i++) {
        cmp = parseInt(a2[i], 10) - parseInt(b2[i], 10);
        if (cmp !== 0) {
          return cmp;
        }
      }
      return a2.length - b2.length;
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
  function toLowerCase(str) {
    return str.toLowerCase();
  }

  // srcts/src/bindings/registry.ts
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass2(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties2(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties2(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty2(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
        return el["data-input-id"] || el.id;
      }
    }, {
      key: "getType",
      value: function getType(el) {
        return false;
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
  "use strict";
  var $14 = require_export();
  var $find = require_array_iteration().find;
  var addToUnscopables = require_add_to_unscopables();
  var FIND = "find";
  var SKIPS_HOLES = true;
  if (FIND in [])
    Array(1)[FIND](function() {
      SKIPS_HOLES = false;
    });
  $14({ target: "Array", proto: true, forced: SKIPS_HOLES }, {
    find: function find(callbackfn) {
      return $find(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });
  addToUnscopables(FIND);

  // node_modules/core-js/modules/es.object.set-prototype-of.js
  var $15 = require_export();
  var setPrototypeOf = require_object_set_prototype_of();
  $15({ target: "Object", stat: true }, {
    setPrototypeOf: setPrototypeOf
  });

  // node_modules/core-js/modules/es.object.get-prototype-of.js
  var $16 = require_export();
  var fails5 = require_fails();
  var toObject4 = require_to_object();
  var nativeGetPrototypeOf = require_object_get_prototype_of();
  var CORRECT_PROTOTYPE_GETTER = require_correct_prototype_getter();
  var FAILS_ON_PRIMITIVES2 = fails5(function() {
    nativeGetPrototypeOf(1);
  });
  $16({ target: "Object", stat: true, forced: FAILS_ON_PRIMITIVES2, sham: !CORRECT_PROTOTYPE_GETTER }, {
    getPrototypeOf: function getPrototypeOf(it) {
      return nativeGetPrototypeOf(toObject4(it));
    }
  });

  // node_modules/core-js/modules/es.reflect.construct.js
  var $17 = require_export();
  var getBuiltIn = require_get_built_in();
  var aFunction = require_a_function();
  var anObject4 = require_an_object();
  var isObject3 = require_is_object();
  var create = require_object_create();
  var bind = require_function_bind();
  var fails6 = require_fails();
  var nativeConstruct = getBuiltIn("Reflect", "construct");
  var NEW_TARGET_BUG = fails6(function() {
    function F() {
    }
    return !(nativeConstruct(function() {
    }, [], F) instanceof F);
  });
  var ARGS_BUG = !fails6(function() {
    nativeConstruct(function() {
    });
  });
  var FORCED3 = NEW_TARGET_BUG || ARGS_BUG;
  $17({ target: "Reflect", stat: true, forced: FORCED3, sham: FORCED3 }, {
    construct: function construct(Target, args) {
      aFunction(Target);
      anObject4(args);
      var newTarget = arguments.length < 3 ? Target : aFunction(arguments[2]);
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
        $args.push.apply($args, args);
        return new (bind.apply(Target, $args))();
      }
      var proto = newTarget.prototype;
      var instance = create(isObject3(proto) ? proto : Object.prototype);
      var result = Function.apply.call(Target, instance, args);
      return isObject3(result) ? result : instance;
    }
  });

  // node_modules/core-js/modules/es.symbol.js
  "use strict";
  var $18 = require_export();
  var global2 = require_global();
  var getBuiltIn2 = require_get_built_in();
  var IS_PURE = require_is_pure();
  var DESCRIPTORS2 = require_descriptors();
  var NATIVE_SYMBOL = require_native_symbol();
  var USE_SYMBOL_AS_UID = require_use_symbol_as_uid();
  var fails7 = require_fails();
  var has = require_has();
  var isArray3 = require_is_array();
  var isObject4 = require_is_object();
  var anObject5 = require_an_object();
  var toObject5 = require_to_object();
  var toIndexedObject2 = require_to_indexed_object();
  var toPrimitive = require_to_primitive();
  var createPropertyDescriptor = require_create_property_descriptor();
  var nativeObjectCreate = require_object_create();
  var objectKeys = require_object_keys();
  var getOwnPropertyNamesModule = require_object_get_own_property_names();
  var getOwnPropertyNamesExternal = require_object_get_own_property_names_external();
  var getOwnPropertySymbolsModule = require_object_get_own_property_symbols();
  var getOwnPropertyDescriptorModule = require_object_get_own_property_descriptor();
  var definePropertyModule = require_object_define_property();
  var propertyIsEnumerableModule = require_object_property_is_enumerable();
  var createNonEnumerableProperty = require_create_non_enumerable_property();
  var redefine3 = require_redefine();
  var shared = require_shared();
  var sharedKey = require_shared_key();
  var hiddenKeys = require_hidden_keys();
  var uid = require_uid();
  var wellKnownSymbol3 = require_well_known_symbol();
  var wrappedWellKnownSymbolModule = require_well_known_symbol_wrapped();
  var defineWellKnownSymbol = require_define_well_known_symbol();
  var setToStringTag = require_set_to_string_tag();
  var InternalStateModule = require_internal_state();
  var $forEach = require_array_iteration().forEach;
  var HIDDEN = sharedKey("hidden");
  var SYMBOL = "Symbol";
  var PROTOTYPE = "prototype";
  var TO_PRIMITIVE = wellKnownSymbol3("toPrimitive");
  var setInternalState = InternalStateModule.set;
  var getInternalState = InternalStateModule.getterFor(SYMBOL);
  var ObjectPrototype = Object[PROTOTYPE];
  var $Symbol = global2.Symbol;
  var $stringify = getBuiltIn2("JSON", "stringify");
  var nativeGetOwnPropertyDescriptor = getOwnPropertyDescriptorModule.f;
  var nativeDefineProperty = definePropertyModule.f;
  var nativeGetOwnPropertyNames = getOwnPropertyNamesExternal.f;
  var nativePropertyIsEnumerable = propertyIsEnumerableModule.f;
  var AllSymbols = shared("symbols");
  var ObjectPrototypeSymbols = shared("op-symbols");
  var StringToSymbolRegistry = shared("string-to-symbol-registry");
  var SymbolToStringRegistry = shared("symbol-to-string-registry");
  var WellKnownSymbolsStore = shared("wks");
  var QObject = global2.QObject;
  var USE_SETTER = !QObject || !QObject[PROTOTYPE] || !QObject[PROTOTYPE].findChild;
  var setSymbolDescriptor = DESCRIPTORS2 && fails7(function() {
    return nativeObjectCreate(nativeDefineProperty({}, "a", {
      get: function() {
        return nativeDefineProperty(this, "a", { value: 7 }).a;
      }
    })).a != 7;
  }) ? function(O, P, Attributes) {
    var ObjectPrototypeDescriptor = nativeGetOwnPropertyDescriptor(ObjectPrototype, P);
    if (ObjectPrototypeDescriptor)
      delete ObjectPrototype[P];
    nativeDefineProperty(O, P, Attributes);
    if (ObjectPrototypeDescriptor && O !== ObjectPrototype) {
      nativeDefineProperty(ObjectPrototype, P, ObjectPrototypeDescriptor);
    }
  } : nativeDefineProperty;
  var wrap = function(tag, description) {
    var symbol = AllSymbols[tag] = nativeObjectCreate($Symbol[PROTOTYPE]);
    setInternalState(symbol, {
      type: SYMBOL,
      tag: tag,
      description: description
    });
    if (!DESCRIPTORS2)
      symbol.description = description;
    return symbol;
  };
  var isSymbol = USE_SYMBOL_AS_UID ? function(it) {
    return typeof it == "symbol";
  } : function(it) {
    return Object(it) instanceof $Symbol;
  };
  var $defineProperty = function defineProperty2(O, P, Attributes) {
    if (O === ObjectPrototype)
      $defineProperty(ObjectPrototypeSymbols, P, Attributes);
    anObject5(O);
    var key = toPrimitive(P, true);
    anObject5(Attributes);
    if (has(AllSymbols, key)) {
      if (!Attributes.enumerable) {
        if (!has(O, HIDDEN))
          nativeDefineProperty(O, HIDDEN, createPropertyDescriptor(1, {}));
        O[HIDDEN][key] = true;
      } else {
        if (has(O, HIDDEN) && O[HIDDEN][key])
          O[HIDDEN][key] = false;
        Attributes = nativeObjectCreate(Attributes, { enumerable: createPropertyDescriptor(0, false) });
      }
      return setSymbolDescriptor(O, key, Attributes);
    }
    return nativeDefineProperty(O, key, Attributes);
  };
  var $defineProperties = function defineProperties(O, Properties) {
    anObject5(O);
    var properties = toIndexedObject2(Properties);
    var keys2 = objectKeys(properties).concat($getOwnPropertySymbols(properties));
    $forEach(keys2, function(key) {
      if (!DESCRIPTORS2 || $propertyIsEnumerable.call(properties, key))
        $defineProperty(O, key, properties[key]);
    });
    return O;
  };
  var $create = function create2(O, Properties) {
    return Properties === void 0 ? nativeObjectCreate(O) : $defineProperties(nativeObjectCreate(O), Properties);
  };
  var $propertyIsEnumerable = function propertyIsEnumerable(V) {
    var P = toPrimitive(V, true);
    var enumerable = nativePropertyIsEnumerable.call(this, P);
    if (this === ObjectPrototype && has(AllSymbols, P) && !has(ObjectPrototypeSymbols, P))
      return false;
    return enumerable || !has(this, P) || !has(AllSymbols, P) || has(this, HIDDEN) && this[HIDDEN][P] ? enumerable : true;
  };
  var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor(O, P) {
    var it = toIndexedObject2(O);
    var key = toPrimitive(P, true);
    if (it === ObjectPrototype && has(AllSymbols, key) && !has(ObjectPrototypeSymbols, key))
      return;
    var descriptor = nativeGetOwnPropertyDescriptor(it, key);
    if (descriptor && has(AllSymbols, key) && !(has(it, HIDDEN) && it[HIDDEN][key])) {
      descriptor.enumerable = true;
    }
    return descriptor;
  };
  var $getOwnPropertyNames = function getOwnPropertyNames(O) {
    var names = nativeGetOwnPropertyNames(toIndexedObject2(O));
    var result = [];
    $forEach(names, function(key) {
      if (!has(AllSymbols, key) && !has(hiddenKeys, key))
        result.push(key);
    });
    return result;
  };
  var $getOwnPropertySymbols = function getOwnPropertySymbols(O) {
    var IS_OBJECT_PROTOTYPE = O === ObjectPrototype;
    var names = nativeGetOwnPropertyNames(IS_OBJECT_PROTOTYPE ? ObjectPrototypeSymbols : toIndexedObject2(O));
    var result = [];
    $forEach(names, function(key) {
      if (has(AllSymbols, key) && (!IS_OBJECT_PROTOTYPE || has(ObjectPrototype, key))) {
        result.push(AllSymbols[key]);
      }
    });
    return result;
  };
  if (!NATIVE_SYMBOL) {
    $Symbol = function Symbol2() {
      if (this instanceof $Symbol)
        throw TypeError("Symbol is not a constructor");
      var description = !arguments.length || arguments[0] === void 0 ? void 0 : String(arguments[0]);
      var tag = uid(description);
      var setter = function(value) {
        if (this === ObjectPrototype)
          setter.call(ObjectPrototypeSymbols, value);
        if (has(this, HIDDEN) && has(this[HIDDEN], tag))
          this[HIDDEN][tag] = false;
        setSymbolDescriptor(this, tag, createPropertyDescriptor(1, value));
      };
      if (DESCRIPTORS2 && USE_SETTER)
        setSymbolDescriptor(ObjectPrototype, tag, { configurable: true, set: setter });
      return wrap(tag, description);
    };
    redefine3($Symbol[PROTOTYPE], "toString", function toString2() {
      return getInternalState(this).tag;
    });
    redefine3($Symbol, "withoutSetter", function(description) {
      return wrap(uid(description), description);
    });
    propertyIsEnumerableModule.f = $propertyIsEnumerable;
    definePropertyModule.f = $defineProperty;
    getOwnPropertyDescriptorModule.f = $getOwnPropertyDescriptor;
    getOwnPropertyNamesModule.f = getOwnPropertyNamesExternal.f = $getOwnPropertyNames;
    getOwnPropertySymbolsModule.f = $getOwnPropertySymbols;
    wrappedWellKnownSymbolModule.f = function(name) {
      return wrap(wellKnownSymbol3(name), name);
    };
    if (DESCRIPTORS2) {
      nativeDefineProperty($Symbol[PROTOTYPE], "description", {
        configurable: true,
        get: function description() {
          return getInternalState(this).description;
        }
      });
      if (!IS_PURE) {
        redefine3(ObjectPrototype, "propertyIsEnumerable", $propertyIsEnumerable, { unsafe: true });
      }
    }
  }
  $18({ global: true, wrap: true, forced: !NATIVE_SYMBOL, sham: !NATIVE_SYMBOL }, {
    Symbol: $Symbol
  });
  $forEach(objectKeys(WellKnownSymbolsStore), function(name) {
    defineWellKnownSymbol(name);
  });
  $18({ target: SYMBOL, stat: true, forced: !NATIVE_SYMBOL }, {
    "for": function(key) {
      var string = String(key);
      if (has(StringToSymbolRegistry, string))
        return StringToSymbolRegistry[string];
      var symbol = $Symbol(string);
      StringToSymbolRegistry[string] = symbol;
      SymbolToStringRegistry[symbol] = string;
      return symbol;
    },
    keyFor: function keyFor(sym) {
      if (!isSymbol(sym))
        throw TypeError(sym + " is not a symbol");
      if (has(SymbolToStringRegistry, sym))
        return SymbolToStringRegistry[sym];
    },
    useSetter: function() {
      USE_SETTER = true;
    },
    useSimple: function() {
      USE_SETTER = false;
    }
  });
  $18({ target: "Object", stat: true, forced: !NATIVE_SYMBOL, sham: !DESCRIPTORS2 }, {
    create: $create,
    defineProperty: $defineProperty,
    defineProperties: $defineProperties,
    getOwnPropertyDescriptor: $getOwnPropertyDescriptor
  });
  $18({ target: "Object", stat: true, forced: !NATIVE_SYMBOL }, {
    getOwnPropertyNames: $getOwnPropertyNames,
    getOwnPropertySymbols: $getOwnPropertySymbols
  });
  $18({ target: "Object", stat: true, forced: fails7(function() {
    getOwnPropertySymbolsModule.f(1);
  }) }, {
    getOwnPropertySymbols: function getOwnPropertySymbols2(it) {
      return getOwnPropertySymbolsModule.f(toObject5(it));
    }
  });
  if ($stringify) {
    FORCED_JSON_STRINGIFY = !NATIVE_SYMBOL || fails7(function() {
      var symbol = $Symbol();
      return $stringify([symbol]) != "[null]" || $stringify({ a: symbol }) != "{}" || $stringify(Object(symbol)) != "{}";
    });
    $18({ target: "JSON", stat: true, forced: FORCED_JSON_STRINGIFY }, {
      stringify: function stringify(it, replacer, space) {
        var args = [it];
        var index = 1;
        var $replacer;
        while (arguments.length > index)
          args.push(arguments[index++]);
        $replacer = replacer;
        if (!isObject4(replacer) && it === void 0 || isSymbol(it))
          return;
        if (!isArray3(replacer))
          replacer = function(key, value) {
            if (typeof $replacer == "function")
              value = $replacer.call(this, key, value);
            if (!isSymbol(value))
              return value;
          };
        args[1] = replacer;
        return $stringify.apply(null, args);
      }
    });
  }
  var FORCED_JSON_STRINGIFY;
  if (!$Symbol[PROTOTYPE][TO_PRIMITIVE]) {
    createNonEnumerableProperty($Symbol[PROTOTYPE], TO_PRIMITIVE, $Symbol[PROTOTYPE].valueOf);
  }
  setToStringTag($Symbol, SYMBOL);
  hiddenKeys[HIDDEN] = true;

  // node_modules/core-js/modules/es.symbol.description.js
  "use strict";
  var $19 = require_export();
  var DESCRIPTORS3 = require_descriptors();
  var global3 = require_global();
  var has2 = require_has();
  var isObject5 = require_is_object();
  var defineProperty3 = require_object_define_property().f;
  var copyConstructorProperties = require_copy_constructor_properties();
  var NativeSymbol = global3.Symbol;
  if (DESCRIPTORS3 && typeof NativeSymbol == "function" && (!("description" in NativeSymbol.prototype) || NativeSymbol().description !== void 0)) {
    EmptyStringDescriptionStore = {};
    SymbolWrapper = function Symbol2() {
      var description = arguments.length < 1 || arguments[0] === void 0 ? void 0 : String(arguments[0]);
      var result = this instanceof SymbolWrapper ? new NativeSymbol(description) : description === void 0 ? NativeSymbol() : NativeSymbol(description);
      if (description === "")
        EmptyStringDescriptionStore[result] = true;
      return result;
    };
    copyConstructorProperties(SymbolWrapper, NativeSymbol);
    symbolPrototype = SymbolWrapper.prototype = NativeSymbol.prototype;
    symbolPrototype.constructor = SymbolWrapper;
    symbolToString = symbolPrototype.toString;
    native = String(NativeSymbol("test")) == "Symbol(test)";
    regexp = /^Symbol\((.*)\)[^)]+$/;
    defineProperty3(symbolPrototype, "description", {
      configurable: true,
      get: function description() {
        var symbol = isObject5(this) ? this.valueOf() : this;
        var string = symbolToString.call(symbol);
        if (has2(EmptyStringDescriptionStore, symbol))
          return "";
        var desc = native ? string.slice(7, -1) : string.replace(regexp, "$1");
        return desc === "" ? void 0 : desc;
      }
    });
    $19({ global: true, forced: true }, {
      Symbol: SymbolWrapper
    });
  }
  var EmptyStringDescriptionStore;
  var SymbolWrapper;
  var symbolPrototype;
  var symbolToString;
  var native;
  var regexp;

  // node_modules/core-js/modules/es.symbol.iterator.js
  var defineWellKnownSymbol2 = require_define_well_known_symbol();
  defineWellKnownSymbol2("iterator");

  // srcts/src/bindings/input/checkbox.ts
  var import_es_array_iterator = __toModule(require_es_array_iterator());

  // node_modules/core-js/modules/es.string.iterator.js
  "use strict";
  var charAt = require_string_multibyte().charAt;
  var InternalStateModule2 = require_internal_state();
  var defineIterator = require_define_iterator();
  var STRING_ITERATOR = "String Iterator";
  var setInternalState2 = InternalStateModule2.set;
  var getInternalState2 = InternalStateModule2.getterFor(STRING_ITERATOR);
  defineIterator(String, "String", function(iterated) {
    setInternalState2(this, {
      type: STRING_ITERATOR,
      string: String(iterated),
      index: 0
    });
  }, function next() {
    var state = getInternalState2(this);
    var string = state.string;
    var index = state.index;
    var point;
    if (index >= string.length)
      return { value: void 0, done: true };
    point = charAt(string, index);
    state.index += point.length;
    return { value: point, done: false };
  });

  // node_modules/core-js/modules/web.dom-collections.iterator.js
  var global4 = require_global();
  var DOMIterables = require_dom_iterables();
  var ArrayIteratorMethods = require_es_array_iterator();
  var createNonEnumerableProperty2 = require_create_non_enumerable_property();
  var wellKnownSymbol4 = require_well_known_symbol();
  var ITERATOR = wellKnownSymbol4("iterator");
  var TO_STRING_TAG = wellKnownSymbol4("toStringTag");
  var ArrayValues = ArrayIteratorMethods.values;
  for (var COLLECTION_NAME in DOMIterables) {
    Collection = global4[COLLECTION_NAME];
    CollectionPrototype = Collection && Collection.prototype;
    if (CollectionPrototype) {
      if (CollectionPrototype[ITERATOR] !== ArrayValues)
        try {
          createNonEnumerableProperty2(CollectionPrototype, ITERATOR, ArrayValues);
        } catch (error) {
          CollectionPrototype[ITERATOR] = ArrayValues;
        }
      if (!CollectionPrototype[TO_STRING_TAG]) {
        createNonEnumerableProperty2(CollectionPrototype, TO_STRING_TAG, COLLECTION_NAME);
      }
      if (DOMIterables[COLLECTION_NAME])
        for (METHOD_NAME in ArrayIteratorMethods) {
          if (CollectionPrototype[METHOD_NAME] !== ArrayIteratorMethods[METHOD_NAME])
            try {
              createNonEnumerableProperty2(CollectionPrototype, METHOD_NAME, ArrayIteratorMethods[METHOD_NAME]);
            } catch (error) {
              CollectionPrototype[METHOD_NAME] = ArrayIteratorMethods[METHOD_NAME];
            }
        }
    }
  }
  var Collection;
  var CollectionPrototype;
  var METHOD_NAME;

  // srcts/src/bindings/input/checkbox.ts
  var import_jquery6 = __toModule(require_jquery());
  function _typeof(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass3(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties3(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties3(Constructor, staticProps);
    return Constructor;
  }
  function _inherits(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf(subClass, superClass);
  }
  function _setPrototypeOf(o, p) {
    _setPrototypeOf = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn(self2, call) {
    if (call && (_typeof(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery6.default)(scope).find('input[type="checkbox"]');
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
        (0, import_jquery6.default)(el).on("change.checkboxInputBinding", function() {
          callback(true);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery6.default)(el).off(".checkboxInputBinding");
      }
    }, {
      key: "getState",
      value: function getState(el) {
        return {
          label: (0, import_jquery6.default)(el).parent().find("span").text(),
          value: el.checked
        };
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        if (hasOwnProperty(data, "value"))
          el.checked = data.value;
        if (hasOwnProperty(data, "label"))
          (0, import_jquery6.default)(el).parent().find("span").text(data.label);
        (0, import_jquery6.default)(el).trigger("change");
      }
    }]);
    return CheckboxInputBinding2;
  }(InputBinding);

  // node_modules/core-js/modules/es.string.trim.js
  "use strict";
  var $21 = require_export();
  var $trim = require_string_trim().trim;
  var forcedStringTrimMethod = require_string_trim_forced();
  $21({ target: "String", proto: true, forced: forcedStringTrimMethod("trim") }, {
    trim: function trim() {
      return $trim(this);
    }
  });

  // srcts/src/bindings/input/checkboxgroup.ts
  var import_es_array_iterator2 = __toModule(require_es_array_iterator());
  var import_jquery7 = __toModule(require_jquery());
  function _typeof2(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof2 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof2 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof2(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass4(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties4(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties4(Constructor, staticProps);
    return Constructor;
  }
  function _inherits2(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf2(subClass, superClass);
  }
  function _setPrototypeOf2(o, p) {
    _setPrototypeOf2 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn2(self2, call) {
    if (call && (_typeof2(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf2 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf2(o);
  }
  function getLabelNode(el) {
    return (0, import_jquery7.default)(el).find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel(obj) {
    if (obj.parentNode.tagName === "LABEL") {
      return (0, import_jquery7.default)(obj.parentNode).find("span").text().trim();
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
        return (0, import_jquery7.default)(scope).find(".shiny-input-checkboxgroup");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $objs = (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"]:checked');
        var values = new Array($objs.length);
        for (var i = 0; i < $objs.length; i++) {
          values[i] = $objs[i].value;
        }
        return values;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"]').prop("checked", false);
        if (value instanceof Array) {
          for (var i = 0; i < value.length; i++) {
            (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]').prop("checked", true);
          }
        } else {
          (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $objs = (0, import_jquery7.default)('input:checkbox[name="' + $escape(el.id) + '"]');
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
        var $el = (0, import_jquery7.default)(el);
        if (hasOwnProperty(data, "options")) {
          $el.find("div.shiny-options-group").remove();
          $el.find("label.checkbox").remove();
          $el.append(data.options);
        }
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        updateLabel(data.label, getLabelNode(el));
        (0, import_jquery7.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery7.default)(el).on("change.checkboxGroupInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery7.default)(el).off(".checkboxGroupInputBinding");
      }
    }]);
    return CheckboxGroupInputBinding2;
  }(InputBinding);

  // node_modules/core-js/modules/es.number.constructor.js
  "use strict";
  var DESCRIPTORS4 = require_descriptors();
  var global5 = require_global();
  var isForced = require_is_forced();
  var redefine4 = require_redefine();
  var has3 = require_has();
  var classof = require_classof_raw();
  var inheritIfRequired = require_inherit_if_required();
  var toPrimitive2 = require_to_primitive();
  var fails8 = require_fails();
  var create3 = require_object_create();
  var getOwnPropertyNames2 = require_object_get_own_property_names().f;
  var getOwnPropertyDescriptor2 = require_object_get_own_property_descriptor().f;
  var defineProperty4 = require_object_define_property().f;
  var trim2 = require_string_trim().trim;
  var NUMBER = "Number";
  var NativeNumber = global5[NUMBER];
  var NumberPrototype = NativeNumber.prototype;
  var BROKEN_CLASSOF = classof(create3(NumberPrototype)) == NUMBER;
  var toNumber = function(argument) {
    var it = toPrimitive2(argument, false);
    var first, third, radix, maxCode, digits, length, index, code;
    if (typeof it == "string" && it.length > 2) {
      it = trim2(it);
      first = it.charCodeAt(0);
      if (first === 43 || first === 45) {
        third = it.charCodeAt(2);
        if (third === 88 || third === 120)
          return NaN;
      } else if (first === 48) {
        switch (it.charCodeAt(1)) {
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
        digits = it.slice(2);
        length = digits.length;
        for (index = 0; index < length; index++) {
          code = digits.charCodeAt(index);
          if (code < 48 || code > maxCode)
            return NaN;
        }
        return parseInt(digits, radix);
      }
    }
    return +it;
  };
  if (isForced(NUMBER, !NativeNumber(" 0o1") || !NativeNumber("0b1") || NativeNumber("+0x1"))) {
    NumberWrapper = function Number2(value) {
      var it = arguments.length < 1 ? 0 : value;
      var dummy = this;
      return dummy instanceof NumberWrapper && (BROKEN_CLASSOF ? fails8(function() {
        NumberPrototype.valueOf.call(dummy);
      }) : classof(dummy) != NUMBER) ? inheritIfRequired(new NativeNumber(toNumber(it)), dummy, NumberWrapper) : toNumber(it);
    };
    for (keys2 = DESCRIPTORS4 ? getOwnPropertyNames2(NativeNumber) : "MAX_VALUE,MIN_VALUE,NaN,NEGATIVE_INFINITY,POSITIVE_INFINITY,EPSILON,isFinite,isInteger,isNaN,isSafeInteger,MAX_SAFE_INTEGER,MIN_SAFE_INTEGER,parseFloat,parseInt,isInteger,fromString,range".split(","), j = 0; keys2.length > j; j++) {
      if (has3(NativeNumber, key = keys2[j]) && !has3(NumberWrapper, key)) {
        defineProperty4(NumberWrapper, key, getOwnPropertyDescriptor2(NativeNumber, key));
      }
    }
    NumberWrapper.prototype = NumberPrototype;
    NumberPrototype.constructor = NumberWrapper;
    redefine4(global5, NUMBER, NumberWrapper);
  }
  var NumberWrapper;
  var keys2;
  var j;
  var key;

  // srcts/src/bindings/input/number.ts
  var import_es_array_iterator4 = __toModule(require_es_array_iterator());
  var import_jquery9 = __toModule(require_jquery());

  // node_modules/core-js/modules/es.reflect.get.js
  var $23 = require_export();
  var isObject6 = require_is_object();
  var anObject6 = require_an_object();
  var has4 = require_has();
  var getOwnPropertyDescriptorModule2 = require_object_get_own_property_descriptor();
  var getPrototypeOf2 = require_object_get_prototype_of();
  function get(target, propertyKey) {
    var receiver = arguments.length < 3 ? target : arguments[2];
    var descriptor, prototype;
    if (anObject6(target) === receiver)
      return target[propertyKey];
    if (descriptor = getOwnPropertyDescriptorModule2.f(target, propertyKey))
      return has4(descriptor, "value") ? descriptor.value : descriptor.get === void 0 ? void 0 : descriptor.get.call(receiver);
    if (isObject6(prototype = getPrototypeOf2(target)))
      return get(prototype, propertyKey, receiver);
  }
  $23({ target: "Reflect", stat: true }, {
    get: get
  });

  // node_modules/core-js/modules/es.object.get-own-property-descriptor.js
  var $24 = require_export();
  var fails9 = require_fails();
  var toIndexedObject3 = require_to_indexed_object();
  var nativeGetOwnPropertyDescriptor2 = require_object_get_own_property_descriptor().f;
  var DESCRIPTORS5 = require_descriptors();
  var FAILS_ON_PRIMITIVES3 = fails9(function() {
    nativeGetOwnPropertyDescriptor2(1);
  });
  var FORCED4 = !DESCRIPTORS5 || FAILS_ON_PRIMITIVES3;
  $24({ target: "Object", stat: true, forced: FORCED4, sham: !DESCRIPTORS5 }, {
    getOwnPropertyDescriptor: function getOwnPropertyDescriptor3(it, key) {
      return nativeGetOwnPropertyDescriptor2(toIndexedObject3(it), key);
    }
  });

  // srcts/src/bindings/input/text.ts
  var import_es_array_iterator3 = __toModule(require_es_array_iterator());
  var import_jquery8 = __toModule(require_jquery());
  function _typeof3(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof3 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof3 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof3(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass5(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties5(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties5(Constructor, staticProps);
    return Constructor;
  }
  function _get(target, property, receiver) {
    if (typeof Reflect !== "undefined" && Reflect.get) {
      _get = Reflect.get;
    } else {
      _get = function _get2(target2, property2, receiver2) {
        var base = _superPropBase(target2, property2);
        if (!base)
          return;
        var desc = Object.getOwnPropertyDescriptor(base, property2);
        if (desc.get) {
          return desc.get.call(receiver2);
        }
        return desc.value;
      };
    }
    return _get(target, property, receiver || target);
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
    if (superClass)
      _setPrototypeOf3(subClass, superClass);
  }
  function _setPrototypeOf3(o, p) {
    _setPrototypeOf3 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn3(self2, call) {
    if (call && (_typeof3(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf3 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf3(o);
  }
  function getLabelNode2(el) {
    return (0, import_jquery8.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
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
        var $inputs = (0, import_jquery8.default)(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
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
        (0, import_jquery8.default)(el).on("keyup.textInputBinding input.textInputBinding", function() {
          callback(true);
        });
        (0, import_jquery8.default)(el).on("change.textInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery8.default)(el).off(".textInputBinding");
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
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        updateLabel(data.label, getLabelNode2(el));
        if (hasOwnProperty(data, "placeholder"))
          el.placeholder = data.placeholder;
        (0, import_jquery8.default)(el).trigger("change");
      }
    }]);
    return TextInputBinding2;
  }(TextInputBindingBase);

  // srcts/src/bindings/input/number.ts
  function _typeof4(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof4 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof4 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof4(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass6(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties6(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties6(Constructor, staticProps);
    return Constructor;
  }
  function _inherits4(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf4(subClass, superClass);
  }
  function _setPrototypeOf4(o, p) {
    _setPrototypeOf4 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn4(self2, call) {
    if (call && (_typeof4(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf4 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf4(o);
  }
  function getLabelNode3(el) {
    return (0, import_jquery9.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
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
        return (0, import_jquery9.default)(scope).find('input[type="number"]');
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var numberVal = (0, import_jquery9.default)(el).val();
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
        if (hasOwnProperty(data, "value"))
          el.value = data.value;
        if (hasOwnProperty(data, "min"))
          el.min = data.min;
        if (hasOwnProperty(data, "max"))
          el.max = data.max;
        if (hasOwnProperty(data, "step"))
          el.step = data.step;
        updateLabel(data.label, getLabelNode3(el));
        (0, import_jquery9.default)(el).trigger("change");
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
  var import_es_array_iterator5 = __toModule(require_es_array_iterator());
  var import_jquery10 = __toModule(require_jquery());
  function _typeof5(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof5 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof5 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof5(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass7(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties7(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties7(Constructor, staticProps);
    return Constructor;
  }
  function _inherits5(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf5(subClass, superClass);
  }
  function _setPrototypeOf5(o, p) {
    _setPrototypeOf5 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn5(self2, call) {
    if (call && (_typeof5(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf5 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery10.default)(scope).find('input[type="password"]');
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
  var import_es_array_iterator6 = __toModule(require_es_array_iterator());
  var import_jquery11 = __toModule(require_jquery());
  function _typeof6(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof6 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof6 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof6(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass8(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties8(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties8(Constructor, staticProps);
    return Constructor;
  }
  function _inherits6(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf6(subClass, superClass);
  }
  function _setPrototypeOf6(o, p) {
    _setPrototypeOf6 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn6(self2, call) {
    if (call && (_typeof6(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf6 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery11.default)(scope).find("textarea");
      }
    }]);
    return TextareaInputBinding2;
  }(TextInputBinding);

  // srcts/src/bindings/input/radio.ts
  var import_es_array_iterator7 = __toModule(require_es_array_iterator());
  var import_jquery12 = __toModule(require_jquery());
  function _typeof7(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof7 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof7 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof7(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass9(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties9(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties9(Constructor, staticProps);
    return Constructor;
  }
  function _inherits7(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf7(subClass, superClass);
  }
  function _setPrototypeOf7(o, p) {
    _setPrototypeOf7 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn7(self2, call) {
    if (call && (_typeof7(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf7 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf7(o);
  }
  function getLabelNode4(el) {
    return (0, import_jquery12.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function getLabel2(obj) {
    if (obj.parentNode.tagName === "LABEL") {
      return (0, import_jquery12.default)(obj.parentNode).find("span").text().trim();
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
        return (0, import_jquery12.default)(scope).find(".shiny-input-radiogroup");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var checkedItems = (0, import_jquery12.default)('input:radio[name="' + $escape(el.id) + '"]:checked');
        if (checkedItems.length === 0) {
          return null;
        }
        return checkedItems.val();
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (import_jquery12.default.isArray(value) && value.length === 0) {
          (0, import_jquery12.default)('input:radio[name="' + $escape(el.id) + '"]').prop("checked", false);
        } else {
          (0, import_jquery12.default)('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
        }
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $objs = (0, import_jquery12.default)('input:radio[name="' + $escape(el.id) + '"]');
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
        var $el = (0, import_jquery12.default)(el);
        if (hasOwnProperty(data, "options")) {
          $el.find("div.shiny-options-group").remove();
          $el.find("label.radio").remove();
          $el.append(data.options);
        }
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        updateLabel(data.label, getLabelNode4(el));
        (0, import_jquery12.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery12.default)(el).on("change.radioInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery12.default)(el).off(".radioInputBinding");
      }
    }]);
    return RadioInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/date.ts
  var import_es_array_iterator8 = __toModule(require_es_array_iterator());
  var import_jquery13 = __toModule(require_jquery());
  function _typeof8(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof8 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof8 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof8(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass10(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties10(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties10(Constructor, staticProps);
    return Constructor;
  }
  function _inherits8(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf8(subClass, superClass);
  }
  function _setPrototypeOf8(o, p) {
    _setPrototypeOf8 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn8(self2, call) {
    if (call && (_typeof8(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf8 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery13.default)(scope).find(".shiny-date-input");
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
        (0, import_jquery13.default)(el).on("keyup.dateInputBinding input.dateInputBinding", function() {
          callback(true);
        });
        (0, import_jquery13.default)(el).on("changeDate.dateInputBinding change.dateInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery13.default)(el).off(".dateInputBinding");
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
        var $input = (0, import_jquery13.default)(el).find("input");
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
        return (0, import_jquery13.default)(el).find('label[for="' + $escape(el.id) + '"]');
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
        if (date === void 0)
          return;
        if (date === null) {
          (0, import_jquery13.default)(el).bsDatepicker("setStartDate", null);
          return;
        }
        var parsedDate = this._newDate(date);
        if (parsedDate === null)
          return;
        date = parsedDate;
        if (isNaN(date.valueOf()))
          return;
        var curValue = (0, import_jquery13.default)(el).bsDatepicker("getUTCDate");
        (0, import_jquery13.default)(el).bsDatepicker("setStartDate", this._utcDateAsLocal(date));
        if (date && curValue && date.getTime() > curValue.getTime()) {
          (0, import_jquery13.default)(el).bsDatepicker("clearDates");
        } else {
          (0, import_jquery13.default)(el).bsDatepicker("setUTCDate", curValue);
        }
      }
    }, {
      key: "_setMax",
      value: function _setMax(el, date) {
        if (date === void 0)
          return;
        if (date === null) {
          (0, import_jquery13.default)(el).bsDatepicker("setEndDate", null);
          return;
        }
        var parsedDate = this._newDate(date);
        if (parsedDate === null)
          return;
        date = parsedDate;
        if (isNaN(date.valueOf()))
          return;
        var curValue = (0, import_jquery13.default)(el).bsDatepicker("getUTCDate");
        (0, import_jquery13.default)(el).bsDatepicker("setEndDate", this._utcDateAsLocal(date));
        if (date && curValue && date.getTime() < curValue.getTime()) {
          (0, import_jquery13.default)(el).bsDatepicker("clearDates");
        } else {
          (0, import_jquery13.default)(el).bsDatepicker("setUTCDate", curValue);
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
        var date = (0, import_jquery13.default)(el).find("input").bsDatepicker("getUTCDate");
        return formatDateUTC(date);
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (value === null) {
          (0, import_jquery13.default)(el).find("input").val("").bsDatepicker("update");
          return;
        }
        var date = this._newDate(value);
        if (date === null) {
          return;
        }
        if (isNaN(date.valueOf()))
          return;
        (0, import_jquery13.default)(el).find("input").bsDatepicker("setUTCDate", date);
      }
    }, {
      key: "getState",
      value: function getState(el) {
        var $el = (0, import_jquery13.default)(el);
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
        var $input = (0, import_jquery13.default)(el).find("input");
        updateLabel(data.label, this._getLabelNode(el));
        if (hasOwnProperty(data, "min"))
          this._setMin($input[0], data.min);
        if (hasOwnProperty(data, "max"))
          this._setMax($input[0], data.max);
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        (0, import_jquery13.default)(el).trigger("change");
      }
    }]);
    return DateInputBinding2;
  }(DateInputBindingBase);

  // srcts/src/bindings/input/slider.ts
  var import_es_regexp_exec2 = __toModule(require_es_regexp_exec());
  var import_es_array_iterator9 = __toModule(require_es_array_iterator());
  var import_jquery14 = __toModule(require_jquery());
  function _typeof9(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof9 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof9 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof9(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass11(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties11(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties11(Constructor, staticProps);
    return Constructor;
  }
  function _inherits9(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf9(subClass, superClass);
  }
  function _setPrototypeOf9(o, p) {
    _setPrototypeOf9 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn9(self2, call) {
    if (call && (_typeof9(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf9 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
    return (0, import_jquery14.default)(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
  function numValues(el) {
    if ((0, import_jquery14.default)(el).data("ionRangeSlider").options.type === "double")
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
        if (!import_jquery14.default.fn.ionRangeSlider) {
          return (0, import_jquery14.default)();
        }
        return (0, import_jquery14.default)(scope).find("input.js-range-slider");
      }
    }, {
      key: "getType",
      value: function getType(el) {
        var dataType = (0, import_jquery14.default)(el).data("data-type");
        if (dataType === "date")
          return "shiny.date";
        else if (dataType === "datetime")
          return "shiny.datetime";
        else
          return false;
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $el = (0, import_jquery14.default)(el);
        var result = (0, import_jquery14.default)(el).data("ionRangeSlider").result;
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
        var $el = (0, import_jquery14.default)(el);
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
        (0, import_jquery14.default)(el).on("change.sliderInputBinding", function() {
          callback(!(0, import_jquery14.default)(el).data("immediate") && !(0, import_jquery14.default)(el).data("animating"));
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery14.default)(el).off(".sliderInputBinding");
      }
    }, {
      key: "receiveMessage",
      value: function receiveMessage(el, data) {
        var $el = (0, import_jquery14.default)(el);
        var slider = $el.data("ionRangeSlider");
        var msg = {};
        if (hasOwnProperty(data, "value")) {
          if (numValues(el) === 2 && data.value instanceof Array) {
            msg.from = data.value[0];
            msg.to = data.value[1];
          } else {
            msg.from = data.value;
          }
        }
        var sliderFeatures = ["min", "max", "step"];
        for (var i = 0; i < sliderFeatures.length; i++) {
          var feats = sliderFeatures[i];
          if (hasOwnProperty(data, feats)) {
            msg[feats] = data[feats];
          }
        }
        updateLabel(data.label, getLabelNode5(el));
        var domElements = ["data-type", "time-format", "timezone"];
        for (var _i = 0; _i < domElements.length; _i++) {
          var elem = domElements[_i];
          if (hasOwnProperty(data, elem)) {
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
        var $el = (0, import_jquery14.default)(el);
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
  (0, import_jquery14.default)(document).on("click", ".slider-animate-button", function(evt) {
    evt.preventDefault();
    var self2 = (0, import_jquery14.default)(this);
    var target = (0, import_jquery14.default)("#" + $escape(self2.attr("data-target-id")));
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
  var import_es_array_iterator10 = __toModule(require_es_array_iterator());
  var import_jquery15 = __toModule(require_jquery());
  function _typeof10(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof10 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof10 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof10(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass12(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties12(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties12(Constructor, staticProps);
    return Constructor;
  }
  function _inherits10(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf10(subClass, superClass);
  }
  function _setPrototypeOf10(o, p) {
    _setPrototypeOf10 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn10(self2, call) {
    if (call && (_typeof10(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf10 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf10(o);
  }
  function getLabelNode6(el) {
    return (0, import_jquery15.default)(el).find('label[for="' + $escape(el.id) + '"]');
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
        return (0, import_jquery15.default)(scope).find(".shiny-date-range-input");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var $inputs = (0, import_jquery15.default)(el).find("input");
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
        var $inputs = (0, import_jquery15.default)(el).find("input");
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
        var $el = (0, import_jquery15.default)(el);
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
        var $el = (0, import_jquery15.default)(el);
        var $inputs = $el.find("input");
        var $startinput = $inputs.eq(0);
        var $endinput = $inputs.eq(1);
        updateLabel(data.label, getLabelNode6(el));
        if (hasOwnProperty(data, "min")) {
          this._setMin($startinput[0], data.min);
          this._setMin($endinput[0], data.min);
        }
        if (hasOwnProperty(data, "max")) {
          this._setMax($startinput[0], data.max);
          this._setMax($endinput[0], data.max);
        }
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        $el.trigger("change");
      }
    }, {
      key: "initialize",
      value: function initialize(el) {
        var $el = (0, import_jquery15.default)(el);
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
        (0, import_jquery15.default)(el).on("keyup.dateRangeInputBinding input.dateRangeInputBinding", function() {
          callback(true);
        });
        (0, import_jquery15.default)(el).on("changeDate.dateRangeInputBinding change.dateRangeInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery15.default)(el).off(".dateRangeInputBinding");
      }
    }]);
    return DateRangeInputBinding2;
  }(DateInputBindingBase);

  // srcts/src/bindings/input/selectInput.ts
  var import_es_array_iterator11 = __toModule(require_es_array_iterator());
  var import_jquery16 = __toModule(require_jquery());

  // srcts/src/utils/eval.ts
  var indirectEval = eval;

  // srcts/src/bindings/input/selectInput.ts
  function _typeof11(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof11 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof11 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof11(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass13(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties13(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties13(Constructor, staticProps);
    return Constructor;
  }
  function _inherits11(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf11(subClass, superClass);
  }
  function _setPrototypeOf11(o, p) {
    _setPrototypeOf11 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn11(self2, call) {
    if (call && (_typeof11(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf11 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf11(o);
  }
  function getLabelNode7(el) {
    var escapedId = $escape(el.id);
    if (isSelectize(el)) {
      escapedId += "-selectized";
    }
    return (0, import_jquery16.default)(el).parent().parent().find('label[for="' + escapedId + '"]');
  }
  function isSelectize(el) {
    var config = (0, import_jquery16.default)(el).parent().find('script[data-for="' + $escape(el.id) + '"]');
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
        return (0, import_jquery16.default)(scope).find("select");
      }
    }, {
      key: "getType",
      value: function getType(el) {
        var $el = (0, import_jquery16.default)(el);
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
        return (0, import_jquery16.default)(el).val();
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        if (!isSelectize(el)) {
          (0, import_jquery16.default)(el).val(value);
        } else {
          var selectize = this._selectize(el);
          if (selectize) {
            selectize.setValue(value);
          }
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
        var $el = (0, import_jquery16.default)(el);
        var selectize;
        if (hasOwnProperty(data, "options")) {
          selectize = this._selectize(el);
          if (selectize)
            selectize.destroy();
          $el.empty().append(data.options);
          this._selectize(el);
        }
        if (hasOwnProperty(data, "config")) {
          $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
          this._selectize(el, true);
        }
        if (hasOwnProperty(data, "url")) {
          selectize = this._selectize(el);
          selectize.clearOptions();
          var loaded = false;
          selectize.settings.load = function(query, callback) {
            var settings = selectize.settings;
            import_jquery16.default.ajax({
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
                import_jquery16.default.each(res, function(index, elem) {
                  var optgroupId = elem[settings.optgroupField || "optgroup"];
                  var optgroup = {};
                  optgroup[settings.optgroupLabelField || "label"] = optgroupId;
                  optgroup[settings.optgroupValueField || "value"] = optgroupId;
                  selectize.addOptionGroup(optgroupId, optgroup);
                });
                callback(res);
                if (!loaded) {
                  if (hasOwnProperty(data, "value")) {
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
        } else if (hasOwnProperty(data, "value")) {
          this.setValue(el, data.value);
        }
        updateLabel(data.label, getLabelNode7(el));
        (0, import_jquery16.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        var _this = this;
        (0, import_jquery16.default)(el).on("change.selectInputBinding", function() {
          if (el.nonempty && _this.getValue(el) === "") {
            return;
          }
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery16.default)(el).off(".selectInputBinding");
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
        if (!import_jquery16.default.fn.selectize)
          return void 0;
        var $el = (0, import_jquery16.default)(el);
        var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
        if (config.length === 0)
          return void 0;
        var options = import_jquery16.default.extend({
          labelField: "label",
          valueField: "value",
          searchField: ["label"]
        }, JSON.parse(config.html()));
        if (typeof config.data("nonempty") !== "undefined") {
          el.nonempty = true;
          options = import_jquery16.default.extend(options, {
            onItemRemove: function onItemRemove(value) {
              if (this.getValue() === "")
                (0, import_jquery16.default)("select#" + $escape(el.id)).empty().append((0, import_jquery16.default)("<option/>", {
                  value: value,
                  selected: true
                })).trigger("change");
            },
            onDropdownClose: function onDropdownClose() {
              if (this.getValue() === "")
                this.setValue((0, import_jquery16.default)("select#" + $escape(el.id)).val());
            }
          });
        } else {
          el.nonempty = false;
        }
        if (config.data("eval") instanceof Array)
          import_jquery16.default.each(config.data("eval"), function(i, x) {
            options[x] = indirectEval("(" + options[x] + ")");
          });
        var control = $el.selectize(options)[0].selectize;
        if (update) {
          var settings = import_jquery16.default.extend(control.settings, options);
          control.destroy();
          control = $el.selectize(settings)[0].selectize;
        }
        return control;
      }
    }]);
    return SelectInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/actionbutton.ts
  var import_es_array_iterator12 = __toModule(require_es_array_iterator());
  var import_jquery17 = __toModule(require_jquery());
  function _typeof12(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof12 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof12 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof12(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass14(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties14(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties14(Constructor, staticProps);
    return Constructor;
  }
  function _inherits12(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf12(subClass, superClass);
  }
  function _setPrototypeOf12(o, p) {
    _setPrototypeOf12 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn12(self2, call) {
    if (call && (_typeof12(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf12 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery17.default)(scope).find(".action-button");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        return (0, import_jquery17.default)(el).data("val") || 0;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        (0, import_jquery17.default)(el).data("val", value);
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
        (0, import_jquery17.default)(el).on("click.actionButtonInputBinding", function() {
          var $el = (0, import_jquery17.default)(this);
          var val = $el.data("val") || 0;
          $el.data("val", val + 1);
          callback(false);
        });
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
        var $el = (0, import_jquery17.default)(el);
        var label = $el.text();
        var icon = "";
        if ($el.find("i[class]").length > 0) {
          var iconHtml = $el.find("i[class]")[0];
          if (iconHtml === $el.children()[0]) {
            icon = (0, import_jquery17.default)(iconHtml).prop("outerHTML");
          }
        }
        if (hasOwnProperty(data, "label"))
          label = data.label;
        if (hasOwnProperty(data, "icon")) {
          icon = data.icon;
          if (icon.length === 0)
            icon = "";
        }
        $el.html(icon + " " + label);
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery17.default)(el).off(".actionButtonInputBinding");
      }
    }]);
    return ActionButtonInputBinding2;
  }(InputBinding);
  (0, import_jquery17.default)(document).on("click", "a.action-button", function(e) {
    e.preventDefault();
  });

  // srcts/src/bindings/input/tabinput.ts
  var import_es_array_iterator13 = __toModule(require_es_array_iterator());
  var import_jquery18 = __toModule(require_jquery());
  function _typeof13(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof13 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof13 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof13(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass15(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties15(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties15(Constructor, staticProps);
    return Constructor;
  }
  function _inherits13(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf13(subClass, superClass);
  }
  function _setPrototypeOf13(o, p) {
    _setPrototypeOf13 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn13(self2, call) {
    if (call && (_typeof13(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf13 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery18.default)(scope).find("ul.nav.shiny-tab-input");
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var anchor = isBS3() ? (0, import_jquery18.default)(el).find("li:not(.dropdown).active > a") : (0, import_jquery18.default)(el).find(".nav-link:not(.dropdown-toggle).active, .dropdown-menu .dropdown-item.active");
        if (anchor.length === 1)
          return getTabName(anchor);
        return null;
      }
    }, {
      key: "setValue",
      value: function setValue(el, value) {
        var success = false;
        if (value) {
          var anchors = isBS3() ? (0, import_jquery18.default)(el).find("li:not(.dropdown) > a") : (0, import_jquery18.default)(el).find(".nav-link:not(.dropdown-toggle), .dropdown-menu .dropdown-item");
          anchors.each(function() {
            if (getTabName((0, import_jquery18.default)(this)) === value) {
              (0, import_jquery18.default)(this).tab("show");
              success = true;
              return false;
            }
            return;
          });
        }
        if (!success) {
          (0, import_jquery18.default)(el).trigger("change");
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
        if (hasOwnProperty(data, "value"))
          this.setValue(el, data.value);
        (0, import_jquery18.default)(el).trigger("change");
      }
    }, {
      key: "subscribe",
      value: function subscribe(el, callback) {
        (0, import_jquery18.default)(el).on("change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding", function() {
          callback(false);
        });
      }
    }, {
      key: "unsubscribe",
      value: function unsubscribe(el) {
        (0, import_jquery18.default)(el).off(".bootstrapTabInputBinding");
      }
    }]);
    return BootstrapTabInputBinding2;
  }(InputBinding);

  // srcts/src/bindings/input/fileinput.ts
  var import_es_array_iterator15 = __toModule(require_es_array_iterator());
  var import_jquery21 = __toModule(require_jquery());

  // node_modules/core-js/modules/es.array.map.js
  "use strict";
  var $36 = require_export();
  var $map = require_array_iteration().map;
  var arrayMethodHasSpeciesSupport4 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT3 = arrayMethodHasSpeciesSupport4("map");
  $36({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT3 }, {
    map: function map(callbackfn) {
      return $map(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/file/fileProcessor.ts
  var import_es_array_iterator14 = __toModule(require_es_array_iterator());
  var import_jquery20 = __toModule(require_jquery());

  // srcts/src/events/inputChanged.ts
  var import_jquery19 = __toModule(require_jquery());
  function triggerFileInputChanged(name, value, binding, el, inputType, onEl) {
    var evt = import_jquery19.default.Event("shiny:inputchanged");
    evt.name = name;
    evt.value = value;
    evt.binding = binding;
    evt.el = el;
    evt.inputType = inputType;
    (0, import_jquery19.default)(onEl).trigger(evt);
    return evt;
  }

  // srcts/src/shiny/initedMethods.ts
  var fullShinyObj = null;
  function setShinyObj(shiny) {
    fullShinyObj = shiny;
  }
  function shinySetInputValue(name, value, opts) {
    fullShinyObj.setInputValue(name, value, opts);
  }
  function shinyShinyApp() {
    return fullShinyObj.shinyapp;
  }
  function setShinyUser(user) {
    fullShinyObj.user = user;
  }
  function shinyForgetLastInputValue(name) {
    fullShinyObj.forgetLastInputValue(name);
  }
  function shinyBindAll(scope) {
    fullShinyObj.bindAll(scope);
  }
  function shinyUnbindAll(scope) {
    var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
    fullShinyObj.unbindAll(scope, includeSelf);
  }
  function shinyInitializeInputs(scope) {
    fullShinyObj.initializeInputs(scope);
  }
  function shinyAppBindOutput(id, binding) {
    fullShinyObj.shinyapp.bindOutput(id, binding);
  }
  function shinyAppUnbindOutput(id, binding) {
    return fullShinyObj.shinyapp.unbindOutput(id, binding);
  }
  function getShinyOnCustomMessage() {
    return fullShinyObj.oncustommessage;
  }
  var fileInputBinding;
  function getFileInputBinding() {
    return fileInputBinding;
  }
  function setFileInputBinding(fileInputBinding_) {
    fileInputBinding = fileInputBinding_;
  }
  function getShinyCreateWebsocket() {
    return fullShinyObj.createSocket;
  }

  // srcts/src/file/fileProcessor.ts
  function _typeof14(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof14 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof14 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof14(obj);
  }
  function _inherits14(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf14(subClass, superClass);
  }
  function _setPrototypeOf14(o, p) {
    _setPrototypeOf14 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn14(self2, call) {
    if (call && (_typeof14(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf14 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass16(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties16(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties16(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty3(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var FileProcessor = /* @__PURE__ */ function() {
    function FileProcessor2(files) {
      var exec$run = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : true;
      _classCallCheck16(this, FileProcessor2);
      _defineProperty3(this, "files", void 0);
      _defineProperty3(this, "fileIndex", -1);
      _defineProperty3(this, "aborted", false);
      _defineProperty3(this, "completed", false);
      this.files = files;
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
        import_jquery20.default.each(files, function(i, file) {
          _this3.totalBytes += file.size;
        });
        var fileInfo = import_jquery20.default.map(files, function(file, i) {
          return {
            name: file.name,
            size: file.size,
            type: file.type
          };
          i;
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
        import_jquery20.default.ajax(this.uploadUrl, {
          type: "POST",
          cache: false,
          xhr: function xhr() {
            var xhrVal = import_jquery20.default.ajaxSettings.xhr();
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
        var fileInfo = import_jquery20.default.map(this.files, function(file, i) {
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
          (0, import_jquery20.default)(evt.el).val("");
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
        return (0, import_jquery20.default)("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
      }
    }, {
      key: "$bar",
      value: function $bar() {
        return (0, import_jquery20.default)("#" + $escape(this.id) + "_progress.shiny-file-input-progress .progress-bar");
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
  function _typeof15(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof15 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof15 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof15(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass17(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties17(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties17(Constructor, staticProps);
    return Constructor;
  }
  function _inherits15(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf15(subClass, superClass);
  }
  function _setPrototypeOf15(o, p) {
    _setPrototypeOf15 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn15(self2, call) {
    if (call && (_typeof15(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf15 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf15(o);
  }
  var zoneActive = "shiny-file-input-active";
  var zoneOver = "shiny-file-input-over";
  function zoneOf(el) {
    return (0, import_jquery21.default)(el).closest("div.input-group");
  }
  function enableDraghover(el) {
    var $el = (0, import_jquery21.default)(el);
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
    return (0, import_jquery21.default)(el).off(".draghover");
  }
  function enableDocumentEvents() {
    var $doc = (0, import_jquery21.default)("html");
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
    var $doc = (0, import_jquery21.default)("html");
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
    var files = e.originalEvent.dataTransfer.files, $el = (0, import_jquery21.default)(el);
    if (files === void 0 || files === null) {
      console.log("Dropping files is not supported on this browser. (no FileList)");
    } else if (!canSetFiles(files)) {
      $el.val("");
      uploadDroppedFilesIE10Plus(el, files);
    } else {
      $el.val("");
      el.files = e.originalEvent.dataTransfer.files;
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
    var $el = (0, import_jquery21.default)(el);
    abortCurrentUpload($el);
    setFileText($el, files);
    $el.data("currentUploader", new FileUploader(shinyShinyApp(), fileInputBindingGetId(el), files, el));
  }
  function uploadFiles(evt) {
    var $el = (0, import_jquery21.default)(evt.target);
    abortCurrentUpload($el);
    var files = evt.target.files;
    var id = fileInputBindingGetId(evt.target);
    if (files.length === 0)
      return;
    setFileText($el, files);
    $el.data("currentUploader", new FileUploader(shinyShinyApp(), id, files, evt.target));
  }
  var $fileInputs = (0, import_jquery21.default)();
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
        return (0, import_jquery21.default)(scope).find('input[type="file"]');
      }
    }, {
      key: "getId",
      value: function getId(el) {
        return fileInputBindingGetId(el);
      }
    }, {
      key: "getValue",
      value: function getValue(el) {
        var data = (0, import_jquery21.default)(el).attr("data-restore");
        if (data) {
          var dataParsed = JSON.parse(data);
          var $fileText = (0, import_jquery21.default)(el).closest("div.input-group").find("input[type=text]");
          if (dataParsed.name.length === 1) {
            $fileText.val(dataParsed.name[0]);
          } else {
            $fileText.val(dataParsed.name.length + " files");
          }
          var $progress = (0, import_jquery21.default)(el).closest("div.form-group").find(".progress");
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
        (0, import_jquery21.default)(el).on("change.fileInputBinding", uploadFiles);
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
        var $el = (0, import_jquery21.default)(el), $zone = zoneOf(el);
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
  var import_es_array_iterator16 = __toModule(require_es_array_iterator());
  var import_jquery23 = __toModule(require_jquery());

  // node_modules/core-js/modules/es.array.join.js
  "use strict";
  var $40 = require_export();
  var IndexedObject = require_indexed_object();
  var toIndexedObject4 = require_to_indexed_object();
  var arrayMethodIsStrict2 = require_array_method_is_strict();
  var nativeJoin = [].join;
  var ES3_STRINGS = IndexedObject != Object;
  var STRICT_METHOD2 = arrayMethodIsStrict2("join", ",");
  $40({ target: "Array", proto: true, forced: ES3_STRINGS || !STRICT_METHOD2 }, {
    join: function join(separator) {
      return nativeJoin.call(toIndexedObject4(this), separator === void 0 ? "," : separator);
    }
  });

  // srcts/src/bindings/output/outputBinding.ts
  var import_es_regexp_exec3 = __toModule(require_es_regexp_exec());
  var import_jquery22 = __toModule(require_jquery());
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass18(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties18(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties18(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty4(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
        return el["data-input-id"] || el.id;
      }
    }, {
      key: "onValueChange",
      value: function onValueChange(el, data) {
        this.clearError(el);
        this.renderValue(el, data);
      }
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
          (0, import_jquery22.default)(el).empty();
          return;
        }
        var errClass = "shiny-output-error";
        if (err.type !== null) {
          errClass = errClass + " " + import_jquery22.default.map(asArray(err.type), function(type) {
            return errClass + "-" + type;
          }).join(" ");
        }
        (0, import_jquery22.default)(el).addClass(errClass).text(err.message);
      }
    }, {
      key: "clearError",
      value: function clearError(el) {
        (0, import_jquery22.default)(el).attr("class", function(i, c) {
          return c.replace(/(^|\s)shiny-output-error\S*/g, "");
        });
      }
    }, {
      key: "showProgress",
      value: function showProgress(el, show3) {
        var recalcClass = "recalculating";
        if (show3)
          (0, import_jquery22.default)(el).addClass(recalcClass);
        else
          (0, import_jquery22.default)(el).removeClass(recalcClass);
      }
    }]);
    return OutputBinding2;
  }();

  // srcts/src/bindings/output/text.ts
  function _typeof16(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof16 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof16 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof16(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass19(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties19(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties19(Constructor, staticProps);
    return Constructor;
  }
  function _inherits16(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf16(subClass, superClass);
  }
  function _setPrototypeOf16(o, p) {
    _setPrototypeOf16 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn16(self2, call) {
    if (call && (_typeof16(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf16 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery23.default)(scope).find(".shiny-text-output");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        (0, import_jquery23.default)(el).text(data);
      }
    }]);
    return TextOutputBinding2;
  }(OutputBinding);

  // srcts/src/bindings/output/downloadlink.ts
  var import_es_array_iterator17 = __toModule(require_es_array_iterator());
  var import_jquery24 = __toModule(require_jquery());
  function _typeof17(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof17 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof17 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof17(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass20(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties20(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties20(Constructor, staticProps);
    return Constructor;
  }
  function _inherits17(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf17(subClass, superClass);
  }
  function _setPrototypeOf17(o, p) {
    _setPrototypeOf17 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn17(self2, call) {
    if (call && (_typeof17(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf17 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery24.default)(scope).find("a.shiny-download-link");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        (0, import_jquery24.default)(el).attr("href", data);
      }
    }]);
    return DownloadLinkOutputBinding2;
  }(OutputBinding);
  (0, import_jquery24.default)(document).on("click.shinyDownloadLink", "a.shiny-download-link", function(e) {
    e;
    var evt = jQuery.Event("shiny:filedownload");
    evt.name = this.id;
    evt.href = this.href;
    (0, import_jquery24.default)(document).trigger(evt);
  });

  // srcts/src/bindings/output/datatable.ts
  var import_es_regexp_exec4 = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.search.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic3 = require_fix_regexp_well_known_symbol_logic();
  var anObject7 = require_an_object();
  var requireObjectCoercible3 = require_require_object_coercible();
  var sameValue = require_same_value();
  var regExpExec2 = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic3("search", 1, function(SEARCH, nativeSearch, maybeCallNative) {
    return [
      function search(regexp) {
        var O = requireObjectCoercible3(this);
        var searcher = regexp == void 0 ? void 0 : regexp[SEARCH];
        return searcher !== void 0 ? searcher.call(regexp, O) : new RegExp(regexp)[SEARCH](String(O));
      },
      function(regexp) {
        var res = maybeCallNative(nativeSearch, regexp, this);
        if (res.done)
          return res.value;
        var rx = anObject7(regexp);
        var S = String(this);
        var previousLastIndex = rx.lastIndex;
        if (!sameValue(previousLastIndex, 0))
          rx.lastIndex = 0;
        var result = regExpExec2(rx, S);
        if (!sameValue(rx.lastIndex, previousLastIndex))
          rx.lastIndex = previousLastIndex;
        return result === null ? -1 : result.index;
      }
    ];
  });

  // srcts/src/bindings/output/datatable.ts
  var import_es_array_iterator18 = __toModule(require_es_array_iterator());
  var import_jquery25 = __toModule(require_jquery());

  // srcts/src/time/debounce.ts
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass21(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties21(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties21(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty5(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
        this.func.apply(this.target, this.args);
        this.args = null;
      }
    }]);
    return Debouncer2;
  }();
  function debounce(threshold, func) {
    var timerId = null;
    return function() {
      var _this2 = this;
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
        func.apply(_this2, args);
      }, threshold);
    };
  }

  // srcts/src/time/invoke.ts
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass22(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties22(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties22(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty6(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass23(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties23(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties23(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty7(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
        var _this = this;
        for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
          args[_key] = arguments[_key];
        }
        this.args = args;
        if (this.timerId === null) {
          this.$invoke();
          this.timerId = setTimeout(function() {
            var _this$normalCall;
            if (_this.timerId === null)
              return;
            _this.$clearTimer();
            if (args.length > 0)
              (_this$normalCall = _this.normalCall).apply.apply(_this$normalCall, [_this].concat(args));
          }, this.delayMs);
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
        this.func.apply(this.target, this.args);
        this.args = null;
      }
    }]);
    return Throttler2;
  }();

  // srcts/src/bindings/output/datatable.ts
  function _typeof18(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof18 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof18 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof18(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass24(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties24(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties24(Constructor, staticProps);
    return Constructor;
  }
  function _inherits18(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf18(subClass, superClass);
  }
  function _setPrototypeOf18(o, p) {
    _setPrototypeOf18 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn18(self2, call) {
    if (call && (_typeof18(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf18 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery25.default)(scope).find(".shiny-datatable-output");
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
        var $el = (0, import_jquery25.default)(el).empty();
        if (!_data || !_data.colnames)
          return;
        var colnames = import_jquery25.default.makeArray(_data.colnames);
        var header = import_jquery25.default.map(colnames, function(x) {
          return "<th>" + x + "</th>";
        }).join("");
        header = "<thead><tr>" + header + "</tr></thead>";
        var footer = "";
        if (_data.options === null || _data.options.searching !== false) {
          footer = import_jquery25.default.map(colnames, function(x) {
            return '<th><input type="text" placeholder="' + escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) + '" /></th>';
          }).join("");
          footer = "<tfoot>" + footer + "</tfoot>";
        }
        var content = '<table class="table table-striped table-hover">' + header + footer + "</table>";
        $el.append(content);
        if (_data.evalOptions)
          import_jquery25.default.each(_data.evalOptions, function(i, x) {
            _data.options[x] = indirectEval("(" + _data.options[x] + ")");
          });
        var searchCI = _data.options === null || typeof _data.options.search === "undefined" || _data.options.search.caseInsensitive !== false;
        var oTable = (0, import_jquery25.default)(el).children("table").DataTable(import_jquery25.default.extend({
          processing: true,
          serverSide: true,
          order: [],
          orderClasses: false,
          pageLength: 25,
          ajax: {
            url: _data.action,
            type: "POST",
            data: function data(d) {
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
          import_jquery25.default.each(oTable.settings()[0].aoColumns, function(i, x) {
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
  var import_es_array_iterator20 = __toModule(require_es_array_iterator());
  var import_jquery28 = __toModule(require_jquery());

  // srcts/src/shiny/render.ts
  var import_es_regexp_exec6 = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.object.entries.js
  var $45 = require_export();
  var $entries = require_object_to_array().entries;
  $45({ target: "Object", stat: true }, {
    entries: function entries(O) {
      return $entries(O);
    }
  });

  // srcts/src/shiny/render.ts
  var import_es_array_iterator19 = __toModule(require_es_array_iterator());

  // node_modules/core-js/modules/es.array.from.js
  var $46 = require_export();
  var from = require_array_from();
  var checkCorrectnessOfIteration = require_check_correctness_of_iteration();
  var INCORRECT_ITERATION = !checkCorrectnessOfIteration(function(iterable) {
    Array.from(iterable);
  });
  $46({ target: "Array", stat: true, forced: INCORRECT_ITERATION }, {
    from: from
  });

  // srcts/src/shiny/render.ts
  var import_jquery27 = __toModule(require_jquery());

  // srcts/src/shiny/sendImageSize.ts
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass25(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties25(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties25(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty8(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
  var import_es_regexp_exec5 = __toModule(require_es_regexp_exec());
  var import_jquery26 = __toModule(require_jquery());
  var reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
  var reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;
  var knownSingletons = {};
  function renderHtml(html, el, where) {
    var processed = processHtml(html);
    addToHead(processed.head);
    register(processed.singletons);
    if (where === "replace") {
      (0, import_jquery26.default)(el).html(processed.html);
    } else {
      var elElements;
      if (el instanceof HTMLElement) {
        elElements = [el];
      } else {
        elElements = el.toArray();
      }
      import_jquery26.default.each(elElements, function(i, el2) {
        el2.insertAdjacentHTML(toLowerCase(where), processed.html);
      });
    }
    return processed;
  }
  function register(s) {
    import_jquery26.default.extend(knownSingletons, s);
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
      var tempDiv = (0, import_jquery26.default)("<div>" + head + "</div>").get(0);
      var $head = (0, import_jquery26.default)("head");
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
  function _slicedToArray(arr, i) {
    return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
  }
  function _nonIterableRest() {
    throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
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
    for (var i = 0, arr2 = new Array(len); i < len; i++) {
      arr2[i] = arr[i];
    }
    return arr2;
  }
  function _iterableToArrayLimit(arr, i) {
    var _i = arr && (typeof Symbol !== "undefined" && arr[Symbol.iterator] || arr["@@iterator"]);
    if (_i == null)
      return;
    var _arr = [];
    var _n = true;
    var _d = false;
    var _s, _e;
    try {
      for (_i = _i.call(arr); !(_n = (_s = _i.next()).done); _n = true) {
        _arr.push(_s.value);
        if (i && _arr.length === i)
          break;
      }
    } catch (err) {
      _d = true;
      _e = err;
    } finally {
      try {
        if (!_n && _i["return"] != null)
          _i["return"]();
      } finally {
        if (_d)
          throw _e;
      }
    }
    return _arr;
  }
  function _arrayWithHoles(arr) {
    if (Array.isArray(arr))
      return arr;
  }
  function _typeof19(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof19 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof19 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof19(obj);
  }
  function renderDependencies(dependencies) {
    if (dependencies) {
      import_jquery27.default.each(dependencies, function(i, dep) {
        renderDependency(dep);
      });
    }
  }
  function renderContent(el, content) {
    var where = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : "replace";
    if (where === "replace") {
      shinyUnbindAll(el);
    }
    var html;
    var dependencies = [];
    if (content === null) {
      html = "";
    } else if (typeof content === "string") {
      html = content;
    } else if (_typeof19(content) === "object") {
      html = content.html;
      dependencies = content.deps || [];
    }
    renderHtml2(html, el, dependencies, where);
    var scope = el;
    if (where === "replace") {
      shinyInitializeInputs(el);
      shinyBindAll(el);
    } else {
      var $parent = (0, import_jquery27.default)(el).parent();
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
  function renderHtml2(html, el, dependencies) {
    var where = arguments.length > 3 && arguments[3] !== void 0 ? arguments[3] : "replace";
    renderDependencies(dependencies);
    return renderHtml(html, el, where);
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
  function renderDependency(dep) {
    var restyle = needsRestyle(dep);
    if (hasOwnProperty(htmlDependencies, dep.name) && !restyle)
      return false;
    registerDependency(dep.name, dep.version);
    var href = dep.src.href;
    var $head = (0, import_jquery27.default)("head").first();
    if (dep.meta && !restyle) {
      var metas = import_jquery27.default.map(asArray(dep.meta), function(obj, idx) {
        var name = Object.keys(obj)[0];
        return (0, import_jquery27.default)("<meta>").attr("name", name).attr("content", obj[name]);
        idx;
      });
      $head.append(metas);
    }
    if (dep.stylesheet) {
      var links = import_jquery27.default.map(asArray(dep.stylesheet), function(stylesheet) {
        return (0, import_jquery27.default)("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
      });
      if (!restyle) {
        $head.append(links);
      } else {
        var refreshStyle = function refreshStyle2(href2, oldSheet) {
          var xhr = new XMLHttpRequest();
          xhr.open("GET", href2);
          xhr.onload = function() {
            var id = "shiny_restyle_" + href2.split("?restyle")[0].replace(/\W/g, "_");
            var oldStyle = $head.find("style#" + id);
            var newStyle = (0, import_jquery27.default)("<style>").attr("id", id).html(xhr.responseText);
            $head.append(newStyle);
            oldStyle.remove();
            removeSheet(oldSheet);
            sendImageSizeFns.transitioned();
          };
          xhr.send();
        };
        var findSheet = function findSheet2(href2) {
          for (var i = 0; i < document.styleSheets.length; i++) {
            var sheet = document.styleSheets[i];
            if (typeof sheet.href === "string" && sheet.href.indexOf(href2) > -1) {
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
          (0, import_jquery27.default)(sheet.ownerNode).remove();
        };
        import_jquery27.default.map(links, function(link) {
          var oldSheet = findSheet(link.attr("href"));
          var href2 = link.attr("href") + "?restyle=" + new Date().getTime();
          if (isIE()) {
            refreshStyle(href2, oldSheet);
          } else {
            link.attr("href", href2);
            link.attr("onload", function() {
              var $dummyEl = (0, import_jquery27.default)("<div>").css("transition", "0.1s all").css("position", "absolute").css("top", "-1000px").css("left", "0");
              $dummyEl.one("transitionend", function() {
                $dummyEl.remove();
                removeSheet(oldSheet);
                sendImageSizeFns.transitioned();
              });
              (0, import_jquery27.default)(document.body).append($dummyEl);
              var color = "#" + Math.floor(Math.random() * 16777215).toString(16);
              setTimeout(function() {
                return $dummyEl.css("color", color);
              }, 10);
            });
            $head.append(link);
          }
        });
      }
    }
    if (dep.script && !restyle) {
      var scriptsAttrs = asArray(dep.script);
      var scripts = import_jquery27.default.map(scriptsAttrs, function(x) {
        var script = document.createElement("script");
        if (typeof x === "string") {
          x = {
            src: x
          };
        }
        for (var _i = 0, _Object$entries = Object.entries(x); _i < _Object$entries.length; _i++) {
          var _Object$entries$_i = _slicedToArray(_Object$entries[_i], 2), attr = _Object$entries$_i[0], val = _Object$entries$_i[1];
          if (attr === "src") {
            val = href + "/" + encodeURI(val);
          }
          script.setAttribute(attr, val ? val : "");
        }
        return script;
      });
      $head.append(scripts);
    }
    if (dep.attachment && !restyle) {
      var attachments = dep.attachment;
      if (typeof attachments === "string")
        attachments = [attachments];
      if (Array.isArray(attachments)) {
        var tmp = {};
        import_jquery27.default.each(attachments, function(index, attachment) {
          var key = index + 1 + "";
          tmp[key] = attachment;
        });
        attachments = tmp;
      }
      var attach = import_jquery27.default.map(attachments, function(attachment, key) {
        return (0, import_jquery27.default)("<link rel='attachment'>").attr("id", dep.name + "-" + key + "-attachment").attr("href", href + "/" + encodeURI(attachment));
      });
      $head.append(attach);
    }
    if (dep.head && !restyle) {
      var $newHead = (0, import_jquery27.default)("<head></head>");
      $newHead.html(dep.head);
      $head.append($newHead.children());
    }
    return true;
  }

  // srcts/src/bindings/output/html.ts
  function _typeof20(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof20 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof20 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof20(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass26(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties26(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties26(Constructor, staticProps);
    return Constructor;
  }
  function _inherits19(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf19(subClass, superClass);
  }
  function _setPrototypeOf19(o, p) {
    _setPrototypeOf19 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn19(self2, call) {
    if (call && (_typeof20(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf19 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery28.default)(scope).find(".shiny-html-output");
      }
    }, {
      key: "onValueError",
      value: function onValueError(el, err) {
        shinyUnbindAll(el);
        this.renderError(el, err);
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        renderContent(el, data);
      }
    }]);
    return HtmlOutputBinding2;
  }(OutputBinding);

  // node_modules/core-js/modules/es.array.filter.js
  "use strict";
  var $50 = require_export();
  var $filter = require_array_iteration().filter;
  var arrayMethodHasSpeciesSupport5 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT4 = arrayMethodHasSpeciesSupport5("filter");
  $50({ target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT4 }, {
    filter: function filter(callbackfn) {
      return $filter(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/bindings/output/image.ts
  var import_es_array_iterator21 = __toModule(require_es_array_iterator());
  var import_jquery33 = __toModule(require_jquery());

  // srcts/src/imageutils/createBrush.ts
  var import_jquery30 = __toModule(require_jquery());

  // srcts/src/imageutils/initCoordmap.ts
  var import_jquery29 = __toModule(require_jquery());

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
  function addScaleFuns(panel) {
    var d = panel.domain;
    var r = panel.range;
    var xlog = panel.log && panel.log.x ? panel.log.x : null;
    var ylog = panel.log && panel.log.y ? panel.log.y : null;
    var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
    var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);
    panel.scaleDataToImg = function(val, clip) {
      return mapValues(val, function(value, key) {
        var prefix = key.substring(0, 1);
        if (prefix === "x") {
          return xscaler.scale(value, clip);
        } else if (prefix === "y") {
          return yscaler.scale(value, clip);
        }
        return null;
      });
    };
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
  }
  function initPanelScales(panels) {
    for (var i = 0; i < panels.length; i++) {
      var panel = panels[i];
      addScaleFuns(panel);
    }
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
    var coordmap = coordmap_;
    var $img = $el.find("img");
    var img = $img[0];
    if (coordmap.panels.length === 0) {
      var bounds = {
        top: 0,
        left: 0,
        right: img.clientWidth - 1,
        bottom: img.clientHeight - 1
      };
      coordmap.panels[0] = {
        domain: bounds,
        range: bounds,
        mapping: {}
      };
    }
    coordmap.dims.height = coordmap.dims.height || img.naturalHeight;
    coordmap.dims.width = coordmap.dims.width || img.naturalWidth;
    initPanelScales(coordmap.panels);
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
          x: coordsData.x,
          y: coordsData.y,
          coords_css: coordsCss,
          coords_img: coordsImg,
          img_css_ratio: coordmap.cssToImgScalingRatio()
        };
        import_jquery29.default.extend(coords, panel.panel_vars);
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
      for (var val in boundsDataVal) {
        if (isnan(boundsDataVal[val]))
          return;
      }
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
        return import_jquery30.default.extend({}, state.boundsCss);
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
      var minData = state.panel.scaleImgToData(cssToImg(minCss));
      var maxData = state.panel.scaleImgToData(cssToImg(maxCss));
      state.boundsData = findBox(minData, maxData);
      state.boundsData = mapValues(state.boundsData, function(val) {
        return roundSignif(val, 14);
      });
      $div.data("bounds-data", state.boundsData);
      $div.data("panel", state.panel);
      return void 0;
    }
    function boundsData(boxData) {
      if (boxData === void 0) {
        return import_jquery30.default.extend({}, state.boundsData);
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
      $div = (0, import_jquery30.default)(document.createElement("div")).attr("id", el.id + "_brush").css({
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
      $div.offset({
        x: 0,
        y: 0
      }).width(0).outerHeight(0);
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
      state.changeStartBounds = import_jquery30.default.extend({}, state.boundsCss);
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
        var panelBoundsImg = state.panel.range;
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
      state.changeStartBounds = import_jquery30.default.extend({}, state.boundsCss);
      state.resizeSides = whichResizeSides(state.down);
    }
    function resizeTo(offsetCss) {
      var dCss = {
        x: offsetCss.x - state.down.x,
        y: offsetCss.y - state.down.y
      };
      var dImg = cssToImg(dCss);
      var bImg = cssToImg(state.changeStartBounds);
      var panelBoundsImg = state.panel.range;
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
  var import_jquery31 = __toModule(require_jquery());
  function createClickInfo($el, dblclickId, dblclickDelay) {
    var clickTimer = null;
    var pendingE = null;
    function triggerEvent(newEventType, e) {
      var e2 = import_jquery31.default.Event(newEventType, {
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
      clickTimer = setTimeout(function() {
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
  var import_jquery32 = __toModule(require_jquery());
  function createClickHandler(inputId, clip, coordmap) {
    var clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);
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
      import_jquery32.default.extend(coords, panel.panel_vars);
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
        (0, import_jquery32.default)(document).on("mousemove.image_brush", mousemoveResizing).on("mouseup.image_brush", mouseupResizing);
      } else if (brush.isInsideBrush(offsetCss)) {
        brush.startDragging(offsetCss);
        setCursorStyle("grabbing");
        (0, import_jquery32.default)(document).on("mousemove.image_brush", mousemoveDragging).on("mouseup.image_brush", mouseupDragging);
      } else {
        var panel = coordmap.getPanelCss(offsetCss, expandPixels);
        brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offsetCss)));
        (0, import_jquery32.default)(document).on("mousemove.image_brush", mousemoveBrushing).on("mouseup.image_brush", mouseupBrushing);
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
      (0, import_jquery32.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
      (0, import_jquery32.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
      brush.up(coordmap.mouseOffsetCss(e));
      brush.stopDragging();
      setCursorStyle("grabbable");
      if (brushInfoSender.isPending())
        brushInfoSender.immediateCall();
    }
    function mouseupResizing(e) {
      if (e.which !== 1)
        return;
      (0, import_jquery32.default)(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
  function _typeof21(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof21 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof21 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof21(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass27(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties27(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties27(Constructor, staticProps);
    return Constructor;
  }
  function _inherits20(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf20(subClass, superClass);
  }
  function _setPrototypeOf20(o, p) {
    _setPrototypeOf20 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
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
  function _possibleConstructorReturn20(self2, call) {
    if (call && (_typeof21(call) === "object" || typeof call === "function")) {
      return call;
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
    _getPrototypeOf20 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
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
        return (0, import_jquery33.default)(scope).find(".shiny-image-output, .shiny-plot-output");
      }
    }, {
      key: "renderValue",
      value: function renderValue(el, data) {
        var outputId = this.getId(el);
        var $el = (0, import_jquery33.default)(el);
        var img;
        var $img = $el.find("img");
        if ($img.length === 0) {
          img = document.createElement("img");
          $el.append(img);
          $img = (0, import_jquery33.default)(img);
        } else {
          img = $img[0];
          $img.trigger("reset");
        }
        if (!data) {
          $el.empty();
          return;
        }
        function ifUndefined(value, alternate) {
          if (value === void 0)
            return alternate;
          return value;
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
        import_jquery33.default.each(data, function(key, value) {
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
        (0, import_jquery33.default)(el).find("img").trigger("reset");
        OutputBinding.prototype.renderError.call(this, el, err);
      }
    }, {
      key: "clearError",
      value: function clearError(el) {
        (0, import_jquery33.default)(el).contents().filter(function() {
          return !(this instanceof HTMLElement && (this.tagName === "IMG" || this.id === el.id + "_brush"));
        }).remove();
        OutputBinding.prototype.clearError.call(this, el);
      }
    }, {
      key: "resize",
      value: function resize(el, width, height) {
        (0, import_jquery33.default)(el).find("img").trigger("resize");
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
  var import_es_regexp_exec7 = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.string.match.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic4 = require_fix_regexp_well_known_symbol_logic();
  var anObject8 = require_an_object();
  var toLength6 = require_to_length();
  var requireObjectCoercible4 = require_require_object_coercible();
  var advanceStringIndex3 = require_advance_string_index();
  var regExpExec3 = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic4("match", 1, function(MATCH, nativeMatch, maybeCallNative) {
    return [
      function match(regexp) {
        var O = requireObjectCoercible4(this);
        var matcher = regexp == void 0 ? void 0 : regexp[MATCH];
        return matcher !== void 0 ? matcher.call(regexp, O) : new RegExp(regexp)[MATCH](String(O));
      },
      function(regexp) {
        var res = maybeCallNative(nativeMatch, regexp, this);
        if (res.done)
          return res.value;
        var rx = anObject8(regexp);
        var S = String(this);
        if (!rx.global)
          return regExpExec3(rx, S);
        var fullUnicode = rx.unicode;
        rx.lastIndex = 0;
        var A = [];
        var n = 0;
        var result;
        while ((result = regExpExec3(rx, S)) !== null) {
          var matchStr = String(result[0]);
          A[n] = matchStr;
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex3(S, toLength6(rx.lastIndex), fullUnicode);
          n++;
        }
        return n === 0 ? null : A;
      }
    ];
  });

  // srcts/src/shiny/notifications.ts
  var import_jquery34 = __toModule(require_jquery());
  var fadeDuration = 250;
  function show() {
    var _ref = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref$html = _ref.html, html = _ref$html === void 0 ? "" : _ref$html, _ref$action = _ref.action, action = _ref$action === void 0 ? "" : _ref$action, _ref$deps = _ref.deps, deps = _ref$deps === void 0 ? [] : _ref$deps, _ref$duration = _ref.duration, duration = _ref$duration === void 0 ? 5e3 : _ref$duration, _ref$id = _ref.id, id = _ref$id === void 0 ? null : _ref$id, _ref$closeButton = _ref.closeButton, closeButton = _ref$closeButton === void 0 ? true : _ref$closeButton, _ref$type = _ref.type, type = _ref$type === void 0 ? null : _ref$type;
    if (!id)
      id = randomId();
    createPanel();
    var $notification = get2(id);
    if ($notification.length === 0)
      $notification = create4(id);
    var newHtml = '<div class="shiny-notification-content-text">'.concat(html, "</div>") + '<div class="shiny-notification-content-action">'.concat(action, "</div>");
    var $content = $notification.find(".shiny-notification-content");
    renderContent($content, {
      html: newHtml,
      deps: deps
    });
    var classes = $notification.attr("class").split(/\s+/).filter(function(cls) {
      return cls.match(/^shiny-notification-/);
    }).join(" ");
    $notification.removeClass(classes);
    if (type && type !== "default")
      $notification.addClass("shiny-notification-" + type);
    var $close = $notification.find(".shiny-notification-close");
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
  function remove(id) {
    get2(id).fadeOut(fadeDuration, function() {
      shinyUnbindAll(this);
      (0, import_jquery34.default)(this).remove();
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
    return (0, import_jquery34.default)("#shiny-notification-panel");
  }
  function createPanel() {
    var $panel = getPanel();
    if ($panel.length > 0)
      return $panel;
    (0, import_jquery34.default)(document.body).append('<div id="shiny-notification-panel">');
    return $panel;
  }
  function create4(id) {
    var $notification = get2(id);
    if ($notification.length === 0) {
      $notification = (0, import_jquery34.default)('<div id="shiny-notification-'.concat(id, '" class="shiny-notification">') + '<div class="shiny-notification-close">&times;</div><div class="shiny-notification-content"></div></div>');
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
    clearRemovalCallback(id);
    var removalCallback = setTimeout(function() {
      remove(id);
    }, delay);
    get2(id).data("removalCallback", removalCallback);
  }
  function clearRemovalCallback(id) {
    var $notification = get2(id);
    var oldRemovalCallback = $notification.data("removalCallback");
    if (oldRemovalCallback) {
      clearTimeout(oldRemovalCallback);
    }
  }

  // srcts/src/shiny/modal.ts
  var import_jquery35 = __toModule(require_jquery());
  function show2() {
    var _ref = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref$html = _ref.html, html = _ref$html === void 0 ? "" : _ref$html, _ref$deps = _ref.deps, deps = _ref$deps === void 0 ? [] : _ref$deps;
    (0, import_jquery35.default)(".modal-backdrop").remove();
    var $modal = (0, import_jquery35.default)("#shiny-modal-wrapper");
    if ($modal.length === 0) {
      $modal = (0, import_jquery35.default)('<div id="shiny-modal-wrapper"></div>');
      (0, import_jquery35.default)(document.body).append($modal);
      $modal.on("hidden.bs.modal", function(e) {
        if (e.target === (0, import_jquery35.default)("#shiny-modal")[0]) {
          shinyUnbindAll($modal);
          $modal.remove();
        }
      });
    }
    $modal.on("keydown.shinymodal", function(e) {
      if ((0, import_jquery35.default)("#shiny-modal").data("keyboard") === false)
        return;
      if (e.keyCode === 27) {
        e.stopPropagation();
        e.preventDefault();
      }
    });
    renderContent($modal, {
      html: html,
      deps: deps
    });
  }
  function remove2() {
    var $modal = (0, import_jquery35.default)("#shiny-modal-wrapper");
    $modal.off("keydown.shinymodal");
    if ($modal.find(".modal").length > 0) {
      $modal.find(".modal").modal("hide");
    } else {
      shinyUnbindAll($modal);
      $modal.remove();
    }
  }

  // srcts/src/shiny/reconnectDialog.ts
  var import_jquery36 = __toModule(require_jquery());
  function updateTime(reconnectTime) {
    var $time = (0, import_jquery36.default)("#shiny-reconnect-time");
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
    if ((0, import_jquery36.default)("#shiny-reconnect-text").length > 0)
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
  var import_es_regexp_exec10 = __toModule(require_es_regexp_exec());
  var import_jquery42 = __toModule(require_jquery());

  // srcts/src/inputPolicies/inputBatchSender.ts
  var import_es_array_iterator22 = __toModule(require_es_array_iterator());
  var import_jquery37 = __toModule(require_jquery());

  // srcts/src/inputPolicies/inputPolicy.ts
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass28(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties28(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties28(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty9(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var InputPolicy = /* @__PURE__ */ function() {
    function InputPolicy2() {
      _classCallCheck28(this, InputPolicy2);
      _defineProperty9(this, "target", void 0);
    }
    _createClass28(InputPolicy2, [{
      key: "setInput",
      value: function setInput(name, value, opts) {
        throw "not implemented";
        name;
        value;
        opts;
      }
    }]);
    return InputPolicy2;
  }();

  // srcts/src/inputPolicies/inputBatchSender.ts
  function _typeof22(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof22 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof22 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof22(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass29(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties29(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties29(Constructor, staticProps);
    return Constructor;
  }
  function _inherits21(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf21(subClass, superClass);
  }
  function _setPrototypeOf21(o, p) {
    _setPrototypeOf21 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf21(o, p);
  }
  function _createSuper21(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct21();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf21(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf21(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn21(this, result);
    };
  }
  function _possibleConstructorReturn21(self2, call) {
    if (call && (_typeof22(call) === "object" || typeof call === "function")) {
      return call;
    }
    return _assertThisInitialized21(self2);
  }
  function _assertThisInitialized21(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct21() {
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
  function _getPrototypeOf21(o) {
    _getPrototypeOf21 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf21(o);
  }
  function _defineProperty10(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var InputBatchSender = /* @__PURE__ */ function(_InputPolicy) {
    _inherits21(InputBatchSender2, _InputPolicy);
    var _super = _createSuper21(InputBatchSender2);
    function InputBatchSender2(shinyapp) {
      var _this;
      _classCallCheck29(this, InputBatchSender2);
      _this = _super.call(this);
      _defineProperty10(_assertThisInitialized21(_this), "shinyapp", void 0);
      _defineProperty10(_assertThisInitialized21(_this), "timerId", null);
      _defineProperty10(_assertThisInitialized21(_this), "pendingData", {});
      _defineProperty10(_assertThisInitialized21(_this), "reentrant", false);
      _defineProperty10(_assertThisInitialized21(_this), "lastChanceCallback", []);
      _this.shinyapp = shinyapp;
      return _this;
    }
    _createClass29(InputBatchSender2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        this.pendingData[nameType] = value;
        if (!this.reentrant) {
          if (opts.priority === "event") {
            this._sendNow();
          } else if (!this.timerId) {
            this.timerId = setTimeout(this._sendNow.bind(this), 0);
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
          this.timerId = null;
          import_jquery37.default.each(this.lastChanceCallback, function(i, callback) {
            callback();
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
  }(InputPolicy);

  // srcts/src/inputPolicies/inputNoResendDecorator.ts
  var import_es_array_iterator23 = __toModule(require_es_array_iterator());

  // srcts/src/inputPolicies/splitInputNameType.ts
  var import_es_regexp_exec8 = __toModule(require_es_regexp_exec());
  function splitInputNameType(nameType) {
    var name2 = nameType.split(":");
    return {
      name: name2[0],
      inputType: name2.length > 1 ? name2[1] : ""
    };
  }

  // srcts/src/inputPolicies/inputNoResendDecorator.ts
  function _typeof23(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof23 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof23 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof23(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass30(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties30(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties30(Constructor, staticProps);
    return Constructor;
  }
  function _inherits22(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf22(subClass, superClass);
  }
  function _setPrototypeOf22(o, p) {
    _setPrototypeOf22 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf22(o, p);
  }
  function _createSuper22(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct22();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf22(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf22(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn22(this, result);
    };
  }
  function _possibleConstructorReturn22(self2, call) {
    if (call && (_typeof23(call) === "object" || typeof call === "function")) {
      return call;
    }
    return _assertThisInitialized22(self2);
  }
  function _assertThisInitialized22(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct22() {
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
  function _getPrototypeOf22(o) {
    _getPrototypeOf22 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf22(o);
  }
  function _defineProperty11(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var InputNoResendDecorator = /* @__PURE__ */ function(_InputPolicy) {
    _inherits22(InputNoResendDecorator2, _InputPolicy);
    var _super = _createSuper22(InputNoResendDecorator2);
    function InputNoResendDecorator2(target) {
      var _this;
      var initialValues = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : {};
      _classCallCheck30(this, InputNoResendDecorator2);
      _this = _super.call(this);
      _defineProperty11(_assertThisInitialized22(_this), "lastSentValues", void 0);
      _this.target = target;
      _this.reset(initialValues);
      return _this;
    }
    _createClass30(InputNoResendDecorator2, [{
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
        var values = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {};
        var cacheValues = {};
        for (var inputName in values) {
          if (hasOwnProperty(values, inputName)) {
            var _splitInputNameType2 = splitInputNameType(inputName), name = _splitInputNameType2.name, inputType = _splitInputNameType2.inputType;
            cacheValues[name] = {
              jsonValue: JSON.stringify(values[inputName]),
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
  }(InputPolicy);

  // srcts/src/inputPolicies/inputEventDecorator.ts
  var import_es_array_iterator24 = __toModule(require_es_array_iterator());
  var import_jquery38 = __toModule(require_jquery());
  function _typeof24(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof24 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof24 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof24(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass31(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties31(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties31(Constructor, staticProps);
    return Constructor;
  }
  function _inherits23(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf23(subClass, superClass);
  }
  function _setPrototypeOf23(o, p) {
    _setPrototypeOf23 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf23(o, p);
  }
  function _createSuper23(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct23();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf23(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf23(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn23(this, result);
    };
  }
  function _possibleConstructorReturn23(self2, call) {
    if (call && (_typeof24(call) === "object" || typeof call === "function")) {
      return call;
    }
    return _assertThisInitialized23(self2);
  }
  function _assertThisInitialized23(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct23() {
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
  function _getPrototypeOf23(o) {
    _getPrototypeOf23 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf23(o);
  }
  var InputEventDecorator = /* @__PURE__ */ function(_InputPolicy) {
    _inherits23(InputEventDecorator2, _InputPolicy);
    var _super = _createSuper23(InputEventDecorator2);
    function InputEventDecorator2(target) {
      var _this;
      _classCallCheck31(this, InputEventDecorator2);
      _this = _super.call(this);
      _this.target = target;
      return _this;
    }
    _createClass31(InputEventDecorator2, [{
      key: "setInput",
      value: function setInput(nameType, value, opts) {
        var evt = jQuery.Event("shiny:inputchanged");
        var input = splitInputNameType(nameType);
        evt.name = input.name;
        evt.inputType = input.inputType;
        evt.value = value;
        evt.binding = opts.binding;
        evt.el = opts.el;
        evt.priority = opts.priority;
        (0, import_jquery38.default)(opts.el).trigger(evt);
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
  }(InputPolicy);

  // srcts/src/inputPolicies/inputRateDecorator.ts
  var import_es_array_iterator25 = __toModule(require_es_array_iterator());
  function _typeof25(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof25 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof25 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof25(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass32(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties32(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties32(Constructor, staticProps);
    return Constructor;
  }
  function _inherits24(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf24(subClass, superClass);
  }
  function _setPrototypeOf24(o, p) {
    _setPrototypeOf24 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf24(o, p);
  }
  function _createSuper24(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct24();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf24(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf24(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn24(this, result);
    };
  }
  function _possibleConstructorReturn24(self2, call) {
    if (call && (_typeof25(call) === "object" || typeof call === "function")) {
      return call;
    }
    return _assertThisInitialized24(self2);
  }
  function _assertThisInitialized24(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct24() {
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
  function _getPrototypeOf24(o) {
    _getPrototypeOf24 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf24(o);
  }
  function _defineProperty12(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var InputRateDecorator = /* @__PURE__ */ function(_InputPolicy) {
    _inherits24(InputRateDecorator2, _InputPolicy);
    var _super = _createSuper24(InputRateDecorator2);
    function InputRateDecorator2(target) {
      var _this;
      _classCallCheck32(this, InputRateDecorator2);
      _this = _super.call(this);
      _defineProperty12(_assertThisInitialized24(_this), "inputRatePolicies", {});
      _this.target = target;
      return _this;
    }
    _createClass32(InputRateDecorator2, [{
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
  }(InputPolicy);

  // srcts/src/inputPolicies/inputDeferDecorator.ts
  var import_es_array_iterator26 = __toModule(require_es_array_iterator());
  function _typeof26(obj) {
    "@babel/helpers - typeof";
    if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
      _typeof26 = function _typeof27(obj2) {
        return typeof obj2;
      };
    } else {
      _typeof26 = function _typeof27(obj2) {
        return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
      };
    }
    return _typeof26(obj);
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass33(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties33(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties33(Constructor, staticProps);
    return Constructor;
  }
  function _inherits25(subClass, superClass) {
    if (typeof superClass !== "function" && superClass !== null) {
      throw new TypeError("Super expression must either be null or a function");
    }
    subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, writable: true, configurable: true } });
    if (superClass)
      _setPrototypeOf25(subClass, superClass);
  }
  function _setPrototypeOf25(o, p) {
    _setPrototypeOf25 = Object.setPrototypeOf || function _setPrototypeOf26(o2, p2) {
      o2.__proto__ = p2;
      return o2;
    };
    return _setPrototypeOf25(o, p);
  }
  function _createSuper25(Derived) {
    var hasNativeReflectConstruct = _isNativeReflectConstruct25();
    return function _createSuperInternal() {
      var Super = _getPrototypeOf25(Derived), result;
      if (hasNativeReflectConstruct) {
        var NewTarget = _getPrototypeOf25(this).constructor;
        result = Reflect.construct(Super, arguments, NewTarget);
      } else {
        result = Super.apply(this, arguments);
      }
      return _possibleConstructorReturn25(this, result);
    };
  }
  function _possibleConstructorReturn25(self2, call) {
    if (call && (_typeof26(call) === "object" || typeof call === "function")) {
      return call;
    }
    return _assertThisInitialized25(self2);
  }
  function _assertThisInitialized25(self2) {
    if (self2 === void 0) {
      throw new ReferenceError("this hasn't been initialised - super() hasn't been called");
    }
    return self2;
  }
  function _isNativeReflectConstruct25() {
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
  function _getPrototypeOf25(o) {
    _getPrototypeOf25 = Object.setPrototypeOf ? Object.getPrototypeOf : function _getPrototypeOf26(o2) {
      return o2.__proto__ || Object.getPrototypeOf(o2);
    };
    return _getPrototypeOf25(o);
  }
  function _defineProperty13(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var InputDeferDecorator = /* @__PURE__ */ function(_InputPolicy) {
    _inherits25(InputDeferDecorator2, _InputPolicy);
    var _super = _createSuper25(InputDeferDecorator2);
    function InputDeferDecorator2(target) {
      var _this;
      _classCallCheck33(this, InputDeferDecorator2);
      _this = _super.call(this);
      _defineProperty13(_assertThisInitialized25(_this), "pendingInput", {});
      _this.target = target;
      return _this;
    }
    _createClass33(InputDeferDecorator2, [{
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
          if (hasOwnProperty(this.pendingInput, nameType)) {
            var _this$pendingInput$na = this.pendingInput[nameType], value = _this$pendingInput$na.value, opts = _this$pendingInput$na.opts;
            this.target.setInput(nameType, value, opts);
          }
        }
      }
    }]);
    return InputDeferDecorator2;
  }(InputPolicy);

  // srcts/src/inputPolicies/inputValidateDecorator.ts
  var import_jquery39 = __toModule(require_jquery());
  function _classCallCheck34(instance, Constructor) {
    if (!(instance instanceof Constructor)) {
      throw new TypeError("Cannot call a class as a function");
    }
  }
  function _defineProperty14(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  function addDefaultInputOpts(opts) {
    var newOpts = import_jquery39.default.extend({
      priority: "immediate",
      binding: null,
      el: null
    }, opts);
    if (newOpts && typeof newOpts.priority !== "undefined") {
      switch (newOpts.priority) {
        case "deferred":
        case "immediate":
        case "event":
          break;
        default:
          throw new Error("Unexpected input value mode: '" + newOpts.priority + "'");
      }
    }
    return newOpts;
  }
  var InputValidateDecorator = function InputValidateDecorator2(target) {
    _classCallCheck34(this, InputValidateDecorator2);
    _defineProperty14(this, "target", void 0);
    _defineProperty14(this, "setInput", function(nameType, value, opts) {
      if (!nameType)
        throw "Can't set input with empty name.";
      var newOpts = addDefaultInputOpts(opts);
      this.target.setInput(nameType, value, newOpts);
    });
    this.target = target;
  };

  // srcts/src/shiny/bind.ts
  var import_jquery40 = __toModule(require_jquery());

  // srcts/src/bindings/outputAdapter.ts
  function _classCallCheck35(instance, Constructor) {
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass34(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties34(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties34(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty15(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
  }
  var OutputBindingAdapter = /* @__PURE__ */ function() {
    function OutputBindingAdapter2(el, binding) {
      _classCallCheck35(this, OutputBindingAdapter2);
      _defineProperty15(this, "el", void 0);
      _defineProperty15(this, "binding", void 0);
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
      value: function onValueChange(data) {
        this.binding.onValueChange(this.el, data);
      }
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
    var _loop = function _loop2(i2) {
      var binding = bindings[i2].binding;
      var matches = binding.find(scope) || [];
      var _loop22 = function _loop23(j2) {
        var el = matches[j2];
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
        (0, import_jquery40.default)(el).data("shiny-input-binding", binding);
        (0, import_jquery40.default)(el).addClass("shiny-bound-input");
        var ratePolicy = binding.getRatePolicy(el);
        if (ratePolicy !== null) {
          inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
        }
        boundInputs[id] = {
          binding: binding,
          node: el
        };
        (0, import_jquery40.default)(el).trigger({
          type: "shiny:bound",
          binding: binding,
          bindingType: "input"
        });
      };
      for (var j = 0; j < matches.length; j++) {
        var _ret = _loop22(j);
        if (_ret === "continue")
          continue;
      }
    };
    for (var i = 0; i < bindings.length; i++) {
      _loop(i);
    }
    return inputItems;
  }
  function bindOutputs(_ref) {
    var sendOutputHiddenState = _ref.sendOutputHiddenState, maybeAddThemeObserver = _ref.maybeAddThemeObserver, outputBindings = _ref.outputBindings;
    var scope = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : document.documentElement;
    var $scope = (0, import_jquery40.default)(scope);
    var bindings = outputBindings.getBindings();
    for (var i = 0; i < bindings.length; i++) {
      var binding = bindings[i].binding;
      var matches = binding.find($scope) || [];
      for (var j = 0; j < matches.length; j++) {
        var _el = matches[j];
        var id = binding.getId(_el);
        if (!id)
          continue;
        if (!import_jquery40.default.contains(document.documentElement, _el))
          continue;
        var $el = (0, import_jquery40.default)(_el);
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
    var inputs = (0, import_jquery40.default)(scope).find(".shiny-bound-input").toArray();
    if (includeSelf && (0, import_jquery40.default)(scope).hasClass("shiny-bound-input")) {
      inputs.push(scope);
    }
    for (var i = 0; i < inputs.length; i++) {
      var _el2 = inputs[i];
      var binding = (0, import_jquery40.default)(_el2).data("shiny-input-binding");
      if (!binding)
        continue;
      var id = binding.getId(_el2);
      (0, import_jquery40.default)(_el2).removeClass("shiny-bound-input");
      delete boundInputs[id];
      binding.unsubscribe(_el2);
      (0, import_jquery40.default)(_el2).trigger({
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
    var outputs = (0, import_jquery40.default)(scope).find(".shiny-bound-output").toArray();
    if (includeSelf && (0, import_jquery40.default)(scope).hasClass("shiny-bound-output")) {
      outputs.push(scope);
    }
    for (var i = 0; i < outputs.length; i++) {
      var $el = (0, import_jquery40.default)(outputs[i]);
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
    import_jquery40.default.each(currentInputItems, function(name, item) {
      inputs.setInput(name, item.value, item.opts);
    });
    shinyCtx.initDeferredIframes();
  }

  // node_modules/core-js/modules/es.array-buffer.constructor.js
  "use strict";
  var $63 = require_export();
  var global6 = require_global();
  var arrayBufferModule = require_array_buffer();
  var setSpecies = require_set_species();
  var ARRAY_BUFFER = "ArrayBuffer";
  var ArrayBuffer2 = arrayBufferModule[ARRAY_BUFFER];
  var NativeArrayBuffer = global6[ARRAY_BUFFER];
  $63({ global: true, forced: NativeArrayBuffer !== ArrayBuffer2 }, {
    ArrayBuffer: ArrayBuffer2
  });
  setSpecies(ARRAY_BUFFER);

  // node_modules/core-js/modules/es.array-buffer.slice.js
  "use strict";
  var $64 = require_export();
  var fails10 = require_fails();
  var ArrayBufferModule = require_array_buffer();
  var anObject9 = require_an_object();
  var toAbsoluteIndex3 = require_to_absolute_index();
  var toLength7 = require_to_length();
  var speciesConstructor2 = require_species_constructor();
  var ArrayBuffer3 = ArrayBufferModule.ArrayBuffer;
  var DataView2 = ArrayBufferModule.DataView;
  var nativeArrayBufferSlice = ArrayBuffer3.prototype.slice;
  var INCORRECT_SLICE = fails10(function() {
    return !new ArrayBuffer3(2).slice(1, void 0).byteLength;
  });
  $64({ target: "ArrayBuffer", proto: true, unsafe: true, forced: INCORRECT_SLICE }, {
    slice: function slice2(start, end) {
      if (nativeArrayBufferSlice !== void 0 && end === void 0) {
        return nativeArrayBufferSlice.call(anObject9(this), start);
      }
      var length = anObject9(this).byteLength;
      var first = toAbsoluteIndex3(start, length);
      var fin = toAbsoluteIndex3(end === void 0 ? length : end, length);
      var result = new (speciesConstructor2(this, ArrayBuffer3))(toLength7(fin - first));
      var viewSource = new DataView2(this);
      var viewTarget = new DataView2(result);
      var index = 0;
      while (first < fin) {
        viewTarget.setUint8(index++, viewSource.getUint8(first++));
      }
      return result;
    }
  });

  // node_modules/core-js/modules/es.data-view.js
  var $65 = require_export();
  var ArrayBufferModule2 = require_array_buffer();
  var NATIVE_ARRAY_BUFFER = require_array_buffer_native();
  $65({ global: true, forced: !NATIVE_ARRAY_BUFFER }, {
    DataView: ArrayBufferModule2.DataView
  });

  // node_modules/core-js/modules/es.array.reduce.js
  "use strict";
  var $66 = require_export();
  var $reduce = require_array_reduce().left;
  var arrayMethodIsStrict3 = require_array_method_is_strict();
  var CHROME_VERSION = require_engine_v8_version();
  var IS_NODE = require_engine_is_node();
  var STRICT_METHOD3 = arrayMethodIsStrict3("reduce");
  var CHROME_BUG = !IS_NODE && CHROME_VERSION > 79 && CHROME_VERSION < 83;
  $66({ target: "Array", proto: true, forced: !STRICT_METHOD3 || CHROME_BUG }, {
    reduce: function reduce(callbackfn) {
      return $reduce(this, callbackfn, arguments.length, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // srcts/src/shiny/shinyapp.ts
  var import_es_regexp_exec9 = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.array.for-each.js
  "use strict";
  var $67 = require_export();
  var forEach = require_array_for_each();
  $67({ target: "Array", proto: true, forced: [].forEach != forEach }, {
    forEach: forEach
  });

  // node_modules/core-js/modules/web.dom-collections.for-each.js
  var global7 = require_global();
  var DOMIterables2 = require_dom_iterables();
  var forEach2 = require_array_for_each();
  var createNonEnumerableProperty3 = require_create_non_enumerable_property();
  for (var COLLECTION_NAME in DOMIterables2) {
    Collection = global7[COLLECTION_NAME];
    CollectionPrototype = Collection && Collection.prototype;
    if (CollectionPrototype && CollectionPrototype.forEach !== forEach2)
      try {
        createNonEnumerableProperty3(CollectionPrototype, "forEach", forEach2);
      } catch (error) {
        CollectionPrototype.forEach = forEach2;
      }
  }
  var Collection;
  var CollectionPrototype;

  // srcts/src/shiny/shinyapp.ts
  var import_jquery41 = __toModule(require_jquery());
  function _classCallCheck36(instance, Constructor) {
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
      Object.defineProperty(target, descriptor.key, descriptor);
    }
  }
  function _createClass35(Constructor, protoProps, staticProps) {
    if (protoProps)
      _defineProperties35(Constructor.prototype, protoProps);
    if (staticProps)
      _defineProperties35(Constructor, staticProps);
    return Constructor;
  }
  function _defineProperty16(obj, key, value) {
    if (key in obj) {
      Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true });
    } else {
      obj[key] = value;
    }
    return obj;
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
      _defineProperty16(this, "$socket", null);
      _defineProperty16(this, "config", null);
      _defineProperty16(this, "$inputValues", {});
      _defineProperty16(this, "$initialInput", void 0);
      _defineProperty16(this, "$bindings", {});
      _defineProperty16(this, "$values", {});
      _defineProperty16(this, "$errors", {});
      _defineProperty16(this, "$conditionals", {});
      _defineProperty16(this, "$pendingMessages", []);
      _defineProperty16(this, "$activeRequests", {});
      _defineProperty16(this, "$nextRequestId", 0);
      _defineProperty16(this, "$allowReconnect", false);
      _defineProperty16(this, "scheduledReconnect", null);
      _defineProperty16(this, "reconnectDelay", function() {
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
      _defineProperty16(this, "progressHandlers", {
        binding: function binding(message) {
          var key = message.id;
          var binding2 = this.$bindings[key];
          if (binding2) {
            (0, import_jquery41.default)(binding2.el).trigger({
              type: "shiny:outputinvalidated",
              binding: binding2,
              name: key
            });
            if (binding2.showProgress)
              binding2.showProgress(true);
          }
        },
        open: function open(message) {
          if (message.style === "notification") {
            show({
              html: '<div id="shiny-progress-'.concat(message.id, '" class="shiny-progress-notification">') + '<div class="progress active" style="display: none;"><div class="progress-bar"></div></div><div class="progress-text"><span class="progress-message">message</span> <span class="progress-detail"></span></div></div>',
              id: message.id,
              duration: null
            });
          } else if (message.style === "old") {
            var $container = (0, import_jquery41.default)(".shiny-progress-container");
            if ($container.length === 0) {
              $container = (0, import_jquery41.default)('<div class="shiny-progress-container"></div>');
              (0, import_jquery41.default)(document.body).append($container);
            }
            var depth = (0, import_jquery41.default)(".shiny-progress.open").length;
            var $progress = (0, import_jquery41.default)('<div class="shiny-progress open"><div class="progress active"><div class="progress-bar bar"></div></div><div class="progress-text"><span class="progress-message">message</span><span class="progress-detail"></span></div></div>');
            $progress.attr("id", message.id);
            $container.append($progress);
            var $progressBar = $progress.find(".progress");
            $progressBar.css("top", depth * $progressBar.height() + "px");
            var $progressText = $progress.find(".progress-text");
            $progressText.css("top", 3 * $progressBar.height() + depth * $progressText.outerHeight() + "px");
            $progress.hide();
          }
        },
        update: function update(message) {
          if (message.style === "notification") {
            var $progress = (0, import_jquery41.default)("#shiny-progress-" + message.id);
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
            var _$progress = (0, import_jquery41.default)("#" + message.id + ".shiny-progress");
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
            var $progress = (0, import_jquery41.default)("#" + message.id + ".shiny-progress");
            $progress.removeClass("open");
            $progress.fadeOut({
              complete: function complete() {
                $progress.remove();
                if ((0, import_jquery41.default)(".shiny-progress").length === 0)
                  (0, import_jquery41.default)(".shiny-progress-container").remove();
              }
            });
          }
        }
      });
      this._init();
    }
    _createClass35(ShinyApp2, [{
      key: "connect",
      value: function connect(initialInput) {
        if (this.$socket)
          throw "Connect was already called on this application object";
        this.$socket = this.createSocket();
        this.$initialInput = initialInput;
        import_jquery41.default.extend(this.$inputValues, initialInput);
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
        this.$initialInput = import_jquery41.default.extend({}, this.$inputValues);
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
          (0, import_jquery41.default)(document).trigger({
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
        };
        socket.onmessage = function(e) {
          _this.dispatchMessage(e.data);
        };
        socket.onclose = function() {
          if (hasOpened) {
            (0, import_jquery41.default)(document).trigger({
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
      key: "sendInput",
      value: function sendInput(values) {
        var msg = JSON.stringify({
          method: "update",
          data: values
        });
        this.$sendMsg(msg);
        import_jquery41.default.extend(this.$inputValues, values);
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
        this.scheduledReconnect = setTimeout(function() {
          _this2.reconnect();
        }, delay);
      }
    }, {
      key: "onDisconnected",
      value: function onDisconnected() {
        var $overlay = (0, import_jquery41.default)("#shiny-disconnected-overlay");
        if ($overlay.length === 0) {
          (0, import_jquery41.default)(document.body).append('<div id="shiny-disconnected-overlay"></div>');
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
        (0, import_jquery41.default)("#shiny-disconnected-overlay").remove();
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
          var jsonBuf = makeBlob([msg]);
          payload.push(uint32ToBuf(jsonBuf.size));
          payload.push(jsonBuf);
          for (var i = 0; i < blobs.length; i++) {
            var _blob = blobs[i];
            payload.push(uint32ToBuf(_blob.byteLength || _blob.size || 0));
            payload.push(_blob);
          }
          var blob = makeBlob(payload);
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
        var evt = jQuery.Event("shiny:error");
        evt.name = name;
        evt.error = error;
        evt.binding = binding;
        (0, import_jquery41.default)(binding ? binding.el : document).trigger(evt);
        if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
          binding.onValueError(evt.error);
        }
      }
    }, {
      key: "receiveOutput",
      value: function receiveOutput(name, value) {
        var binding = this.$bindings[name];
        var evt = jQuery.Event("shiny:value");
        evt.name = name;
        evt.value = value;
        evt.binding = binding;
        if (this.$values[name] === value) {
          (0, import_jquery41.default)(binding ? binding.el : document).trigger(evt);
          return void 0;
        }
        this.$values[name] = value;
        delete this.$errors[name];
        (0, import_jquery41.default)(binding ? binding.el : document).trigger(evt);
        if (!evt.isDefaultPrevented() && binding) {
          binding.onValueChange(evt.value);
        }
        return value;
      }
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
          return _defineProperty16({}, k.substring(nsPrefix.length), scopeComponent[k]);
        }).reduce(function(obj, pair) {
          return import_jquery41.default.extend(obj, pair);
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
        (0, import_jquery41.default)(document).trigger({
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
        var conditionals = (0, import_jquery41.default)(document).find("[data-display-if]");
        for (var i = 0; i < conditionals.length; i++) {
          var el = (0, import_jquery41.default)(conditionals[i]);
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
      value: function dispatchMessage(data) {
        var msgObj = {};
        if (typeof data === "string") {
          msgObj = JSON.parse(data);
        } else {
          var len = new DataView(data, 0, 1).getUint8(0);
          var typedv = new DataView(data, 1, len);
          var typebuf = [];
          for (var i = 0; i < len; i++) {
            typebuf.push(String.fromCharCode(typedv.getUint8(i)));
          }
          var type = typebuf.join("");
          data = data.slice(len + 1);
          msgObj.custom = {};
          msgObj.custom[type] = data;
        }
        var evt = jQuery.Event("shiny:message");
        evt.message = msgObj;
        (0, import_jquery41.default)(document).trigger(evt);
        if (evt.isDefaultPrevented())
          return;
        this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);
        this.$updateConditionals();
      }
    }, {
      key: "_sendMessagesToHandlers",
      value: function _sendMessagesToHandlers(msgObj, handlers, handlerOrder) {
        for (var i = 0; i < handlerOrder.length; i++) {
          var msgType = handlerOrder[i];
          if (hasOwnProperty(msgObj, msgType)) {
            handlers[msgType].call(this, msgObj[msgType]);
          }
        }
      }
    }, {
      key: "_init",
      value: function _init() {
        var _this3 = this;
        addMessageHandler("values", function(message) {
          for (var name in _this3.$bindings) {
            if (hasOwnProperty(_this3.$bindings, name))
              _this3.$bindings[name].showProgress(false);
          }
          for (var _key in message) {
            if (hasOwnProperty(message, _key)) {
              _this3.receiveOutput(_key, message[_key]);
            }
          }
        });
        addMessageHandler("errors", function(message) {
          for (var _key2 in message) {
            if (hasOwnProperty(message, _key2))
              this.receiveError(_key2, message[_key2]);
          }
        });
        addMessageHandler("inputMessages", function(message) {
          for (var i = 0; i < message.length; i++) {
            var $obj = (0, import_jquery41.default)(".shiny-bound-input#" + $escape(message[i].id));
            var inputBinding = $obj.data("shiny-input-binding");
            if ($obj.length > 0) {
              if (!$obj.attr("aria-live"))
                $obj.attr("aria-live", "polite");
              var el = $obj[0];
              var evt = jQuery.Event("shiny:updateinput");
              evt.message = message[i].message;
              evt.binding = inputBinding;
              (0, import_jquery41.default)(el).trigger(evt);
              if (!evt.isDefaultPrevented())
                inputBinding.receiveMessage(el, evt.message);
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
        addMessageHandler("progress", function(message) {
          if (message.type && message.message) {
            var handler = _this3.progressHandlers[message.type];
            if (handler)
              handler.call(_this3, message.message);
          }
        });
        addMessageHandler("notification", function(message) {
          if (message.type === "show")
            show(message.message);
          else if (message.type === "remove")
            remove(message.message);
          else
            throw "Unkown notification type: " + message.type;
        });
        addMessageHandler("modal", function(message) {
          if (message.type === "show")
            show2(message.message);
          else if (message.type === "remove")
            remove2();
          else
            throw "Unkown modal type: " + message.type;
        });
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
          (0, import_jquery41.default)(document).trigger("shiny:sessioninitialized");
        });
        addMessageHandler("busy", function(message) {
          if (message === "busy") {
            (0, import_jquery41.default)(document.documentElement).addClass("shiny-busy");
            (0, import_jquery41.default)(document).trigger("shiny:busy");
          } else if (message === "idle") {
            (0, import_jquery41.default)(document.documentElement).removeClass("shiny-busy");
            (0, import_jquery41.default)(document).trigger("shiny:idle");
          }
        });
        addMessageHandler("recalculating", function(message) {
          if (hasOwnProperty(message, "name") && hasOwnProperty(message, "status")) {
            var binding = _this3.$bindings[message.name];
            (0, import_jquery41.default)(binding ? binding.el : null).trigger({
              type: "shiny:" + message.status
            });
          }
        });
        addMessageHandler("reload", function(message) {
          window.location.reload();
          return;
          message;
        });
        addMessageHandler("shiny-insert-ui", function(message) {
          var targets = (0, import_jquery41.default)(message.selector);
          if (targets.length === 0) {
            console.warn('The selector you chose ("' + message.selector + '") could not be found in the DOM.');
            renderHtml2(message.content.html, (0, import_jquery41.default)([]).get(0), message.content.deps);
          } else {
            targets.each(function(i, target) {
              renderContent(target, message.content, message.where);
              return message.multiple;
            });
          }
        });
        addMessageHandler("shiny-remove-ui", function(message) {
          var els = (0, import_jquery41.default)(message.selector);
          els.each(function(i, el) {
            shinyUnbindAll(el, true);
            (0, import_jquery41.default)(el).remove();
            return message.multiple;
          });
        });
        addMessageHandler("frozen", function(message) {
          for (var i = 0; i < message.ids.length; i++) {
            shinyForgetLastInputValue(message.ids[i]);
          }
        });
        function getTabset(id) {
          var $tabset = (0, import_jquery41.default)("#" + $escape(id));
          if ($tabset.length === 0)
            throw "There is no tabsetPanel (or navbarPage or navlistPanel) with id equal to '" + id + "'";
          return $tabset;
        }
        function getTabContent($tabset) {
          var tabsetId = $tabset.attr("data-tabsetid");
          var $tabContent = (0, import_jquery41.default)("div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']");
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
              $liTags.push((0, import_jquery41.default)(el));
            });
            var selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
            var $dropdownDivs = $tabContent.find(selector);
            $dropdownDivs.each(function(i, el) {
              $divTags.push((0, import_jquery41.default)(el));
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
        addMessageHandler("shiny-insert-tab", function(message) {
          var $parentTabset = getTabset(message.inputId);
          var $tabset = $parentTabset;
          var $tabContent = getTabContent($tabset);
          var tabsetId = $parentTabset.attr("data-tabsetid");
          var $divTag = (0, import_jquery41.default)(message.divTag.html);
          var $liTag = (0, import_jquery41.default)(message.liTag.html);
          var $aTag = $liTag.find("> a");
          var target = null;
          var $targetLiTag = null;
          if (message.target !== null) {
            target = getTargetTabs($tabset, $tabContent, message.target);
            $targetLiTag = target.$liTag;
          }
          var dropdown = getDropdown();
          if (dropdown !== null) {
            if ($aTag.attr("data-toggle") === "dropdown")
              throw "Cannot insert a navbarMenu inside another one";
            $tabset = dropdown.$tabset;
            tabsetId = dropdown.id;
            $liTag.removeClass("nav-item").find(".nav-link").removeClass("nav-link").addClass("dropdown-item");
          }
          if ($aTag.attr("data-toggle") === "tab") {
            var index = getTabIndex($tabset, tabsetId);
            var tabId = "tab-" + tabsetId + "-" + index;
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
          renderContent($liTag[0], {
            html: $liTag.html(),
            deps: message.liTag.deps
          });
          renderContent($tabContent[0], {
            html: "",
            deps: message.divTag.deps
          }, "beforeend");
          $divTag.get().forEach(function(el) {
            $tabContent[0].appendChild(el);
            renderContent(el, el.innerHTML || el.textContent);
          });
          if (message.select) {
            $liTag.find("a").tab("show");
          }
          function getTabIndex($tabset2, tabsetId2) {
            var existingTabIds = [0];
            $tabset2.find("> li").each(function() {
              var $tab = (0, import_jquery41.default)(this).find("> a[data-toggle='tab']");
              if ($tab.length > 0) {
                var href = $tab.attr("href").replace(/.*(?=#[^\s]+$)/, "");
                var _index = href.replace("#tab-" + tabsetId2 + "-", "");
                existingTabIds.push(Number(_index));
              }
            });
            return Math.max.apply(null, existingTabIds) + 1;
          }
          function getDropdown() {
            if (message.menuName !== null) {
              var $dropdownATag = (0, import_jquery41.default)("a.dropdown-toggle[data-value='" + $escape(message.menuName) + "']");
              if ($dropdownATag.length === 0) {
                throw "There is no navbarMenu with menuName equal to '" + message.menuName + "'";
              }
              var $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
              var dropdownId = $dropdownTabset.attr("data-tabsetid");
              return {
                $tabset: $dropdownTabset,
                id: dropdownId
              };
            } else if (message.target !== null) {
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
          }
        });
        function ensureTabsetHasVisibleTab($tabset) {
          var inputBinding = $tabset.data("shiny-input-binding");
          if (!inputBinding.getValue($tabset)) {
            var destTabValue = getFirstTab($tabset);
            var evt = jQuery.Event("shiny:updateinput");
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
          import_jquery41.default.each(target, function(key, el) {
            if (key === "$liTag") {
              func(el);
            } else if (key === "$divTags") {
              import_jquery41.default.each(el, function(i, div) {
                func(div);
              });
            } else if (liTags && key === "$liTags") {
              import_jquery41.default.each(el, function(i, div) {
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
          var path = window.location.pathname;
          var oldQS = window.location.search;
          var oldHash = window.location.hash;
          var relURL = path;
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
            (0, import_jquery41.default)(document).trigger("hashchange");
        });
        addMessageHandler("resetBrush", function(message) {
          resetBrush(message.brushId);
        });
      }
    }, {
      key: "getTestSnapshotBaseUrl",
      value: function getTestSnapshotBaseUrl() {
        var _ref2 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref2$fullUrl = _ref2.fullUrl, fullUrl = _ref2$fullUrl === void 0 ? true : _ref2$fullUrl;
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
    if ((0, import_jquery42.default)('input[type="submit"], button[type="submit"]').length > 0) {
      target = inputsDefer;
      (0, import_jquery42.default)('input[type="submit"], button[type="submit"]').each(function() {
        (0, import_jquery42.default)(this).click(function(event) {
          event.preventDefault();
          inputsDefer.submit();
        });
      });
    } else {
      target = inputsRate;
    }
    var inputs = new InputValidateDecorator(target);
    windowShiny3.setInputValue = windowShiny3.onInputChange = function(name, value, opts) {
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
            var $inputObjectJ = (0, import_jquery42.default)(inputObjects[j]);
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
      var $el = (0, import_jquery42.default)(el);
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
    (0, import_jquery42.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
      var id = getIdFromEl(this);
      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        initialValues[".clientdata_output_" + id + "_width"] = this.offsetWidth;
        initialValues[".clientdata_output_" + id + "_height"] = this.offsetHeight;
      }
    });
    function getComputedBgColor(el) {
      if (!el) {
        return null;
      }
      var bgColor = getStyle(el, "background-color");
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
        families: fontFamily.replace(/"/g, "").split(", "),
        size: fontSize
      };
    }
    (0, import_jquery42.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
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
      var $el = (0, import_jquery42.default)(el);
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
      (0, import_jquery42.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
        var id = getIdFromEl(this);
        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          inputs.setInput(".clientdata_output_" + id + "_width", this.offsetWidth);
          inputs.setInput(".clientdata_output_" + id + "_height", this.offsetHeight);
        }
      });
      (0, import_jquery42.default)(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
        doSendTheme(this);
      });
      (0, import_jquery42.default)(".shiny-bound-output").each(function() {
        var $this = (0, import_jquery42.default)(this), binding = $this.data("shiny-output-binding");
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
    (0, import_jquery42.default)(".shiny-bound-output").each(function() {
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
      (0, import_jquery42.default)(".shiny-bound-output").each(function() {
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
        var $this = (0, import_jquery42.default)(this);
        evt.binding = $this.data("shiny-output-binding");
        $this.trigger(evt);
      });
      for (var name in lastKnownVisibleOutputs) {
        if (hasOwnProperty(lastKnownVisibleOutputs, name))
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
      namespace = namespace.split(".");
      return function(e) {
        var eventNamespace = e.namespace.split(".");
        for (var i = 0; i < namespace.length; i++) {
          if (eventNamespace.indexOf(namespace[i]) === -1)
            return;
        }
        handler.apply(this, [namespace, handler].concat(args));
      };
    }
    (0, import_jquery42.default)(window).resize(debounce(500, sendImageSizeFns.regular));
    var bs3classes = ["modal", "dropdown", "tab", "tooltip", "popover", "collapse"];
    import_jquery42.default.each(bs3classes, function(idx, classname) {
      (0, import_jquery42.default)(document.body).on("shown.bs." + classname + ".sendImageSize", "*", filterEventsByNamespace("bs", sendImageSizeFns.regular));
      (0, import_jquery42.default)(document.body).on("shown.bs." + classname + ".sendOutputHiddenState hidden.bs." + classname + ".sendOutputHiddenState", "*", filterEventsByNamespace("bs", sendOutputHiddenState));
    });
    (0, import_jquery42.default)(document.body).on("shown.sendImageSize", "*", sendImageSizeFns.regular);
    (0, import_jquery42.default)(document.body).on("shown.sendOutputHiddenState hidden.sendOutputHiddenState", "*", sendOutputHiddenState);
    initialValues[".clientdata_pixelratio"] = pixelRatio();
    (0, import_jquery42.default)(window).resize(function() {
      inputs.setInput(".clientdata_pixelratio", pixelRatio());
    });
    initialValues[".clientdata_url_protocol"] = window.location.protocol;
    initialValues[".clientdata_url_hostname"] = window.location.hostname;
    initialValues[".clientdata_url_port"] = window.location.port;
    initialValues[".clientdata_url_pathname"] = window.location.pathname;
    initialValues[".clientdata_url_search"] = window.location.search;
    (0, import_jquery42.default)(window).on("pushstate", function(e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
      return;
      e;
    });
    (0, import_jquery42.default)(window).on("popstate", function(e) {
      inputs.setInput(".clientdata_url_search", window.location.search);
      return;
      e;
    });
    initialValues[".clientdata_url_hash_initial"] = window.location.hash;
    initialValues[".clientdata_url_hash"] = window.location.hash;
    (0, import_jquery42.default)(window).on("hashchange", function(e) {
      inputs.setInput(".clientdata_url_hash", window.location.hash);
      return;
      e;
    });
    var singletonText = initialValues[".clientdata_singletons"] = (0, import_jquery42.default)('script[type="application/shiny-singletons"]').text();
    registerNames(singletonText.split(/,/));
    var dependencyText = (0, import_jquery42.default)('script[type="application/html-dependencies"]').text();
    import_jquery42.default.each(dependencyText.split(/;/), function(i, depStr) {
      var match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
      if (match) {
        registerDependency(match[1], match[2]);
      }
    });
    inputsNoResend.reset(initialValues);
    shinyapp.connect(initialValues);
    (0, import_jquery42.default)(document).one("shiny:connected", function() {
      initDeferredIframes();
    });
  }
  function initDeferredIframes() {
    if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
      return;
    }
    (0, import_jquery42.default)(".shiny-frame-deferred").each(function(i, el) {
      var $el = (0, import_jquery42.default)(el);
      $el.removeClass("shiny-frame-deferred");
      $el.attr("src", $el.attr("data-deferred-src"));
      $el.attr("data-deferred-src", null);
    });
  }

  // srcts/src/shiny/index.ts
  var windowShiny2;
  function setShiny(windowShiny_) {
    windowShiny2 = windowShiny_;
    windowShiny2.version = "1.7.0";
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
    windowShiny2.renderDependencies = renderDependencies;
    windowShiny2.renderContent = renderContent;
    windowShiny2.renderHtml = renderHtml2;
    (0, import_jquery43.default)(function() {
      setTimeout(function() {
        initShiny(windowShiny2);
      }, 1);
    });
  }

  // srcts/src/window/blobBuilder.ts
  function windowBlobBuilder() {
    var blob = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder || window.MSBlobBuilder;
    return blob;
  }

  // srcts/src/window/userAgent.ts
  function windowUserAgent() {
    return window.navigator.userAgent;
  }

  // srcts/src/shiny/reactlog.ts
  var import_jquery44 = __toModule(require_jquery());
  function initReactlog() {
    (0, import_jquery44.default)(document).on("keydown", function(e) {
      if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey)
        return;
      var url = "reactlog?w=" + window.escape(shinyShinyApp().config.workerId) + "&s=" + window.escape(shinyShinyApp().config.sessionId);
      window.open(url);
      e.preventDefault();
    });
    (0, import_jquery44.default)(document).on("keydown", function(e) {
      if (!(e.which === 115 && (e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey || e.which === 114 && (e.ctrlKey || e.metaKey) && e.shiftKey && !e.altKey)) {
        return;
      }
      var url = "reactlog/mark?w=" + window.escape(shinyShinyApp().config.workerId) + "&s=" + window.escape(shinyShinyApp().config.sessionId);
      import_jquery44.default.get(url, function(result) {
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
    setBlobBuilder(windowBlobBuilder());
    initReactlog();
  }

  // srcts/src/index.ts
  init();
})();
//# sourceMappingURL=shiny.js.map
