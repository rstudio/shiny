(function() {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __markAsModule = function(target) {
    return __defProp(target, "__esModule", {value: true});
  };
  var __commonJS = function(callback, module2) {
    return function() {
      if (!module2) {
        module2 = {exports: {}};
        callback(module2.exports, module2);
      }
      return module2.exports;
    };
  };
  var __export = function(target, all) {
    for (var name in all)
      __defProp(target, name, {get: all[name], enumerable: true});
  };
  var __exportStar = function(target, module2, desc) {
    if (module2 && typeof module2 === "object" || typeof module2 === "function")
      for (var keys2 = __getOwnPropNames(module2), i = 0, n = keys2.length, key; i < n; i++) {
        key = keys2[i];
        if (!__hasOwnProp.call(target, key) && key !== "default")
          __defProp(target, key, {get: function(k) {
            return module2[k];
          }.bind(null, key), enumerable: !(desc = __getOwnPropDesc(module2, key)) || desc.enumerable});
      }
    return target;
  };
  var __toModule = function(module2) {
    if (module2 && module2.__esModule)
      return module2;
    return __exportStar(__markAsModule(__defProp(module2 != null ? __create(__getProtoOf(module2)) : {}, "default", {value: module2, enumerable: true})), module2);
  };

  // globals:jquery
  var require_jquery = __commonJS(function(exports2, module2) {
    module2.exports = window.jQuery;
  });

  // node_modules/core-js/internals/global.js
  var require_global = __commonJS(function(exports2, module2) {
    var check = function(it) {
      return it && it.Math == Math && it;
    };
    module2.exports = check(typeof globalThis == "object" && globalThis) || check(typeof window == "object" && window) || check(typeof self == "object" && self) || check(typeof global == "object" && global) || function() {
      return this;
    }() || Function("return this")();
  });

  // node_modules/core-js/internals/fails.js
  var require_fails = __commonJS(function(exports2, module2) {
    module2.exports = function(exec) {
      try {
        return !!exec();
      } catch (error) {
        return true;
      }
    };
  });

  // node_modules/core-js/internals/descriptors.js
  var require_descriptors = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    module2.exports = !fails8(function() {
      return Object.defineProperty({}, 1, {get: function() {
        return 7;
      }})[1] != 7;
    });
  });

  // node_modules/core-js/internals/object-property-is-enumerable.js
  var require_object_property_is_enumerable = __commonJS(function(exports2) {
    "use strict";
    var nativePropertyIsEnumerable = {}.propertyIsEnumerable;
    var getOwnPropertyDescriptor2 = Object.getOwnPropertyDescriptor;
    var NASHORN_BUG = getOwnPropertyDescriptor2 && !nativePropertyIsEnumerable.call({1: 2}, 1);
    exports2.f = NASHORN_BUG ? function propertyIsEnumerable(V) {
      var descriptor = getOwnPropertyDescriptor2(this, V);
      return !!descriptor && descriptor.enumerable;
    } : nativePropertyIsEnumerable;
  });

  // node_modules/core-js/internals/create-property-descriptor.js
  var require_create_property_descriptor = __commonJS(function(exports2, module2) {
    module2.exports = function(bitmap, value) {
      return {
        enumerable: !(bitmap & 1),
        configurable: !(bitmap & 2),
        writable: !(bitmap & 4),
        value: value
      };
    };
  });

  // node_modules/core-js/internals/classof-raw.js
  var require_classof_raw = __commonJS(function(exports2, module2) {
    var toString2 = {}.toString;
    module2.exports = function(it) {
      return toString2.call(it).slice(8, -1);
    };
  });

  // node_modules/core-js/internals/indexed-object.js
  var require_indexed_object = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    var classof2 = require_classof_raw();
    var split = "".split;
    module2.exports = fails8(function() {
      return !Object("z").propertyIsEnumerable(0);
    }) ? function(it) {
      return classof2(it) == "String" ? split.call(it, "") : Object(it);
    } : Object;
  });

  // node_modules/core-js/internals/require-object-coercible.js
  var require_require_object_coercible = __commonJS(function(exports2, module2) {
    module2.exports = function(it) {
      if (it == void 0)
        throw TypeError("Can't call method on " + it);
      return it;
    };
  });

  // node_modules/core-js/internals/to-indexed-object.js
  var require_to_indexed_object = __commonJS(function(exports2, module2) {
    var IndexedObject2 = require_indexed_object();
    var requireObjectCoercible5 = require_require_object_coercible();
    module2.exports = function(it) {
      return IndexedObject2(requireObjectCoercible5(it));
    };
  });

  // node_modules/core-js/internals/is-object.js
  var require_is_object = __commonJS(function(exports2, module2) {
    module2.exports = function(it) {
      return typeof it === "object" ? it !== null : typeof it === "function";
    };
  });

  // node_modules/core-js/internals/to-primitive.js
  var require_to_primitive = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    module2.exports = function(input, PREFERRED_STRING) {
      if (!isObject3(input))
        return input;
      var fn, val;
      if (PREFERRED_STRING && typeof (fn = input.toString) == "function" && !isObject3(val = fn.call(input)))
        return val;
      if (typeof (fn = input.valueOf) == "function" && !isObject3(val = fn.call(input)))
        return val;
      if (!PREFERRED_STRING && typeof (fn = input.toString) == "function" && !isObject3(val = fn.call(input)))
        return val;
      throw TypeError("Can't convert object to primitive value");
    };
  });

  // node_modules/core-js/internals/has.js
  var require_has = __commonJS(function(exports2, module2) {
    var hasOwnProperty = {}.hasOwnProperty;
    module2.exports = function(it, key) {
      return hasOwnProperty.call(it, key);
    };
  });

  // node_modules/core-js/internals/document-create-element.js
  var require_document_create_element = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var isObject3 = require_is_object();
    var document2 = global5.document;
    var EXISTS = isObject3(document2) && isObject3(document2.createElement);
    module2.exports = function(it) {
      return EXISTS ? document2.createElement(it) : {};
    };
  });

  // node_modules/core-js/internals/ie8-dom-define.js
  var require_ie8_dom_define = __commonJS(function(exports2, module2) {
    var DESCRIPTORS3 = require_descriptors();
    var fails8 = require_fails();
    var createElement = require_document_create_element();
    module2.exports = !DESCRIPTORS3 && !fails8(function() {
      return Object.defineProperty(createElement("div"), "a", {
        get: function() {
          return 7;
        }
      }).a != 7;
    });
  });

  // node_modules/core-js/internals/object-get-own-property-descriptor.js
  var require_object_get_own_property_descriptor = __commonJS(function(exports2) {
    var DESCRIPTORS3 = require_descriptors();
    var propertyIsEnumerableModule = require_object_property_is_enumerable();
    var createPropertyDescriptor = require_create_property_descriptor();
    var toIndexedObject3 = require_to_indexed_object();
    var toPrimitive2 = require_to_primitive();
    var has2 = require_has();
    var IE8_DOM_DEFINE = require_ie8_dom_define();
    var nativeGetOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
    exports2.f = DESCRIPTORS3 ? nativeGetOwnPropertyDescriptor : function getOwnPropertyDescriptor2(O, P) {
      O = toIndexedObject3(O);
      P = toPrimitive2(P, true);
      if (IE8_DOM_DEFINE)
        try {
          return nativeGetOwnPropertyDescriptor(O, P);
        } catch (error) {
        }
      if (has2(O, P))
        return createPropertyDescriptor(!propertyIsEnumerableModule.f.call(O, P), O[P]);
    };
  });

  // node_modules/core-js/internals/an-object.js
  var require_an_object = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    module2.exports = function(it) {
      if (!isObject3(it)) {
        throw TypeError(String(it) + " is not an object");
      }
      return it;
    };
  });

  // node_modules/core-js/internals/object-define-property.js
  var require_object_define_property = __commonJS(function(exports2) {
    var DESCRIPTORS3 = require_descriptors();
    var IE8_DOM_DEFINE = require_ie8_dom_define();
    var anObject7 = require_an_object();
    var toPrimitive2 = require_to_primitive();
    var nativeDefineProperty = Object.defineProperty;
    exports2.f = DESCRIPTORS3 ? nativeDefineProperty : function defineProperty3(O, P, Attributes) {
      anObject7(O);
      P = toPrimitive2(P, true);
      anObject7(Attributes);
      if (IE8_DOM_DEFINE)
        try {
          return nativeDefineProperty(O, P, Attributes);
        } catch (error) {
        }
      if ("get" in Attributes || "set" in Attributes)
        throw TypeError("Accessors not supported");
      if ("value" in Attributes)
        O[P] = Attributes.value;
      return O;
    };
  });

  // node_modules/core-js/internals/create-non-enumerable-property.js
  var require_create_non_enumerable_property = __commonJS(function(exports2, module2) {
    var DESCRIPTORS3 = require_descriptors();
    var definePropertyModule = require_object_define_property();
    var createPropertyDescriptor = require_create_property_descriptor();
    module2.exports = DESCRIPTORS3 ? function(object, key, value) {
      return definePropertyModule.f(object, key, createPropertyDescriptor(1, value));
    } : function(object, key, value) {
      object[key] = value;
      return object;
    };
  });

  // node_modules/core-js/internals/set-global.js
  var require_set_global = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    module2.exports = function(key, value) {
      try {
        createNonEnumerableProperty2(global5, key, value);
      } catch (error) {
        global5[key] = value;
      }
      return value;
    };
  });

  // node_modules/core-js/internals/shared-store.js
  var require_shared_store = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var setGlobal = require_set_global();
    var SHARED = "__core-js_shared__";
    var store = global5[SHARED] || setGlobal(SHARED, {});
    module2.exports = store;
  });

  // node_modules/core-js/internals/inspect-source.js
  var require_inspect_source = __commonJS(function(exports2, module2) {
    var store = require_shared_store();
    var functionToString = Function.toString;
    if (typeof store.inspectSource != "function") {
      store.inspectSource = function(it) {
        return functionToString.call(it);
      };
    }
    module2.exports = store.inspectSource;
  });

  // node_modules/core-js/internals/native-weak-map.js
  var require_native_weak_map = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var inspectSource = require_inspect_source();
    var WeakMap = global5.WeakMap;
    module2.exports = typeof WeakMap === "function" && /native code/.test(inspectSource(WeakMap));
  });

  // node_modules/core-js/internals/is-pure.js
  var require_is_pure = __commonJS(function(exports2, module2) {
    module2.exports = false;
  });

  // node_modules/core-js/internals/shared.js
  var require_shared = __commonJS(function(exports2, module2) {
    var IS_PURE = require_is_pure();
    var store = require_shared_store();
    (module2.exports = function(key, value) {
      return store[key] || (store[key] = value !== void 0 ? value : {});
    })("versions", []).push({
      version: "3.9.0",
      mode: IS_PURE ? "pure" : "global",
      copyright: "\xA9 2021 Denis Pushkarev (zloirock.ru)"
    });
  });

  // node_modules/core-js/internals/uid.js
  var require_uid = __commonJS(function(exports2, module2) {
    var id = 0;
    var postfix = Math.random();
    module2.exports = function(key) {
      return "Symbol(" + String(key === void 0 ? "" : key) + ")_" + (++id + postfix).toString(36);
    };
  });

  // node_modules/core-js/internals/shared-key.js
  var require_shared_key = __commonJS(function(exports2, module2) {
    var shared = require_shared();
    var uid = require_uid();
    var keys2 = shared("keys");
    module2.exports = function(key) {
      return keys2[key] || (keys2[key] = uid(key));
    };
  });

  // node_modules/core-js/internals/hidden-keys.js
  var require_hidden_keys = __commonJS(function(exports2, module2) {
    module2.exports = {};
  });

  // node_modules/core-js/internals/internal-state.js
  var require_internal_state = __commonJS(function(exports2, module2) {
    var NATIVE_WEAK_MAP = require_native_weak_map();
    var global5 = require_global();
    var isObject3 = require_is_object();
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    var objectHas = require_has();
    var shared = require_shared_store();
    var sharedKey = require_shared_key();
    var hiddenKeys = require_hidden_keys();
    var WeakMap = global5.WeakMap;
    var set;
    var get;
    var has2;
    var enforce = function(it) {
      return has2(it) ? get(it) : set(it, {});
    };
    var getterFor = function(TYPE) {
      return function(it) {
        var state;
        if (!isObject3(it) || (state = get(it)).type !== TYPE) {
          throw TypeError("Incompatible receiver, " + TYPE + " required");
        }
        return state;
      };
    };
    if (NATIVE_WEAK_MAP) {
      store = shared.state || (shared.state = new WeakMap());
      wmget = store.get;
      wmhas = store.has;
      wmset = store.set;
      set = function(it, metadata) {
        metadata.facade = it;
        wmset.call(store, it, metadata);
        return metadata;
      };
      get = function(it) {
        return wmget.call(store, it) || {};
      };
      has2 = function(it) {
        return wmhas.call(store, it);
      };
    } else {
      STATE = sharedKey("state");
      hiddenKeys[STATE] = true;
      set = function(it, metadata) {
        metadata.facade = it;
        createNonEnumerableProperty2(it, STATE, metadata);
        return metadata;
      };
      get = function(it) {
        return objectHas(it, STATE) ? it[STATE] : {};
      };
      has2 = function(it) {
        return objectHas(it, STATE);
      };
    }
    var store;
    var wmget;
    var wmhas;
    var wmset;
    var STATE;
    module2.exports = {
      set: set,
      get: get,
      has: has2,
      enforce: enforce,
      getterFor: getterFor
    };
  });

  // node_modules/core-js/internals/redefine.js
  var require_redefine = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    var has2 = require_has();
    var setGlobal = require_set_global();
    var inspectSource = require_inspect_source();
    var InternalStateModule = require_internal_state();
    var getInternalState = InternalStateModule.get;
    var enforceInternalState = InternalStateModule.enforce;
    var TEMPLATE = String(String).split("String");
    (module2.exports = function(O, key, value, options) {
      var unsafe = options ? !!options.unsafe : false;
      var simple = options ? !!options.enumerable : false;
      var noTargetGet = options ? !!options.noTargetGet : false;
      var state;
      if (typeof value == "function") {
        if (typeof key == "string" && !has2(value, "name")) {
          createNonEnumerableProperty2(value, "name", key);
        }
        state = enforceInternalState(value);
        if (!state.source) {
          state.source = TEMPLATE.join(typeof key == "string" ? key : "");
        }
      }
      if (O === global5) {
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
        createNonEnumerableProperty2(O, key, value);
    })(Function.prototype, "toString", function toString2() {
      return typeof this == "function" && getInternalState(this).source || inspectSource(this);
    });
  });

  // node_modules/core-js/internals/path.js
  var require_path = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    module2.exports = global5;
  });

  // node_modules/core-js/internals/get-built-in.js
  var require_get_built_in = __commonJS(function(exports2, module2) {
    var path = require_path();
    var global5 = require_global();
    var aFunction = function(variable) {
      return typeof variable == "function" ? variable : void 0;
    };
    module2.exports = function(namespace, method) {
      return arguments.length < 2 ? aFunction(path[namespace]) || aFunction(global5[namespace]) : path[namespace] && path[namespace][method] || global5[namespace] && global5[namespace][method];
    };
  });

  // node_modules/core-js/internals/to-integer.js
  var require_to_integer = __commonJS(function(exports2, module2) {
    var ceil = Math.ceil;
    var floor = Math.floor;
    module2.exports = function(argument) {
      return isNaN(argument = +argument) ? 0 : (argument > 0 ? floor : ceil)(argument);
    };
  });

  // node_modules/core-js/internals/to-length.js
  var require_to_length = __commonJS(function(exports2, module2) {
    var toInteger3 = require_to_integer();
    var min4 = Math.min;
    module2.exports = function(argument) {
      return argument > 0 ? min4(toInteger3(argument), 9007199254740991) : 0;
    };
  });

  // node_modules/core-js/internals/to-absolute-index.js
  var require_to_absolute_index = __commonJS(function(exports2, module2) {
    var toInteger3 = require_to_integer();
    var max4 = Math.max;
    var min4 = Math.min;
    module2.exports = function(index, length) {
      var integer = toInteger3(index);
      return integer < 0 ? max4(integer + length, 0) : min4(integer, length);
    };
  });

  // node_modules/core-js/internals/array-includes.js
  var require_array_includes = __commonJS(function(exports2, module2) {
    var toIndexedObject3 = require_to_indexed_object();
    var toLength8 = require_to_length();
    var toAbsoluteIndex4 = require_to_absolute_index();
    var createMethod = function(IS_INCLUDES) {
      return function($this, el, fromIndex) {
        var O = toIndexedObject3($this);
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
    module2.exports = {
      includes: createMethod(true),
      indexOf: createMethod(false)
    };
  });

  // node_modules/core-js/internals/object-keys-internal.js
  var require_object_keys_internal = __commonJS(function(exports2, module2) {
    var has2 = require_has();
    var toIndexedObject3 = require_to_indexed_object();
    var indexOf2 = require_array_includes().indexOf;
    var hiddenKeys = require_hidden_keys();
    module2.exports = function(object, names) {
      var O = toIndexedObject3(object);
      var i = 0;
      var result = [];
      var key;
      for (key in O)
        !has2(hiddenKeys, key) && has2(O, key) && result.push(key);
      while (names.length > i)
        if (has2(O, key = names[i++])) {
          ~indexOf2(result, key) || result.push(key);
        }
      return result;
    };
  });

  // node_modules/core-js/internals/enum-bug-keys.js
  var require_enum_bug_keys = __commonJS(function(exports2, module2) {
    module2.exports = [
      "constructor",
      "hasOwnProperty",
      "isPrototypeOf",
      "propertyIsEnumerable",
      "toLocaleString",
      "toString",
      "valueOf"
    ];
  });

  // node_modules/core-js/internals/object-get-own-property-names.js
  var require_object_get_own_property_names = __commonJS(function(exports2) {
    var internalObjectKeys = require_object_keys_internal();
    var enumBugKeys = require_enum_bug_keys();
    var hiddenKeys = enumBugKeys.concat("length", "prototype");
    exports2.f = Object.getOwnPropertyNames || function getOwnPropertyNames2(O) {
      return internalObjectKeys(O, hiddenKeys);
    };
  });

  // node_modules/core-js/internals/object-get-own-property-symbols.js
  var require_object_get_own_property_symbols = __commonJS(function(exports2) {
    exports2.f = Object.getOwnPropertySymbols;
  });

  // node_modules/core-js/internals/own-keys.js
  var require_own_keys = __commonJS(function(exports2, module2) {
    var getBuiltIn = require_get_built_in();
    var getOwnPropertyNamesModule = require_object_get_own_property_names();
    var getOwnPropertySymbolsModule = require_object_get_own_property_symbols();
    var anObject7 = require_an_object();
    module2.exports = getBuiltIn("Reflect", "ownKeys") || function ownKeys(it) {
      var keys2 = getOwnPropertyNamesModule.f(anObject7(it));
      var getOwnPropertySymbols = getOwnPropertySymbolsModule.f;
      return getOwnPropertySymbols ? keys2.concat(getOwnPropertySymbols(it)) : keys2;
    };
  });

  // node_modules/core-js/internals/copy-constructor-properties.js
  var require_copy_constructor_properties = __commonJS(function(exports2, module2) {
    var has2 = require_has();
    var ownKeys = require_own_keys();
    var getOwnPropertyDescriptorModule = require_object_get_own_property_descriptor();
    var definePropertyModule = require_object_define_property();
    module2.exports = function(target, source) {
      var keys2 = ownKeys(source);
      var defineProperty3 = definePropertyModule.f;
      var getOwnPropertyDescriptor2 = getOwnPropertyDescriptorModule.f;
      for (var i = 0; i < keys2.length; i++) {
        var key = keys2[i];
        if (!has2(target, key))
          defineProperty3(target, key, getOwnPropertyDescriptor2(source, key));
      }
    };
  });

  // node_modules/core-js/internals/is-forced.js
  var require_is_forced = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    var replacement = /#|\.prototype\./;
    var isForced2 = function(feature, detection) {
      var value = data[normalize(feature)];
      return value == POLYFILL ? true : value == NATIVE ? false : typeof detection == "function" ? fails8(detection) : !!detection;
    };
    var normalize = isForced2.normalize = function(string) {
      return String(string).replace(replacement, ".").toLowerCase();
    };
    var data = isForced2.data = {};
    var NATIVE = isForced2.NATIVE = "N";
    var POLYFILL = isForced2.POLYFILL = "P";
    module2.exports = isForced2;
  });

  // node_modules/core-js/internals/export.js
  var require_export = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var getOwnPropertyDescriptor2 = require_object_get_own_property_descriptor().f;
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    var redefine4 = require_redefine();
    var setGlobal = require_set_global();
    var copyConstructorProperties = require_copy_constructor_properties();
    var isForced2 = require_is_forced();
    module2.exports = function(options, source) {
      var TARGET = options.target;
      var GLOBAL = options.global;
      var STATIC = options.stat;
      var FORCED3, target, key, targetProperty, sourceProperty, descriptor;
      if (GLOBAL) {
        target = global5;
      } else if (STATIC) {
        target = global5[TARGET] || setGlobal(TARGET, {});
      } else {
        target = (global5[TARGET] || {}).prototype;
      }
      if (target)
        for (key in source) {
          sourceProperty = source[key];
          if (options.noTargetGet) {
            descriptor = getOwnPropertyDescriptor2(target, key);
            targetProperty = descriptor && descriptor.value;
          } else
            targetProperty = target[key];
          FORCED3 = isForced2(GLOBAL ? key : TARGET + (STATIC ? "." : "#") + key, options.forced);
          if (!FORCED3 && targetProperty !== void 0) {
            if (typeof sourceProperty === typeof targetProperty)
              continue;
            copyConstructorProperties(sourceProperty, targetProperty);
          }
          if (options.sham || targetProperty && targetProperty.sham) {
            createNonEnumerableProperty2(sourceProperty, "sham", true);
          }
          redefine4(target, key, sourceProperty, options);
        }
    };
  });

  // node_modules/core-js/internals/array-method-is-strict.js
  var require_array_method_is_strict = __commonJS(function(exports2, module2) {
    "use strict";
    var fails8 = require_fails();
    module2.exports = function(METHOD_NAME, argument) {
      var method = [][METHOD_NAME];
      return !!method && fails8(function() {
        method.call(null, argument || function() {
          throw 1;
        }, 1);
      });
    };
  });

  // node_modules/core-js/internals/whitespaces.js
  var require_whitespaces = __commonJS(function(exports2, module2) {
    module2.exports = "	\n\v\f\r \xA0\u1680\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\u2028\u2029\uFEFF";
  });

  // node_modules/core-js/internals/string-trim.js
  var require_string_trim = __commonJS(function(exports2, module2) {
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
    module2.exports = {
      start: createMethod(1),
      end: createMethod(2),
      trim: createMethod(3)
    };
  });

  // node_modules/core-js/internals/number-parse-int.js
  var require_number_parse_int = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var trim3 = require_string_trim().trim;
    var whitespaces = require_whitespaces();
    var $parseInt = global5.parseInt;
    var hex = /^[+-]?0[Xx]/;
    var FORCED3 = $parseInt(whitespaces + "08") !== 8 || $parseInt(whitespaces + "0x16") !== 22;
    module2.exports = FORCED3 ? function parseInt2(string, radix) {
      var S = trim3(String(string));
      return $parseInt(S, radix >>> 0 || (hex.test(S) ? 16 : 10));
    } : $parseInt;
  });

  // node_modules/core-js/internals/is-array.js
  var require_is_array = __commonJS(function(exports2, module2) {
    var classof2 = require_classof_raw();
    module2.exports = Array.isArray || function isArray3(arg) {
      return classof2(arg) == "Array";
    };
  });

  // node_modules/core-js/internals/to-object.js
  var require_to_object = __commonJS(function(exports2, module2) {
    var requireObjectCoercible5 = require_require_object_coercible();
    module2.exports = function(argument) {
      return Object(requireObjectCoercible5(argument));
    };
  });

  // node_modules/core-js/internals/create-property.js
  var require_create_property = __commonJS(function(exports2, module2) {
    "use strict";
    var toPrimitive2 = require_to_primitive();
    var definePropertyModule = require_object_define_property();
    var createPropertyDescriptor = require_create_property_descriptor();
    module2.exports = function(object, key, value) {
      var propertyKey = toPrimitive2(key);
      if (propertyKey in object)
        definePropertyModule.f(object, propertyKey, createPropertyDescriptor(0, value));
      else
        object[propertyKey] = value;
    };
  });

  // node_modules/core-js/internals/native-symbol.js
  var require_native_symbol = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    module2.exports = !!Object.getOwnPropertySymbols && !fails8(function() {
      return !String(Symbol());
    });
  });

  // node_modules/core-js/internals/use-symbol-as-uid.js
  var require_use_symbol_as_uid = __commonJS(function(exports2, module2) {
    var NATIVE_SYMBOL = require_native_symbol();
    module2.exports = NATIVE_SYMBOL && !Symbol.sham && typeof Symbol.iterator == "symbol";
  });

  // node_modules/core-js/internals/well-known-symbol.js
  var require_well_known_symbol = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var shared = require_shared();
    var has2 = require_has();
    var uid = require_uid();
    var NATIVE_SYMBOL = require_native_symbol();
    var USE_SYMBOL_AS_UID = require_use_symbol_as_uid();
    var WellKnownSymbolsStore = shared("wks");
    var Symbol2 = global5.Symbol;
    var createWellKnownSymbol = USE_SYMBOL_AS_UID ? Symbol2 : Symbol2 && Symbol2.withoutSetter || uid;
    module2.exports = function(name) {
      if (!has2(WellKnownSymbolsStore, name)) {
        if (NATIVE_SYMBOL && has2(Symbol2, name))
          WellKnownSymbolsStore[name] = Symbol2[name];
        else
          WellKnownSymbolsStore[name] = createWellKnownSymbol("Symbol." + name);
      }
      return WellKnownSymbolsStore[name];
    };
  });

  // node_modules/core-js/internals/array-species-create.js
  var require_array_species_create = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    var isArray3 = require_is_array();
    var wellKnownSymbol3 = require_well_known_symbol();
    var SPECIES2 = wellKnownSymbol3("species");
    module2.exports = function(originalArray, length) {
      var C;
      if (isArray3(originalArray)) {
        C = originalArray.constructor;
        if (typeof C == "function" && (C === Array || isArray3(C.prototype)))
          C = void 0;
        else if (isObject3(C)) {
          C = C[SPECIES2];
          if (C === null)
            C = void 0;
        }
      }
      return new (C === void 0 ? Array : C)(length === 0 ? 0 : length);
    };
  });

  // node_modules/core-js/internals/engine-user-agent.js
  var require_engine_user_agent = __commonJS(function(exports2, module2) {
    var getBuiltIn = require_get_built_in();
    module2.exports = getBuiltIn("navigator", "userAgent") || "";
  });

  // node_modules/core-js/internals/engine-v8-version.js
  var require_engine_v8_version = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var userAgent = require_engine_user_agent();
    var process2 = global5.process;
    var versions = process2 && process2.versions;
    var v8 = versions && versions.v8;
    var match;
    var version;
    if (v8) {
      match = v8.split(".");
      version = match[0] + match[1];
    } else if (userAgent) {
      match = userAgent.match(/Edge\/(\d+)/);
      if (!match || match[1] >= 74) {
        match = userAgent.match(/Chrome\/(\d+)/);
        if (match)
          version = match[1];
      }
    }
    module2.exports = version && +version;
  });

  // node_modules/core-js/internals/array-method-has-species-support.js
  var require_array_method_has_species_support = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    var wellKnownSymbol3 = require_well_known_symbol();
    var V8_VERSION2 = require_engine_v8_version();
    var SPECIES2 = wellKnownSymbol3("species");
    module2.exports = function(METHOD_NAME) {
      return V8_VERSION2 >= 51 || !fails8(function() {
        var array = [];
        var constructor = array.constructor = {};
        constructor[SPECIES2] = function() {
          return {foo: 1};
        };
        return array[METHOD_NAME](Boolean).foo !== 1;
      });
    };
  });

  // node_modules/core-js/internals/this-number-value.js
  var require_this_number_value = __commonJS(function(exports2, module2) {
    var classof2 = require_classof_raw();
    module2.exports = function(value) {
      if (typeof value != "number" && classof2(value) != "Number") {
        throw TypeError("Incorrect invocation");
      }
      return +value;
    };
  });

  // node_modules/core-js/internals/object-keys.js
  var require_object_keys = __commonJS(function(exports2, module2) {
    var internalObjectKeys = require_object_keys_internal();
    var enumBugKeys = require_enum_bug_keys();
    module2.exports = Object.keys || function keys2(O) {
      return internalObjectKeys(O, enumBugKeys);
    };
  });

  // node_modules/core-js/internals/to-string-tag-support.js
  var require_to_string_tag_support = __commonJS(function(exports2, module2) {
    var wellKnownSymbol3 = require_well_known_symbol();
    var TO_STRING_TAG = wellKnownSymbol3("toStringTag");
    var test = {};
    test[TO_STRING_TAG] = "z";
    module2.exports = String(test) === "[object z]";
  });

  // node_modules/core-js/internals/classof.js
  var require_classof = __commonJS(function(exports2, module2) {
    var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
    var classofRaw = require_classof_raw();
    var wellKnownSymbol3 = require_well_known_symbol();
    var TO_STRING_TAG = wellKnownSymbol3("toStringTag");
    var CORRECT_ARGUMENTS = classofRaw(function() {
      return arguments;
    }()) == "Arguments";
    var tryGet = function(it, key) {
      try {
        return it[key];
      } catch (error) {
      }
    };
    module2.exports = TO_STRING_TAG_SUPPORT2 ? classofRaw : function(it) {
      var O, tag, result;
      return it === void 0 ? "Undefined" : it === null ? "Null" : typeof (tag = tryGet(O = Object(it), TO_STRING_TAG)) == "string" ? tag : CORRECT_ARGUMENTS ? classofRaw(O) : (result = classofRaw(O)) == "Object" && typeof O.callee == "function" ? "Arguments" : result;
    };
  });

  // node_modules/core-js/internals/object-to-string.js
  var require_object_to_string = __commonJS(function(exports2, module2) {
    "use strict";
    var TO_STRING_TAG_SUPPORT2 = require_to_string_tag_support();
    var classof2 = require_classof();
    module2.exports = TO_STRING_TAG_SUPPORT2 ? {}.toString : function toString2() {
      return "[object " + classof2(this) + "]";
    };
  });

  // node_modules/core-js/internals/number-parse-float.js
  var require_number_parse_float = __commonJS(function(exports2, module2) {
    var global5 = require_global();
    var trim3 = require_string_trim().trim;
    var whitespaces = require_whitespaces();
    var $parseFloat = global5.parseFloat;
    var FORCED3 = 1 / $parseFloat(whitespaces + "-0") !== -Infinity;
    module2.exports = FORCED3 ? function parseFloat2(string) {
      var trimmedString = trim3(String(string));
      var result = $parseFloat(trimmedString);
      return result === 0 && trimmedString.charAt(0) == "-" ? -0 : result;
    } : $parseFloat;
  });

  // node_modules/core-js/internals/regexp-flags.js
  var require_regexp_flags = __commonJS(function(exports2, module2) {
    "use strict";
    var anObject7 = require_an_object();
    module2.exports = function() {
      var that = anObject7(this);
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
  });

  // node_modules/core-js/internals/regexp-sticky-helpers.js
  var require_regexp_sticky_helpers = __commonJS(function(exports2) {
    "use strict";
    var fails8 = require_fails();
    function RE(s, f) {
      return RegExp(s, f);
    }
    exports2.UNSUPPORTED_Y = fails8(function() {
      var re = RE("a", "y");
      re.lastIndex = 2;
      return re.exec("abcd") != null;
    });
    exports2.BROKEN_CARET = fails8(function() {
      var re = RE("^r", "gy");
      re.lastIndex = 2;
      return re.exec("str") != null;
    });
  });

  // node_modules/core-js/internals/regexp-exec.js
  var require_regexp_exec = __commonJS(function(exports2, module2) {
    "use strict";
    var regexpFlags = require_regexp_flags();
    var stickyHelpers = require_regexp_sticky_helpers();
    var nativeExec = RegExp.prototype.exec;
    var nativeReplace = String.prototype.replace;
    var patchedExec = nativeExec;
    var UPDATES_LAST_INDEX_WRONG = function() {
      var re1 = /a/;
      var re2 = /b*/g;
      nativeExec.call(re1, "a");
      nativeExec.call(re2, "a");
      return re1.lastIndex !== 0 || re2.lastIndex !== 0;
    }();
    var UNSUPPORTED_Y = stickyHelpers.UNSUPPORTED_Y || stickyHelpers.BROKEN_CARET;
    var NPCG_INCLUDED = /()??/.exec("")[1] !== void 0;
    var PATCH = UPDATES_LAST_INDEX_WRONG || NPCG_INCLUDED || UNSUPPORTED_Y;
    if (PATCH) {
      patchedExec = function exec(str) {
        var re = this;
        var lastIndex, reCopy, match, i;
        var sticky = UNSUPPORTED_Y && re.sticky;
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
    module2.exports = patchedExec;
  });

  // node_modules/core-js/modules/es.regexp.exec.js
  var require_es_regexp_exec = __commonJS(function() {
    "use strict";
    var $25 = require_export();
    var exec = require_regexp_exec();
    $25({target: "RegExp", proto: true, forced: /./.exec !== exec}, {
      exec: exec
    });
  });

  // node_modules/core-js/internals/fix-regexp-well-known-symbol-logic.js
  var require_fix_regexp_well_known_symbol_logic = __commonJS(function(exports2, module2) {
    "use strict";
    require_es_regexp_exec();
    var redefine4 = require_redefine();
    var fails8 = require_fails();
    var wellKnownSymbol3 = require_well_known_symbol();
    var regexpExec2 = require_regexp_exec();
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    var SPECIES2 = wellKnownSymbol3("species");
    var REPLACE_SUPPORTS_NAMED_GROUPS = !fails8(function() {
      var re = /./;
      re.exec = function() {
        var result = [];
        result.groups = {a: "7"};
        return result;
      };
      return "".replace(re, "$<a>") !== "7";
    });
    var REPLACE_KEEPS_$0 = function() {
      return "a".replace(/./, "$0") === "$0";
    }();
    var REPLACE = wellKnownSymbol3("replace");
    var REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE = function() {
      if (/./[REPLACE]) {
        return /./[REPLACE]("a", "$0") === "";
      }
      return false;
    }();
    var SPLIT_WORKS_WITH_OVERWRITTEN_EXEC = !fails8(function() {
      var re = /(?:)/;
      var originalExec = re.exec;
      re.exec = function() {
        return originalExec.apply(this, arguments);
      };
      var result = "ab".split(re);
      return result.length !== 2 || result[0] !== "a" || result[1] !== "b";
    });
    module2.exports = function(KEY, length, exec, sham) {
      var SYMBOL = wellKnownSymbol3(KEY);
      var DELEGATES_TO_SYMBOL = !fails8(function() {
        var O = {};
        O[SYMBOL] = function() {
          return 7;
        };
        return ""[KEY](O) != 7;
      });
      var DELEGATES_TO_EXEC = DELEGATES_TO_SYMBOL && !fails8(function() {
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
      if (!DELEGATES_TO_SYMBOL || !DELEGATES_TO_EXEC || KEY === "replace" && !(REPLACE_SUPPORTS_NAMED_GROUPS && REPLACE_KEEPS_$0 && !REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE) || KEY === "split" && !SPLIT_WORKS_WITH_OVERWRITTEN_EXEC) {
        var nativeRegExpMethod = /./[SYMBOL];
        var methods = exec(SYMBOL, ""[KEY], function(nativeMethod, regexp, str, arg2, forceStringMethod) {
          if (regexp.exec === regexpExec2) {
            if (DELEGATES_TO_SYMBOL && !forceStringMethod) {
              return {done: true, value: nativeRegExpMethod.call(regexp, str, arg2)};
            }
            return {done: true, value: nativeMethod.call(str, regexp, arg2)};
          }
          return {done: false};
        }, {
          REPLACE_KEEPS_$0: REPLACE_KEEPS_$0,
          REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE: REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE
        });
        var stringMethod = methods[0];
        var regexMethod = methods[1];
        redefine4(String.prototype, KEY, stringMethod);
        redefine4(RegExp.prototype, SYMBOL, length == 2 ? function(string, arg) {
          return regexMethod.call(string, this, arg);
        } : function(string) {
          return regexMethod.call(string, this);
        });
      }
      if (sham)
        createNonEnumerableProperty2(RegExp.prototype[SYMBOL], "sham", true);
    };
  });

  // node_modules/core-js/internals/string-multibyte.js
  var require_string_multibyte = __commonJS(function(exports2, module2) {
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
    module2.exports = {
      codeAt: createMethod(false),
      charAt: createMethod(true)
    };
  });

  // node_modules/core-js/internals/advance-string-index.js
  var require_advance_string_index = __commonJS(function(exports2, module2) {
    "use strict";
    var charAt = require_string_multibyte().charAt;
    module2.exports = function(S, index, unicode) {
      return index + (unicode ? charAt(S, index).length : 1);
    };
  });

  // node_modules/core-js/internals/regexp-exec-abstract.js
  var require_regexp_exec_abstract = __commonJS(function(exports2, module2) {
    var classof2 = require_classof_raw();
    var regexpExec2 = require_regexp_exec();
    module2.exports = function(R, S) {
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
  });

  // node_modules/core-js/internals/get-substitution.js
  var require_get_substitution = __commonJS(function(exports2, module2) {
    var toObject4 = require_to_object();
    var floor = Math.floor;
    var replace = "".replace;
    var SUBSTITUTION_SYMBOLS = /\$([$&'`]|\d\d?|<[^>]*>)/g;
    var SUBSTITUTION_SYMBOLS_NO_NAMED = /\$([$&'`]|\d\d?)/g;
    module2.exports = function(matched, str, position, captures, namedCaptures, replacement) {
      var tailPos = position + matched.length;
      var m = captures.length;
      var symbols = SUBSTITUTION_SYMBOLS_NO_NAMED;
      if (namedCaptures !== void 0) {
        namedCaptures = toObject4(namedCaptures);
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
  });

  // node_modules/core-js/internals/is-regexp.js
  var require_is_regexp = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    var classof2 = require_classof_raw();
    var wellKnownSymbol3 = require_well_known_symbol();
    var MATCH = wellKnownSymbol3("match");
    module2.exports = function(it) {
      var isRegExp2;
      return isObject3(it) && ((isRegExp2 = it[MATCH]) !== void 0 ? !!isRegExp2 : classof2(it) == "RegExp");
    };
  });

  // node_modules/core-js/internals/a-function.js
  var require_a_function = __commonJS(function(exports2, module2) {
    module2.exports = function(it) {
      if (typeof it != "function") {
        throw TypeError(String(it) + " is not a function");
      }
      return it;
    };
  });

  // node_modules/core-js/internals/species-constructor.js
  var require_species_constructor = __commonJS(function(exports2, module2) {
    var anObject7 = require_an_object();
    var aFunction = require_a_function();
    var wellKnownSymbol3 = require_well_known_symbol();
    var SPECIES2 = wellKnownSymbol3("species");
    module2.exports = function(O, defaultConstructor) {
      var C = anObject7(O).constructor;
      var S;
      return C === void 0 || (S = anObject7(C)[SPECIES2]) == void 0 ? defaultConstructor : aFunction(S);
    };
  });

  // node_modules/core-js/internals/function-bind-context.js
  var require_function_bind_context = __commonJS(function(exports2, module2) {
    var aFunction = require_a_function();
    module2.exports = function(fn, that, length) {
      aFunction(fn);
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
  });

  // node_modules/core-js/internals/array-iteration.js
  var require_array_iteration = __commonJS(function(exports2, module2) {
    var bind = require_function_bind_context();
    var IndexedObject2 = require_indexed_object();
    var toObject4 = require_to_object();
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
        var O = toObject4($this);
        var self2 = IndexedObject2(O);
        var boundFunction = bind(callbackfn, that, 3);
        var length = toLength8(self2.length);
        var index = 0;
        var create2 = specificCreate || arraySpeciesCreate3;
        var target = IS_MAP ? create2($this, length) : IS_FILTER || IS_FILTER_OUT ? create2($this, 0) : void 0;
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
    module2.exports = {
      forEach: createMethod(0),
      map: createMethod(1),
      filter: createMethod(2),
      some: createMethod(3),
      every: createMethod(4),
      find: createMethod(5),
      findIndex: createMethod(6),
      filterOut: createMethod(7)
    };
  });

  // node_modules/core-js/internals/object-define-properties.js
  var require_object_define_properties = __commonJS(function(exports2, module2) {
    var DESCRIPTORS3 = require_descriptors();
    var definePropertyModule = require_object_define_property();
    var anObject7 = require_an_object();
    var objectKeys = require_object_keys();
    module2.exports = DESCRIPTORS3 ? Object.defineProperties : function defineProperties(O, Properties) {
      anObject7(O);
      var keys2 = objectKeys(Properties);
      var length = keys2.length;
      var index = 0;
      var key;
      while (length > index)
        definePropertyModule.f(O, key = keys2[index++], Properties[key]);
      return O;
    };
  });

  // node_modules/core-js/internals/html.js
  var require_html = __commonJS(function(exports2, module2) {
    var getBuiltIn = require_get_built_in();
    module2.exports = getBuiltIn("document", "documentElement");
  });

  // node_modules/core-js/internals/object-create.js
  var require_object_create = __commonJS(function(exports2, module2) {
    var anObject7 = require_an_object();
    var defineProperties = require_object_define_properties();
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
        activeXDocument = document.domain && new ActiveXObject("htmlfile");
      } catch (error) {
      }
      NullProtoObject = activeXDocument ? NullProtoObjectViaActiveX(activeXDocument) : NullProtoObjectViaIFrame();
      var length = enumBugKeys.length;
      while (length--)
        delete NullProtoObject[PROTOTYPE][enumBugKeys[length]];
      return NullProtoObject();
    };
    hiddenKeys[IE_PROTO] = true;
    module2.exports = Object.create || function create2(O, Properties) {
      var result;
      if (O !== null) {
        EmptyConstructor[PROTOTYPE] = anObject7(O);
        result = new EmptyConstructor();
        EmptyConstructor[PROTOTYPE] = null;
        result[IE_PROTO] = O;
      } else
        result = NullProtoObject();
      return Properties === void 0 ? result : defineProperties(result, Properties);
    };
  });

  // node_modules/core-js/internals/add-to-unscopables.js
  var require_add_to_unscopables = __commonJS(function(exports2, module2) {
    var wellKnownSymbol3 = require_well_known_symbol();
    var create2 = require_object_create();
    var definePropertyModule = require_object_define_property();
    var UNSCOPABLES = wellKnownSymbol3("unscopables");
    var ArrayPrototype = Array.prototype;
    if (ArrayPrototype[UNSCOPABLES] == void 0) {
      definePropertyModule.f(ArrayPrototype, UNSCOPABLES, {
        configurable: true,
        value: create2(null)
      });
    }
    module2.exports = function(key) {
      ArrayPrototype[UNSCOPABLES][key] = true;
    };
  });

  // node_modules/core-js/internals/array-for-each.js
  var require_array_for_each = __commonJS(function(exports2, module2) {
    "use strict";
    var $forEach = require_array_iteration().forEach;
    var arrayMethodIsStrict4 = require_array_method_is_strict();
    var STRICT_METHOD4 = arrayMethodIsStrict4("forEach");
    module2.exports = !STRICT_METHOD4 ? function forEach3(callbackfn) {
      return $forEach(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    } : [].forEach;
  });

  // node_modules/core-js/internals/array-reduce.js
  var require_array_reduce = __commonJS(function(exports2, module2) {
    var aFunction = require_a_function();
    var toObject4 = require_to_object();
    var IndexedObject2 = require_indexed_object();
    var toLength8 = require_to_length();
    var createMethod = function(IS_RIGHT) {
      return function(that, callbackfn, argumentsLength, memo) {
        aFunction(callbackfn);
        var O = toObject4(that);
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
    module2.exports = {
      left: createMethod(false),
      right: createMethod(true)
    };
  });

  // node_modules/core-js/internals/engine-is-node.js
  var require_engine_is_node = __commonJS(function(exports2, module2) {
    var classof2 = require_classof_raw();
    var global5 = require_global();
    module2.exports = classof2(global5.process) == "process";
  });

  // node_modules/core-js/internals/array-buffer-native.js
  var require_array_buffer_native = __commonJS(function(exports2, module2) {
    module2.exports = typeof ArrayBuffer !== "undefined" && typeof DataView !== "undefined";
  });

  // node_modules/core-js/internals/redefine-all.js
  var require_redefine_all = __commonJS(function(exports2, module2) {
    var redefine4 = require_redefine();
    module2.exports = function(target, src, options) {
      for (var key in src)
        redefine4(target, key, src[key], options);
      return target;
    };
  });

  // node_modules/core-js/internals/an-instance.js
  var require_an_instance = __commonJS(function(exports2, module2) {
    module2.exports = function(it, Constructor, name) {
      if (!(it instanceof Constructor)) {
        throw TypeError("Incorrect " + (name ? name + " " : "") + "invocation");
      }
      return it;
    };
  });

  // node_modules/core-js/internals/to-index.js
  var require_to_index = __commonJS(function(exports2, module2) {
    var toInteger3 = require_to_integer();
    var toLength8 = require_to_length();
    module2.exports = function(it) {
      if (it === void 0)
        return 0;
      var number = toInteger3(it);
      var length = toLength8(number);
      if (number !== length)
        throw RangeError("Wrong length or index");
      return length;
    };
  });

  // node_modules/core-js/internals/ieee754.js
  var require_ieee754 = __commonJS(function(exports2, module2) {
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
    module2.exports = {
      pack: pack,
      unpack: unpack
    };
  });

  // node_modules/core-js/internals/correct-prototype-getter.js
  var require_correct_prototype_getter = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    module2.exports = !fails8(function() {
      function F() {
      }
      F.prototype.constructor = null;
      return Object.getPrototypeOf(new F()) !== F.prototype;
    });
  });

  // node_modules/core-js/internals/object-get-prototype-of.js
  var require_object_get_prototype_of = __commonJS(function(exports2, module2) {
    var has2 = require_has();
    var toObject4 = require_to_object();
    var sharedKey = require_shared_key();
    var CORRECT_PROTOTYPE_GETTER = require_correct_prototype_getter();
    var IE_PROTO = sharedKey("IE_PROTO");
    var ObjectPrototype = Object.prototype;
    module2.exports = CORRECT_PROTOTYPE_GETTER ? Object.getPrototypeOf : function(O) {
      O = toObject4(O);
      if (has2(O, IE_PROTO))
        return O[IE_PROTO];
      if (typeof O.constructor == "function" && O instanceof O.constructor) {
        return O.constructor.prototype;
      }
      return O instanceof Object ? ObjectPrototype : null;
    };
  });

  // node_modules/core-js/internals/a-possible-prototype.js
  var require_a_possible_prototype = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    module2.exports = function(it) {
      if (!isObject3(it) && it !== null) {
        throw TypeError("Can't set " + String(it) + " as a prototype");
      }
      return it;
    };
  });

  // node_modules/core-js/internals/object-set-prototype-of.js
  var require_object_set_prototype_of = __commonJS(function(exports2, module2) {
    var anObject7 = require_an_object();
    var aPossiblePrototype = require_a_possible_prototype();
    module2.exports = Object.setPrototypeOf || ("__proto__" in {} ? function() {
      var CORRECT_SETTER = false;
      var test = {};
      var setter;
      try {
        setter = Object.getOwnPropertyDescriptor(Object.prototype, "__proto__").set;
        setter.call(test, []);
        CORRECT_SETTER = test instanceof Array;
      } catch (error) {
      }
      return function setPrototypeOf(O, proto) {
        anObject7(O);
        aPossiblePrototype(proto);
        if (CORRECT_SETTER)
          setter.call(O, proto);
        else
          O.__proto__ = proto;
        return O;
      };
    }() : void 0);
  });

  // node_modules/core-js/internals/array-fill.js
  var require_array_fill = __commonJS(function(exports2, module2) {
    "use strict";
    var toObject4 = require_to_object();
    var toAbsoluteIndex4 = require_to_absolute_index();
    var toLength8 = require_to_length();
    module2.exports = function fill(value) {
      var O = toObject4(this);
      var length = toLength8(O.length);
      var argumentsLength = arguments.length;
      var index = toAbsoluteIndex4(argumentsLength > 1 ? arguments[1] : void 0, length);
      var end = argumentsLength > 2 ? arguments[2] : void 0;
      var endPos = end === void 0 ? length : toAbsoluteIndex4(end, length);
      while (endPos > index)
        O[index++] = value;
      return O;
    };
  });

  // node_modules/core-js/internals/set-to-string-tag.js
  var require_set_to_string_tag = __commonJS(function(exports2, module2) {
    var defineProperty3 = require_object_define_property().f;
    var has2 = require_has();
    var wellKnownSymbol3 = require_well_known_symbol();
    var TO_STRING_TAG = wellKnownSymbol3("toStringTag");
    module2.exports = function(it, TAG, STATIC) {
      if (it && !has2(it = STATIC ? it : it.prototype, TO_STRING_TAG)) {
        defineProperty3(it, TO_STRING_TAG, {configurable: true, value: TAG});
      }
    };
  });

  // node_modules/core-js/internals/array-buffer.js
  var require_array_buffer = __commonJS(function(exports2, module2) {
    "use strict";
    var global5 = require_global();
    var DESCRIPTORS3 = require_descriptors();
    var NATIVE_ARRAY_BUFFER2 = require_array_buffer_native();
    var createNonEnumerableProperty2 = require_create_non_enumerable_property();
    var redefineAll = require_redefine_all();
    var fails8 = require_fails();
    var anInstance = require_an_instance();
    var toInteger3 = require_to_integer();
    var toLength8 = require_to_length();
    var toIndex = require_to_index();
    var IEEE754 = require_ieee754();
    var getPrototypeOf = require_object_get_prototype_of();
    var setPrototypeOf = require_object_set_prototype_of();
    var getOwnPropertyNames2 = require_object_get_own_property_names().f;
    var defineProperty3 = require_object_define_property().f;
    var arrayFill = require_array_fill();
    var setToStringTag = require_set_to_string_tag();
    var InternalStateModule = require_internal_state();
    var getInternalState = InternalStateModule.get;
    var setInternalState = InternalStateModule.set;
    var ARRAY_BUFFER2 = "ArrayBuffer";
    var DATA_VIEW = "DataView";
    var PROTOTYPE = "prototype";
    var WRONG_LENGTH = "Wrong length";
    var WRONG_INDEX = "Wrong index";
    var NativeArrayBuffer2 = global5[ARRAY_BUFFER2];
    var $ArrayBuffer = NativeArrayBuffer2;
    var $DataView = global5[DATA_VIEW];
    var $DataViewPrototype = $DataView && $DataView[PROTOTYPE];
    var ObjectPrototype = Object.prototype;
    var RangeError2 = global5.RangeError;
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
      defineProperty3(Constructor[PROTOTYPE], key2, {get: function() {
        return getInternalState(this)[key2];
      }});
    };
    var get = function(view, count, index, isLittleEndian) {
      var intIndex = toIndex(index);
      var store = getInternalState(view);
      if (intIndex + count > store.byteLength)
        throw RangeError2(WRONG_INDEX);
      var bytes = getInternalState(store.buffer).bytes;
      var start = intIndex + store.byteOffset;
      var pack = bytes.slice(start, start + count);
      return isLittleEndian ? pack : pack.reverse();
    };
    var set = function(view, count, index, conversion, value, isLittleEndian) {
      var intIndex = toIndex(index);
      var store = getInternalState(view);
      if (intIndex + count > store.byteLength)
        throw RangeError2(WRONG_INDEX);
      var bytes = getInternalState(store.buffer).bytes;
      var start = intIndex + store.byteOffset;
      var pack = conversion(+value);
      for (var i = 0; i < count; i++)
        bytes[start + i] = pack[isLittleEndian ? i : count - i - 1];
    };
    if (!NATIVE_ARRAY_BUFFER2) {
      $ArrayBuffer = function ArrayBuffer4(length) {
        anInstance(this, $ArrayBuffer, ARRAY_BUFFER2);
        var byteLength = toIndex(length);
        setInternalState(this, {
          bytes: arrayFill.call(new Array(byteLength), 0),
          byteLength: byteLength
        });
        if (!DESCRIPTORS3)
          this.byteLength = byteLength;
      };
      $DataView = function DataView3(buffer, byteOffset, byteLength) {
        anInstance(this, $DataView, DATA_VIEW);
        anInstance(buffer, $ArrayBuffer, DATA_VIEW);
        var bufferLength = getInternalState(buffer).byteLength;
        var offset = toInteger3(byteOffset);
        if (offset < 0 || offset > bufferLength)
          throw RangeError2("Wrong offset");
        byteLength = byteLength === void 0 ? bufferLength - offset : toLength8(byteLength);
        if (offset + byteLength > bufferLength)
          throw RangeError2(WRONG_LENGTH);
        setInternalState(this, {
          buffer: buffer,
          byteLength: byteLength,
          byteOffset: offset
        });
        if (!DESCRIPTORS3) {
          this.buffer = buffer;
          this.byteLength = byteLength;
          this.byteOffset = offset;
        }
      };
      if (DESCRIPTORS3) {
        addGetter($ArrayBuffer, "byteLength");
        addGetter($DataView, "buffer");
        addGetter($DataView, "byteLength");
        addGetter($DataView, "byteOffset");
      }
      redefineAll($DataView[PROTOTYPE], {
        getInt8: function getInt8(byteOffset) {
          return get(this, 1, byteOffset)[0] << 24 >> 24;
        },
        getUint8: function getUint8(byteOffset) {
          return get(this, 1, byteOffset)[0];
        },
        getInt16: function getInt16(byteOffset) {
          var bytes = get(this, 2, byteOffset, arguments.length > 1 ? arguments[1] : void 0);
          return (bytes[1] << 8 | bytes[0]) << 16 >> 16;
        },
        getUint16: function getUint16(byteOffset) {
          var bytes = get(this, 2, byteOffset, arguments.length > 1 ? arguments[1] : void 0);
          return bytes[1] << 8 | bytes[0];
        },
        getInt32: function getInt32(byteOffset) {
          return unpackInt32(get(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0));
        },
        getUint32: function getUint32(byteOffset) {
          return unpackInt32(get(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0)) >>> 0;
        },
        getFloat32: function getFloat32(byteOffset) {
          return unpackIEEE754(get(this, 4, byteOffset, arguments.length > 1 ? arguments[1] : void 0), 23);
        },
        getFloat64: function getFloat64(byteOffset) {
          return unpackIEEE754(get(this, 8, byteOffset, arguments.length > 1 ? arguments[1] : void 0), 52);
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
      if (!fails8(function() {
        NativeArrayBuffer2(1);
      }) || !fails8(function() {
        new NativeArrayBuffer2(-1);
      }) || fails8(function() {
        new NativeArrayBuffer2();
        new NativeArrayBuffer2(1.5);
        new NativeArrayBuffer2(NaN);
        return NativeArrayBuffer2.name != ARRAY_BUFFER2;
      })) {
        $ArrayBuffer = function ArrayBuffer4(length) {
          anInstance(this, $ArrayBuffer);
          return new NativeArrayBuffer2(toIndex(length));
        };
        ArrayBufferPrototype = $ArrayBuffer[PROTOTYPE] = NativeArrayBuffer2[PROTOTYPE];
        for (keys2 = getOwnPropertyNames2(NativeArrayBuffer2), j = 0; keys2.length > j; ) {
          if (!((key = keys2[j++]) in $ArrayBuffer)) {
            createNonEnumerableProperty2($ArrayBuffer, key, NativeArrayBuffer2[key]);
          }
        }
        ArrayBufferPrototype.constructor = $ArrayBuffer;
      }
      if (setPrototypeOf && getPrototypeOf($DataViewPrototype) !== ObjectPrototype) {
        setPrototypeOf($DataViewPrototype, ObjectPrototype);
      }
      testView = new $DataView(new $ArrayBuffer(2));
      nativeSetInt8 = $DataViewPrototype.setInt8;
      testView.setInt8(0, 2147483648);
      testView.setInt8(1, 2147483649);
      if (testView.getInt8(0) || !testView.getInt8(1))
        redefineAll($DataViewPrototype, {
          setInt8: function setInt8(byteOffset, value) {
            nativeSetInt8.call(this, byteOffset, value << 24 >> 24);
          },
          setUint8: function setUint8(byteOffset, value) {
            nativeSetInt8.call(this, byteOffset, value << 24 >> 24);
          }
        }, {unsafe: true});
    }
    var ArrayBufferPrototype;
    var keys2;
    var j;
    var key;
    var testView;
    var nativeSetInt8;
    setToStringTag($ArrayBuffer, ARRAY_BUFFER2);
    setToStringTag($DataView, DATA_VIEW);
    module2.exports = {
      ArrayBuffer: $ArrayBuffer,
      DataView: $DataView
    };
  });

  // node_modules/core-js/internals/set-species.js
  var require_set_species = __commonJS(function(exports2, module2) {
    "use strict";
    var getBuiltIn = require_get_built_in();
    var definePropertyModule = require_object_define_property();
    var wellKnownSymbol3 = require_well_known_symbol();
    var DESCRIPTORS3 = require_descriptors();
    var SPECIES2 = wellKnownSymbol3("species");
    module2.exports = function(CONSTRUCTOR_NAME) {
      var Constructor = getBuiltIn(CONSTRUCTOR_NAME);
      var defineProperty3 = definePropertyModule.f;
      if (DESCRIPTORS3 && Constructor && !Constructor[SPECIES2]) {
        defineProperty3(Constructor, SPECIES2, {
          configurable: true,
          get: function() {
            return this;
          }
        });
      }
    };
  });

  // node_modules/core-js/internals/inherit-if-required.js
  var require_inherit_if_required = __commonJS(function(exports2, module2) {
    var isObject3 = require_is_object();
    var setPrototypeOf = require_object_set_prototype_of();
    module2.exports = function($this, dummy, Wrapper) {
      var NewTarget, NewTargetPrototype;
      if (setPrototypeOf && typeof (NewTarget = dummy.constructor) == "function" && NewTarget !== Wrapper && isObject3(NewTargetPrototype = NewTarget.prototype) && NewTargetPrototype !== Wrapper.prototype)
        setPrototypeOf($this, NewTargetPrototype);
      return $this;
    };
  });

  // node_modules/core-js/internals/same-value.js
  var require_same_value = __commonJS(function(exports2, module2) {
    module2.exports = Object.is || function is(x, y) {
      return x === y ? x !== 0 || 1 / x === 1 / y : x != x && y != y;
    };
  });

  // node_modules/core-js/internals/string-trim-forced.js
  var require_string_trim_forced = __commonJS(function(exports2, module2) {
    var fails8 = require_fails();
    var whitespaces = require_whitespaces();
    var non = "\u200B\x85\u180E";
    module2.exports = function(METHOD_NAME) {
      return fails8(function() {
        return !!whitespaces[METHOD_NAME]() || non[METHOD_NAME]() != non || whitespaces[METHOD_NAME].name !== METHOD_NAME;
      });
    };
  });

  // node_modules/core-js/internals/dom-iterables.js
  var require_dom_iterables = __commonJS(function(exports2, module2) {
    module2.exports = {
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
  });

  // src/main.ts
  var require_main = __commonJS(function(exports, module) {
    __markAsModule(exports);
    __export(exports, {
      main: function() {
        return main;
      }
    });
    var import_es_regexp_exec2 = __toModule(require_es_regexp_exec());
    var import_jquery6 = __toModule(require_jquery());
    function _typeof(obj) {
      "@babel/helpers - typeof";
      if (typeof Symbol === "function" && typeof Symbol.iterator === "symbol") {
        _typeof = function _typeof2(obj2) {
          return typeof obj2;
        };
      } else {
        _typeof = function _typeof2(obj2) {
          return obj2 && typeof Symbol === "function" && obj2.constructor === Symbol && obj2 !== Symbol.prototype ? "symbol" : typeof obj2;
        };
      }
      return _typeof(obj);
    }
    function _defineProperty(obj, key, value) {
      if (key in obj) {
        Object.defineProperty(obj, key, {value: value, enumerable: true, configurable: true, writable: true});
      } else {
        obj[key] = value;
      }
      return obj;
    }
    jQuery = import_jquery6.default;
    function main() {
      var Invoker = function Invoker(target, func) {
        this.target = target;
        this.func = func;
      };
      (function() {
        this.normalCall = this.immediateCall = function() {
          for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
            args[_key] = arguments[_key];
          }
          this.func.apply(this.target, args);
        };
      }).call(Invoker.prototype);
      var Debouncer = function Debouncer(target, func, delayMs) {
        this.target = target;
        this.func = func;
        this.delayMs = delayMs;
        this.timerId = null;
        this.args = null;
      };
      (function() {
        this.normalCall = function() {
          var self2 = this;
          this.$clearTimer();
          for (var _len2 = arguments.length, args = new Array(_len2), _key2 = 0; _key2 < _len2; _key2++) {
            args[_key2] = arguments[_key2];
          }
          this.args = args;
          this.timerId = setTimeout(function() {
            if (self2.timerId === null)
              return;
            self2.$clearTimer();
            self2.$invoke();
          }, this.delayMs);
        };
        this.immediateCall = function() {
          this.$clearTimer();
          for (var _len3 = arguments.length, args = new Array(_len3), _key3 = 0; _key3 < _len3; _key3++) {
            args[_key3] = arguments[_key3];
          }
          this.args = args;
          this.$invoke();
        };
        this.isPending = function() {
          return this.timerId !== null;
        };
        this.$clearTimer = function() {
          if (this.timerId !== null) {
            clearTimeout(this.timerId);
            this.timerId = null;
          }
        };
        this.$invoke = function() {
          this.func.apply(this.target, this.args);
          this.args = null;
        };
      }).call(Debouncer.prototype);
      var Throttler = function Throttler(target, func, delayMs) {
        this.target = target;
        this.func = func;
        this.delayMs = delayMs;
        this.timerId = null;
        this.args = null;
      };
      (function() {
        this.normalCall = function() {
          for (var _len4 = arguments.length, args = new Array(_len4), _key4 = 0; _key4 < _len4; _key4++) {
            args[_key4] = arguments[_key4];
          }
          var self2 = this;
          this.args = args;
          if (this.timerId === null) {
            this.$invoke();
            this.timerId = setTimeout(function() {
              var _self$normalCall;
              if (self2.timerId === null)
                return;
              self2.$clearTimer();
              if (args.length > 0)
                (_self$normalCall = self2.normalCall).apply.apply(_self$normalCall, [self2].concat(args));
            }, this.delayMs);
          }
        };
        this.immediateCall = function() {
          this.$clearTimer();
          for (var _len5 = arguments.length, args = new Array(_len5), _key5 = 0; _key5 < _len5; _key5++) {
            args[_key5] = arguments[_key5];
          }
          this.args = args;
          this.$invoke();
        };
        this.isPending = function() {
          return this.timerId !== null;
        };
        this.$clearTimer = function() {
          if (this.timerId !== null) {
            clearTimeout(this.timerId);
            this.timerId = null;
          }
        };
        this.$invoke = function() {
          this.func.apply(this.target, this.args);
          this.args = null;
        };
      }).call(Throttler.prototype);
      function debounce(threshold, func) {
        var timerId = null;
        var self2, args;
        return function() {
          for (var _len6 = arguments.length, args2 = new Array(_len6), _key6 = 0; _key6 < _len6; _key6++) {
            args2[_key6] = arguments[_key6];
          }
          self2 = this;
          if (timerId !== null) {
            clearTimeout(timerId);
            timerId = null;
          }
          timerId = setTimeout(function() {
            if (timerId === null)
              return;
            timerId = null;
            func.apply(self2, args2);
          }, threshold);
        };
      }
      function throttle(threshold, func) {
        var executionPending = false;
        var timerId = null;
        var self2, args;
        function throttled() {
          self2 = null;
          args = null;
          for (var _len7 = arguments.length, argumentVals = new Array(_len7), _key7 = 0; _key7 < _len7; _key7++) {
            argumentVals[_key7] = arguments[_key7];
          }
          if (timerId === null) {
            timerId = setTimeout(function() {
              timerId = null;
              if (executionPending) {
                executionPending = false;
                throttled.apply(self2, args);
              }
            }, threshold);
            func.apply(this, argumentVals);
          } else {
            executionPending = true;
            self2 = this;
            args = argumentVals;
          }
        }
        return throttled;
      }
      var InputBatchSender = function InputBatchSender(shinyapp) {
        this.shinyapp = shinyapp;
        this.timerId = null;
        this.pendingData = {};
        this.reentrant = false;
        this.lastChanceCallback = [];
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          this.pendingData[nameType] = value;
          if (!this.reentrant) {
            if (opts.priority === "event") {
              this.$sendNow();
            } else if (!this.timerId) {
              this.timerId = setTimeout(this.$sendNow.bind(this), 0);
            }
          }
        };
        this.$sendNow = function() {
          if (this.reentrant) {
            console.trace("Unexpected reentrancy in InputBatchSender!");
          }
          this.reentrant = true;
          try {
            this.timerId = null;
            import_jquery6.default.each(this.lastChanceCallback, function(i, callback) {
              callback();
            });
            var currentData = this.pendingData;
            this.pendingData = {};
            this.shinyapp.sendInput(currentData);
          } finally {
            this.reentrant = false;
          }
        };
      }).call(InputBatchSender.prototype);
      var InputNoResendDecorator = function InputNoResendDecorator(target, initialValues) {
        this.target = target;
        this.lastSentValues = this.reset(initialValues);
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
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
        };
        this.reset = function() {
          var values = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {};
          var cacheValues = {};
          for (var inputName in values) {
            if (values.hasOwnProperty(inputName)) {
              var _splitInputNameType2 = splitInputNameType(inputName), name = _splitInputNameType2.name, inputType = _splitInputNameType2.inputType;
              cacheValues[name] = {
                jsonValue: JSON.stringify(values[inputName]),
                inputType: inputType
              };
            }
          }
          this.lastSentValues = cacheValues;
        };
        this.forget = function(name) {
          delete this.lastSentValues[name];
        };
      }).call(InputNoResendDecorator.prototype);
      var InputEventDecorator = function InputEventDecorator(target) {
        this.target = target;
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          var evt = jQuery.Event("shiny:inputchanged");
          var input = splitInputNameType(nameType);
          evt.name = input.name;
          evt.inputType = input.inputType;
          evt.value = value;
          evt.binding = opts.binding;
          evt.el = opts.el;
          evt.priority = opts.priority;
          import_jquery6.default(opts.el).trigger(evt);
          if (!evt.isDefaultPrevented()) {
            var name = evt.name;
            if (evt.inputType !== "")
              name += ":" + evt.inputType;
            this.target.setInput(name, evt.value, {
              priority: opts.priority
            });
          }
        };
      }).call(InputEventDecorator.prototype);
      var InputRateDecorator = function InputRateDecorator(target) {
        this.target = target;
        this.inputRatePolicies = {};
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          var _splitInputNameType3 = splitInputNameType(nameType), inputName = _splitInputNameType3.name;
          this.$ensureInit(inputName);
          if (opts.priority !== "deferred")
            this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
          else
            this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
        };
        this.setRatePolicy = function(nameType, mode, millis) {
          var _splitInputNameType4 = splitInputNameType(nameType), inputName = _splitInputNameType4.name;
          if (mode === "direct") {
            this.inputRatePolicies[inputName] = new Invoker(this, this.$doSetInput);
          } else if (mode === "debounce") {
            this.inputRatePolicies[inputName] = new Debouncer(this, this.$doSetInput, millis);
          } else if (mode === "throttle") {
            this.inputRatePolicies[inputName] = new Throttler(this, this.$doSetInput, millis);
          }
        };
        this.$ensureInit = function(name) {
          if (!(name in this.inputRatePolicies))
            this.setRatePolicy(name, "direct");
        };
        this.$doSetInput = function(nameType, value, opts) {
          this.target.setInput(nameType, value, opts);
        };
      }).call(InputRateDecorator.prototype);
      var InputDeferDecorator = function InputDeferDecorator(target) {
        this.target = target;
        this.pendingInput = {};
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          if (/^\./.test(nameType))
            this.target.setInput(nameType, value, opts);
          else
            this.pendingInput[nameType] = {
              value: value,
              opts: opts
            };
        };
        this.submit = function() {
          for (var nameType in this.pendingInput) {
            if (this.pendingInput.hasOwnProperty(nameType)) {
              var _this$pendingInput$na = this.pendingInput[nameType], value = _this$pendingInput$na.value, opts = _this$pendingInput$na.opts;
              this.target.setInput(nameType, value, opts);
            }
          }
        };
      }).call(InputDeferDecorator.prototype);
      var InputValidateDecorator = function InputValidateDecorator(target) {
        this.target = target;
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          if (!nameType)
            throw "Can't set input with empty name.";
          opts = addDefaultInputOpts(opts);
          this.target.setInput(nameType, value, opts);
        };
      }).call(InputValidateDecorator.prototype);
      function addDefaultInputOpts(opts) {
        opts = import_jquery6.default.extend({
          priority: "immediate",
          binding: null,
          el: null
        }, opts);
        if (opts && typeof opts.priority !== "undefined") {
          switch (opts.priority) {
            case "deferred":
            case "immediate":
            case "event":
              break;
            default:
              throw new Error("Unexpected input value mode: '" + opts.priority + "'");
          }
        }
        return opts;
      }
      function splitInputNameType(nameType) {
        var name2 = nameType.split(":");
        return {
          name: name2[0],
          inputType: name2.length > 1 ? name2[1] : ""
        };
      }
      var ShinyApp = function ShinyApp() {
        this.$socket = null;
        this.$inputValues = {};
        this.$initialInput = {};
        this.$bindings = {};
        this.$values = {};
        this.$errors = {};
        this.$conditionals = {};
        this.$pendingMessages = [];
        this.$activeRequests = {};
        this.$nextRequestId = 0;
        this.$allowReconnect = false;
      };
      (function() {
        this.connect = function(initialInput) {
          if (this.$socket)
            throw "Connect was already called on this application object";
          this.$socket = this.createSocket();
          this.$initialInput = initialInput;
          import_jquery6.default.extend(this.$inputValues, initialInput);
          this.$updateConditionals();
        };
        this.isConnected = function() {
          return !!this.$socket;
        };
        var scheduledReconnect = null;
        this.reconnect = function() {
          clearTimeout(scheduledReconnect);
          if (this.isConnected())
            throw "Attempted to reconnect, but already connected.";
          this.$socket = this.createSocket();
          this.$initialInput = import_jquery6.default.extend({}, this.$inputValues);
          this.$updateConditionals();
        };
        this.createSocket = function() {
          var self2 = this;
          var createSocketFunc = Shiny.createSocket || function() {
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
            import_jquery6.default(document).trigger({
              type: "shiny:connected",
              socket: socket
            });
            self2.onConnected();
            socket.send(JSON.stringify({
              method: "init",
              data: self2.$initialInput
            }));
            while (self2.$pendingMessages.length) {
              var msg = self2.$pendingMessages.shift();
              socket.send(msg);
            }
          };
          socket.onmessage = function(e) {
            self2.dispatchMessage(e.data);
          };
          socket.onclose = function() {
            if (hasOpened) {
              import_jquery6.default(document).trigger({
                type: "shiny:disconnected",
                socket: socket
              });
              self2.$notifyDisconnected();
            }
            self2.onDisconnected();
            self2.$removeSocket();
          };
          return socket;
        };
        this.sendInput = function(values) {
          var msg = JSON.stringify({
            method: "update",
            data: values
          });
          this.$sendMsg(msg);
          import_jquery6.default.extend(this.$inputValues, values);
          this.$updateConditionals();
        };
        this.$notifyDisconnected = function() {
          if (window.parent) {
            window.parent.postMessage("disconnected", "*");
          }
        };
        this.$removeSocket = function() {
          this.$socket = null;
        };
        this.$scheduleReconnect = function(delay) {
          var self2 = this;
          scheduledReconnect = setTimeout(function() {
            self2.reconnect();
          }, delay);
        };
        var reconnectDelay = function() {
          var attempts = 0;
          var delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];
          return {
            next: function next() {
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
        }();
        this.onDisconnected = function() {
          var $overlay = import_jquery6.default("#shiny-disconnected-overlay");
          if ($overlay.length === 0) {
            import_jquery6.default(document.body).append('<div id="shiny-disconnected-overlay"></div>');
          }
          if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
            var delay = reconnectDelay.next();
            Shiny.showReconnectDialog(delay);
            this.$scheduleReconnect(delay);
          }
        };
        this.onConnected = function() {
          import_jquery6.default("#shiny-disconnected-overlay").remove();
          Shiny.hideReconnectDialog();
          reconnectDelay.reset();
        };
        this.makeRequest = function(method, args, onSuccess, onError, blobs) {
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
            var uint32_to_buf = function uint32_to_buf2(val) {
              var buffer = new ArrayBuffer(4);
              var view = new DataView(buffer);
              view.setUint32(0, val, true);
              return buffer;
            };
            var payload = [];
            payload.push(uint32_to_buf(16908802));
            var jsonBuf = makeBlob([msg]);
            payload.push(uint32_to_buf(jsonBuf.size));
            payload.push(jsonBuf);
            for (var i = 0; i < blobs.length; i++) {
              payload.push(uint32_to_buf(blobs[i].byteLength || blobs[i].size || 0));
              payload.push(blobs[i]);
            }
            msg = makeBlob(payload);
          }
          this.$sendMsg(msg);
        };
        this.$sendMsg = function(msg) {
          if (!this.$socket.readyState) {
            this.$pendingMessages.push(msg);
          } else {
            this.$socket.send(msg);
          }
        };
        this.receiveError = function(name, error) {
          if (this.$errors[name] === error)
            return;
          this.$errors[name] = error;
          delete this.$values[name];
          var binding = this.$bindings[name];
          var evt = jQuery.Event("shiny:error");
          evt.name = name;
          evt.error = error;
          evt.binding = binding;
          import_jquery6.default(binding ? binding.el : document).trigger(evt);
          if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
            binding.onValueError(evt.error);
          }
        };
        this.receiveOutput = function(name, value) {
          var binding = this.$bindings[name];
          var evt = jQuery.Event("shiny:value");
          evt.name = name;
          evt.value = value;
          evt.binding = binding;
          if (this.$values[name] === value) {
            import_jquery6.default(binding ? binding.el : document).trigger(evt);
            return void 0;
          }
          this.$values[name] = value;
          delete this.$errors[name];
          import_jquery6.default(binding ? binding.el : document).trigger(evt);
          if (!evt.isDefaultPrevented() && binding) {
            binding.onValueChange(evt.value);
          }
          return value;
        };
        this.bindOutput = function(id, binding) {
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
        };
        this.unbindOutput = function(id, binding) {
          if (this.$bindings[id] === binding) {
            delete this.$bindings[id];
            return true;
          } else {
            return false;
          }
        };
        function narrowScopeComponent(scopeComponent, nsPrefix) {
          return Object.keys(scopeComponent).filter(function(k) {
            return k.indexOf(nsPrefix) === 0;
          }).map(function(k) {
            return _defineProperty({}, k.substring(nsPrefix.length), scopeComponent[k]);
          }).reduce(function(obj, pair) {
            return import_jquery6.default.extend(obj, pair);
          }, {});
        }
        function narrowScope(scope, nsPrefix) {
          if (nsPrefix) {
            return {
              input: narrowScopeComponent(scope.input, nsPrefix),
              output: narrowScopeComponent(scope.output, nsPrefix)
            };
          }
          return scope;
        }
        this.$updateConditionals = function() {
          import_jquery6.default(document).trigger({
            type: "shiny:conditional"
          });
          var inputs = {};
          for (var name in this.$inputValues) {
            if (this.$inputValues.hasOwnProperty(name)) {
              var shortName = name.replace(/:.*/, "");
              inputs[shortName] = this.$inputValues[name];
            }
          }
          var scope = {
            input: inputs,
            output: this.$values
          };
          var conditionals = import_jquery6.default(document).find("[data-display-if]");
          for (var i = 0; i < conditionals.length; i++) {
            var el = import_jquery6.default(conditionals[i]);
            var condFunc = el.data("data-display-if-func");
            if (!condFunc) {
              var condExpr = el.attr("data-display-if");
              condFunc = scopeExprToFunc(condExpr);
              el.data("data-display-if-func", condFunc);
            }
            var nsPrefix = el.attr("data-ns-prefix");
            var nsScope = narrowScope(scope, nsPrefix);
            var show = condFunc(nsScope);
            var showing = el.css("display") !== "none";
            if (show !== showing) {
              if (show) {
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
        };
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
        Shiny.addCustomMessageHandler = addCustomMessageHandler;
        this.dispatchMessage = function(data) {
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
          import_jquery6.default(document).trigger(evt);
          if (evt.isDefaultPrevented())
            return;
          this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);
          this.$updateConditionals();
        };
        this._sendMessagesToHandlers = function(msgObj, handlers, handlerOrder) {
          for (var i = 0; i < handlerOrder.length; i++) {
            var msgType = handlerOrder[i];
            if (msgObj.hasOwnProperty(msgType)) {
              handlers[msgType].call(this, msgObj[msgType]);
            }
          }
        };
        addMessageHandler("values", function(message) {
          for (var name in this.$bindings) {
            if (this.$bindings.hasOwnProperty(name))
              this.$bindings[name].showProgress(false);
          }
          for (var key in message) {
            if (message.hasOwnProperty(key))
              this.receiveOutput(key, message[key]);
          }
        });
        addMessageHandler("errors", function(message) {
          for (var key in message) {
            if (message.hasOwnProperty(key))
              this.receiveError(key, message[key]);
          }
        });
        addMessageHandler("inputMessages", function(message) {
          for (var i = 0; i < message.length; i++) {
            var $obj = import_jquery6.default(".shiny-bound-input#" + $escape(message[i].id));
            var inputBinding = $obj.data("shiny-input-binding");
            if ($obj.length > 0) {
              if (!$obj.attr("aria-live"))
                $obj.attr("aria-live", "polite");
              var el = $obj[0];
              var evt = jQuery.Event("shiny:updateinput");
              evt.message = message[i].message;
              evt.binding = inputBinding;
              import_jquery6.default(el).trigger(evt);
              if (!evt.isDefaultPrevented())
                inputBinding.receiveMessage(el, evt.message);
            }
          }
        });
        addMessageHandler("javascript", function(message) {
          eval(message);
        });
        addMessageHandler("console", function(message) {
          for (var i = 0; i < message.length; i++) {
            if (console.log)
              console.log(message[i]);
          }
        });
        addMessageHandler("progress", function(message) {
          if (message.type && message.message) {
            var handler = progressHandlers[message.type];
            if (handler)
              handler.call(this, message.message);
          }
        });
        addMessageHandler("notification", function(message) {
          if (message.type === "show")
            Shiny.notifications.show(message.message);
          else if (message.type === "remove")
            Shiny.notifications.remove(message.message);
          else
            throw "Unkown notification type: " + message.type;
        });
        addMessageHandler("modal", function(message) {
          if (message.type === "show")
            Shiny.modal.show(message.message);
          else if (message.type === "remove")
            Shiny.modal.remove();
          else
            throw "Unkown modal type: " + message.type;
        });
        addMessageHandler("response", function(message) {
          var requestId = message.tag;
          var request = this.$activeRequests[requestId];
          if (request) {
            delete this.$activeRequests[requestId];
            if ("value" in message)
              request.onSuccess(message.value);
            else
              request.onError(message.error);
          }
        });
        addMessageHandler("allowReconnect", function(message) {
          if (message === true || message === false || message === "force") {
            this.$allowReconnect = message;
          } else {
            throw "Invalid value for allowReconnect: " + message;
          }
        });
        addMessageHandler("custom", function(message) {
          if (Shiny.oncustommessage) {
            Shiny.oncustommessage(message);
          }
          this._sendMessagesToHandlers(message, customMessageHandlers, customMessageHandlerOrder);
        });
        addMessageHandler("config", function(message) {
          this.config = {
            workerId: message.workerId,
            sessionId: message.sessionId
          };
          if (message.user)
            Shiny.user = message.user;
          import_jquery6.default(document).trigger("shiny:sessioninitialized");
        });
        addMessageHandler("busy", function(message) {
          if (message === "busy") {
            import_jquery6.default(document.documentElement).addClass("shiny-busy");
            import_jquery6.default(document).trigger("shiny:busy");
          } else if (message === "idle") {
            import_jquery6.default(document.documentElement).removeClass("shiny-busy");
            import_jquery6.default(document).trigger("shiny:idle");
          }
        });
        addMessageHandler("recalculating", function(message) {
          if (message.hasOwnProperty("name") && message.hasOwnProperty("status")) {
            var binding = this.$bindings[message.name];
            import_jquery6.default(binding ? binding.el : null).trigger({
              type: "shiny:" + message.status
            });
          }
        });
        addMessageHandler("reload", function(message) {
          window.location.reload();
        });
        addMessageHandler("shiny-insert-ui", function(message) {
          var targets = import_jquery6.default(message.selector);
          if (targets.length === 0) {
            console.warn('The selector you chose ("' + message.selector + '") could not be found in the DOM.');
            Shiny.renderHtml(message.content.html, import_jquery6.default([]), message.content.deps);
          } else {
            targets.each(function(i, target) {
              Shiny.renderContent(target, message.content, message.where);
              return message.multiple;
            });
          }
        });
        addMessageHandler("shiny-remove-ui", function(message) {
          var els = import_jquery6.default(message.selector);
          els.each(function(i, el) {
            Shiny.unbindAll(el, true);
            import_jquery6.default(el).remove();
            return message.multiple;
          });
        });
        addMessageHandler("frozen", function(message) {
          for (var i = 0; i < message.ids.length; i++) {
            Shiny.forgetLastInputValue(message.ids[i]);
          }
        });
        function getTabset(id) {
          var $tabset = import_jquery6.default("#" + $escape(id));
          if ($tabset.length === 0)
            throw "There is no tabsetPanel (or navbarPage or navlistPanel) with id equal to '" + id + "'";
          return $tabset;
        }
        function getTabContent($tabset) {
          var tabsetId = $tabset.attr("data-tabsetid");
          var $tabContent = import_jquery6.default("div.tab-content[data-tabsetid='" + $escape(tabsetId) + "']");
          return $tabContent;
        }
        function getTargetTabs($tabset, $tabContent, target) {
          var dataValue = "[data-value='" + $escape(target) + "']";
          var $aTag = $tabset.find("a" + dataValue);
          var $liTag = $aTag.parent("li");
          if ($liTag.length === 0)
            $liTag = $aTag;
          if ($liTag.length === 0) {
            throw "There is no tabPanel (or navbarMenu) with value (or menuName) equal to '" + target + "'";
          }
          var $liTags = [];
          var $divTags = [];
          if ($aTag.attr("data-toggle") === "dropdown") {
            var $dropdownTabset = $aTag.find("+ ul.dropdown-menu");
            var dropdownId = $dropdownTabset.attr("data-tabsetid");
            var $dropdownLiTags = $dropdownTabset.find("a[data-toggle='tab']");
            if ($dropdownLiTags.parent("li").length > 0) {
              $dropdownLiTags = $dropdownLiTags.parent("li");
            }
            $dropdownLiTags.each(function(i, el) {
              $liTags.push(import_jquery6.default(el));
            });
            var selector = "div.tab-pane[id^='tab-" + $escape(dropdownId) + "']";
            var $dropdownDivs = $tabContent.find(selector);
            $dropdownDivs.each(function(i, el) {
              $divTags.push(import_jquery6.default(el));
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
          var $divTag = import_jquery6.default(message.divTag.html);
          var $liTag = import_jquery6.default(message.liTag.html);
          var $aTag = $liTag.find("> a");
          var target = null;
          var $targetLiTag = null;
          if (message.target !== null) {
            target = getTargetTabs($tabset, $tabContent, message.target);
            $targetLiTag = target.$liTag;
            if ($targetLiTag.hasClass("dropdown-item")) {
              $liTag = $aTag.removeClass("nav-link").addClass("dropdown-item");
            }
          }
          var dropdown = getDropdown();
          if (dropdown !== null) {
            if ($aTag.attr("data-toggle") === "dropdown")
              throw "Cannot insert a navbarMenu inside another one";
            $tabset = dropdown.$tabset;
            tabsetId = dropdown.id;
          }
          if ($aTag.attr("data-toggle") === "tab") {
            var index = getTabIndex($tabset, tabsetId);
            var tabId = "tab-" + tabsetId + "-" + index;
            var anchor = $liTag.find("> a");
            if (anchor.length === 0)
              anchor = $liTag;
            anchor.attr("href", "#" + tabId);
            $divTag.attr("id", tabId);
          }
          if (message.position === "before") {
            if ($targetLiTag) {
              $targetLiTag.before($liTag);
            } else {
              $tabset.append($liTag);
            }
          } else if (message.position === "after") {
            if ($targetLiTag) {
              $targetLiTag.after($liTag);
            } else {
              $tabset.prepend($liTag);
            }
          }
          Shiny.renderContent($liTag[0], {
            html: $liTag.html(),
            deps: message.liTag.deps
          });
          Shiny.renderContent($tabContent[0], {
            html: "",
            deps: message.divTag.deps
          }, "beforeend");
          $divTag.get().forEach(function(el) {
            $tabContent[0].appendChild(el);
            Shiny.renderContent(el, el.innerHTML || el.textContent);
          });
          if (message.select) {
            $liTag.find("a").tab("show");
          }
          function getTabIndex($tabset2, tabsetId2) {
            var existingTabIds = [0];
            $tabset2.find("a[data-toggle='tab']").each(function() {
              var $tab = import_jquery6.default(this);
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
              var $dropdownATag = import_jquery6.default("a.dropdown-toggle[data-value='" + $escape(message.menuName) + "']");
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
          import_jquery6.default.each(target, function(key, el) {
            if (key === "$liTag") {
              func(el);
            } else if (key === "$divTags") {
              import_jquery6.default.each(el, function(i, div) {
                func(div);
              });
            } else if (liTags && key === "$liTags") {
              import_jquery6.default.each(el, function(i, div) {
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
            Shiny.unbindAll($el, true);
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
            import_jquery6.default(document).trigger("hashchange");
        });
        addMessageHandler("resetBrush", function(message) {
          Shiny.resetBrush(message.brushId);
        });
        var progressHandlers = {
          binding: function binding(message) {
            var key = message.id;
            var binding2 = this.$bindings[key];
            if (binding2) {
              import_jquery6.default(binding2.el).trigger({
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
              Shiny.notifications.show({
                html: '<div id="shiny-progress-'.concat(message.id, '" class="shiny-progress-notification">') + '<div class="progress active" style="display: none;"><div class="progress-bar"></div></div><div class="progress-text"><span class="progress-message">message</span> <span class="progress-detail"></span></div></div>',
                id: message.id,
                duration: null
              });
            } else if (message.style === "old") {
              var $container = import_jquery6.default(".shiny-progress-container");
              if ($container.length === 0) {
                $container = import_jquery6.default('<div class="shiny-progress-container"></div>');
                import_jquery6.default(document.body).append($container);
              }
              var depth = import_jquery6.default(".shiny-progress.open").length;
              var $progress = import_jquery6.default('<div class="shiny-progress open"><div class="progress active"><div class="progress-bar bar"></div></div><div class="progress-text"><span class="progress-message">message</span><span class="progress-detail"></span></div></div>');
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
              var $progress = import_jquery6.default("#shiny-progress-" + message.id);
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
              var _$progress = import_jquery6.default("#" + message.id + ".shiny-progress");
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
              Shiny.notifications.remove(message.id);
            } else if (message.style === "old") {
              var $progress = import_jquery6.default("#" + message.id + ".shiny-progress");
              $progress.removeClass("open");
              $progress.fadeOut({
                complete: function complete() {
                  $progress.remove();
                  if (import_jquery6.default(".shiny-progress").length === 0)
                    import_jquery6.default(".shiny-progress-container").remove();
                }
              });
            }
          }
        };
        Shiny.progressHandlers = progressHandlers;
        this.getTestSnapshotBaseUrl = function() {
          var _ref2 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref2$fullUrl = _ref2.fullUrl, fullUrl = _ref2$fullUrl === void 0 ? true : _ref2$fullUrl;
          var loc = window.location;
          var url = "";
          if (fullUrl) {
            url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
          }
          url += "/session/" + encodeURIComponent(this.config.sessionId) + "/dataobj/shinytest?w=" + encodeURIComponent(this.config.workerId) + "&nonce=" + randomId();
          return url;
        };
      }).call(ShinyApp.prototype);
      Shiny.showReconnectDialog = function() {
        var reconnectTime = null;
        function updateTime() {
          var $time = import_jquery6.default("#shiny-reconnect-time");
          if ($time.length === 0)
            return;
          var seconds = Math.floor((reconnectTime - new Date().getTime()) / 1e3);
          if (seconds > 0) {
            $time.text(" in " + seconds + "s");
          } else {
            $time.text("...");
          }
          setTimeout(updateTime, 1e3);
        }
        return function(delay) {
          reconnectTime = new Date().getTime() + delay;
          if (import_jquery6.default("#shiny-reconnect-text").length > 0)
            return;
          var html = '<span id="shiny-reconnect-text">Attempting to reconnect</span><span id="shiny-reconnect-time"></span>';
          var action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';
          Shiny.notifications.show({
            id: "reconnect",
            html: html,
            action: action,
            duration: null,
            closeButton: false,
            type: "warning"
          });
          updateTime();
        };
      }();
      Shiny.hideReconnectDialog = function() {
        Shiny.notifications.remove("reconnect");
      };
      Shiny.notifications = function() {
        var fadeDuration = 250;
        function show() {
          var _ref3 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref3$html = _ref3.html, html = _ref3$html === void 0 ? "" : _ref3$html, _ref3$action = _ref3.action, action = _ref3$action === void 0 ? "" : _ref3$action, _ref3$deps = _ref3.deps, deps = _ref3$deps === void 0 ? [] : _ref3$deps, _ref3$duration = _ref3.duration, duration = _ref3$duration === void 0 ? 5e3 : _ref3$duration, _ref3$id = _ref3.id, id = _ref3$id === void 0 ? null : _ref3$id, _ref3$closeButton = _ref3.closeButton, closeButton = _ref3$closeButton === void 0 ? true : _ref3$closeButton, _ref3$type = _ref3.type, type = _ref3$type === void 0 ? null : _ref3$type;
          if (!id)
            id = randomId();
          _createPanel();
          var $notification = _get(id);
          if ($notification.length === 0)
            $notification = _create(id);
          var newHtml = '<div class="shiny-notification-content-text">'.concat(html, "</div>") + '<div class="shiny-notification-content-action">'.concat(action, "</div>");
          var $content = $notification.find(".shiny-notification-content");
          Shiny.renderContent($content, {
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
            _addRemovalCallback(id, duration);
          else
            _clearRemovalCallback(id);
          return id;
        }
        function remove(id) {
          _get(id).fadeOut(fadeDuration, function() {
            Shiny.unbindAll(this);
            import_jquery6.default(this).remove();
            if (_ids().length === 0) {
              _getPanel().remove();
            }
          });
        }
        function _get(id) {
          if (!id)
            return null;
          return _getPanel().find("#shiny-notification-" + $escape(id));
        }
        function _ids() {
          return _getPanel().find(".shiny-notification").map(function() {
            return this.id.replace(/shiny-notification-/, "");
          }).get();
        }
        function _getPanel() {
          return import_jquery6.default("#shiny-notification-panel");
        }
        function _createPanel() {
          var $panel = _getPanel();
          if ($panel.length > 0)
            return $panel;
          import_jquery6.default(document.body).append('<div id="shiny-notification-panel">');
          return $panel;
        }
        function _create(id) {
          var $notification = _get(id);
          if ($notification.length === 0) {
            $notification = import_jquery6.default('<div id="shiny-notification-'.concat(id, '" class="shiny-notification">') + '<div class="shiny-notification-close">&times;</div><div class="shiny-notification-content"></div></div>');
            $notification.find(".shiny-notification-close").on("click", function(e) {
              e.preventDefault();
              e.stopPropagation();
              remove(id);
            });
            _getPanel().append($notification);
          }
          return $notification;
        }
        function _addRemovalCallback(id, delay) {
          _clearRemovalCallback(id);
          var removalCallback = setTimeout(function() {
            remove(id);
          }, delay);
          _get(id).data("removalCallback", removalCallback);
        }
        function _clearRemovalCallback(id) {
          var $notification = _get(id);
          var oldRemovalCallback = $notification.data("removalCallback");
          if (oldRemovalCallback) {
            clearTimeout(oldRemovalCallback);
          }
        }
        return {
          show: show,
          remove: remove
        };
      }();
      Shiny.modal = {
        show: function show() {
          var _ref4 = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : {}, _ref4$html = _ref4.html, html = _ref4$html === void 0 ? "" : _ref4$html, _ref4$deps = _ref4.deps, deps = _ref4$deps === void 0 ? [] : _ref4$deps;
          import_jquery6.default(".modal-backdrop").remove();
          var $modal = import_jquery6.default("#shiny-modal-wrapper");
          if ($modal.length === 0) {
            $modal = import_jquery6.default('<div id="shiny-modal-wrapper"></div>');
            import_jquery6.default(document.body).append($modal);
            $modal.on("hidden.bs.modal", function(e) {
              if (e.target === import_jquery6.default("#shiny-modal")[0]) {
                Shiny.unbindAll($modal);
                $modal.remove();
              }
            });
          }
          $modal.on("keydown.shinymodal", function(e) {
            if (import_jquery6.default("#shiny-modal").data("keyboard") === false)
              return;
            if (e.keyCode === 27) {
              e.stopPropagation();
              e.preventDefault();
            }
          });
          Shiny.renderContent($modal, {
            html: html,
            deps: deps
          });
        },
        remove: function remove() {
          var $modal = import_jquery6.default("#shiny-modal-wrapper");
          $modal.off("keydown.shinymodal");
          if ($modal.find(".modal").length > 0) {
            $modal.find(".modal").modal("hide");
          } else {
            Shiny.unbindAll($modal);
            $modal.remove();
          }
        }
      };
      var BindingRegistry = function BindingRegistry() {
        this.bindings = [];
        this.bindingNames = {};
      };
      (function() {
        this.register = function(binding, bindingName, priority) {
          var bindingObj = {
            binding: binding,
            priority: priority || 0
          };
          this.bindings.unshift(bindingObj);
          if (bindingName) {
            this.bindingNames[bindingName] = bindingObj;
            binding.name = bindingName;
          }
        };
        this.setPriority = function(bindingName, priority) {
          var bindingObj = this.bindingNames[bindingName];
          if (!bindingObj)
            throw "Tried to set priority on unknown binding " + bindingName;
          bindingObj.priority = priority || 0;
        };
        this.getPriority = function(bindingName) {
          var bindingObj = this.bindingNames[bindingName];
          if (!bindingObj)
            return false;
          return bindingObj.priority;
        };
        this.getBindings = function() {
          return mergeSort(this.bindings, function(a, b) {
            return b.priority - a.priority;
          });
        };
      }).call(BindingRegistry.prototype);
      var inputBindings = Shiny.inputBindings = new BindingRegistry();
      var outputBindings = Shiny.outputBindings = new BindingRegistry();
      var OutputBinding = Shiny.OutputBinding = function() {
      };
      (function() {
        this.find = function(scope) {
          throw "Not implemented";
        };
        this.getId = function(el) {
          return el["data-input-id"] || el.id;
        };
        this.onValueChange = function(el, data) {
          this.clearError(el);
          this.renderValue(el, data);
        };
        this.onValueError = function(el, err) {
          this.renderError(el, err);
        };
        this.renderError = function(el, err) {
          this.clearError(el);
          if (err.message === "") {
            import_jquery6.default(el).empty();
            return;
          }
          var errClass = "shiny-output-error";
          if (err.type !== null) {
            errClass = errClass + " " + import_jquery6.default.map(asArray(err.type), function(type) {
              return errClass + "-" + type;
            }).join(" ");
          }
          import_jquery6.default(el).addClass(errClass).text(err.message);
        };
        this.clearError = function(el) {
          import_jquery6.default(el).attr("class", function(i, c) {
            return c.replace(/(^|\s)shiny-output-error\S*/g, "");
          });
        };
        this.showProgress = function(el, show) {
          var RECALC_CLASS = "recalculating";
          if (show)
            import_jquery6.default(el).addClass(RECALC_CLASS);
          else
            import_jquery6.default(el).removeClass(RECALC_CLASS);
        };
      }).call(OutputBinding.prototype);
      var textOutputBinding = new OutputBinding();
      import_jquery6.default.extend(textOutputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-text-output");
        },
        renderValue: function renderValue(el, data) {
          import_jquery6.default(el).text(data);
        }
      });
      outputBindings.register(textOutputBinding, "shiny.textOutput");
      var imageOutputBinding = new OutputBinding();
      import_jquery6.default.extend(imageOutputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-image-output, .shiny-plot-output");
        },
        renderValue: function renderValue(el, data) {
          var outputId = this.getId(el);
          var $el = import_jquery6.default(el);
          var img;
          var $img = $el.find("img");
          if ($img.length === 0) {
            img = document.createElement("img");
            $el.append(img);
            $img = import_jquery6.default(img);
          } else {
            img = $img[0];
            $img.trigger("reset");
          }
          if (!data) {
            $el.empty();
            return;
          }
          function OR(value, alternate) {
            if (value === void 0)
              return alternate;
            return value;
          }
          var opts = {
            clickId: $el.data("click-id"),
            clickClip: OR(strToBool($el.data("click-clip")), true),
            dblclickId: $el.data("dblclick-id"),
            dblclickClip: OR(strToBool($el.data("dblclick-clip")), true),
            dblclickDelay: OR($el.data("dblclick-delay"), 400),
            hoverId: $el.data("hover-id"),
            hoverClip: OR(strToBool($el.data("hover-clip")), true),
            hoverDelayType: OR($el.data("hover-delay-type"), "debounce"),
            hoverDelay: OR($el.data("hover-delay"), 300),
            hoverNullOutside: OR(strToBool($el.data("hover-null-outside")), false),
            brushId: $el.data("brush-id"),
            brushClip: OR(strToBool($el.data("brush-clip")), true),
            brushDelayType: OR($el.data("brush-delay-type"), "debounce"),
            brushDelay: OR($el.data("brush-delay"), 300),
            brushFill: OR($el.data("brush-fill"), "#666"),
            brushStroke: OR($el.data("brush-stroke"), "#000"),
            brushOpacity: OR($el.data("brush-opacity"), 0.3),
            brushDirection: OR($el.data("brush-direction"), "xy"),
            brushResetOnNew: OR(strToBool($el.data("brush-reset-on-new")), false),
            coordmap: data.coordmap
          };
          if (opts.brushFill === "auto") {
            opts.brushFill = getComputedLinkColor($el[0]);
          }
          if (opts.brushStroke === "auto") {
            opts.brushStroke = getStyle($el[0], "color");
          }
          import_jquery6.default.each(data, function(key, value) {
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
            if (attrib.specified && !data.hasOwnProperty(attrib.name)) {
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
            imageutils.initCoordmap($el, opts.coordmap);
            var clickInfo = imageutils.createClickInfo($el, opts.dblclickId, opts.dblclickDelay);
            $el.on("mousedown.image_output", clickInfo.mousedown);
            if (isIE() && IEVersion() === 8) {
              $el.on("dblclick.image_output", clickInfo.dblclickIE8);
            }
            if (opts.clickId) {
              imageutils.disableDrag($el, $img);
              var clickHandler = imageutils.createClickHandler(opts.clickId, opts.clickClip, opts.coordmap);
              $el.on("mousedown2.image_output", clickHandler.mousedown);
              $el.on("resize.image_output", clickHandler.onResize);
              $img.on("reset.image_output", clickHandler.onResetImg);
            }
            if (opts.dblclickId) {
              imageutils.disableDrag($el, $img);
              var dblclickHandler = imageutils.createClickHandler(opts.dblclickId, opts.clickClip, opts.coordmap);
              $el.on("dblclick2.image_output", dblclickHandler.mousedown);
              $el.on("resize.image_output", dblclickHandler.onResize);
              $img.on("reset.image_output", dblclickHandler.onResetImg);
            }
            if (opts.hoverId) {
              imageutils.disableDrag($el, $img);
              var hoverHandler = imageutils.createHoverHandler(opts.hoverId, opts.hoverDelay, opts.hoverDelayType, opts.hoverClip, opts.hoverNullOutside, opts.coordmap);
              $el.on("mousemove.image_output", hoverHandler.mousemove);
              $el.on("mouseout.image_output", hoverHandler.mouseout);
              $el.on("resize.image_output", hoverHandler.onResize);
              $img.on("reset.image_output", hoverHandler.onResetImg);
            }
            if (opts.brushId) {
              imageutils.disableDrag($el, $img);
              var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts, opts.coordmap, outputId);
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
        },
        renderError: function renderError(el, err) {
          import_jquery6.default(el).find("img").trigger("reset");
          OutputBinding.prototype.renderError.call(this, el, err);
        },
        clearError: function clearError(el) {
          import_jquery6.default(el).contents().filter(function() {
            return this.tagName !== "IMG" && this.id !== el.id + "_brush";
          }).remove();
          OutputBinding.prototype.clearError.call(this, el);
        },
        resize: function resize(el, width, height) {
          import_jquery6.default(el).find("img").trigger("resize");
        }
      });
      outputBindings.register(imageOutputBinding, "shiny.imageOutput");
      var imageutils = {};
      imageutils.disableDrag = function($el, $img) {
        $img.css("-webkit-user-drag", "none");
        $img.off("dragstart.image_output");
        $img.on("dragstart.image_output", function() {
          return false;
        });
        $el.off("selectstart.image_output");
        $el.on("selectstart.image_output", function() {
          return false;
        });
      };
      imageutils.initPanelScales = function(panels) {
        function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
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
        function addScaleFuns(panel2) {
          var d = panel2.domain;
          var r = panel2.range;
          var xlog = panel2.log && panel2.log.x ? panel2.log.x : null;
          var ylog = panel2.log && panel2.log.y ? panel2.log.y : null;
          var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
          var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);
          panel2.scaleDataToImg = function(val, clip) {
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
          panel2.scaleImgToData = function(val, clip) {
            return mapValues(val, function(value, key) {
              var prefix = key.substring(0, 1);
              if (prefix === "x") {
                return xscaler.scaleInv(value, clip);
              } else if (prefix === "y") {
                return yscaler.scaleInv(value, clip);
              }
              return null;
            });
          };
          panel2.clipImg = function(offset_img) {
            var newOffset = {
              x: offset_img.x,
              y: offset_img.y
            };
            var bounds = panel2.range;
            if (offset_img.x > bounds.right)
              newOffset.x = bounds.right;
            else if (offset_img.x < bounds.left)
              newOffset.x = bounds.left;
            if (offset_img.y > bounds.bottom)
              newOffset.y = bounds.bottom;
            else if (offset_img.y < bounds.top)
              newOffset.y = bounds.top;
            return newOffset;
          };
        }
        for (var i = 0; i < panels.length; i++) {
          var panel = panels[i];
          addScaleFuns(panel);
        }
      };
      imageutils.initCoordmap = function($el, coordmap) {
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
        imageutils.initPanelScales(coordmap.panels);
        coordmap.mouseOffsetCss = function(mouseEvent) {
          var img_origin = findOrigin($img);
          return {
            x: mouseEvent.pageX - img_origin.x,
            y: mouseEvent.pageY - img_origin.y
          };
        };
        coordmap.scaleCssToImg = function(offset_css) {
          var pixel_scaling = coordmap.imgToCssScalingRatio();
          var result = mapValues(offset_css, function(value, key) {
            var prefix = key.substring(0, 1);
            if (prefix === "x") {
              return offset_css[key] / pixel_scaling.x;
            } else if (prefix === "y") {
              return offset_css[key] / pixel_scaling.y;
            }
            return null;
          });
          return result;
        };
        coordmap.scaleImgToCss = function(offset_img) {
          var pixel_scaling = coordmap.imgToCssScalingRatio();
          var result = mapValues(offset_img, function(value, key) {
            var prefix = key.substring(0, 1);
            if (prefix === "x") {
              return offset_img[key] * pixel_scaling.x;
            } else if (prefix === "y") {
              return offset_img[key] * pixel_scaling.y;
            }
            return null;
          });
          return result;
        };
        coordmap.imgToCssScalingRatio = function() {
          var img_dims = findDims($img);
          return {
            x: img_dims.x / coordmap.dims.width,
            y: img_dims.y / coordmap.dims.height
          };
        };
        coordmap.cssToImgScalingRatio = function() {
          var res = coordmap.imgToCssScalingRatio();
          return {
            x: 1 / res.x,
            y: 1 / res.y
          };
        };
        coordmap.getPanelCss = function(offset_css) {
          var expand = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 0;
          var offset_img = coordmap.scaleCssToImg(offset_css);
          var x = offset_img.x;
          var y = offset_img.y;
          var cssToImgRatio = coordmap.cssToImgScalingRatio();
          var expand_img = {
            x: expand * cssToImgRatio.x,
            y: expand * cssToImgRatio.y
          };
          var matches = [];
          var dists = [];
          var b;
          var i;
          for (i = 0; i < coordmap.panels.length; i++) {
            b = coordmap.panels[i].range;
            if (x <= b.right + expand_img.x && x >= b.left - expand_img.x && y <= b.bottom + expand_img.y && y >= b.top - expand_img.y) {
              matches.push(coordmap.panels[i]);
              var xdist = 0;
              var ydist = 0;
              if (x > b.right && x <= b.right + expand_img.x) {
                xdist = x - b.right;
              } else if (x < b.left && x >= b.left - expand_img.x) {
                xdist = x - b.left;
              }
              if (y > b.bottom && y <= b.bottom + expand_img.y) {
                ydist = y - b.bottom;
              } else if (y < b.top && y >= b.top - expand_img.y) {
                ydist = y - b.top;
              }
              dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
            }
          }
          if (matches.length) {
            var min_dist = Math.min.apply(null, dists);
            for (i = 0; i < matches.length; i++) {
              if (dists[i] === min_dist) {
                return matches[i];
              }
            }
          }
          return null;
        };
        coordmap.isInPanelCss = function(offset_css) {
          var expand = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : 0;
          if (coordmap.getPanelCss(offset_css, expand))
            return true;
          return false;
        };
        coordmap.mouseCoordinateSender = function(inputId, clip, nullOutside) {
          if (clip === void 0)
            clip = true;
          if (nullOutside === void 0)
            nullOutside = false;
          return function(e) {
            if (e === null) {
              Shiny.setInputValue(inputId, null);
              return;
            }
            var coords = {};
            var coords_css = coordmap.mouseOffsetCss(e);
            if (!coordmap.isInPanelCss(coords_css)) {
              if (nullOutside) {
                Shiny.setInputValue(inputId, null);
                return;
              }
              if (clip)
                return;
              coords.coords_css = coords_css;
              coords.coords_img = coordmap.scaleCssToImg(coords_css);
              Shiny.setInputValue(inputId, coords, {
                priority: "event"
              });
              return;
            }
            var panel = coordmap.getPanelCss(coords_css);
            var coords_img = coordmap.scaleCssToImg(coords_css);
            var coords_data = panel.scaleImgToData(coords_img);
            coords.x = coords_data.x;
            coords.y = coords_data.y;
            coords.coords_css = coords_css;
            coords.coords_img = coords_img;
            coords.img_css_ratio = coordmap.cssToImgScalingRatio();
            import_jquery6.default.extend(coords, panel.panel_vars);
            coords.mapping = panel.mapping;
            coords.domain = panel.domain;
            coords.range = panel.range;
            coords.log = panel.log;
            Shiny.setInputValue(inputId, coords, {
              priority: "event"
            });
          };
        };
      };
      imageutils.findBox = function(offset1, offset2) {
        return {
          xmin: Math.min(offset1.x, offset2.x),
          xmax: Math.max(offset1.x, offset2.x),
          ymin: Math.min(offset1.y, offset2.y),
          ymax: Math.max(offset1.y, offset2.y)
        };
      };
      imageutils.shiftToRange = function(vals, min4, max4) {
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
      };
      imageutils.createClickInfo = function($el, dblclickId, dblclickDelay) {
        var clickTimer = null;
        var pending_e = null;
        function triggerEvent(newEventType, e) {
          var e2 = import_jquery6.default.Event(newEventType, {
            which: e.which,
            pageX: e.pageX,
            pageY: e.pageY
          });
          $el.trigger(e2);
        }
        function triggerPendingMousedown2() {
          if (pending_e) {
            triggerEvent("mousedown2", pending_e);
            pending_e = null;
          }
        }
        function scheduleMousedown2(e) {
          pending_e = e;
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
          if (pending_e === null) {
            scheduleMousedown2(e);
          } else {
            clearTimeout(clickTimer);
            if (pending_e && Math.abs(pending_e.pageX - e.pageX) > 2 || Math.abs(pending_e.pageY - e.pageY) > 2) {
              triggerPendingMousedown2();
              scheduleMousedown2(e);
            } else {
              pending_e = null;
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
      };
      imageutils.createClickHandler = function(inputId, clip, coordmap) {
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
      };
      imageutils.createHoverHandler = function(inputId, delay, delayType, clip, nullOutside, coordmap) {
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
      };
      imageutils.createBrushHandler = function(inputId, $el, opts, coordmap, outputId) {
        var expandPixels = 20;
        var brush = imageutils.createBrush($el, opts, coordmap, expandPixels);
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
            Shiny.setInputValue(inputId, null);
            imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
              brushId: inputId,
              outputId: null
            });
            return;
          }
          var panel = brush.getPanel();
          import_jquery6.default.extend(coords, panel.panel_vars);
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
          Shiny.setInputValue(inputId, coords);
          $el.data("mostRecentBrush", true);
          imageOutputBinding.find(document).trigger("shiny-internal:brushed", coords);
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
          var offset_css = coordmap.mouseOffsetCss(e);
          if (opts.brushClip && !coordmap.isInPanelCss(offset_css, expandPixels))
            return;
          brush.up({
            x: NaN,
            y: NaN
          });
          brush.down(offset_css);
          if (brush.isInResizeArea(offset_css)) {
            brush.startResizing(offset_css);
            import_jquery6.default(document).on("mousemove.image_brush", mousemoveResizing).on("mouseup.image_brush", mouseupResizing);
          } else if (brush.isInsideBrush(offset_css)) {
            brush.startDragging(offset_css);
            setCursorStyle("grabbing");
            import_jquery6.default(document).on("mousemove.image_brush", mousemoveDragging).on("mouseup.image_brush", mouseupDragging);
          } else {
            var panel = coordmap.getPanelCss(offset_css, expandPixels);
            brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offset_css)));
            import_jquery6.default(document).on("mousemove.image_brush", mousemoveBrushing).on("mouseup.image_brush", mouseupBrushing);
          }
        }
        function mousemove(e) {
          var offset_css = coordmap.mouseOffsetCss(e);
          if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
            if (brush.isInResizeArea(offset_css)) {
              var r = brush.whichResizeSides(offset_css);
              if (r.left && r.top || r.right && r.bottom) {
                setCursorStyle("nwse-resize");
              } else if (r.left && r.bottom || r.right && r.top) {
                setCursorStyle("nesw-resize");
              } else if (r.left || r.right) {
                setCursorStyle("ew-resize");
              } else if (r.top || r.bottom) {
                setCursorStyle("ns-resize");
              }
            } else if (brush.isInsideBrush(offset_css)) {
              setCursorStyle("grabbable");
            } else if (coordmap.isInPanelCss(offset_css, expandPixels)) {
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
          import_jquery6.default(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
          import_jquery6.default(document).off("mousemove.image_brush").off("mouseup.image_brush");
          brush.up(coordmap.mouseOffsetCss(e));
          brush.stopDragging();
          setCursorStyle("grabbable");
          if (brushInfoSender.isPending())
            brushInfoSender.immediateCall();
        }
        function mouseupResizing(e) {
          if (e.which !== 1)
            return;
          import_jquery6.default(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
      };
      imageutils.createBrush = function($el, opts, coordmap, expandPixels) {
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
          var bounds_data = boundsData();
          for (var val in bounds_data) {
            if (isnan(bounds_data[val]))
              return;
          }
          boundsData(bounds_data);
          updateDiv();
        }
        function isInsideBrush(offset_css) {
          var bounds = state.boundsCss;
          return offset_css.x <= bounds.xmax && offset_css.x >= bounds.xmin && offset_css.y <= bounds.ymax && offset_css.y >= bounds.ymin;
        }
        function isInResizeArea(offset_css) {
          var sides = whichResizeSides(offset_css);
          return sides.left || sides.right || sides.top || sides.bottom;
        }
        function whichResizeSides(offset_css) {
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
          if ((opts.brushDirection === "xy" || opts.brushDirection === "x") && offset_css.y <= e.ymax && offset_css.y >= e.ymin) {
            if (offset_css.x < b.xmin && offset_css.x >= e.xmin)
              res.left = true;
            else if (offset_css.x > b.xmax && offset_css.x <= e.xmax)
              res.right = true;
          }
          if ((opts.brushDirection === "xy" || opts.brushDirection === "y") && offset_css.x <= e.xmax && offset_css.x >= e.xmin) {
            if (offset_css.y < b.ymin && offset_css.y >= e.ymin)
              res.top = true;
            else if (offset_css.y > b.ymax && offset_css.y <= e.ymax)
              res.bottom = true;
          }
          return res;
        }
        function boundsCss(box_css) {
          if (box_css === void 0) {
            return import_jquery6.default.extend({}, state.boundsCss);
          }
          var min_css = {
            x: box_css.xmin,
            y: box_css.ymin
          };
          var max_css = {
            x: box_css.xmax,
            y: box_css.ymax
          };
          var panel = state.panel;
          var panelBounds_img = panel.range;
          if (opts.brushClip) {
            min_css = imgToCss(panel.clipImg(cssToImg(min_css)));
            max_css = imgToCss(panel.clipImg(cssToImg(max_css)));
          }
          if (opts.brushDirection === "xy") {
          } else if (opts.brushDirection === "x") {
            min_css.y = imgToCss({
              y: panelBounds_img.top
            }).y;
            max_css.y = imgToCss({
              y: panelBounds_img.bottom
            }).y;
          } else if (opts.brushDirection === "y") {
            min_css.x = imgToCss({
              x: panelBounds_img.left
            }).x;
            max_css.x = imgToCss({
              x: panelBounds_img.right
            }).x;
          }
          state.boundsCss = {
            xmin: min_css.x,
            xmax: max_css.x,
            ymin: min_css.y,
            ymax: max_css.y
          };
          var min_data = state.panel.scaleImgToData(cssToImg(min_css));
          var max_data = state.panel.scaleImgToData(cssToImg(max_css));
          state.boundsData = imageutils.findBox(min_data, max_data);
          state.boundsData = mapValues(state.boundsData, function(val) {
            return roundSignif(val, 14);
          });
          $div.data("bounds-data", state.boundsData);
          $div.data("panel", state.panel);
          return void 0;
        }
        function boundsData(box_data) {
          if (box_data === void 0) {
            return import_jquery6.default.extend({}, state.boundsData);
          }
          var box_css = imgToCss(state.panel.scaleDataToImg(box_data));
          box_css = mapValues(box_css, function(val) {
            return roundSignif(val, 13);
          });
          boundsCss({
            xmin: Math.min(box_css.xmin, box_css.xmax),
            xmax: Math.max(box_css.xmin, box_css.xmax),
            ymin: Math.min(box_css.ymin, box_css.ymax),
            ymax: Math.max(box_css.ymin, box_css.ymax)
          });
          return void 0;
        }
        function getPanel() {
          return state.panel;
        }
        function addDiv() {
          if ($div)
            $div.remove();
          $div = import_jquery6.default(document.createElement("div")).attr("id", el.id + "_brush").css({
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
          var img_offset_css = findOrigin($el.find("img"));
          var b = state.boundsCss;
          $div.offset({
            top: img_offset_css.y + b.ymin,
            left: img_offset_css.x + b.xmin
          }).outerWidth(b.xmax - b.xmin + 1).outerHeight(b.ymax - b.ymin + 1);
        }
        function down(offset_css) {
          if (offset_css === void 0)
            return state.down;
          state.down = offset_css;
          return void 0;
        }
        function up(offset_css) {
          if (offset_css === void 0)
            return state.up;
          state.up = offset_css;
          return void 0;
        }
        function isBrushing() {
          return state.brushing;
        }
        function startBrushing() {
          state.brushing = true;
          addDiv();
          state.panel = coordmap.getPanelCss(state.down, expandPixels);
          boundsCss(imageutils.findBox(state.down, state.down));
          updateDiv();
        }
        function brushTo(offset_css) {
          boundsCss(imageutils.findBox(state.down, offset_css));
          $div.show();
          updateDiv();
        }
        function stopBrushing() {
          state.brushing = false;
          boundsCss(imageutils.findBox(state.down, state.up));
        }
        function isDragging() {
          return state.dragging;
        }
        function startDragging() {
          state.dragging = true;
          state.changeStartBounds = import_jquery6.default.extend({}, state.boundsCss);
        }
        function dragTo(offset_css) {
          var dx = offset_css.x - state.down.x;
          var dy = offset_css.y - state.down.y;
          var start = state.changeStartBounds;
          var newBounds_css = {
            xmin: start.xmin + dx,
            xmax: start.xmax + dx,
            ymin: start.ymin + dy,
            ymax: start.ymax + dy
          };
          if (opts.brushClip) {
            var panelBounds_img = state.panel.range;
            var newBounds_img = cssToImg(newBounds_css);
            var xvals_img = [newBounds_img.xmin, newBounds_img.xmax];
            var yvals_img = [newBounds_img.ymin, newBounds_img.ymax];
            xvals_img = imageutils.shiftToRange(xvals_img, panelBounds_img.left, panelBounds_img.right);
            yvals_img = imageutils.shiftToRange(yvals_img, panelBounds_img.top, panelBounds_img.bottom);
            newBounds_css = imgToCss({
              xmin: xvals_img[0],
              xmax: xvals_img[1],
              ymin: yvals_img[0],
              ymax: yvals_img[1]
            });
          }
          boundsCss(newBounds_css);
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
          state.changeStartBounds = import_jquery6.default.extend({}, state.boundsCss);
          state.resizeSides = whichResizeSides(state.down);
        }
        function resizeTo(offset_css) {
          var d_css = {
            x: offset_css.x - state.down.x,
            y: offset_css.y - state.down.y
          };
          var d_img = cssToImg(d_css);
          var b_img = cssToImg(state.changeStartBounds);
          var panelBounds_img = state.panel.range;
          if (state.resizeSides.left) {
            var xmin_img = imageutils.shiftToRange(b_img.xmin + d_img.x, panelBounds_img.left, b_img.xmax)[0];
            b_img.xmin = xmin_img;
          } else if (state.resizeSides.right) {
            var xmax_img = imageutils.shiftToRange(b_img.xmax + d_img.x, b_img.xmin, panelBounds_img.right)[0];
            b_img.xmax = xmax_img;
          }
          if (state.resizeSides.top) {
            var ymin_img = imageutils.shiftToRange(b_img.ymin + d_img.y, panelBounds_img.top, b_img.ymax)[0];
            b_img.ymin = ymin_img;
          } else if (state.resizeSides.bottom) {
            var ymax_img = imageutils.shiftToRange(b_img.ymax + d_img.y, b_img.ymin, panelBounds_img.bottom)[0];
            b_img.ymax = ymax_img;
          }
          boundsCss(imgToCss(b_img));
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
          getPanel: getPanel,
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
      };
      Shiny.resetBrush = function(brushId) {
        Shiny.setInputValue(brushId, null);
        imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
          brushId: brushId,
          outputId: null
        });
      };
      function findScalingRatio($el) {
        var boundingRect = $el[0].getBoundingClientRect();
        return {
          x: boundingRect.width / $el.outerWidth(),
          y: boundingRect.height / $el.outerHeight()
        };
      }
      function findOrigin($el) {
        var offset = $el.offset();
        var scaling_ratio = findScalingRatio($el);
        var paddingBorder = {
          left: parseInt($el.css("border-left-width")) + parseInt($el.css("padding-left")),
          top: parseInt($el.css("border-top-width")) + parseInt($el.css("padding-top"))
        };
        return {
          x: offset.left + scaling_ratio.x * paddingBorder.left,
          y: offset.top + scaling_ratio.y * paddingBorder.top
        };
      }
      function findDims($el) {
        var content_ratio = {
          x: $el.width() / $el.outerWidth(),
          y: $el.height() / $el.outerHeight()
        };
        var bounding_rect = $el[0].getBoundingClientRect();
        return {
          x: content_ratio.x * bounding_rect.width,
          y: content_ratio.y * bounding_rect.height
        };
      }
      var htmlOutputBinding = new OutputBinding();
      import_jquery6.default.extend(htmlOutputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-html-output");
        },
        onValueError: function onValueError(el, err) {
          Shiny.unbindAll(el);
          this.renderError(el, err);
        },
        renderValue: function renderValue(el, data) {
          Shiny.renderContent(el, data);
        }
      });
      outputBindings.register(htmlOutputBinding, "shiny.htmlOutput");
      var renderDependencies = Shiny.renderDependencies = function(dependencies) {
        if (dependencies) {
          import_jquery6.default.each(dependencies, function(i, dep) {
            renderDependency(dep);
          });
        }
      };
      Shiny.renderContent = function(el, content) {
        var where = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : "replace";
        if (where === "replace") {
          Shiny.unbindAll(el);
        }
        var html;
        var dependencies = [];
        if (content === null) {
          html = "";
        } else if (typeof content === "string") {
          html = content;
        } else if (_typeof(content) === "object") {
          html = content.html;
          dependencies = content.deps || [];
        }
        Shiny.renderHtml(html, el, dependencies, where);
        var scope = el;
        if (where === "replace") {
          Shiny.initializeInputs(el);
          Shiny.bindAll(el);
        } else {
          var $parent = import_jquery6.default(el).parent();
          if ($parent.length > 0) {
            scope = $parent;
            if (where === "beforeBegin" || where === "afterEnd") {
              var $grandparent = $parent.parent();
              if ($grandparent.length > 0)
                scope = $grandparent;
            }
          }
          Shiny.initializeInputs(scope);
          Shiny.bindAll(scope);
        }
      };
      Shiny.renderHtml = function(html, el, dependencies) {
        var where = arguments.length > 3 && arguments[3] !== void 0 ? arguments[3] : "replace";
        renderDependencies(dependencies);
        return singletons.renderHtml(html, el, where);
      };
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
        if (htmlDependencies.hasOwnProperty(dep.name) && !restyle)
          return false;
        registerDependency(dep.name, dep.version);
        var href = dep.src.href;
        var $head = import_jquery6.default("head").first();
        if (dep.meta && !restyle) {
          var metas = import_jquery6.default.map(asArray(dep.meta), function(obj, idx) {
            var name = Object.keys(obj)[0];
            return import_jquery6.default("<meta>").attr("name", name).attr("content", obj[name]);
          });
          $head.append(metas);
        }
        if (dep.stylesheet) {
          var links = import_jquery6.default.map(asArray(dep.stylesheet), function(stylesheet) {
            return import_jquery6.default("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
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
                var newStyle = import_jquery6.default("<style>").attr("id", id).html(xhr.responseText);
                $head.append(newStyle);
                oldStyle.remove();
                removeSheet(oldSheet);
                sendImageSize();
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
              import_jquery6.default(sheet.ownerNode).remove();
            };
            import_jquery6.default.map(links, function(link) {
              var oldSheet = findSheet(link.attr("href"));
              var href2 = link.attr("href") + "?restyle=" + new Date().getTime();
              if (isIE()) {
                refreshStyle(href2, oldSheet);
              } else {
                link.attr("href", href2);
                link.attr("onload", function() {
                  var dummy_id = "dummy-" + Math.floor(Math.random() * 999999999);
                  var css_string = "#" + dummy_id + " { color: #a7c920; transition: 0.1s all; visibility: hidden; position: absolute !important; top: -1000px !important; left: 0 !important; }";
                  var base64_css_string = "data:text/css;base64," + btoa(css_string);
                  var $dummy_link = import_jquery6.default("<link rel='stylesheet' type='text/css' />");
                  $dummy_link.attr("href", base64_css_string);
                  var $dummy_el = import_jquery6.default("<div id='" + dummy_id + "'></div>");
                  $dummy_el.one("transitionend", function() {
                    $dummy_el.remove();
                    $dummy_link.remove();
                    sendImageSize();
                    removeSheet(oldSheet);
                  });
                  import_jquery6.default(document.body).append($dummy_el);
                  setTimeout(function() {
                    return $head.append($dummy_link);
                  }, 0);
                });
                $head.append(link);
              }
            });
          }
        }
        if (dep.script && !restyle) {
          var scripts = import_jquery6.default.map(asArray(dep.script), function(scriptName) {
            return import_jquery6.default("<script>").attr("src", href + "/" + encodeURI(scriptName));
          });
          $head.append(scripts);
        }
        if (dep.attachment && !restyle) {
          var attachments = dep.attachment;
          if (typeof attachments === "string")
            attachments = [attachments];
          if (import_jquery6.default.isArray(attachments)) {
            var tmp = {};
            import_jquery6.default.each(attachments, function(index, attachment) {
              tmp[index + 1 + ""] = attachment;
            });
            attachments = tmp;
          }
          var attach = import_jquery6.default.map(attachments, function(attachment, key) {
            return import_jquery6.default("<link rel='attachment'>").attr("id", dep.name + "-" + key + "-attachment").attr("href", href + "/" + encodeURI(attachment));
          });
          $head.append(attach);
        }
        if (dep.head && !restyle) {
          var $newHead = import_jquery6.default("<head></head>");
          $newHead.html(dep.head);
          $head.append($newHead.children());
        }
        return true;
      }
      var singletons = {
        knownSingletons: {},
        renderHtml: function renderHtml(html, el, where) {
          var processed = this._processHtml(html);
          this._addToHead(processed.head);
          this.register(processed.singletons);
          if (where === "replace") {
            import_jquery6.default(el).html(processed.html);
          } else {
            el.insertAdjacentHTML(where, processed.html);
          }
          return processed;
        },
        register: function register(s) {
          import_jquery6.default.extend(this.knownSingletons, s);
        },
        registerNames: function registerNames(s) {
          if (typeof s === "string") {
            this.knownSingletons[s] = true;
          } else if (s instanceof Array) {
            for (var i = 0; i < s.length; i++) {
              this.knownSingletons[s[i]] = true;
            }
          }
        },
        _addToHead: function _addToHead(head) {
          if (head.length > 0) {
            var tempDiv = import_jquery6.default("<div>" + head + "</div>")[0];
            var $head = import_jquery6.default("head");
            while (tempDiv.hasChildNodes()) {
              $head.append(tempDiv.firstChild);
            }
          }
        },
        _processHtml: function _processHtml(val) {
          var self2 = this;
          var newSingletons = {};
          var newVal;
          var findNewPayload = function findNewPayload2(match, p1, sig, payload) {
            if (self2.knownSingletons[sig] || newSingletons[sig])
              return "";
            newSingletons[sig] = true;
            return payload;
          };
          while (true) {
            newVal = val.replace(self2._reSingleton, findNewPayload);
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
            newVal = val.replace(self2._reHead, headAddPayload);
            if (val.length === newVal.length)
              break;
            val = newVal;
          }
          return {
            html: val,
            head: heads.join("\n"),
            singletons: newSingletons
          };
        },
        _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
        _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/
      };
      var downloadLinkOutputBinding = new OutputBinding();
      import_jquery6.default.extend(downloadLinkOutputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find("a.shiny-download-link");
        },
        renderValue: function renderValue(el, data) {
          import_jquery6.default(el).attr("href", data);
        }
      });
      outputBindings.register(downloadLinkOutputBinding, "shiny.downloadLink");
      import_jquery6.default(document).on("click.shinyDownloadLink", "a.shiny-download-link", function(e) {
        var evt = jQuery.Event("shiny:filedownload");
        evt.name = this.id;
        evt.href = this.href;
        import_jquery6.default(document).trigger(evt);
      });
      var datatableOutputBinding = new OutputBinding();
      import_jquery6.default.extend(datatableOutputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-datatable-output");
        },
        onValueError: function onValueError(el, err) {
          Shiny.unbindAll(el);
          this.renderError(el, err);
        },
        renderValue: function renderValue(el, _data) {
          var $el = import_jquery6.default(el).empty();
          if (!_data || !_data.colnames)
            return;
          var colnames = import_jquery6.default.makeArray(_data.colnames);
          var header = import_jquery6.default.map(colnames, function(x) {
            return "<th>" + x + "</th>";
          }).join("");
          header = "<thead><tr>" + header + "</tr></thead>";
          var footer = "";
          if (_data.options === null || _data.options.searching !== false) {
            footer = import_jquery6.default.map(colnames, function(x) {
              return '<th><input type="text" placeholder="' + escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) + '" /></th>';
            }).join("");
            footer = "<tfoot>" + footer + "</tfoot>";
          }
          var content = '<table class="table table-striped table-hover">' + header + footer + "</table>";
          $el.append(content);
          if (_data.evalOptions)
            import_jquery6.default.each(_data.evalOptions, function(i, x) {
              _data.options[x] = eval("(" + _data.options[x] + ")");
            });
          var searchCI = _data.options === null || typeof _data.options.search === "undefined" || _data.options.search.caseInsensitive !== false;
          var oTable = import_jquery6.default(el).children("table").DataTable(import_jquery6.default.extend({
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
            var callback = eval("(" + _data.callback + ")");
            if (typeof callback === "function")
              callback(oTable);
          }
          $el.find("label input").first().unbind("keyup").keyup(debounce(_data.searchDelay, function() {
            oTable.search(this.value).draw();
          }));
          var searchInputs = $el.find("tfoot input");
          if (searchInputs.length > 0) {
            import_jquery6.default.each(oTable.settings()[0].aoColumns, function(i, x) {
              if (!x.bSearchable)
                searchInputs.eq(i).hide();
            });
            searchInputs.keyup(debounce(_data.searchDelay, function() {
              oTable.column(searchInputs.index(this)).search(this.value).draw();
            }));
          }
          $el.parents(".tab-content").css("overflow", "visible");
        }
      });
      outputBindings.register(datatableOutputBinding, "shiny.datatableOutput");
      var OutputBindingAdapter = function OutputBindingAdapter(el, binding) {
        this.el = el;
        this.binding = binding;
        if (binding.resize) {
          this.onResize = makeResizeFilter(el, function(width, height) {
            binding.resize(el, width, height);
          });
        }
      };
      (function() {
        this.getId = function() {
          return this.binding.getId(this.el);
        };
        this.onValueChange = function(data) {
          this.binding.onValueChange(this.el, data);
        };
        this.onValueError = function(err) {
          this.binding.onValueError(this.el, err);
        };
        this.showProgress = function(show) {
          this.binding.showProgress(this.el, show);
        };
        this.onResize = function() {
        };
      }).call(OutputBindingAdapter.prototype);
      var InputBinding = Shiny.InputBinding = function() {
      };
      (function() {
        this.find = function(scope) {
          throw "Not implemented";
        };
        this.getId = function(el) {
          return el["data-input-id"] || el.id;
        };
        this.getType = function() {
          return false;
        };
        this.getValue = function(el) {
          throw "Not implemented";
        };
        this.subscribe = function(el, callback) {
        };
        this.unsubscribe = function(el) {
        };
        this.receiveMessage = function(el, data) {
          throw "Not implemented";
        };
        this.getState = function(el, data) {
          throw "Not implemented";
        };
        this.getRatePolicy = function() {
          return null;
        };
        this.initialize = function(el) {
        };
        this.dispose = function(el) {
        };
      }).call(InputBinding.prototype);
      var textInputBinding = new InputBinding();
      import_jquery6.default.extend(textInputBinding, {
        find: function find2(scope) {
          var $inputs = import_jquery6.default(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
          return $inputs.not('input[type="text"][id$="-selectized"]');
        },
        getId: function getId(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function getValue(el) {
          return el.value;
        },
        setValue: function setValue(el, value) {
          el.value = value;
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("keyup.textInputBinding input.textInputBinding", function(event) {
            callback(true);
          });
          import_jquery6.default(el).on("change.textInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".textInputBinding");
        },
        receiveMessage: function receiveMessage(el, data) {
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          updateLabel(data.label, this._getLabelNode(el));
          if (data.hasOwnProperty("placeholder"))
            el.placeholder = data.placeholder;
          import_jquery6.default(el).trigger("change");
        },
        getState: function getState(el) {
          return {
            label: this._getLabelNode(el).text(),
            value: el.value,
            placeholder: el.placeholder
          };
        },
        getRatePolicy: function getRatePolicy() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).parent().find('label[for="' + $escape(el.id) + '"]');
        }
      });
      inputBindings.register(textInputBinding, "shiny.textInput");
      var textareaInputBinding = {};
      import_jquery6.default.extend(textareaInputBinding, textInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find("textarea");
        }
      });
      inputBindings.register(textareaInputBinding, "shiny.textareaInput");
      var passwordInputBinding = {};
      import_jquery6.default.extend(passwordInputBinding, textInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find('input[type="password"]');
        },
        getType: function getType(el) {
          return "shiny.password";
        }
      });
      inputBindings.register(passwordInputBinding, "shiny.passwordInput");
      var numberInputBinding = {};
      import_jquery6.default.extend(numberInputBinding, textInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find('input[type="number"]');
        },
        getValue: function getValue(el) {
          var numberVal = import_jquery6.default(el).val();
          if (/^\s*$/.test(numberVal))
            return null;
          else if (!isNaN(numberVal))
            return +numberVal;
          else
            return numberVal;
        },
        setValue: function setValue(el, value) {
          el.value = value;
        },
        getType: function getType(el) {
          return "shiny.number";
        },
        receiveMessage: function receiveMessage(el, data) {
          if (data.hasOwnProperty("value"))
            el.value = data.value;
          if (data.hasOwnProperty("min"))
            el.min = data.min;
          if (data.hasOwnProperty("max"))
            el.max = data.max;
          if (data.hasOwnProperty("step"))
            el.step = data.step;
          updateLabel(data.label, this._getLabelNode(el));
          import_jquery6.default(el).trigger("change");
        },
        getState: function getState(el) {
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            min: Number(el.min),
            max: Number(el.max),
            step: Number(el.step)
          };
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).parent().find('label[for="' + $escape(el.id) + '"]');
        }
      });
      inputBindings.register(numberInputBinding, "shiny.numberInput");
      var checkboxInputBinding = new InputBinding();
      import_jquery6.default.extend(checkboxInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find('input[type="checkbox"]');
        },
        getValue: function getValue(el) {
          return el.checked;
        },
        setValue: function setValue(el, value) {
          el.checked = value;
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("change.checkboxInputBinding", function(event) {
            callback(true);
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".checkboxInputBinding");
        },
        getState: function getState(el) {
          return {
            label: import_jquery6.default(el).parent().find("span").text(),
            value: el.checked
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          if (data.hasOwnProperty("value"))
            el.checked = data.value;
          if (data.hasOwnProperty("label"))
            import_jquery6.default(el).parent().find("span").text(data.label);
          import_jquery6.default(el).trigger("change");
        }
      });
      inputBindings.register(checkboxInputBinding, "shiny.checkboxInput");
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
          timeFormatter = strftime.utc();
          prettify = function prettify2(num) {
            return timeFormatter(timeFormat, new Date(num));
          };
        } else if (dataType === "datetime") {
          if (timezone)
            timeFormatter = strftime.timezone(timezone);
          else
            timeFormatter = strftime;
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
      var sliderInputBinding = {};
      import_jquery6.default.extend(sliderInputBinding, textInputBinding, {
        find: function find2(scope) {
          if (!import_jquery6.default.fn.ionRangeSlider)
            return [];
          return import_jquery6.default(scope).find("input.js-range-slider");
        },
        getType: function getType(el) {
          var dataType = import_jquery6.default(el).data("data-type");
          if (dataType === "date")
            return "shiny.date";
          else if (dataType === "datetime")
            return "shiny.datetime";
          else
            return false;
        },
        getValue: function getValue(el) {
          var $el = import_jquery6.default(el);
          var result = import_jquery6.default(el).data("ionRangeSlider").result;
          var convert;
          var dataType = $el.data("data-type");
          if (dataType === "date") {
            convert = function convert2(val) {
              return formatDateUTC(new Date(+val));
            };
          } else if (dataType === "datetime") {
            convert = function convert2(val) {
              return +val / 1e3;
            };
          } else {
            convert = function convert2(val) {
              return +val;
            };
          }
          if (this._numValues(el) === 2) {
            return [convert(result.from), convert(result.to)];
          } else {
            return convert(result.from);
          }
        },
        setValue: function setValue(el, value) {
          var $el = import_jquery6.default(el);
          var slider = $el.data("ionRangeSlider");
          $el.data("immediate", true);
          try {
            if (this._numValues(el) === 2 && value instanceof Array) {
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
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("change.sliderInputBinding", function(event) {
            callback(!import_jquery6.default(el).data("immediate") && !import_jquery6.default(el).data("animating"));
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".sliderInputBinding");
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el);
          var slider = $el.data("ionRangeSlider");
          var msg = {};
          if (data.hasOwnProperty("value")) {
            if (this._numValues(el) === 2 && data.value instanceof Array) {
              msg.from = data.value[0];
              msg.to = data.value[1];
            } else {
              msg.from = data.value;
            }
          }
          var sliderFeatures = ["min", "max", "step"];
          for (var i = 0; i < sliderFeatures.length; i++) {
            var feats = sliderFeatures[i];
            if (data.hasOwnProperty(feats)) {
              msg[feats] = data[feats];
            }
          }
          updateLabel(data.label, this._getLabelNode(el));
          var domElements = ["data-type", "time-format", "timezone"];
          for (var _i = 0; _i < domElements.length; _i++) {
            var elem = domElements[_i];
            if (data.hasOwnProperty(elem)) {
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
        },
        getRatePolicy: function getRatePolicy() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        getState: function getState(el) {
        },
        initialize: function initialize(el) {
          var opts = {};
          var $el = import_jquery6.default(el);
          var dataType = $el.data("data-type");
          var timeFormat = $el.data("time-format");
          var timezone = $el.data("timezone");
          opts.prettify = getTypePrettifyer(dataType, timeFormat, timezone);
          $el.ionRangeSlider(opts);
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).parent().find('label[for="' + $escape(el.id) + '"]');
        },
        _numValues: function _numValues(el) {
          if (import_jquery6.default(el).data("ionRangeSlider").options.type === "double")
            return 2;
          else
            return 1;
        }
      });
      inputBindings.register(sliderInputBinding, "shiny.sliderInput");
      function formatNumber(num) {
        var thousand_sep = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : ",";
        var decimal_sep = arguments.length > 2 && arguments[2] !== void 0 ? arguments[2] : ".";
        var parts = num.toString().split(".");
        parts[0] = parts[0].replace(/(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g, "$1" + thousand_sep);
        if (parts.length === 1)
          return parts[0];
        else if (parts.length === 2)
          return parts[0] + decimal_sep + parts[1];
        else
          return "";
      }
      import_jquery6.default(document).on("click", ".slider-animate-button", function(evt) {
        evt.preventDefault();
        var self2 = import_jquery6.default(this);
        var target = import_jquery6.default("#" + $escape(self2.attr("data-target-id")));
        var startLabel = "Play";
        var stopLabel = "Pause";
        var loop = self2.attr("data-loop") !== void 0 && !/^\s*false\s*$/i.test(self2.attr("data-loop"));
        var animInterval = self2.attr("data-interval");
        if (isNaN(animInterval))
          animInterval = 1500;
        else
          animInterval = +animInterval;
        if (!target.data("animTimer")) {
          var slider;
          var timer;
          if (target.hasClass("jslider")) {
            slider = target.slider();
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
            slider = target.data("ionRangeSlider");
            var sliderCanStep = function sliderCanStep2() {
              if (slider.options.type === "double")
                return slider.result.to < slider.result.max;
              else
                return slider.result.from < slider.result.max;
            };
            var sliderReset = function sliderReset2() {
              var val = {
                from: slider.result.min
              };
              if (slider.options.type === "double")
                val.to = val.from + (slider.result.to - slider.result.from);
              slider.update(val);
              forceIonSliderUpdate(slider);
            };
            var sliderStep = function sliderStep2() {
              var val = {
                from: Math.min(slider.result.max, slider.result.from + slider.options.step)
              };
              if (slider.options.type === "double")
                val.to = Math.min(slider.result.max, slider.result.to + slider.options.step);
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
      var dateInputBinding = new InputBinding();
      import_jquery6.default.extend(dateInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-date-input");
        },
        getType: function getType(el) {
          return "shiny.date";
        },
        getValue: function getValue(el) {
          var date = import_jquery6.default(el).find("input").bsDatepicker("getUTCDate");
          return formatDateUTC(date);
        },
        setValue: function setValue(el, value) {
          if (value === null) {
            import_jquery6.default(el).find("input").val("").bsDatepicker("update");
            return;
          }
          var date = this._newDate(value);
          if (isNaN(date))
            return;
          import_jquery6.default(el).find("input").bsDatepicker("setUTCDate", date);
        },
        getState: function getState(el) {
          var $el = import_jquery6.default(el);
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
        },
        receiveMessage: function receiveMessage(el, data) {
          var $input = import_jquery6.default(el).find("input");
          updateLabel(data.label, this._getLabelNode(el));
          if (data.hasOwnProperty("min"))
            this._setMin($input[0], data.min);
          if (data.hasOwnProperty("max"))
            this._setMax($input[0], data.max);
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          import_jquery6.default(el).trigger("change");
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("keyup.dateInputBinding input.dateInputBinding", function(event) {
            callback(true);
          });
          import_jquery6.default(el).on("changeDate.dateInputBinding change.dateInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".dateInputBinding");
        },
        getRatePolicy: function getRatePolicy() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        initialize: function initialize(el) {
          var $input = import_jquery6.default(el).find("input");
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
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).find('label[for="' + $escape(el.id) + '"]');
        },
        _formatToString: function _formatToString(format) {
          var str = "";
          var i;
          for (i = 0; i < format.parts.length; i++) {
            str += format.separators[i] + format.parts[i];
          }
          str += format.separators[i];
          return str;
        },
        _setMin: function _setMin(el, date) {
          if (date === void 0)
            return;
          if (date === null) {
            import_jquery6.default(el).bsDatepicker("setStartDate", null);
            return;
          }
          date = this._newDate(date);
          if (date === null)
            return;
          if (isNaN(date))
            return;
          var curValue = import_jquery6.default(el).bsDatepicker("getUTCDate");
          import_jquery6.default(el).bsDatepicker("setStartDate", this._UTCDateAsLocal(date));
          if (date && curValue && date.getTime() > curValue.getTime()) {
            import_jquery6.default(el).bsDatepicker("clearDates");
          } else {
            import_jquery6.default(el).bsDatepicker("setUTCDate", curValue);
          }
        },
        _setMax: function _setMax(el, date) {
          if (date === void 0)
            return;
          if (date === null) {
            import_jquery6.default(el).bsDatepicker("setEndDate", null);
            return;
          }
          date = this._newDate(date);
          if (date === null)
            return;
          if (isNaN(date))
            return;
          var curValue = import_jquery6.default(el).bsDatepicker("getUTCDate");
          import_jquery6.default(el).bsDatepicker("setEndDate", this._UTCDateAsLocal(date));
          if (date && curValue && date.getTime() < curValue.getTime()) {
            import_jquery6.default(el).bsDatepicker("clearDates");
          } else {
            import_jquery6.default(el).bsDatepicker("setUTCDate", curValue);
          }
        },
        _newDate: function _newDate(date) {
          if (date instanceof Date)
            return date;
          if (!date)
            return null;
          var d = parseDate(date);
          if (isNaN(d))
            return null;
          return d;
        },
        _floorDateTime: function _floorDateTime(date) {
          date = new Date(date.getTime());
          date.setUTCHours(0, 0, 0, 0);
          return date;
        },
        _dateAsUTC: function _dateAsUTC(date) {
          return new Date(date.getTime() - date.getTimezoneOffset() * 6e4);
        },
        _UTCDateAsLocal: function _UTCDateAsLocal(date) {
          return new Date(date.getTime() + date.getTimezoneOffset() * 6e4);
        }
      });
      inputBindings.register(dateInputBinding, "shiny.dateInput");
      var dateRangeInputBinding = {};
      import_jquery6.default.extend(dateRangeInputBinding, dateInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-date-range-input");
        },
        getValue: function getValue(el) {
          var $inputs = import_jquery6.default(el).find("input");
          var start = $inputs.eq(0).bsDatepicker("getUTCDate");
          var end = $inputs.eq(1).bsDatepicker("getUTCDate");
          return [formatDateUTC(start), formatDateUTC(end)];
        },
        setValue: function setValue(el, value) {
          if (!(value instanceof Object)) {
            return;
          }
          var $inputs = import_jquery6.default(el).find("input");
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
        },
        getState: function getState(el) {
          var $el = import_jquery6.default(el);
          var $inputs = $el.find("input");
          var $startinput = $inputs.eq(0);
          var $endinput = $inputs.eq(1);
          var min4 = $startinput.bsDatepicker("getStartDate");
          var max4 = $startinput.bsDatepicker("getEndDate");
          min4 = min4 === -Infinity ? null : formatDateUTC(min4);
          max4 = max4 === Infinity ? null : formatDateUTC(max4);
          var startview = $startinput.data("datepicker").startView;
          if (startview === 2)
            startview = "decade";
          else if (startview === 1)
            startview = "year";
          else if (startview === 0)
            startview = "month";
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            valueString: [$startinput.val(), $endinput.val()],
            min: min4,
            max: max4,
            weekstart: $startinput.data("datepicker").weekStart,
            format: this._formatToString($startinput.data("datepicker").format),
            language: $startinput.data("datepicker").language,
            startview: startview
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el);
          var $inputs = $el.find("input");
          var $startinput = $inputs.eq(0);
          var $endinput = $inputs.eq(1);
          updateLabel(data.label, this._getLabelNode(el));
          if (data.hasOwnProperty("min")) {
            this._setMin($startinput[0], data.min);
            this._setMin($endinput[0], data.min);
          }
          if (data.hasOwnProperty("max")) {
            this._setMax($startinput[0], data.max);
            this._setMax($endinput[0], data.max);
          }
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          $el.trigger("change");
        },
        initialize: function initialize(el) {
          var $el = import_jquery6.default(el);
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
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("keyup.dateRangeInputBinding input.dateRangeInputBinding", function(event) {
            callback(true);
          });
          import_jquery6.default(el).on("changeDate.dateRangeInputBinding change.dateRangeInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".dateRangeInputBinding");
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).find('label[for="' + $escape(el.id) + '"]');
        }
      });
      inputBindings.register(dateRangeInputBinding, "shiny.dateRangeInput");
      var selectInputBinding = new InputBinding();
      import_jquery6.default.extend(selectInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find("select");
        },
        getType: function getType(el) {
          var $el = import_jquery6.default(el);
          if (!$el.hasClass("symbol")) {
            return null;
          }
          if ($el.attr("multiple") === "multiple") {
            return "shiny.symbolList";
          } else {
            return "shiny.symbol";
          }
        },
        getId: function getId(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function getValue(el) {
          return import_jquery6.default(el).val();
        },
        setValue: function setValue(el, value) {
          if (!this._is_selectize(el)) {
            import_jquery6.default(el).val(value);
          } else {
            var selectize = this._selectize(el);
            if (selectize) {
              selectize.setValue(value);
            }
          }
        },
        getState: function getState(el) {
          var options = new Array(el.length);
          for (var i = 0; i < el.length; i++) {
            options[i] = {
              value: el[i].value,
              label: el[i].label
            };
          }
          return {
            label: this._getLabelNode(el),
            value: this.getValue(el),
            options: options
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el), selectize;
          if (data.hasOwnProperty("options")) {
            selectize = this._selectize(el);
            if (selectize)
              selectize.destroy();
            $el.empty().append(data.options);
            this._selectize(el);
          }
          if (data.hasOwnProperty("config")) {
            $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
            this._selectize(el, true);
          }
          if (data.hasOwnProperty("url")) {
            selectize = this._selectize(el);
            selectize.clearOptions();
            var loaded = false;
            selectize.settings.load = function(query, callback) {
              var settings = selectize.settings;
              import_jquery6.default.ajax({
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
                  import_jquery6.default.each(res, function(index, elem) {
                    var optgroupId = elem[settings.optgroupField || "optgroup"];
                    var optgroup = {};
                    optgroup[settings.optgroupLabelField || "label"] = optgroupId;
                    optgroup[settings.optgroupValueField || "value"] = optgroupId;
                    selectize.addOptionGroup(optgroupId, optgroup);
                  });
                  callback(res);
                  if (!loaded) {
                    if (data.hasOwnProperty("value")) {
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
          } else if (data.hasOwnProperty("value")) {
            this.setValue(el, data.value);
          }
          updateLabel(data.label, this._getLabelNode(el));
          import_jquery6.default(el).trigger("change");
        },
        subscribe: function subscribe(el, callback) {
          var _this = this;
          import_jquery6.default(el).on("change.selectInputBinding", function(event) {
            if (el.nonempty && _this.getValue(el) === "") {
              return;
            }
            callback();
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".selectInputBinding");
        },
        initialize: function initialize(el) {
          this._selectize(el);
        },
        _getLabelNode: function _getLabelNode(el) {
          var escaped_id = $escape(el.id);
          if (this._is_selectize(el)) {
            escaped_id += "-selectized";
          }
          return import_jquery6.default(el).parent().parent().find('label[for="' + escaped_id + '"]');
        },
        _is_selectize: function _is_selectize(el) {
          var config = import_jquery6.default(el).parent().find('script[data-for="' + $escape(el.id) + '"]');
          return config.length > 0;
        },
        _selectize: function _selectize(el, update) {
          if (!import_jquery6.default.fn.selectize)
            return void 0;
          var $el = import_jquery6.default(el);
          var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
          if (config.length === 0)
            return void 0;
          var options = import_jquery6.default.extend({
            labelField: "label",
            valueField: "value",
            searchField: ["label"]
          }, JSON.parse(config.html()));
          if (typeof config.data("nonempty") !== "undefined") {
            el.nonempty = true;
            options = import_jquery6.default.extend(options, {
              onItemRemove: function onItemRemove(value) {
                if (this.getValue() === "")
                  import_jquery6.default("select#" + $escape(el.id)).empty().append(import_jquery6.default("<option/>", {
                    value: value,
                    selected: true
                  })).trigger("change");
              },
              onDropdownClose: function onDropdownClose($dropdown) {
                if (this.getValue() === "")
                  this.setValue(import_jquery6.default("select#" + $escape(el.id)).val());
              }
            });
          } else {
            el.nonempty = false;
          }
          if (config.data("eval") instanceof Array)
            import_jquery6.default.each(config.data("eval"), function(i, x) {
              options[x] = eval("(" + options[x] + ")");
            });
          var control = $el.selectize(options)[0].selectize;
          if (update) {
            var settings = import_jquery6.default.extend(control.settings, options);
            control.destroy();
            control = $el.selectize(settings)[0].selectize;
          }
          return control;
        }
      });
      inputBindings.register(selectInputBinding, "shiny.selectInput");
      var radioInputBinding = new InputBinding();
      import_jquery6.default.extend(radioInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-input-radiogroup");
        },
        getValue: function getValue(el) {
          var checked_items = import_jquery6.default('input:radio[name="' + $escape(el.id) + '"]:checked');
          if (checked_items.length === 0) {
            return null;
          }
          return checked_items.val();
        },
        setValue: function setValue(el, value) {
          if (import_jquery6.default.isArray(value) && value.length === 0) {
            import_jquery6.default('input:radio[name="' + $escape(el.id) + '"]').prop("checked", false);
          } else {
            import_jquery6.default('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
          }
        },
        getState: function getState(el) {
          var $objs = import_jquery6.default('input:radio[name="' + $escape(el.id) + '"]');
          var options = new Array($objs.length);
          for (var i = 0; i < options.length; i++) {
            options[i] = {
              value: $objs[i].value,
              label: this._getLabel($objs[i])
            };
          }
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            options: options
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el);
          if (data.hasOwnProperty("options")) {
            $el.find("div.shiny-options-group").remove();
            $el.find("label.radio").remove();
            $el.append(data.options);
          }
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          updateLabel(data.label, this._getLabelNode(el));
          import_jquery6.default(el).trigger("change");
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("change.radioInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".radioInputBinding");
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).parent().find('label[for="' + $escape(el.id) + '"]');
        },
        _getLabel: function _getLabel(obj) {
          if (obj.parentNode.tagName === "LABEL") {
            return import_jquery6.default(obj.parentNode).find("span").text().trim();
          }
          return null;
        },
        _setLabel: function _setLabel(obj, value) {
          if (obj.parentNode.tagName === "LABEL") {
            import_jquery6.default(obj.parentNode).find("span").text(value);
          }
          return null;
        }
      });
      inputBindings.register(radioInputBinding, "shiny.radioInput");
      var checkboxGroupInputBinding = new InputBinding();
      import_jquery6.default.extend(checkboxGroupInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".shiny-input-checkboxgroup");
        },
        getValue: function getValue(el) {
          var $objs = import_jquery6.default('input:checkbox[name="' + $escape(el.id) + '"]:checked');
          var values = new Array($objs.length);
          for (var i = 0; i < $objs.length; i++) {
            values[i] = $objs[i].value;
          }
          return values;
        },
        setValue: function setValue(el, value) {
          import_jquery6.default('input:checkbox[name="' + $escape(el.id) + '"]').prop("checked", false);
          if (value instanceof Array) {
            for (var i = 0; i < value.length; i++) {
              import_jquery6.default('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]').prop("checked", true);
            }
          } else {
            import_jquery6.default('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop("checked", true);
          }
        },
        getState: function getState(el) {
          var $objs = import_jquery6.default('input:checkbox[name="' + $escape(el.id) + '"]');
          var options = new Array($objs.length);
          for (var i = 0; i < options.length; i++) {
            options[i] = {
              value: $objs[i].value,
              label: this._getLabel($objs[i])
            };
          }
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            options: options
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el);
          if (data.hasOwnProperty("options")) {
            $el.find("div.shiny-options-group").remove();
            $el.find("label.checkbox").remove();
            $el.append(data.options);
          }
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          updateLabel(data.label, this._getLabelNode(el));
          import_jquery6.default(el).trigger("change");
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("change.checkboxGroupInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".checkboxGroupInputBinding");
        },
        _getLabelNode: function _getLabelNode(el) {
          return import_jquery6.default(el).find('label[for="' + $escape(el.id) + '"]');
        },
        _getLabel: function _getLabel(obj) {
          if (obj.parentNode.tagName === "LABEL") {
            return import_jquery6.default(obj.parentNode).find("span").text().trim();
          }
          return null;
        },
        _setLabel: function _setLabel(obj, value) {
          if (obj.parentNode.tagName === "LABEL") {
            import_jquery6.default(obj.parentNode).find("span").text(value);
          }
          return null;
        }
      });
      inputBindings.register(checkboxGroupInputBinding, "shiny.checkboxGroupInput");
      var actionButtonInputBinding = new InputBinding();
      import_jquery6.default.extend(actionButtonInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find(".action-button");
        },
        getValue: function getValue(el) {
          return import_jquery6.default(el).data("val") || 0;
        },
        setValue: function setValue(el, value) {
          import_jquery6.default(el).data("val", value);
        },
        getType: function getType(el) {
          return "shiny.action";
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("click.actionButtonInputBinding", function(e) {
            var $el = import_jquery6.default(this);
            var val = $el.data("val") || 0;
            $el.data("val", val + 1);
            callback();
          });
        },
        getState: function getState(el) {
          return {
            value: this.getValue(el)
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          var $el = import_jquery6.default(el);
          var label = $el.text();
          var icon = "";
          if ($el.find("i[class]").length > 0) {
            var icon_html = $el.find("i[class]")[0];
            if (icon_html === $el.children()[0]) {
              icon = import_jquery6.default(icon_html).prop("outerHTML");
            }
          }
          if (data.hasOwnProperty("label"))
            label = data.label;
          if (data.hasOwnProperty("icon")) {
            icon = data.icon;
            if (icon.length === 0)
              icon = "";
          }
          $el.html(icon + " " + label);
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".actionButtonInputBinding");
        }
      });
      inputBindings.register(actionButtonInputBinding, "shiny.actionButtonInput");
      import_jquery6.default(document).on("click", "a.action-button", function(e) {
        e.preventDefault();
      });
      var bootstrapTabInputBinding = new InputBinding();
      import_jquery6.default.extend(bootstrapTabInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find("ul.nav.shiny-tab-input");
        },
        getValue: function getValue(el) {
          var anchor = isBS3() ? import_jquery6.default(el).find("li:not(.dropdown).active > a") : import_jquery6.default(el).find(".nav-link:not(.dropdown-toggle).active, .dropdown-menu > .dropdown-item.active");
          if (anchor.length === 1)
            return this._getTabName(anchor);
          return null;
        },
        setValue: function setValue(el, value) {
          var self2 = this;
          var success = false;
          if (value) {
            var anchors = isBS3() ? import_jquery6.default(el).find("li:not(.dropdown) > a") : import_jquery6.default(el).find(".nav-link:not(.dropdown-toggle), .dropdown-menu > .dropdown-item");
            anchors.each(function() {
              if (self2._getTabName(import_jquery6.default(this)) === value) {
                import_jquery6.default(this).tab("show");
                success = true;
                return false;
              }
              return true;
            });
          }
          if (!success) {
            import_jquery6.default(el).trigger("change");
          }
        },
        getState: function getState(el) {
          return {
            value: this.getValue(el)
          };
        },
        receiveMessage: function receiveMessage(el, data) {
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          import_jquery6.default(el).trigger("change");
        },
        subscribe: function subscribe(el, callback) {
          import_jquery6.default(el).on("change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function unsubscribe(el) {
          import_jquery6.default(el).off(".bootstrapTabInputBinding");
        },
        _getTabName: function _getTabName(anchor) {
          return anchor.attr("data-value") || anchor.text();
        }
      });
      inputBindings.register(bootstrapTabInputBinding, "shiny.bootstrapTabInput");
      var FileUploader = function FileUploader(shinyapp, id, files, el) {
        this.shinyapp = shinyapp;
        this.id = id;
        this.el = el;
        FileProcessor.call(this, files);
      };
      import_jquery6.default.extend(FileUploader.prototype, FileProcessor.prototype);
      (function() {
        this.makeRequest = function(method, args, onSuccess, onFailure, blobs) {
          this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
        };
        this.onBegin = function(files, cont) {
          var self2 = this;
          this.$setError(null);
          this.$setActive(true);
          this.$setVisible(true);
          this.onProgress(null, 0);
          this.totalBytes = 0;
          this.progressBytes = 0;
          import_jquery6.default.each(files, function(i, file) {
            self2.totalBytes += file.size;
          });
          var fileInfo = import_jquery6.default.map(files, function(file, i) {
            return {
              name: file.name,
              size: file.size,
              type: file.type
            };
          });
          this.makeRequest("uploadInit", [fileInfo], function(response) {
            self2.jobId = response.jobId;
            self2.uploadUrl = response.uploadUrl;
            cont();
          }, function(error) {
            self2.onError(error);
          });
        };
        this.onFile = function(file, cont) {
          var self2 = this;
          this.onProgress(file, 0);
          import_jquery6.default.ajax(this.uploadUrl, {
            type: "POST",
            cache: false,
            xhr: function xhr() {
              var xhrVal = import_jquery6.default.ajaxSettings.xhr();
              if (xhrVal.upload) {
                xhrVal.upload.onprogress = function(e) {
                  if (e.lengthComputable) {
                    self2.onProgress(file, (self2.progressBytes + e.loaded) / self2.totalBytes);
                  }
                };
              }
              return xhrVal;
            },
            data: file,
            contentType: "application/octet-stream",
            processData: false,
            success: function success() {
              self2.progressBytes += file.size;
              cont();
            },
            error: function error(jqXHR, textStatus, errorThrown) {
              self2.onError(jqXHR.responseText || textStatus);
            }
          });
        };
        this.onComplete = function() {
          var self2 = this;
          var fileInfo = import_jquery6.default.map(this.files, function(file, i) {
            return {
              name: file.name,
              size: file.size,
              type: file.type
            };
          });
          var evt = jQuery.Event("shiny:inputchanged");
          evt.name = this.id;
          evt.value = fileInfo;
          evt.binding = fileInputBinding;
          evt.el = this.el;
          evt.inputType = "shiny.fileupload";
          import_jquery6.default(document).trigger(evt);
          this.makeRequest("uploadEnd", [this.jobId, this.id], function(response) {
            self2.$setActive(false);
            self2.onProgress(null, 1);
            self2.$bar().text("Upload complete");
            import_jquery6.default(evt.el).val("");
          }, function(error) {
            self2.onError(error);
          });
          this.$bar().text("Finishing upload");
        };
        this.onError = function(message) {
          this.$setError(message || "");
          this.$setActive(false);
        };
        this.onAbort = function() {
          this.$setVisible(false);
        };
        this.onProgress = function(file, completed) {
          this.$bar().width(Math.round(completed * 100) + "%");
          this.$bar().text(file ? file.name : "");
        };
        this.$container = function() {
          return import_jquery6.default("#" + $escape(this.id) + "_progress.shiny-file-input-progress");
        };
        this.$bar = function() {
          return import_jquery6.default("#" + $escape(this.id) + "_progress.shiny-file-input-progress .progress-bar");
        };
        this.$setVisible = function(visible) {
          this.$container().css("visibility", visible ? "visible" : "hidden");
        };
        this.$setError = function(error) {
          this.$bar().toggleClass("progress-bar-danger", error !== null);
          if (error !== null) {
            this.onProgress(null, 1);
            this.$bar().text(error);
          }
        };
        this.$setActive = function(active) {
          this.$container().toggleClass("active", !!active);
        };
      }).call(FileUploader.prototype);
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
        var $el = import_jquery6.default(el);
        abortCurrentUpload($el);
        setFileText($el, files);
        $el.data("currentUploader", new FileUploader(Shiny.shinyapp, fileInputBinding.getId(el), files, el));
      }
      function uploadFiles(evt) {
        var $el = import_jquery6.default(evt.target);
        abortCurrentUpload($el);
        var files = evt.target.files;
        var id = fileInputBinding.getId(evt.target);
        if (files.length === 0)
          return;
        setFileText($el, files);
        $el.data("currentUploader", new FileUploader(Shiny.shinyapp, id, files, evt.target));
      }
      var $fileInputs = import_jquery6.default();
      var fileInputBinding = new InputBinding();
      import_jquery6.default.extend(fileInputBinding, {
        find: function find2(scope) {
          return import_jquery6.default(scope).find('input[type="file"]');
        },
        getId: function getId(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function getValue(el) {
          var data = import_jquery6.default(el).attr("data-restore");
          if (data) {
            data = JSON.parse(data);
            var $fileText = import_jquery6.default(el).closest("div.input-group").find("input[type=text]");
            if (data.name.length === 1) {
              $fileText.val(data.name[0]);
            } else {
              $fileText.val(data.name.length + " files");
            }
            var $progress = import_jquery6.default(el).closest("div.form-group").find(".progress");
            var $bar = $progress.find(".progress-bar");
            $progress.removeClass("active");
            $bar.width("100%");
            $bar.css("visibility", "visible");
            return data;
          } else {
            return null;
          }
        },
        setValue: function setValue(el, value) {
        },
        getType: function getType(el) {
          return "shiny.file";
        },
        _zoneOf: function _zoneOf(el) {
          return import_jquery6.default(el).closest("div.input-group");
        },
        _enableDraghover: function _enableDraghover(el) {
          var $el = import_jquery6.default(el), childCounter = 0;
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
        },
        _disableDraghover: function _disableDraghover(el) {
          return import_jquery6.default(el).off(".draghover");
        },
        _ZoneClass: {
          ACTIVE: "shiny-file-input-active",
          OVER: "shiny-file-input-over"
        },
        _enableDocumentEvents: function _enableDocumentEvents() {
          var _this2 = this;
          var $doc = import_jquery6.default("html"), _this$_ZoneClass = this._ZoneClass, ACTIVE = _this$_ZoneClass.ACTIVE, OVER = _this$_ZoneClass.OVER;
          this._enableDraghover($doc).on({
            "draghover:enter.draghover": function draghoverEnterDraghover(e) {
              _this2._zoneOf($fileInputs).addClass(ACTIVE);
            },
            "draghover:leave.draghover": function draghoverLeaveDraghover(e) {
              _this2._zoneOf($fileInputs).removeClass(ACTIVE);
            },
            "draghover:drop.draghover": function draghoverDropDraghover(e) {
              _this2._zoneOf($fileInputs).removeClass(OVER).removeClass(ACTIVE);
            }
          });
        },
        _disableDocumentEvents: function _disableDocumentEvents() {
          var $doc = import_jquery6.default("html");
          $doc.off(".draghover");
          this._disableDraghover($doc);
        },
        _canSetFiles: function _canSetFiles(fileList) {
          var testEl = document.createElement("input");
          testEl.type = "file";
          try {
            testEl.files = fileList;
          } catch (e) {
            return false;
          }
          return true;
        },
        _handleDrop: function _handleDrop(e, el) {
          var files = e.originalEvent.dataTransfer.files, $el = import_jquery6.default(el);
          if (files === void 0 || files === null) {
            console.log("Dropping files is not supported on this browser. (no FileList)");
          } else if (!this._canSetFiles(files)) {
            $el.val("");
            uploadDroppedFilesIE10Plus(el, files);
          } else {
            $el.val("");
            el.files = e.originalEvent.dataTransfer.files;
            $el.trigger("change");
          }
        },
        subscribe: function subscribe(el, callback) {
          var _this3 = this;
          import_jquery6.default(el).on("change.fileInputBinding", uploadFiles);
          if ($fileInputs.length === 0)
            this._enableDocumentEvents();
          $fileInputs = $fileInputs.add(el);
          var $zone = this._zoneOf(el), OVER = this._ZoneClass.OVER;
          this._enableDraghover($zone).on({
            "draghover:enter.draghover": function draghoverEnterDraghover(e) {
              $zone.addClass(OVER);
            },
            "draghover:leave.draghover": function draghoverLeaveDraghover(e) {
              $zone.removeClass(OVER);
              e.stopPropagation();
            },
            "draghover:drop.draghover": function draghoverDropDraghover(e, dropEvent) {
              _this3._handleDrop(dropEvent, el);
            }
          });
        },
        unsubscribe: function unsubscribe(el) {
          var $el = import_jquery6.default(el), $zone = this._zoneOf(el);
          $zone.removeClass(this._ZoneClass.OVER).removeClass(this._ZoneClass.ACTIVE);
          this._disableDraghover($zone);
          $el.off(".fileInputBinding");
          $zone.off(".draghover");
          $fileInputs = $fileInputs.not(el);
          if ($fileInputs.length === 0)
            this._disableDocumentEvents();
        }
      });
      inputBindings.register(fileInputBinding, "shiny.fileInputBinding");
      var sendImageSize;
      function initShiny() {
        var shinyapp = Shiny.shinyapp = new ShinyApp();
        function bindOutputs() {
          var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document;
          scope = import_jquery6.default(scope);
          var bindings = outputBindings.getBindings();
          for (var i = 0; i < bindings.length; i++) {
            var binding = bindings[i].binding;
            var matches = binding.find(scope) || [];
            for (var j = 0; j < matches.length; j++) {
              var el = matches[j];
              var id = binding.getId(el);
              if (!id)
                continue;
              if (!import_jquery6.default.contains(document, el))
                continue;
              var $el = import_jquery6.default(el);
              if ($el.hasClass("shiny-bound-output")) {
                continue;
              }
              maybeAddThemeObserver(el);
              var bindingAdapter = new OutputBindingAdapter(el, binding);
              shinyapp.bindOutput(id, bindingAdapter);
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
          setTimeout(sendImageSize, 0);
          setTimeout(sendOutputHiddenState, 0);
        }
        function unbindOutputs() {
          var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document;
          var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
          var outputs = import_jquery6.default(scope).find(".shiny-bound-output");
          if (includeSelf && import_jquery6.default(scope).hasClass("shiny-bound-output")) {
            outputs.push(scope);
          }
          for (var i = 0; i < outputs.length; i++) {
            var $el = import_jquery6.default(outputs[i]);
            var bindingAdapter = $el.data("shiny-output-binding");
            if (!bindingAdapter)
              continue;
            var id = bindingAdapter.binding.getId(outputs[i]);
            shinyapp.unbindOutput(id, bindingAdapter);
            $el.removeClass("shiny-bound-output");
            $el.removeData("shiny-output-binding");
            $el.trigger({
              type: "shiny:unbound",
              binding: bindingAdapter.binding,
              bindingType: "output"
            });
          }
          setTimeout(sendImageSize, 0);
          setTimeout(sendOutputHiddenState, 0);
        }
        var inputBatchSender = new InputBatchSender(shinyapp);
        var inputsNoResend = new InputNoResendDecorator(inputBatchSender);
        var inputsEvent = new InputEventDecorator(inputsNoResend);
        var inputsRate = new InputRateDecorator(inputsEvent);
        var inputsDefer = new InputDeferDecorator(inputsEvent);
        var inputs;
        if (import_jquery6.default('input[type="submit"], button[type="submit"]').length > 0) {
          inputs = inputsDefer;
          import_jquery6.default('input[type="submit"], button[type="submit"]').each(function() {
            import_jquery6.default(this).click(function(event) {
              event.preventDefault();
              inputsDefer.submit();
            });
          });
        } else {
          inputs = inputsRate;
        }
        inputs = new InputValidateDecorator(inputs);
        Shiny.setInputValue = Shiny.onInputChange = function(name, value, opts) {
          opts = addDefaultInputOpts(opts);
          inputs.setInput(name, value, opts);
        };
        Shiny.forgetLastInputValue = function(name) {
          inputsNoResend.forget(name);
        };
        var boundInputs = {};
        function valueChangeCallback(binding, el, allowDeferred) {
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
        function bindInputs() {
          var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document;
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
                  valueChangeCallback(thisBinding, thisEl, allowDeferred);
                };
              }();
              binding.subscribe(el, thisCallback);
              import_jquery6.default(el).data("shiny-input-binding", binding);
              import_jquery6.default(el).addClass("shiny-bound-input");
              var ratePolicy = binding.getRatePolicy(el);
              if (ratePolicy !== null) {
                inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
              }
              boundInputs[id] = {
                binding: binding,
                node: el
              };
              import_jquery6.default(el).trigger({
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
        function unbindInputs() {
          var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document;
          var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
          var inputs2 = import_jquery6.default(scope).find(".shiny-bound-input");
          if (includeSelf && import_jquery6.default(scope).hasClass("shiny-bound-input")) {
            inputs2.push(scope);
          }
          for (var i = 0; i < inputs2.length; i++) {
            var el = inputs2[i];
            var binding = import_jquery6.default(el).data("shiny-input-binding");
            if (!binding)
              continue;
            var id = binding.getId(el);
            import_jquery6.default(el).removeClass("shiny-bound-input");
            delete boundInputs[id];
            binding.unsubscribe(el);
            import_jquery6.default(el).trigger({
              type: "shiny:unbound",
              binding: binding,
              bindingType: "input"
            });
          }
        }
        function _bindAll(scope) {
          bindOutputs(scope);
          return bindInputs(scope);
        }
        function unbindAll(scope) {
          var includeSelf = arguments.length > 1 && arguments[1] !== void 0 ? arguments[1] : false;
          unbindInputs(scope, includeSelf);
          unbindOutputs(scope, includeSelf);
        }
        Shiny.bindAll = function(scope) {
          var currentInputItems = _bindAll(scope);
          import_jquery6.default.each(currentInputItems, function(name, item) {
            inputs.setInput(name, item.value, item.opts);
          });
          initDeferredIframes();
        };
        Shiny.unbindAll = unbindAll;
        function initializeInputs() {
          var scope = arguments.length > 0 && arguments[0] !== void 0 ? arguments[0] : document;
          var bindings = inputBindings.getBindings();
          for (var i = 0; i < bindings.length; i++) {
            var binding = bindings[i].binding;
            var inputObjects = binding.find(scope) || [];
            for (var j = 0; j < inputObjects.length; j++) {
              if (!inputObjects[j]._shiny_initialized) {
                inputObjects[j]._shiny_initialized = true;
                binding.initialize(inputObjects[j]);
              }
            }
          }
        }
        Shiny.initializeInputs = initializeInputs;
        function getIdFromEl(el) {
          var $el = import_jquery6.default(el);
          var bindingAdapter = $el.data("shiny-output-binding");
          if (!bindingAdapter)
            return null;
          else
            return bindingAdapter.getId();
        }
        initializeInputs(document);
        var initialValues = mapValues(_bindAll(document), function(x) {
          return x.value;
        });
        import_jquery6.default(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
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
        import_jquery6.default(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
          var el = this, id = getIdFromEl(el);
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
          var $el = import_jquery6.default(el);
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
          import_jquery6.default(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
            var id = getIdFromEl(this);
            if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
              inputs.setInput(".clientdata_output_" + id + "_width", this.offsetWidth);
              inputs.setInput(".clientdata_output_" + id + "_height", this.offsetHeight);
            }
          });
          import_jquery6.default(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
            doSendTheme(this);
          });
          import_jquery6.default(".shiny-bound-output").each(function() {
            var $this = import_jquery6.default(this), binding = $this.data("shiny-output-binding");
            $this.trigger({
              type: "shiny:visualchange",
              visible: !isHidden(this),
              binding: binding
            });
            binding.onResize();
          });
        }
        var sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
        sendImageSize = function sendImageSize() {
          sendImageSizeDebouncer.normalCall();
        };
        inputBatchSender.lastChanceCallback.push(function() {
          if (sendImageSizeDebouncer.isPending())
            sendImageSizeDebouncer.immediateCall();
        });
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
        import_jquery6.default(".shiny-bound-output").each(function() {
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
          import_jquery6.default(".shiny-bound-output").each(function() {
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
            var $this = import_jquery6.default(this);
            evt.binding = $this.data("shiny-output-binding");
            $this.trigger(evt);
          });
          for (var name in lastKnownVisibleOutputs) {
            if (lastKnownVisibleOutputs.hasOwnProperty(name))
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
          for (var _len8 = arguments.length, args = new Array(_len8 > 2 ? _len8 - 2 : 0), _key8 = 2; _key8 < _len8; _key8++) {
            args[_key8 - 2] = arguments[_key8];
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
        import_jquery6.default(window).resize(debounce(500, sendImageSize));
        var bs3classes = ["modal", "dropdown", "tab", "tooltip", "popover", "collapse"];
        import_jquery6.default.each(bs3classes, function(idx, classname) {
          import_jquery6.default(document.body).on("shown.bs." + classname + ".sendImageSize", "*", filterEventsByNamespace("bs", sendImageSize));
          import_jquery6.default(document.body).on("shown.bs." + classname + ".sendOutputHiddenState hidden.bs." + classname + ".sendOutputHiddenState", "*", filterEventsByNamespace("bs", sendOutputHiddenState));
        });
        import_jquery6.default(document.body).on("shown.sendImageSize", "*", sendImageSize);
        import_jquery6.default(document.body).on("shown.sendOutputHiddenState hidden.sendOutputHiddenState", "*", sendOutputHiddenState);
        initialValues[".clientdata_pixelratio"] = pixelRatio();
        import_jquery6.default(window).resize(function() {
          inputs.setInput(".clientdata_pixelratio", pixelRatio());
        });
        initialValues[".clientdata_url_protocol"] = window.location.protocol;
        initialValues[".clientdata_url_hostname"] = window.location.hostname;
        initialValues[".clientdata_url_port"] = window.location.port;
        initialValues[".clientdata_url_pathname"] = window.location.pathname;
        initialValues[".clientdata_url_search"] = window.location.search;
        import_jquery6.default(window).on("pushstate", function(e) {
          inputs.setInput(".clientdata_url_search", window.location.search);
        });
        import_jquery6.default(window).on("popstate", function(e) {
          inputs.setInput(".clientdata_url_search", window.location.search);
        });
        initialValues[".clientdata_url_hash_initial"] = window.location.hash;
        initialValues[".clientdata_url_hash"] = window.location.hash;
        import_jquery6.default(window).on("hashchange", function(e) {
          inputs.setInput(".clientdata_url_hash", window.location.hash);
        });
        var singletonText = initialValues[".clientdata_singletons"] = import_jquery6.default('script[type="application/shiny-singletons"]').text();
        singletons.registerNames(singletonText.split(/,/));
        var dependencyText = import_jquery6.default('script[type="application/html-dependencies"]').text();
        import_jquery6.default.each(dependencyText.split(/;/), function(i, depStr) {
          var match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
          if (match) {
            registerDependency(match[1], match[2]);
          }
        });
        inputsNoResend.reset(initialValues);
        shinyapp.connect(initialValues);
        import_jquery6.default(document).one("shiny:connected", function() {
          initDeferredIframes();
        });
      }
      function initDeferredIframes() {
        if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
          return;
        }
        import_jquery6.default(".shiny-frame-deferred").each(function(i, el) {
          var $el = import_jquery6.default(el);
          $el.removeClass("shiny-frame-deferred");
          $el.attr("src", $el.attr("data-deferred-src"));
          $el.attr("data-deferred-src", null);
        });
      }
      import_jquery6.default(function() {
        setTimeout(initShiny, 1);
      });
      import_jquery6.default(document).on("keydown", function(e) {
        if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey)
          return;
        var url = "reactlog?w=" + window.escape(Shiny.shinyapp.config.workerId) + "&s=" + window.escape(Shiny.shinyapp.config.sessionId);
        window.open(url);
        e.preventDefault();
      });
      import_jquery6.default(document).on("keydown", function(e) {
        if (!(e.which === 115 && (e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey || e.which === 114 && (e.ctrlKey || e.metaKey) && e.shiftKey && !e.altKey)) {
          return;
        }
        var url = "reactlog/mark?w=" + window.escape(Shiny.shinyapp.config.workerId) + "&s=" + window.escape(Shiny.shinyapp.config.sessionId);
        import_jquery6.default.get(url, function(result) {
          if (result !== "marked")
            return;
          var html = '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';
          Shiny.notifications.show({
            html: html,
            closeButton: true
          });
        }).fail(function() {
          window.open(url);
        });
        e.preventDefault();
      });
    }
  });

  // src/initialize/disableForm.ts
  var import_jquery = __toModule(require_jquery());
  function disableFormSubmission() {
    import_jquery.default(document).on("submit", "form:not([action])", function(e) {
      e.preventDefault();
    });
  }

  // src/initialize/history.ts
  var import_jquery2 = __toModule(require_jquery());
  function trackHistory() {
    var origPushState = window.history.pushState;
    window.history.pushState = function() {
      for (var _len = arguments.length, args = new Array(_len), _key = 0; _key < _len; _key++) {
        args[_key] = arguments[_key];
      }
      var result = origPushState.apply(this, args);
      import_jquery2.default(document).trigger("pushstate");
      return result;
    };
  }

  // node_modules/core-js/modules/es.array.index-of.js
  "use strict";
  var $4 = require_export();
  var $indexOf = require_array_includes().indexOf;
  var arrayMethodIsStrict = require_array_method_is_strict();
  var nativeIndexOf = [].indexOf;
  var NEGATIVE_ZERO = !!nativeIndexOf && 1 / [1].indexOf(1, -0) < 0;
  var STRICT_METHOD = arrayMethodIsStrict("indexOf");
  $4({target: "Array", proto: true, forced: NEGATIVE_ZERO || !STRICT_METHOD}, {
    indexOf: function indexOf(searchElement) {
      return NEGATIVE_ZERO ? nativeIndexOf.apply(this, arguments) || 0 : $indexOf(this, searchElement, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.parse-int.js
  var $5 = require_export();
  var parseIntImplementation = require_number_parse_int();
  $5({global: true, forced: parseInt != parseIntImplementation}, {
    parseInt: parseIntImplementation
  });

  // src/initialize/browser.ts
  var import_jquery3 = __toModule(require_jquery());

  // src/utils/browser.ts
  var isQtVal = false;
  var isIEVal = false;
  var IEVersionVal = -1;
  function setIsQt(isQt2) {
    isQtVal = isQt2;
  }
  function setIsIE(isIE2) {
    isIEVal = isIE2;
  }
  function setIEVersion(IEVersion_) {
    IEVersionVal = IEVersion_;
  }
  function isQt() {
    return isQtVal;
  }
  function isIE() {
    return isIEVal;
  }
  function IEVersion() {
    return IEVersionVal;
  }

  // src/utils/userAgent.ts
  var userAgentVal;
  function setUserAgent(userAgent) {
    userAgentVal = userAgent;
  }

  // src/initialize/browser.ts
  function getIEVersion() {
    var msie = userAgentVal.indexOf("MSIE ");
    if (isIE() && msie > 0) {
      return parseInt(userAgentVal.substring(msie + 5, userAgentVal.indexOf(".", msie)), 10);
    }
    var trident = userAgentVal.indexOf("Trident/");
    if (trident > 0) {
      var rv = userAgentVal.indexOf("rv:");
      return parseInt(userAgentVal.substring(rv + 3, userAgentVal.indexOf(".", rv)), 10);
    }
    return -1;
  }
  function determineBrowserInfo() {
    if (/\bQt\//.test(userAgentVal)) {
      import_jquery3.default(document.documentElement).addClass("qt");
      setIsQt(true);
    } else {
      setIsQt(false);
    }
    if (/\bQt\/5/.test(userAgentVal) && /Linux/.test(userAgentVal)) {
      import_jquery3.default(document.documentElement).addClass("qt5");
    }
    setIsIE(/MSIE|Trident|Edge/.test(userAgentVal));
    setIEVersion(getIEVersion());
  }

  // src/window/libraries.ts
  function windowShiny() {
    return window["Shiny"] || {};
  }

  // node_modules/core-js/modules/es.array.concat.js
  "use strict";
  var $7 = require_export();
  var fails = require_fails();
  var isArray = require_is_array();
  var isObject = require_is_object();
  var toObject = require_to_object();
  var toLength = require_to_length();
  var createProperty = require_create_property();
  var arraySpeciesCreate = require_array_species_create();
  var arrayMethodHasSpeciesSupport = require_array_method_has_species_support();
  var wellKnownSymbol = require_well_known_symbol();
  var V8_VERSION = require_engine_v8_version();
  var IS_CONCAT_SPREADABLE = wellKnownSymbol("isConcatSpreadable");
  var MAX_SAFE_INTEGER = 9007199254740991;
  var MAXIMUM_ALLOWED_INDEX_EXCEEDED = "Maximum allowed index exceeded";
  var IS_CONCAT_SPREADABLE_SUPPORT = V8_VERSION >= 51 || !fails(function() {
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
  var FORCED = !IS_CONCAT_SPREADABLE_SUPPORT || !SPECIES_SUPPORT;
  $7({target: "Array", proto: true, forced: FORCED}, {
    concat: function concat(arg) {
      var O = toObject(this);
      var A = arraySpeciesCreate(O, 0);
      var n = 0;
      var i, k, length, len, E;
      for (i = -1, length = arguments.length; i < length; i++) {
        E = i === -1 ? O : arguments[i];
        if (isConcatSpreadable(E)) {
          len = toLength(E.length);
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
  var $8 = require_export();
  var isObject2 = require_is_object();
  var isArray2 = require_is_array();
  var toAbsoluteIndex = require_to_absolute_index();
  var toLength2 = require_to_length();
  var toIndexedObject = require_to_indexed_object();
  var createProperty2 = require_create_property();
  var wellKnownSymbol2 = require_well_known_symbol();
  var arrayMethodHasSpeciesSupport2 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT = arrayMethodHasSpeciesSupport2("slice");
  var SPECIES = wellKnownSymbol2("species");
  var nativeSlice = [].slice;
  var max = Math.max;
  $8({target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT}, {
    slice: function slice(start, end) {
      var O = toIndexedObject(this);
      var length = toLength2(O.length);
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
      result = new (Constructor === void 0 ? Array : Constructor)(max(fin - k, 0));
      for (n = 0; k < fin; k++, n++)
        if (k in O)
          createProperty2(result, n, O[k]);
      result.length = n;
      return result;
    }
  });

  // node_modules/core-js/modules/es.array.splice.js
  "use strict";
  var $9 = require_export();
  var toAbsoluteIndex2 = require_to_absolute_index();
  var toInteger = require_to_integer();
  var toLength3 = require_to_length();
  var toObject2 = require_to_object();
  var arraySpeciesCreate2 = require_array_species_create();
  var createProperty3 = require_create_property();
  var arrayMethodHasSpeciesSupport3 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT2 = arrayMethodHasSpeciesSupport3("splice");
  var max2 = Math.max;
  var min = Math.min;
  var MAX_SAFE_INTEGER2 = 9007199254740991;
  var MAXIMUM_ALLOWED_LENGTH_EXCEEDED = "Maximum allowed length exceeded";
  $9({target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT2}, {
    splice: function splice(start, deleteCount) {
      var O = toObject2(this);
      var len = toLength3(O.length);
      var actualStart = toAbsoluteIndex2(start, len);
      var argumentsLength = arguments.length;
      var insertCount, actualDeleteCount, A, k, from, to;
      if (argumentsLength === 0) {
        insertCount = actualDeleteCount = 0;
      } else if (argumentsLength === 1) {
        insertCount = 0;
        actualDeleteCount = len - actualStart;
      } else {
        insertCount = argumentsLength - 2;
        actualDeleteCount = min(max2(toInteger(deleteCount), 0), len - actualStart);
      }
      if (len + insertCount - actualDeleteCount > MAX_SAFE_INTEGER2) {
        throw TypeError(MAXIMUM_ALLOWED_LENGTH_EXCEEDED);
      }
      A = arraySpeciesCreate2(O, actualDeleteCount);
      for (k = 0; k < actualDeleteCount; k++) {
        from = actualStart + k;
        if (from in O)
          createProperty3(A, k, O[from]);
      }
      A.length = actualDeleteCount;
      if (insertCount < actualDeleteCount) {
        for (k = actualStart; k < len - actualDeleteCount; k++) {
          from = k + actualDeleteCount;
          to = k + insertCount;
          if (from in O)
            O[to] = O[from];
          else
            delete O[to];
        }
        for (k = len; k > len - actualDeleteCount + insertCount; k--)
          delete O[k - 1];
      } else if (insertCount > actualDeleteCount) {
        for (k = len - actualDeleteCount; k > actualStart; k--) {
          from = k + actualDeleteCount - 1;
          to = k + insertCount - 1;
          if (from in O)
            O[to] = O[from];
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

  // node_modules/core-js/modules/es.number.to-precision.js
  "use strict";
  var $10 = require_export();
  var fails2 = require_fails();
  var thisNumberValue = require_this_number_value();
  var nativeToPrecision = 1 .toPrecision;
  var FORCED2 = fails2(function() {
    return nativeToPrecision.call(1, void 0) !== "1";
  }) || !fails2(function() {
    nativeToPrecision.call({});
  });
  $10({target: "Number", proto: true, forced: FORCED2}, {
    toPrecision: function toPrecision(precision) {
      return precision === void 0 ? nativeToPrecision.call(thisNumberValue(this)) : nativeToPrecision.call(thisNumberValue(this), precision);
    }
  });

  // node_modules/core-js/modules/es.object.keys.js
  var $11 = require_export();
  var toObject3 = require_to_object();
  var nativeKeys = require_object_keys();
  var fails3 = require_fails();
  var FAILS_ON_PRIMITIVES = fails3(function() {
    nativeKeys(1);
  });
  $11({target: "Object", stat: true, forced: FAILS_ON_PRIMITIVES}, {
    keys: function keys(it) {
      return nativeKeys(toObject3(it));
    }
  });

  // node_modules/core-js/modules/es.object.to-string.js
  var TO_STRING_TAG_SUPPORT = require_to_string_tag_support();
  var redefine = require_redefine();
  var toString = require_object_to_string();
  if (!TO_STRING_TAG_SUPPORT) {
    redefine(Object.prototype, "toString", toString, {unsafe: true});
  }

  // node_modules/core-js/modules/es.parse-float.js
  var $12 = require_export();
  var parseFloatImplementation = require_number_parse_float();
  $12({global: true, forced: parseFloat != parseFloatImplementation}, {
    parseFloat: parseFloatImplementation
  });

  // src/utils/index.ts
  var import_es_regexp_exec = __toModule(require_es_regexp_exec());

  // node_modules/core-js/modules/es.regexp.to-string.js
  "use strict";
  var redefine2 = require_redefine();
  var anObject = require_an_object();
  var fails4 = require_fails();
  var flags = require_regexp_flags();
  var TO_STRING = "toString";
  var RegExpPrototype = RegExp.prototype;
  var nativeToString = RegExpPrototype[TO_STRING];
  var NOT_GENERIC = fails4(function() {
    return nativeToString.call({source: "a", flags: "b"}) != "/a/b";
  });
  var INCORRECT_NAME = nativeToString.name != TO_STRING;
  if (NOT_GENERIC || INCORRECT_NAME) {
    redefine2(RegExp.prototype, TO_STRING, function toString2() {
      var R = anObject(this);
      var p = String(R.source);
      var rf = R.flags;
      var f = String(rf === void 0 && R instanceof RegExp && !("flags" in RegExpPrototype) ? flags.call(R) : rf);
      return "/" + p + "/" + f;
    }, {unsafe: true});
  }

  // node_modules/core-js/modules/es.string.match.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic = require_fix_regexp_well_known_symbol_logic();
  var anObject2 = require_an_object();
  var toLength4 = require_to_length();
  var requireObjectCoercible = require_require_object_coercible();
  var advanceStringIndex = require_advance_string_index();
  var regExpExec = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic("match", 1, function(MATCH, nativeMatch, maybeCallNative) {
    return [
      function match(regexp) {
        var O = requireObjectCoercible(this);
        var matcher = regexp == void 0 ? void 0 : regexp[MATCH];
        return matcher !== void 0 ? matcher.call(regexp, O) : new RegExp(regexp)[MATCH](String(O));
      },
      function(regexp) {
        var res = maybeCallNative(nativeMatch, regexp, this);
        if (res.done)
          return res.value;
        var rx = anObject2(regexp);
        var S = String(this);
        if (!rx.global)
          return regExpExec(rx, S);
        var fullUnicode = rx.unicode;
        rx.lastIndex = 0;
        var A = [];
        var n = 0;
        var result;
        while ((result = regExpExec(rx, S)) !== null) {
          var matchStr = String(result[0]);
          A[n] = matchStr;
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex(S, toLength4(rx.lastIndex), fullUnicode);
          n++;
        }
        return n === 0 ? null : A;
      }
    ];
  });

  // node_modules/core-js/modules/es.string.replace.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic2 = require_fix_regexp_well_known_symbol_logic();
  var anObject3 = require_an_object();
  var toLength5 = require_to_length();
  var toInteger2 = require_to_integer();
  var requireObjectCoercible2 = require_require_object_coercible();
  var advanceStringIndex2 = require_advance_string_index();
  var getSubstitution = require_get_substitution();
  var regExpExec2 = require_regexp_exec_abstract();
  var max3 = Math.max;
  var min2 = Math.min;
  var maybeToString = function(it) {
    return it === void 0 ? it : String(it);
  };
  fixRegExpWellKnownSymbolLogic2("replace", 2, function(REPLACE, nativeReplace, maybeCallNative, reason) {
    var REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE = reason.REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE;
    var REPLACE_KEEPS_$0 = reason.REPLACE_KEEPS_$0;
    var UNSAFE_SUBSTITUTE = REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE ? "$" : "$0";
    return [
      function replace(searchValue, replaceValue) {
        var O = requireObjectCoercible2(this);
        var replacer = searchValue == void 0 ? void 0 : searchValue[REPLACE];
        return replacer !== void 0 ? replacer.call(searchValue, O, replaceValue) : nativeReplace.call(String(O), searchValue, replaceValue);
      },
      function(regexp, replaceValue) {
        if (!REGEXP_REPLACE_SUBSTITUTES_UNDEFINED_CAPTURE && REPLACE_KEEPS_$0 || typeof replaceValue === "string" && replaceValue.indexOf(UNSAFE_SUBSTITUTE) === -1) {
          var res = maybeCallNative(nativeReplace, regexp, this, replaceValue);
          if (res.done)
            return res.value;
        }
        var rx = anObject3(regexp);
        var S = String(this);
        var functionalReplace = typeof replaceValue === "function";
        if (!functionalReplace)
          replaceValue = String(replaceValue);
        var global5 = rx.global;
        if (global5) {
          var fullUnicode = rx.unicode;
          rx.lastIndex = 0;
        }
        var results = [];
        while (true) {
          var result = regExpExec2(rx, S);
          if (result === null)
            break;
          results.push(result);
          if (!global5)
            break;
          var matchStr = String(result[0]);
          if (matchStr === "")
            rx.lastIndex = advanceStringIndex2(S, toLength5(rx.lastIndex), fullUnicode);
        }
        var accumulatedResult = "";
        var nextSourcePosition = 0;
        for (var i = 0; i < results.length; i++) {
          result = results[i];
          var matched = String(result[0]);
          var position = max3(min2(toInteger2(result.index), S.length), 0);
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

  // node_modules/core-js/modules/es.string.split.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic3 = require_fix_regexp_well_known_symbol_logic();
  var isRegExp = require_is_regexp();
  var anObject4 = require_an_object();
  var requireObjectCoercible3 = require_require_object_coercible();
  var speciesConstructor = require_species_constructor();
  var advanceStringIndex3 = require_advance_string_index();
  var toLength6 = require_to_length();
  var callRegExpExec = require_regexp_exec_abstract();
  var regexpExec = require_regexp_exec();
  var fails5 = require_fails();
  var arrayPush = [].push;
  var min3 = Math.min;
  var MAX_UINT32 = 4294967295;
  var SUPPORTS_Y = !fails5(function() {
    return !RegExp(MAX_UINT32, "y");
  });
  fixRegExpWellKnownSymbolLogic3("split", 2, function(SPLIT, nativeSplit, maybeCallNative) {
    var internalSplit;
    if ("abbc".split(/(b)*/)[1] == "c" || "test".split(/(?:)/, -1).length != 4 || "ab".split(/(?:ab)*/).length != 2 || ".".split(/(.?)(.?)/).length != 4 || ".".split(/()()/).length > 1 || "".split(/.?/).length) {
      internalSplit = function(separator, limit) {
        var string = String(requireObjectCoercible3(this));
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
        var O = requireObjectCoercible3(this);
        var splitter = separator == void 0 ? void 0 : separator[SPLIT];
        return splitter !== void 0 ? splitter.call(separator, O, limit) : internalSplit.call(String(O), separator, limit);
      },
      function(regexp, limit) {
        var res = maybeCallNative(internalSplit, regexp, this, limit, internalSplit !== nativeSplit);
        if (res.done)
          return res.value;
        var rx = anObject4(regexp);
        var S = String(this);
        var C = speciesConstructor(rx, RegExp);
        var unicodeMatching = rx.unicode;
        var flags2 = (rx.ignoreCase ? "i" : "") + (rx.multiline ? "m" : "") + (rx.unicode ? "u" : "") + (SUPPORTS_Y ? "y" : "g");
        var splitter = new C(SUPPORTS_Y ? rx : "^(?:" + rx.source + ")", flags2);
        var lim = limit === void 0 ? MAX_UINT32 : limit >>> 0;
        if (lim === 0)
          return [];
        if (S.length === 0)
          return callRegExpExec(splitter, S) === null ? [S] : [];
        var p = 0;
        var q = 0;
        var A = [];
        while (q < S.length) {
          splitter.lastIndex = SUPPORTS_Y ? q : 0;
          var z = callRegExpExec(splitter, SUPPORTS_Y ? S : S.slice(q));
          var e;
          if (z === null || (e = min3(toLength6(splitter.lastIndex + (SUPPORTS_Y ? 0 : q)), S.length)) === p) {
            q = advanceStringIndex3(S, q, unicodeMatching);
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
  }, !SUPPORTS_Y);

  // src/utils/index.ts
  var import_jquery5 = __toModule(require_jquery());

  // src/window/pixelRatio.ts
  function windowDevicePixelRatio() {
    return window.devicePixelRatio;
  }

  // src/utils/blob.ts
  var import_jquery4 = __toModule(require_jquery());
  var BlobBuilder;
  function setBlobBuilder(BlobBuilder_) {
    BlobBuilder = BlobBuilder_;
    return;
  }
  function makeBlob(parts) {
    try {
      return new Blob(parts);
    } catch (e) {
      var blobBuilder = new BlobBuilder();
      import_jquery4.default.each(parts, function(i, part) {
        blobBuilder.append(part);
      });
      return blobBuilder.getBlob();
    }
  }

  // src/utils/index.ts
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
  scopeExprToFunc.call;
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
      if (obj.hasOwnProperty(_key))
        newObj[_key] = f(obj[_key], _key, obj);
    }
    return newObj;
  }
  function isnan(x) {
    return typeof x === "number" && isNaN(x);
  }
  function _equal(x, y) {
    if (import_jquery5.default.type(x) === "object" && import_jquery5.default.type(y) === "object") {
      if (Object.keys(x).length !== Object.keys(y).length)
        return false;
      for (var prop in x) {
        if (!y.hasOwnProperty(prop) || !_equal(x[prop], y[prop]))
          return false;
      }
      return true;
    } else if (import_jquery5.default.type(x) === "array" && import_jquery5.default.type(y) === "array") {
      if (x.length !== y.length)
        return false;
      for (var i = 0; i < x.length; i++) {
        if (!_equal(x[i], y[i]))
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
    if (!import_jquery5.default.fn.tab) {
      return false;
    }
    return import_jquery5.default.fn.tab.Constructor.VERSION.match(/^3\./);
  }

  // src/shiny.ts
  var Shiny;
  function setShiny(Shiny_) {
    Shiny = Shiny_;
    Shiny.version = "1.6.0.9000";
    Shiny.$escape = $escape;
    Shiny.compareVersion = compareVersion;
  }

  // src/window/blobBuilder.ts
  function windowBlobBuilder() {
    var blob = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder || window.MSBlobBuilder;
    return blob;
  }

  // src/window/userAgent.ts
  function windowUserAgent() {
    return window.navigator.userAgent;
  }

  // src/initialize/index.ts
  function init() {
    setShiny(windowShiny());
    setUserAgent(windowUserAgent());
    determineBrowserInfo();
    trackHistory();
    disableFormSubmission();
    setBlobBuilder(windowBlobBuilder());
  }

  // node_modules/core-js/modules/es.array.filter.js
  "use strict";
  var $15 = require_export();
  var $filter = require_array_iteration().filter;
  var arrayMethodHasSpeciesSupport4 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT3 = arrayMethodHasSpeciesSupport4("filter");
  $15({target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT3}, {
    filter: function filter(callbackfn) {
      return $filter(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.array.find.js
  "use strict";
  var $16 = require_export();
  var $find = require_array_iteration().find;
  var addToUnscopables = require_add_to_unscopables();
  var FIND = "find";
  var SKIPS_HOLES = true;
  if (FIND in [])
    Array(1)[FIND](function() {
      SKIPS_HOLES = false;
    });
  $16({target: "Array", proto: true, forced: SKIPS_HOLES}, {
    find: function find(callbackfn) {
      return $find(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });
  addToUnscopables(FIND);

  // node_modules/core-js/modules/es.array.for-each.js
  "use strict";
  var $17 = require_export();
  var forEach = require_array_for_each();
  $17({target: "Array", proto: true, forced: [].forEach != forEach}, {
    forEach: forEach
  });

  // node_modules/core-js/modules/es.array.join.js
  "use strict";
  var $18 = require_export();
  var IndexedObject = require_indexed_object();
  var toIndexedObject2 = require_to_indexed_object();
  var arrayMethodIsStrict2 = require_array_method_is_strict();
  var nativeJoin = [].join;
  var ES3_STRINGS = IndexedObject != Object;
  var STRICT_METHOD2 = arrayMethodIsStrict2("join", ",");
  $18({target: "Array", proto: true, forced: ES3_STRINGS || !STRICT_METHOD2}, {
    join: function join(separator) {
      return nativeJoin.call(toIndexedObject2(this), separator === void 0 ? "," : separator);
    }
  });

  // node_modules/core-js/modules/es.array.map.js
  "use strict";
  var $19 = require_export();
  var $map = require_array_iteration().map;
  var arrayMethodHasSpeciesSupport5 = require_array_method_has_species_support();
  var HAS_SPECIES_SUPPORT4 = arrayMethodHasSpeciesSupport5("map");
  $19({target: "Array", proto: true, forced: !HAS_SPECIES_SUPPORT4}, {
    map: function map(callbackfn) {
      return $map(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.array.reduce.js
  "use strict";
  var $20 = require_export();
  var $reduce = require_array_reduce().left;
  var arrayMethodIsStrict3 = require_array_method_is_strict();
  var CHROME_VERSION = require_engine_v8_version();
  var IS_NODE = require_engine_is_node();
  var STRICT_METHOD3 = arrayMethodIsStrict3("reduce");
  var CHROME_BUG = !IS_NODE && CHROME_VERSION > 79 && CHROME_VERSION < 83;
  $20({target: "Array", proto: true, forced: !STRICT_METHOD3 || CHROME_BUG}, {
    reduce: function reduce(callbackfn) {
      return $reduce(this, callbackfn, arguments.length, arguments.length > 1 ? arguments[1] : void 0);
    }
  });

  // node_modules/core-js/modules/es.array-buffer.constructor.js
  "use strict";
  var $21 = require_export();
  var global2 = require_global();
  var arrayBufferModule = require_array_buffer();
  var setSpecies = require_set_species();
  var ARRAY_BUFFER = "ArrayBuffer";
  var ArrayBuffer2 = arrayBufferModule[ARRAY_BUFFER];
  var NativeArrayBuffer = global2[ARRAY_BUFFER];
  $21({global: true, forced: NativeArrayBuffer !== ArrayBuffer2}, {
    ArrayBuffer: ArrayBuffer2
  });
  setSpecies(ARRAY_BUFFER);

  // node_modules/core-js/modules/es.array-buffer.slice.js
  "use strict";
  var $22 = require_export();
  var fails6 = require_fails();
  var ArrayBufferModule = require_array_buffer();
  var anObject5 = require_an_object();
  var toAbsoluteIndex3 = require_to_absolute_index();
  var toLength7 = require_to_length();
  var speciesConstructor2 = require_species_constructor();
  var ArrayBuffer3 = ArrayBufferModule.ArrayBuffer;
  var DataView2 = ArrayBufferModule.DataView;
  var nativeArrayBufferSlice = ArrayBuffer3.prototype.slice;
  var INCORRECT_SLICE = fails6(function() {
    return !new ArrayBuffer3(2).slice(1, void 0).byteLength;
  });
  $22({target: "ArrayBuffer", proto: true, unsafe: true, forced: INCORRECT_SLICE}, {
    slice: function slice2(start, end) {
      if (nativeArrayBufferSlice !== void 0 && end === void 0) {
        return nativeArrayBufferSlice.call(anObject5(this), start);
      }
      var length = anObject5(this).byteLength;
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
  var $23 = require_export();
  var ArrayBufferModule2 = require_array_buffer();
  var NATIVE_ARRAY_BUFFER = require_array_buffer_native();
  $23({global: true, forced: !NATIVE_ARRAY_BUFFER}, {
    DataView: ArrayBufferModule2.DataView
  });

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

  // node_modules/core-js/modules/es.number.constructor.js
  "use strict";
  var DESCRIPTORS2 = require_descriptors();
  var global3 = require_global();
  var isForced = require_is_forced();
  var redefine3 = require_redefine();
  var has = require_has();
  var classof = require_classof_raw();
  var inheritIfRequired = require_inherit_if_required();
  var toPrimitive = require_to_primitive();
  var fails7 = require_fails();
  var create = require_object_create();
  var getOwnPropertyNames = require_object_get_own_property_names().f;
  var getOwnPropertyDescriptor = require_object_get_own_property_descriptor().f;
  var defineProperty2 = require_object_define_property().f;
  var trim = require_string_trim().trim;
  var NUMBER = "Number";
  var NativeNumber = global3[NUMBER];
  var NumberPrototype = NativeNumber.prototype;
  var BROKEN_CLASSOF = classof(create(NumberPrototype)) == NUMBER;
  var toNumber = function(argument) {
    var it = toPrimitive(argument, false);
    var first, third, radix, maxCode, digits, length, index, code;
    if (typeof it == "string" && it.length > 2) {
      it = trim(it);
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
      return dummy instanceof NumberWrapper && (BROKEN_CLASSOF ? fails7(function() {
        NumberPrototype.valueOf.call(dummy);
      }) : classof(dummy) != NUMBER) ? inheritIfRequired(new NativeNumber(toNumber(it)), dummy, NumberWrapper) : toNumber(it);
    };
    for (keys2 = DESCRIPTORS2 ? getOwnPropertyNames(NativeNumber) : "MAX_VALUE,MIN_VALUE,NaN,NEGATIVE_INFINITY,POSITIVE_INFINITY,EPSILON,isFinite,isInteger,isNaN,isSafeInteger,MAX_SAFE_INTEGER,MIN_SAFE_INTEGER,parseFloat,parseInt,isInteger,fromString,range".split(","), j = 0; keys2.length > j; j++) {
      if (has(NativeNumber, key = keys2[j]) && !has(NumberWrapper, key)) {
        defineProperty2(NumberWrapper, key, getOwnPropertyDescriptor(NativeNumber, key));
      }
    }
    NumberWrapper.prototype = NumberPrototype;
    NumberPrototype.constructor = NumberWrapper;
    redefine3(global3, NUMBER, NumberWrapper);
  }
  var NumberWrapper;
  var keys2;
  var j;
  var key;

  // node_modules/core-js/modules/es.string.search.js
  "use strict";
  var fixRegExpWellKnownSymbolLogic4 = require_fix_regexp_well_known_symbol_logic();
  var anObject6 = require_an_object();
  var requireObjectCoercible4 = require_require_object_coercible();
  var sameValue = require_same_value();
  var regExpExec3 = require_regexp_exec_abstract();
  fixRegExpWellKnownSymbolLogic4("search", 1, function(SEARCH, nativeSearch, maybeCallNative) {
    return [
      function search(regexp) {
        var O = requireObjectCoercible4(this);
        var searcher = regexp == void 0 ? void 0 : regexp[SEARCH];
        return searcher !== void 0 ? searcher.call(regexp, O) : new RegExp(regexp)[SEARCH](String(O));
      },
      function(regexp) {
        var res = maybeCallNative(nativeSearch, regexp, this);
        if (res.done)
          return res.value;
        var rx = anObject6(regexp);
        var S = String(this);
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

  // node_modules/core-js/modules/es.string.trim.js
  "use strict";
  var $24 = require_export();
  var $trim = require_string_trim().trim;
  var forcedStringTrimMethod = require_string_trim_forced();
  $24({target: "String", proto: true, forced: forcedStringTrimMethod("trim")}, {
    trim: function trim2() {
      return $trim(this);
    }
  });

  // node_modules/core-js/modules/web.dom-collections.for-each.js
  var global4 = require_global();
  var DOMIterables = require_dom_iterables();
  var forEach2 = require_array_for_each();
  var createNonEnumerableProperty = require_create_non_enumerable_property();
  for (var COLLECTION_NAME in DOMIterables) {
    Collection = global4[COLLECTION_NAME];
    CollectionPrototype = Collection && Collection.prototype;
    if (CollectionPrototype && CollectionPrototype.forEach !== forEach2)
      try {
        createNonEnumerableProperty(CollectionPrototype, "forEach", forEach2);
      } catch (error) {
        CollectionPrototype.forEach = forEach2;
      }
  }
  var Collection;
  var CollectionPrototype;

  // src/file/FileProcessor.ts
  var FileProcessor = function FileProcessor2(files) {
    this.files = files;
    this.fileIndex = -1;
    this.aborted = false;
    this.completed = false;
    this.$run();
  };
  (function() {
    this.onBegin = function(files, cont) {
      setTimeout(cont, 0);
    };
    this.onFile = function(file, cont) {
      setTimeout(cont, 0);
    };
    this.onComplete = function() {
      return;
    };
    this.onAbort = function() {
      return;
    };
    this.abort = function() {
      if (this.completed || this.aborted)
        return;
      this.aborted = true;
      this.onAbort();
    };
    this.$getRun = function() {
      var _this = this;
      var called = false;
      return function() {
        if (called)
          return;
        called = true;
        _this.$run();
      };
    };
    this.$run = function() {
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
    };
  }).call(FileProcessor.prototype);

  // src/index.ts
  var import_main = __toModule(require_main());
  init();
  import_main.main();
  window["Shiny"] = Shiny;
  window.console.log("Shiny version: ", Shiny.version);
})();
//# sourceMappingURL=shiny.js.map
