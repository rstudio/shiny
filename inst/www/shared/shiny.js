(() => {
  var __create = Object.create;
  var __defProp = Object.defineProperty;
  var __getProtoOf = Object.getPrototypeOf;
  var __hasOwnProp = Object.prototype.hasOwnProperty;
  var __getOwnPropNames = Object.getOwnPropertyNames;
  var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
  var __markAsModule = (target) => __defProp(target, "__esModule", {value: true});
  var __commonJS = (callback, module2) => () => {
    if (!module2) {
      module2 = {exports: {}};
      callback(module2.exports, module2);
    }
    return module2.exports;
  };
  var __export = (target, all) => {
    for (var name in all)
      __defProp(target, name, {get: all[name], enumerable: true});
  };
  var __exportStar = (target, module2, desc) => {
    if (module2 && typeof module2 === "object" || typeof module2 === "function") {
      for (let key of __getOwnPropNames(module2))
        if (!__hasOwnProp.call(target, key) && key !== "default")
          __defProp(target, key, {get: () => module2[key], enumerable: !(desc = __getOwnPropDesc(module2, key)) || desc.enumerable});
    }
    return target;
  };
  var __toModule = (module2) => {
    if (module2 && module2.__esModule)
      return module2;
    return __exportStar(__markAsModule(__defProp(module2 != null ? __create(__getProtoOf(module2)) : {}, "default", {value: module2, enumerable: true})), module2);
  };

  // node_modules/core-js/modules/_global.js
  var require_global = __commonJS((exports2, module2) => {
    var global2 = module2.exports = typeof window != "undefined" && window.Math == Math ? window : typeof self != "undefined" && self.Math == Math ? self : Function("return this")();
    if (typeof __g == "number")
      __g = global2;
  });

  // node_modules/core-js/modules/_has.js
  var require_has = __commonJS((exports2, module2) => {
    var hasOwnProperty = {}.hasOwnProperty;
    module2.exports = function(it, key) {
      return hasOwnProperty.call(it, key);
    };
  });

  // node_modules/core-js/modules/_fails.js
  var require_fails = __commonJS((exports2, module2) => {
    module2.exports = function(exec) {
      try {
        return !!exec();
      } catch (e) {
        return true;
      }
    };
  });

  // node_modules/core-js/modules/_descriptors.js
  var require_descriptors = __commonJS((exports2, module2) => {
    module2.exports = !require_fails()(function() {
      return Object.defineProperty({}, "a", {get: function() {
        return 7;
      }}).a != 7;
    });
  });

  // node_modules/core-js/modules/_core.js
  var require_core = __commonJS((exports2, module2) => {
    var core = module2.exports = {version: "2.6.12"};
    if (typeof __e == "number")
      __e = core;
  });

  // node_modules/core-js/modules/_is-object.js
  var require_is_object = __commonJS((exports2, module2) => {
    module2.exports = function(it) {
      return typeof it === "object" ? it !== null : typeof it === "function";
    };
  });

  // node_modules/core-js/modules/_an-object.js
  var require_an_object = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    module2.exports = function(it) {
      if (!isObject(it))
        throw TypeError(it + " is not an object!");
      return it;
    };
  });

  // node_modules/core-js/modules/_dom-create.js
  var require_dom_create = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var document2 = require_global().document;
    var is = isObject(document2) && isObject(document2.createElement);
    module2.exports = function(it) {
      return is ? document2.createElement(it) : {};
    };
  });

  // node_modules/core-js/modules/_ie8-dom-define.js
  var require_ie8_dom_define = __commonJS((exports2, module2) => {
    module2.exports = !require_descriptors() && !require_fails()(function() {
      return Object.defineProperty(require_dom_create()("div"), "a", {get: function() {
        return 7;
      }}).a != 7;
    });
  });

  // node_modules/core-js/modules/_to-primitive.js
  var require_to_primitive = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    module2.exports = function(it, S) {
      if (!isObject(it))
        return it;
      var fn, val;
      if (S && typeof (fn = it.toString) == "function" && !isObject(val = fn.call(it)))
        return val;
      if (typeof (fn = it.valueOf) == "function" && !isObject(val = fn.call(it)))
        return val;
      if (!S && typeof (fn = it.toString) == "function" && !isObject(val = fn.call(it)))
        return val;
      throw TypeError("Can't convert object to primitive value");
    };
  });

  // node_modules/core-js/modules/_object-dp.js
  var require_object_dp = __commonJS((exports2) => {
    var anObject = require_an_object();
    var IE8_DOM_DEFINE = require_ie8_dom_define();
    var toPrimitive = require_to_primitive();
    var dP = Object.defineProperty;
    exports2.f = require_descriptors() ? Object.defineProperty : function defineProperty(O, P, Attributes) {
      anObject(O);
      P = toPrimitive(P, true);
      anObject(Attributes);
      if (IE8_DOM_DEFINE)
        try {
          return dP(O, P, Attributes);
        } catch (e) {
        }
      if ("get" in Attributes || "set" in Attributes)
        throw TypeError("Accessors not supported!");
      if ("value" in Attributes)
        O[P] = Attributes.value;
      return O;
    };
  });

  // node_modules/core-js/modules/_property-desc.js
  var require_property_desc = __commonJS((exports2, module2) => {
    module2.exports = function(bitmap, value) {
      return {
        enumerable: !(bitmap & 1),
        configurable: !(bitmap & 2),
        writable: !(bitmap & 4),
        value
      };
    };
  });

  // node_modules/core-js/modules/_hide.js
  var require_hide = __commonJS((exports2, module2) => {
    var dP = require_object_dp();
    var createDesc = require_property_desc();
    module2.exports = require_descriptors() ? function(object, key, value) {
      return dP.f(object, key, createDesc(1, value));
    } : function(object, key, value) {
      object[key] = value;
      return object;
    };
  });

  // node_modules/core-js/modules/_uid.js
  var require_uid = __commonJS((exports2, module2) => {
    var id = 0;
    var px = Math.random();
    module2.exports = function(key) {
      return "Symbol(".concat(key === void 0 ? "" : key, ")_", (++id + px).toString(36));
    };
  });

  // node_modules/core-js/modules/_library.js
  var require_library = __commonJS((exports2, module2) => {
    module2.exports = false;
  });

  // node_modules/core-js/modules/_shared.js
  var require_shared = __commonJS((exports2, module2) => {
    var core = require_core();
    var global2 = require_global();
    var SHARED = "__core-js_shared__";
    var store = global2[SHARED] || (global2[SHARED] = {});
    (module2.exports = function(key, value) {
      return store[key] || (store[key] = value !== void 0 ? value : {});
    })("versions", []).push({
      version: core.version,
      mode: require_library() ? "pure" : "global",
      copyright: "\xA9 2020 Denis Pushkarev (zloirock.ru)"
    });
  });

  // node_modules/core-js/modules/_function-to-string.js
  var require_function_to_string = __commonJS((exports2, module2) => {
    module2.exports = require_shared()("native-function-to-string", Function.toString);
  });

  // node_modules/core-js/modules/_redefine.js
  var require_redefine = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var hide = require_hide();
    var has = require_has();
    var SRC = require_uid()("src");
    var $toString = require_function_to_string();
    var TO_STRING = "toString";
    var TPL = ("" + $toString).split(TO_STRING);
    require_core().inspectSource = function(it) {
      return $toString.call(it);
    };
    (module2.exports = function(O, key, val, safe) {
      var isFunction = typeof val == "function";
      if (isFunction)
        has(val, "name") || hide(val, "name", key);
      if (O[key] === val)
        return;
      if (isFunction)
        has(val, SRC) || hide(val, SRC, O[key] ? "" + O[key] : TPL.join(String(key)));
      if (O === global2) {
        O[key] = val;
      } else if (!safe) {
        delete O[key];
        hide(O, key, val);
      } else if (O[key]) {
        O[key] = val;
      } else {
        hide(O, key, val);
      }
    })(Function.prototype, TO_STRING, function toString() {
      return typeof this == "function" && this[SRC] || $toString.call(this);
    });
  });

  // node_modules/core-js/modules/_a-function.js
  var require_a_function = __commonJS((exports2, module2) => {
    module2.exports = function(it) {
      if (typeof it != "function")
        throw TypeError(it + " is not a function!");
      return it;
    };
  });

  // node_modules/core-js/modules/_ctx.js
  var require_ctx = __commonJS((exports2, module2) => {
    var aFunction = require_a_function();
    module2.exports = function(fn, that, length) {
      aFunction(fn);
      if (that === void 0)
        return fn;
      switch (length) {
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

  // node_modules/core-js/modules/_export.js
  var require_export = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var core = require_core();
    var hide = require_hide();
    var redefine = require_redefine();
    var ctx = require_ctx();
    var PROTOTYPE = "prototype";
    var $export = function(type, name, source) {
      var IS_FORCED = type & $export.F;
      var IS_GLOBAL = type & $export.G;
      var IS_STATIC = type & $export.S;
      var IS_PROTO = type & $export.P;
      var IS_BIND = type & $export.B;
      var target = IS_GLOBAL ? global2 : IS_STATIC ? global2[name] || (global2[name] = {}) : (global2[name] || {})[PROTOTYPE];
      var exports3 = IS_GLOBAL ? core : core[name] || (core[name] = {});
      var expProto = exports3[PROTOTYPE] || (exports3[PROTOTYPE] = {});
      var key, own, out, exp;
      if (IS_GLOBAL)
        source = name;
      for (key in source) {
        own = !IS_FORCED && target && target[key] !== void 0;
        out = (own ? target : source)[key];
        exp = IS_BIND && own ? ctx(out, global2) : IS_PROTO && typeof out == "function" ? ctx(Function.call, out) : out;
        if (target)
          redefine(target, key, out, type & $export.U);
        if (exports3[key] != out)
          hide(exports3, key, exp);
        if (IS_PROTO && expProto[key] != out)
          expProto[key] = out;
      }
    };
    global2.core = core;
    $export.F = 1;
    $export.G = 2;
    $export.S = 4;
    $export.P = 8;
    $export.B = 16;
    $export.W = 32;
    $export.U = 64;
    $export.R = 128;
    module2.exports = $export;
  });

  // node_modules/core-js/modules/_meta.js
  var require_meta = __commonJS((exports2, module2) => {
    var META = require_uid()("meta");
    var isObject = require_is_object();
    var has = require_has();
    var setDesc = require_object_dp().f;
    var id = 0;
    var isExtensible = Object.isExtensible || function() {
      return true;
    };
    var FREEZE = !require_fails()(function() {
      return isExtensible(Object.preventExtensions({}));
    });
    var setMeta = function(it) {
      setDesc(it, META, {value: {
        i: "O" + ++id,
        w: {}
      }});
    };
    var fastKey = function(it, create) {
      if (!isObject(it))
        return typeof it == "symbol" ? it : (typeof it == "string" ? "S" : "P") + it;
      if (!has(it, META)) {
        if (!isExtensible(it))
          return "F";
        if (!create)
          return "E";
        setMeta(it);
      }
      return it[META].i;
    };
    var getWeak = function(it, create) {
      if (!has(it, META)) {
        if (!isExtensible(it))
          return true;
        if (!create)
          return false;
        setMeta(it);
      }
      return it[META].w;
    };
    var onFreeze = function(it) {
      if (FREEZE && meta.NEED && isExtensible(it) && !has(it, META))
        setMeta(it);
      return it;
    };
    var meta = module2.exports = {
      KEY: META,
      NEED: false,
      fastKey,
      getWeak,
      onFreeze
    };
  });

  // node_modules/core-js/modules/_wks.js
  var require_wks = __commonJS((exports2, module2) => {
    var store = require_shared()("wks");
    var uid = require_uid();
    var Symbol2 = require_global().Symbol;
    var USE_SYMBOL = typeof Symbol2 == "function";
    var $exports = module2.exports = function(name) {
      return store[name] || (store[name] = USE_SYMBOL && Symbol2[name] || (USE_SYMBOL ? Symbol2 : uid)("Symbol." + name));
    };
    $exports.store = store;
  });

  // node_modules/core-js/modules/_set-to-string-tag.js
  var require_set_to_string_tag = __commonJS((exports2, module2) => {
    var def = require_object_dp().f;
    var has = require_has();
    var TAG = require_wks()("toStringTag");
    module2.exports = function(it, tag, stat) {
      if (it && !has(it = stat ? it : it.prototype, TAG))
        def(it, TAG, {configurable: true, value: tag});
    };
  });

  // node_modules/core-js/modules/_wks-ext.js
  var require_wks_ext = __commonJS((exports2) => {
    exports2.f = require_wks();
  });

  // node_modules/core-js/modules/_wks-define.js
  var require_wks_define = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var core = require_core();
    var LIBRARY = require_library();
    var wksExt = require_wks_ext();
    var defineProperty = require_object_dp().f;
    module2.exports = function(name) {
      var $Symbol = core.Symbol || (core.Symbol = LIBRARY ? {} : global2.Symbol || {});
      if (name.charAt(0) != "_" && !(name in $Symbol))
        defineProperty($Symbol, name, {value: wksExt.f(name)});
    };
  });

  // node_modules/core-js/modules/_cof.js
  var require_cof = __commonJS((exports2, module2) => {
    var toString = {}.toString;
    module2.exports = function(it) {
      return toString.call(it).slice(8, -1);
    };
  });

  // node_modules/core-js/modules/_iobject.js
  var require_iobject = __commonJS((exports2, module2) => {
    var cof = require_cof();
    module2.exports = Object("z").propertyIsEnumerable(0) ? Object : function(it) {
      return cof(it) == "String" ? it.split("") : Object(it);
    };
  });

  // node_modules/core-js/modules/_defined.js
  var require_defined = __commonJS((exports2, module2) => {
    module2.exports = function(it) {
      if (it == void 0)
        throw TypeError("Can't call method on  " + it);
      return it;
    };
  });

  // node_modules/core-js/modules/_to-iobject.js
  var require_to_iobject = __commonJS((exports2, module2) => {
    var IObject = require_iobject();
    var defined = require_defined();
    module2.exports = function(it) {
      return IObject(defined(it));
    };
  });

  // node_modules/core-js/modules/_to-integer.js
  var require_to_integer = __commonJS((exports2, module2) => {
    var ceil = Math.ceil;
    var floor = Math.floor;
    module2.exports = function(it) {
      return isNaN(it = +it) ? 0 : (it > 0 ? floor : ceil)(it);
    };
  });

  // node_modules/core-js/modules/_to-length.js
  var require_to_length = __commonJS((exports2, module2) => {
    var toInteger = require_to_integer();
    var min = Math.min;
    module2.exports = function(it) {
      return it > 0 ? min(toInteger(it), 9007199254740991) : 0;
    };
  });

  // node_modules/core-js/modules/_to-absolute-index.js
  var require_to_absolute_index = __commonJS((exports2, module2) => {
    var toInteger = require_to_integer();
    var max = Math.max;
    var min = Math.min;
    module2.exports = function(index, length) {
      index = toInteger(index);
      return index < 0 ? max(index + length, 0) : min(index, length);
    };
  });

  // node_modules/core-js/modules/_array-includes.js
  var require_array_includes = __commonJS((exports2, module2) => {
    var toIObject = require_to_iobject();
    var toLength = require_to_length();
    var toAbsoluteIndex = require_to_absolute_index();
    module2.exports = function(IS_INCLUDES) {
      return function($this, el, fromIndex) {
        var O = toIObject($this);
        var length = toLength(O.length);
        var index = toAbsoluteIndex(fromIndex, length);
        var value;
        if (IS_INCLUDES && el != el)
          while (length > index) {
            value = O[index++];
            if (value != value)
              return true;
          }
        else
          for (; length > index; index++)
            if (IS_INCLUDES || index in O) {
              if (O[index] === el)
                return IS_INCLUDES || index || 0;
            }
        return !IS_INCLUDES && -1;
      };
    };
  });

  // node_modules/core-js/modules/_shared-key.js
  var require_shared_key = __commonJS((exports2, module2) => {
    var shared = require_shared()("keys");
    var uid = require_uid();
    module2.exports = function(key) {
      return shared[key] || (shared[key] = uid(key));
    };
  });

  // node_modules/core-js/modules/_object-keys-internal.js
  var require_object_keys_internal = __commonJS((exports2, module2) => {
    var has = require_has();
    var toIObject = require_to_iobject();
    var arrayIndexOf = require_array_includes()(false);
    var IE_PROTO = require_shared_key()("IE_PROTO");
    module2.exports = function(object, names) {
      var O = toIObject(object);
      var i = 0;
      var result = [];
      var key;
      for (key in O)
        if (key != IE_PROTO)
          has(O, key) && result.push(key);
      while (names.length > i)
        if (has(O, key = names[i++])) {
          ~arrayIndexOf(result, key) || result.push(key);
        }
      return result;
    };
  });

  // node_modules/core-js/modules/_enum-bug-keys.js
  var require_enum_bug_keys = __commonJS((exports2, module2) => {
    module2.exports = "constructor,hasOwnProperty,isPrototypeOf,propertyIsEnumerable,toLocaleString,toString,valueOf".split(",");
  });

  // node_modules/core-js/modules/_object-keys.js
  var require_object_keys = __commonJS((exports2, module2) => {
    var $keys = require_object_keys_internal();
    var enumBugKeys = require_enum_bug_keys();
    module2.exports = Object.keys || function keys(O) {
      return $keys(O, enumBugKeys);
    };
  });

  // node_modules/core-js/modules/_object-gops.js
  var require_object_gops = __commonJS((exports2) => {
    exports2.f = Object.getOwnPropertySymbols;
  });

  // node_modules/core-js/modules/_object-pie.js
  var require_object_pie = __commonJS((exports2) => {
    exports2.f = {}.propertyIsEnumerable;
  });

  // node_modules/core-js/modules/_enum-keys.js
  var require_enum_keys = __commonJS((exports2, module2) => {
    var getKeys = require_object_keys();
    var gOPS = require_object_gops();
    var pIE = require_object_pie();
    module2.exports = function(it) {
      var result = getKeys(it);
      var getSymbols = gOPS.f;
      if (getSymbols) {
        var symbols = getSymbols(it);
        var isEnum = pIE.f;
        var i = 0;
        var key;
        while (symbols.length > i)
          if (isEnum.call(it, key = symbols[i++]))
            result.push(key);
      }
      return result;
    };
  });

  // node_modules/core-js/modules/_is-array.js
  var require_is_array = __commonJS((exports2, module2) => {
    var cof = require_cof();
    module2.exports = Array.isArray || function isArray(arg) {
      return cof(arg) == "Array";
    };
  });

  // node_modules/core-js/modules/_to-object.js
  var require_to_object = __commonJS((exports2, module2) => {
    var defined = require_defined();
    module2.exports = function(it) {
      return Object(defined(it));
    };
  });

  // node_modules/core-js/modules/_object-dps.js
  var require_object_dps = __commonJS((exports2, module2) => {
    var dP = require_object_dp();
    var anObject = require_an_object();
    var getKeys = require_object_keys();
    module2.exports = require_descriptors() ? Object.defineProperties : function defineProperties(O, Properties) {
      anObject(O);
      var keys = getKeys(Properties);
      var length = keys.length;
      var i = 0;
      var P;
      while (length > i)
        dP.f(O, P = keys[i++], Properties[P]);
      return O;
    };
  });

  // node_modules/core-js/modules/_html.js
  var require_html = __commonJS((exports2, module2) => {
    var document2 = require_global().document;
    module2.exports = document2 && document2.documentElement;
  });

  // node_modules/core-js/modules/_object-create.js
  var require_object_create = __commonJS((exports2, module2) => {
    var anObject = require_an_object();
    var dPs = require_object_dps();
    var enumBugKeys = require_enum_bug_keys();
    var IE_PROTO = require_shared_key()("IE_PROTO");
    var Empty = function() {
    };
    var PROTOTYPE = "prototype";
    var createDict = function() {
      var iframe = require_dom_create()("iframe");
      var i = enumBugKeys.length;
      var lt = "<";
      var gt = ">";
      var iframeDocument;
      iframe.style.display = "none";
      require_html().appendChild(iframe);
      iframe.src = "javascript:";
      iframeDocument = iframe.contentWindow.document;
      iframeDocument.open();
      iframeDocument.write(lt + "script" + gt + "document.F=Object" + lt + "/script" + gt);
      iframeDocument.close();
      createDict = iframeDocument.F;
      while (i--)
        delete createDict[PROTOTYPE][enumBugKeys[i]];
      return createDict();
    };
    module2.exports = Object.create || function create(O, Properties) {
      var result;
      if (O !== null) {
        Empty[PROTOTYPE] = anObject(O);
        result = new Empty();
        Empty[PROTOTYPE] = null;
        result[IE_PROTO] = O;
      } else
        result = createDict();
      return Properties === void 0 ? result : dPs(result, Properties);
    };
  });

  // node_modules/core-js/modules/_object-gopn.js
  var require_object_gopn = __commonJS((exports2) => {
    var $keys = require_object_keys_internal();
    var hiddenKeys = require_enum_bug_keys().concat("length", "prototype");
    exports2.f = Object.getOwnPropertyNames || function getOwnPropertyNames(O) {
      return $keys(O, hiddenKeys);
    };
  });

  // node_modules/core-js/modules/_object-gopn-ext.js
  var require_object_gopn_ext = __commonJS((exports2, module2) => {
    var toIObject = require_to_iobject();
    var gOPN = require_object_gopn().f;
    var toString = {}.toString;
    var windowNames = typeof window == "object" && window && Object.getOwnPropertyNames ? Object.getOwnPropertyNames(window) : [];
    var getWindowNames = function(it) {
      try {
        return gOPN(it);
      } catch (e) {
        return windowNames.slice();
      }
    };
    module2.exports.f = function getOwnPropertyNames(it) {
      return windowNames && toString.call(it) == "[object Window]" ? getWindowNames(it) : gOPN(toIObject(it));
    };
  });

  // node_modules/core-js/modules/_object-gopd.js
  var require_object_gopd = __commonJS((exports2) => {
    var pIE = require_object_pie();
    var createDesc = require_property_desc();
    var toIObject = require_to_iobject();
    var toPrimitive = require_to_primitive();
    var has = require_has();
    var IE8_DOM_DEFINE = require_ie8_dom_define();
    var gOPD = Object.getOwnPropertyDescriptor;
    exports2.f = require_descriptors() ? gOPD : function getOwnPropertyDescriptor(O, P) {
      O = toIObject(O);
      P = toPrimitive(P, true);
      if (IE8_DOM_DEFINE)
        try {
          return gOPD(O, P);
        } catch (e) {
        }
      if (has(O, P))
        return createDesc(!pIE.f.call(O, P), O[P]);
    };
  });

  // node_modules/core-js/modules/es6.symbol.js
  var require_es6_symbol = __commonJS(() => {
    "use strict";
    var global2 = require_global();
    var has = require_has();
    var DESCRIPTORS = require_descriptors();
    var $export = require_export();
    var redefine = require_redefine();
    var META = require_meta().KEY;
    var $fails = require_fails();
    var shared = require_shared();
    var setToStringTag = require_set_to_string_tag();
    var uid = require_uid();
    var wks = require_wks();
    var wksExt = require_wks_ext();
    var wksDefine = require_wks_define();
    var enumKeys = require_enum_keys();
    var isArray = require_is_array();
    var anObject = require_an_object();
    var isObject = require_is_object();
    var toObject = require_to_object();
    var toIObject = require_to_iobject();
    var toPrimitive = require_to_primitive();
    var createDesc = require_property_desc();
    var _create = require_object_create();
    var gOPNExt = require_object_gopn_ext();
    var $GOPD = require_object_gopd();
    var $GOPS = require_object_gops();
    var $DP = require_object_dp();
    var $keys = require_object_keys();
    var gOPD = $GOPD.f;
    var dP = $DP.f;
    var gOPN = gOPNExt.f;
    var $Symbol = global2.Symbol;
    var $JSON = global2.JSON;
    var _stringify = $JSON && $JSON.stringify;
    var PROTOTYPE = "prototype";
    var HIDDEN = wks("_hidden");
    var TO_PRIMITIVE = wks("toPrimitive");
    var isEnum = {}.propertyIsEnumerable;
    var SymbolRegistry = shared("symbol-registry");
    var AllSymbols = shared("symbols");
    var OPSymbols = shared("op-symbols");
    var ObjectProto = Object[PROTOTYPE];
    var USE_NATIVE = typeof $Symbol == "function" && !!$GOPS.f;
    var QObject = global2.QObject;
    var setter = !QObject || !QObject[PROTOTYPE] || !QObject[PROTOTYPE].findChild;
    var setSymbolDesc = DESCRIPTORS && $fails(function() {
      return _create(dP({}, "a", {
        get: function() {
          return dP(this, "a", {value: 7}).a;
        }
      })).a != 7;
    }) ? function(it, key, D) {
      var protoDesc = gOPD(ObjectProto, key);
      if (protoDesc)
        delete ObjectProto[key];
      dP(it, key, D);
      if (protoDesc && it !== ObjectProto)
        dP(ObjectProto, key, protoDesc);
    } : dP;
    var wrap = function(tag) {
      var sym = AllSymbols[tag] = _create($Symbol[PROTOTYPE]);
      sym._k = tag;
      return sym;
    };
    var isSymbol = USE_NATIVE && typeof $Symbol.iterator == "symbol" ? function(it) {
      return typeof it == "symbol";
    } : function(it) {
      return it instanceof $Symbol;
    };
    var $defineProperty = function defineProperty(it, key, D) {
      if (it === ObjectProto)
        $defineProperty(OPSymbols, key, D);
      anObject(it);
      key = toPrimitive(key, true);
      anObject(D);
      if (has(AllSymbols, key)) {
        if (!D.enumerable) {
          if (!has(it, HIDDEN))
            dP(it, HIDDEN, createDesc(1, {}));
          it[HIDDEN][key] = true;
        } else {
          if (has(it, HIDDEN) && it[HIDDEN][key])
            it[HIDDEN][key] = false;
          D = _create(D, {enumerable: createDesc(0, false)});
        }
        return setSymbolDesc(it, key, D);
      }
      return dP(it, key, D);
    };
    var $defineProperties = function defineProperties(it, P) {
      anObject(it);
      var keys = enumKeys(P = toIObject(P));
      var i = 0;
      var l = keys.length;
      var key;
      while (l > i)
        $defineProperty(it, key = keys[i++], P[key]);
      return it;
    };
    var $create = function create(it, P) {
      return P === void 0 ? _create(it) : $defineProperties(_create(it), P);
    };
    var $propertyIsEnumerable = function propertyIsEnumerable(key) {
      var E = isEnum.call(this, key = toPrimitive(key, true));
      if (this === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))
        return false;
      return E || !has(this, key) || !has(AllSymbols, key) || has(this, HIDDEN) && this[HIDDEN][key] ? E : true;
    };
    var $getOwnPropertyDescriptor = function getOwnPropertyDescriptor(it, key) {
      it = toIObject(it);
      key = toPrimitive(key, true);
      if (it === ObjectProto && has(AllSymbols, key) && !has(OPSymbols, key))
        return;
      var D = gOPD(it, key);
      if (D && has(AllSymbols, key) && !(has(it, HIDDEN) && it[HIDDEN][key]))
        D.enumerable = true;
      return D;
    };
    var $getOwnPropertyNames = function getOwnPropertyNames(it) {
      var names = gOPN(toIObject(it));
      var result = [];
      var i = 0;
      var key;
      while (names.length > i) {
        if (!has(AllSymbols, key = names[i++]) && key != HIDDEN && key != META)
          result.push(key);
      }
      return result;
    };
    var $getOwnPropertySymbols = function getOwnPropertySymbols(it) {
      var IS_OP = it === ObjectProto;
      var names = gOPN(IS_OP ? OPSymbols : toIObject(it));
      var result = [];
      var i = 0;
      var key;
      while (names.length > i) {
        if (has(AllSymbols, key = names[i++]) && (IS_OP ? has(ObjectProto, key) : true))
          result.push(AllSymbols[key]);
      }
      return result;
    };
    if (!USE_NATIVE) {
      $Symbol = function Symbol2() {
        if (this instanceof $Symbol)
          throw TypeError("Symbol is not a constructor!");
        var tag = uid(arguments.length > 0 ? arguments[0] : void 0);
        var $set = function(value) {
          if (this === ObjectProto)
            $set.call(OPSymbols, value);
          if (has(this, HIDDEN) && has(this[HIDDEN], tag))
            this[HIDDEN][tag] = false;
          setSymbolDesc(this, tag, createDesc(1, value));
        };
        if (DESCRIPTORS && setter)
          setSymbolDesc(ObjectProto, tag, {configurable: true, set: $set});
        return wrap(tag);
      };
      redefine($Symbol[PROTOTYPE], "toString", function toString() {
        return this._k;
      });
      $GOPD.f = $getOwnPropertyDescriptor;
      $DP.f = $defineProperty;
      require_object_gopn().f = gOPNExt.f = $getOwnPropertyNames;
      require_object_pie().f = $propertyIsEnumerable;
      $GOPS.f = $getOwnPropertySymbols;
      if (DESCRIPTORS && !require_library()) {
        redefine(ObjectProto, "propertyIsEnumerable", $propertyIsEnumerable, true);
      }
      wksExt.f = function(name) {
        return wrap(wks(name));
      };
    }
    $export($export.G + $export.W + $export.F * !USE_NATIVE, {Symbol: $Symbol});
    for (var es6Symbols = "hasInstance,isConcatSpreadable,iterator,match,replace,search,species,split,toPrimitive,toStringTag,unscopables".split(","), j = 0; es6Symbols.length > j; )
      wks(es6Symbols[j++]);
    for (var wellKnownSymbols = $keys(wks.store), k = 0; wellKnownSymbols.length > k; )
      wksDefine(wellKnownSymbols[k++]);
    $export($export.S + $export.F * !USE_NATIVE, "Symbol", {
      for: function(key) {
        return has(SymbolRegistry, key += "") ? SymbolRegistry[key] : SymbolRegistry[key] = $Symbol(key);
      },
      keyFor: function keyFor(sym) {
        if (!isSymbol(sym))
          throw TypeError(sym + " is not a symbol!");
        for (var key in SymbolRegistry)
          if (SymbolRegistry[key] === sym)
            return key;
      },
      useSetter: function() {
        setter = true;
      },
      useSimple: function() {
        setter = false;
      }
    });
    $export($export.S + $export.F * !USE_NATIVE, "Object", {
      create: $create,
      defineProperty: $defineProperty,
      defineProperties: $defineProperties,
      getOwnPropertyDescriptor: $getOwnPropertyDescriptor,
      getOwnPropertyNames: $getOwnPropertyNames,
      getOwnPropertySymbols: $getOwnPropertySymbols
    });
    var FAILS_ON_PRIMITIVES = $fails(function() {
      $GOPS.f(1);
    });
    $export($export.S + $export.F * FAILS_ON_PRIMITIVES, "Object", {
      getOwnPropertySymbols: function getOwnPropertySymbols(it) {
        return $GOPS.f(toObject(it));
      }
    });
    $JSON && $export($export.S + $export.F * (!USE_NATIVE || $fails(function() {
      var S = $Symbol();
      return _stringify([S]) != "[null]" || _stringify({a: S}) != "{}" || _stringify(Object(S)) != "{}";
    })), "JSON", {
      stringify: function stringify(it) {
        var args = [it];
        var i = 1;
        var replacer, $replacer;
        while (arguments.length > i)
          args.push(arguments[i++]);
        $replacer = replacer = args[1];
        if (!isObject(replacer) && it === void 0 || isSymbol(it))
          return;
        if (!isArray(replacer))
          replacer = function(key, value) {
            if (typeof $replacer == "function")
              value = $replacer.call(this, key, value);
            if (!isSymbol(value))
              return value;
          };
        args[1] = replacer;
        return _stringify.apply($JSON, args);
      }
    });
    $Symbol[PROTOTYPE][TO_PRIMITIVE] || require_hide()($Symbol[PROTOTYPE], TO_PRIMITIVE, $Symbol[PROTOTYPE].valueOf);
    setToStringTag($Symbol, "Symbol");
    setToStringTag(Math, "Math", true);
    setToStringTag(global2.JSON, "JSON", true);
  });

  // node_modules/core-js/modules/es6.object.create.js
  var require_es6_object_create = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Object", {create: require_object_create()});
  });

  // node_modules/core-js/modules/es6.object.define-property.js
  var require_es6_object_define_property = __commonJS(() => {
    var $export = require_export();
    $export($export.S + $export.F * !require_descriptors(), "Object", {defineProperty: require_object_dp().f});
  });

  // node_modules/core-js/modules/es6.object.define-properties.js
  var require_es6_object_define_properties = __commonJS(() => {
    var $export = require_export();
    $export($export.S + $export.F * !require_descriptors(), "Object", {defineProperties: require_object_dps()});
  });

  // node_modules/core-js/modules/_object-sap.js
  var require_object_sap = __commonJS((exports2, module2) => {
    var $export = require_export();
    var core = require_core();
    var fails = require_fails();
    module2.exports = function(KEY, exec) {
      var fn = (core.Object || {})[KEY] || Object[KEY];
      var exp = {};
      exp[KEY] = exec(fn);
      $export($export.S + $export.F * fails(function() {
        fn(1);
      }), "Object", exp);
    };
  });

  // node_modules/core-js/modules/es6.object.get-own-property-descriptor.js
  var require_es6_object_get_own_property_descriptor = __commonJS(() => {
    var toIObject = require_to_iobject();
    var $getOwnPropertyDescriptor = require_object_gopd().f;
    require_object_sap()("getOwnPropertyDescriptor", function() {
      return function getOwnPropertyDescriptor(it, key) {
        return $getOwnPropertyDescriptor(toIObject(it), key);
      };
    });
  });

  // node_modules/core-js/modules/_object-gpo.js
  var require_object_gpo = __commonJS((exports2, module2) => {
    var has = require_has();
    var toObject = require_to_object();
    var IE_PROTO = require_shared_key()("IE_PROTO");
    var ObjectProto = Object.prototype;
    module2.exports = Object.getPrototypeOf || function(O) {
      O = toObject(O);
      if (has(O, IE_PROTO))
        return O[IE_PROTO];
      if (typeof O.constructor == "function" && O instanceof O.constructor) {
        return O.constructor.prototype;
      }
      return O instanceof Object ? ObjectProto : null;
    };
  });

  // node_modules/core-js/modules/es6.object.get-prototype-of.js
  var require_es6_object_get_prototype_of = __commonJS(() => {
    var toObject = require_to_object();
    var $getPrototypeOf = require_object_gpo();
    require_object_sap()("getPrototypeOf", function() {
      return function getPrototypeOf(it) {
        return $getPrototypeOf(toObject(it));
      };
    });
  });

  // node_modules/core-js/modules/es6.object.keys.js
  var require_es6_object_keys = __commonJS(() => {
    var toObject = require_to_object();
    var $keys = require_object_keys();
    require_object_sap()("keys", function() {
      return function keys(it) {
        return $keys(toObject(it));
      };
    });
  });

  // node_modules/core-js/modules/es6.object.get-own-property-names.js
  var require_es6_object_get_own_property_names = __commonJS(() => {
    require_object_sap()("getOwnPropertyNames", function() {
      return require_object_gopn_ext().f;
    });
  });

  // node_modules/core-js/modules/es6.object.freeze.js
  var require_es6_object_freeze = __commonJS(() => {
    var isObject = require_is_object();
    var meta = require_meta().onFreeze;
    require_object_sap()("freeze", function($freeze) {
      return function freeze(it) {
        return $freeze && isObject(it) ? $freeze(meta(it)) : it;
      };
    });
  });

  // node_modules/core-js/modules/es6.object.seal.js
  var require_es6_object_seal = __commonJS(() => {
    var isObject = require_is_object();
    var meta = require_meta().onFreeze;
    require_object_sap()("seal", function($seal) {
      return function seal(it) {
        return $seal && isObject(it) ? $seal(meta(it)) : it;
      };
    });
  });

  // node_modules/core-js/modules/es6.object.prevent-extensions.js
  var require_es6_object_prevent_extensions = __commonJS(() => {
    var isObject = require_is_object();
    var meta = require_meta().onFreeze;
    require_object_sap()("preventExtensions", function($preventExtensions) {
      return function preventExtensions(it) {
        return $preventExtensions && isObject(it) ? $preventExtensions(meta(it)) : it;
      };
    });
  });

  // node_modules/core-js/modules/es6.object.is-frozen.js
  var require_es6_object_is_frozen = __commonJS(() => {
    var isObject = require_is_object();
    require_object_sap()("isFrozen", function($isFrozen) {
      return function isFrozen(it) {
        return isObject(it) ? $isFrozen ? $isFrozen(it) : false : true;
      };
    });
  });

  // node_modules/core-js/modules/es6.object.is-sealed.js
  var require_es6_object_is_sealed = __commonJS(() => {
    var isObject = require_is_object();
    require_object_sap()("isSealed", function($isSealed) {
      return function isSealed(it) {
        return isObject(it) ? $isSealed ? $isSealed(it) : false : true;
      };
    });
  });

  // node_modules/core-js/modules/es6.object.is-extensible.js
  var require_es6_object_is_extensible = __commonJS(() => {
    var isObject = require_is_object();
    require_object_sap()("isExtensible", function($isExtensible) {
      return function isExtensible(it) {
        return isObject(it) ? $isExtensible ? $isExtensible(it) : true : false;
      };
    });
  });

  // node_modules/core-js/modules/_object-assign.js
  var require_object_assign = __commonJS((exports2, module2) => {
    "use strict";
    var DESCRIPTORS = require_descriptors();
    var getKeys = require_object_keys();
    var gOPS = require_object_gops();
    var pIE = require_object_pie();
    var toObject = require_to_object();
    var IObject = require_iobject();
    var $assign = Object.assign;
    module2.exports = !$assign || require_fails()(function() {
      var A = {};
      var B = {};
      var S = Symbol();
      var K = "abcdefghijklmnopqrst";
      A[S] = 7;
      K.split("").forEach(function(k) {
        B[k] = k;
      });
      return $assign({}, A)[S] != 7 || Object.keys($assign({}, B)).join("") != K;
    }) ? function assign(target, source) {
      var T = toObject(target);
      var aLen = arguments.length;
      var index = 1;
      var getSymbols = gOPS.f;
      var isEnum = pIE.f;
      while (aLen > index) {
        var S = IObject(arguments[index++]);
        var keys = getSymbols ? getKeys(S).concat(getSymbols(S)) : getKeys(S);
        var length = keys.length;
        var j = 0;
        var key;
        while (length > j) {
          key = keys[j++];
          if (!DESCRIPTORS || isEnum.call(S, key))
            T[key] = S[key];
        }
      }
      return T;
    } : $assign;
  });

  // node_modules/core-js/modules/es6.object.assign.js
  var require_es6_object_assign = __commonJS(() => {
    var $export = require_export();
    $export($export.S + $export.F, "Object", {assign: require_object_assign()});
  });

  // node_modules/core-js/modules/_same-value.js
  var require_same_value = __commonJS((exports2, module2) => {
    module2.exports = Object.is || function is(x, y) {
      return x === y ? x !== 0 || 1 / x === 1 / y : x != x && y != y;
    };
  });

  // node_modules/core-js/modules/es6.object.is.js
  var require_es6_object_is = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Object", {is: require_same_value()});
  });

  // node_modules/core-js/modules/_set-proto.js
  var require_set_proto = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var anObject = require_an_object();
    var check = function(O, proto) {
      anObject(O);
      if (!isObject(proto) && proto !== null)
        throw TypeError(proto + ": can't set as prototype!");
    };
    module2.exports = {
      set: Object.setPrototypeOf || ("__proto__" in {} ? function(test, buggy, set) {
        try {
          set = require_ctx()(Function.call, require_object_gopd().f(Object.prototype, "__proto__").set, 2);
          set(test, []);
          buggy = !(test instanceof Array);
        } catch (e) {
          buggy = true;
        }
        return function setPrototypeOf(O, proto) {
          check(O, proto);
          if (buggy)
            O.__proto__ = proto;
          else
            set(O, proto);
          return O;
        };
      }({}, false) : void 0),
      check
    };
  });

  // node_modules/core-js/modules/es6.object.set-prototype-of.js
  var require_es6_object_set_prototype_of = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Object", {setPrototypeOf: require_set_proto().set});
  });

  // node_modules/core-js/modules/_classof.js
  var require_classof = __commonJS((exports2, module2) => {
    var cof = require_cof();
    var TAG = require_wks()("toStringTag");
    var ARG = cof(function() {
      return arguments;
    }()) == "Arguments";
    var tryGet = function(it, key) {
      try {
        return it[key];
      } catch (e) {
      }
    };
    module2.exports = function(it) {
      var O, T, B;
      return it === void 0 ? "Undefined" : it === null ? "Null" : typeof (T = tryGet(O = Object(it), TAG)) == "string" ? T : ARG ? cof(O) : (B = cof(O)) == "Object" && typeof O.callee == "function" ? "Arguments" : B;
    };
  });

  // node_modules/core-js/modules/es6.object.to-string.js
  var require_es6_object_to_string = __commonJS(() => {
    "use strict";
    var classof = require_classof();
    var test = {};
    test[require_wks()("toStringTag")] = "z";
    if (test + "" != "[object z]") {
      require_redefine()(Object.prototype, "toString", function toString() {
        return "[object " + classof(this) + "]";
      }, true);
    }
  });

  // node_modules/core-js/modules/_invoke.js
  var require_invoke = __commonJS((exports2, module2) => {
    module2.exports = function(fn, args, that) {
      var un = that === void 0;
      switch (args.length) {
        case 0:
          return un ? fn() : fn.call(that);
        case 1:
          return un ? fn(args[0]) : fn.call(that, args[0]);
        case 2:
          return un ? fn(args[0], args[1]) : fn.call(that, args[0], args[1]);
        case 3:
          return un ? fn(args[0], args[1], args[2]) : fn.call(that, args[0], args[1], args[2]);
        case 4:
          return un ? fn(args[0], args[1], args[2], args[3]) : fn.call(that, args[0], args[1], args[2], args[3]);
      }
      return fn.apply(that, args);
    };
  });

  // node_modules/core-js/modules/_bind.js
  var require_bind = __commonJS((exports2, module2) => {
    "use strict";
    var aFunction = require_a_function();
    var isObject = require_is_object();
    var invoke = require_invoke();
    var arraySlice = [].slice;
    var factories = {};
    var construct = function(F, len, args) {
      if (!(len in factories)) {
        for (var n = [], i = 0; i < len; i++)
          n[i] = "a[" + i + "]";
        factories[len] = Function("F,a", "return new F(" + n.join(",") + ")");
      }
      return factories[len](F, args);
    };
    module2.exports = Function.bind || function bind(that) {
      var fn = aFunction(this);
      var partArgs = arraySlice.call(arguments, 1);
      var bound = function() {
        var args = partArgs.concat(arraySlice.call(arguments));
        return this instanceof bound ? construct(fn, args.length, args) : invoke(fn, args, that);
      };
      if (isObject(fn.prototype))
        bound.prototype = fn.prototype;
      return bound;
    };
  });

  // node_modules/core-js/modules/es6.function.bind.js
  var require_es6_function_bind = __commonJS(() => {
    var $export = require_export();
    $export($export.P, "Function", {bind: require_bind()});
  });

  // node_modules/core-js/modules/es6.function.name.js
  var require_es6_function_name = __commonJS(() => {
    var dP = require_object_dp().f;
    var FProto = Function.prototype;
    var nameRE = /^\s*function ([^ (]*)/;
    var NAME = "name";
    NAME in FProto || require_descriptors() && dP(FProto, NAME, {
      configurable: true,
      get: function() {
        try {
          return ("" + this).match(nameRE)[1];
        } catch (e) {
          return "";
        }
      }
    });
  });

  // node_modules/core-js/modules/es6.function.has-instance.js
  var require_es6_function_has_instance = __commonJS(() => {
    "use strict";
    var isObject = require_is_object();
    var getPrototypeOf = require_object_gpo();
    var HAS_INSTANCE = require_wks()("hasInstance");
    var FunctionProto = Function.prototype;
    if (!(HAS_INSTANCE in FunctionProto))
      require_object_dp().f(FunctionProto, HAS_INSTANCE, {value: function(O) {
        if (typeof this != "function" || !isObject(O))
          return false;
        if (!isObject(this.prototype))
          return O instanceof this;
        while (O = getPrototypeOf(O))
          if (this.prototype === O)
            return true;
        return false;
      }});
  });

  // node_modules/core-js/modules/_string-ws.js
  var require_string_ws = __commonJS((exports2, module2) => {
    module2.exports = "	\n\v\f\r \xA0\u1680\u180E\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\u2028\u2029\uFEFF";
  });

  // node_modules/core-js/modules/_string-trim.js
  var require_string_trim = __commonJS((exports2, module2) => {
    var $export = require_export();
    var defined = require_defined();
    var fails = require_fails();
    var spaces = require_string_ws();
    var space = "[" + spaces + "]";
    var non = "\u200B\x85";
    var ltrim = RegExp("^" + space + space + "*");
    var rtrim = RegExp(space + space + "*$");
    var exporter = function(KEY, exec, ALIAS) {
      var exp = {};
      var FORCE = fails(function() {
        return !!spaces[KEY]() || non[KEY]() != non;
      });
      var fn = exp[KEY] = FORCE ? exec(trim) : spaces[KEY];
      if (ALIAS)
        exp[ALIAS] = fn;
      $export($export.P + $export.F * FORCE, "String", exp);
    };
    var trim = exporter.trim = function(string, TYPE) {
      string = String(defined(string));
      if (TYPE & 1)
        string = string.replace(ltrim, "");
      if (TYPE & 2)
        string = string.replace(rtrim, "");
      return string;
    };
    module2.exports = exporter;
  });

  // node_modules/core-js/modules/_parse-int.js
  var require_parse_int = __commonJS((exports2, module2) => {
    var $parseInt = require_global().parseInt;
    var $trim = require_string_trim().trim;
    var ws = require_string_ws();
    var hex = /^[-+]?0[xX]/;
    module2.exports = $parseInt(ws + "08") !== 8 || $parseInt(ws + "0x16") !== 22 ? function parseInt2(str, radix) {
      var string = $trim(String(str), 3);
      return $parseInt(string, radix >>> 0 || (hex.test(string) ? 16 : 10));
    } : $parseInt;
  });

  // node_modules/core-js/modules/es6.parse-int.js
  var require_es6_parse_int = __commonJS(() => {
    var $export = require_export();
    var $parseInt = require_parse_int();
    $export($export.G + $export.F * (parseInt != $parseInt), {parseInt: $parseInt});
  });

  // node_modules/core-js/modules/_parse-float.js
  var require_parse_float = __commonJS((exports2, module2) => {
    var $parseFloat = require_global().parseFloat;
    var $trim = require_string_trim().trim;
    module2.exports = 1 / $parseFloat(require_string_ws() + "-0") !== -Infinity ? function parseFloat2(str) {
      var string = $trim(String(str), 3);
      var result = $parseFloat(string);
      return result === 0 && string.charAt(0) == "-" ? -0 : result;
    } : $parseFloat;
  });

  // node_modules/core-js/modules/es6.parse-float.js
  var require_es6_parse_float = __commonJS(() => {
    var $export = require_export();
    var $parseFloat = require_parse_float();
    $export($export.G + $export.F * (parseFloat != $parseFloat), {parseFloat: $parseFloat});
  });

  // node_modules/core-js/modules/_inherit-if-required.js
  var require_inherit_if_required = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var setPrototypeOf = require_set_proto().set;
    module2.exports = function(that, target, C) {
      var S = target.constructor;
      var P;
      if (S !== C && typeof S == "function" && (P = S.prototype) !== C.prototype && isObject(P) && setPrototypeOf) {
        setPrototypeOf(that, P);
      }
      return that;
    };
  });

  // node_modules/core-js/modules/es6.number.constructor.js
  var require_es6_number_constructor = __commonJS(() => {
    "use strict";
    var global2 = require_global();
    var has = require_has();
    var cof = require_cof();
    var inheritIfRequired = require_inherit_if_required();
    var toPrimitive = require_to_primitive();
    var fails = require_fails();
    var gOPN = require_object_gopn().f;
    var gOPD = require_object_gopd().f;
    var dP = require_object_dp().f;
    var $trim = require_string_trim().trim;
    var NUMBER = "Number";
    var $Number = global2[NUMBER];
    var Base = $Number;
    var proto = $Number.prototype;
    var BROKEN_COF = cof(require_object_create()(proto)) == NUMBER;
    var TRIM = "trim" in String.prototype;
    var toNumber = function(argument) {
      var it = toPrimitive(argument, false);
      if (typeof it == "string" && it.length > 2) {
        it = TRIM ? it.trim() : $trim(it, 3);
        var first = it.charCodeAt(0);
        var third, radix, maxCode;
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
          for (var digits = it.slice(2), i = 0, l = digits.length, code; i < l; i++) {
            code = digits.charCodeAt(i);
            if (code < 48 || code > maxCode)
              return NaN;
          }
          return parseInt(digits, radix);
        }
      }
      return +it;
    };
    if (!$Number(" 0o1") || !$Number("0b1") || $Number("+0x1")) {
      $Number = function Number2(value) {
        var it = arguments.length < 1 ? 0 : value;
        var that = this;
        return that instanceof $Number && (BROKEN_COF ? fails(function() {
          proto.valueOf.call(that);
        }) : cof(that) != NUMBER) ? inheritIfRequired(new Base(toNumber(it)), that, $Number) : toNumber(it);
      };
      for (keys = require_descriptors() ? gOPN(Base) : "MAX_VALUE,MIN_VALUE,NaN,NEGATIVE_INFINITY,POSITIVE_INFINITY,EPSILON,isFinite,isInteger,isNaN,isSafeInteger,MAX_SAFE_INTEGER,MIN_SAFE_INTEGER,parseFloat,parseInt,isInteger".split(","), j = 0; keys.length > j; j++) {
        if (has(Base, key = keys[j]) && !has($Number, key)) {
          dP($Number, key, gOPD(Base, key));
        }
      }
      $Number.prototype = proto;
      proto.constructor = $Number;
      require_redefine()(global2, NUMBER, $Number);
    }
    var keys;
    var j;
    var key;
  });

  // node_modules/core-js/modules/_a-number-value.js
  var require_a_number_value = __commonJS((exports2, module2) => {
    var cof = require_cof();
    module2.exports = function(it, msg) {
      if (typeof it != "number" && cof(it) != "Number")
        throw TypeError(msg);
      return +it;
    };
  });

  // node_modules/core-js/modules/_string-repeat.js
  var require_string_repeat = __commonJS((exports2, module2) => {
    "use strict";
    var toInteger = require_to_integer();
    var defined = require_defined();
    module2.exports = function repeat(count) {
      var str = String(defined(this));
      var res = "";
      var n = toInteger(count);
      if (n < 0 || n == Infinity)
        throw RangeError("Count can't be negative");
      for (; n > 0; (n >>>= 1) && (str += str))
        if (n & 1)
          res += str;
      return res;
    };
  });

  // node_modules/core-js/modules/es6.number.to-fixed.js
  var require_es6_number_to_fixed = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toInteger = require_to_integer();
    var aNumberValue = require_a_number_value();
    var repeat = require_string_repeat();
    var $toFixed = 1 .toFixed;
    var floor = Math.floor;
    var data = [0, 0, 0, 0, 0, 0];
    var ERROR = "Number.toFixed: incorrect invocation!";
    var ZERO = "0";
    var multiply = function(n, c) {
      var i = -1;
      var c2 = c;
      while (++i < 6) {
        c2 += n * data[i];
        data[i] = c2 % 1e7;
        c2 = floor(c2 / 1e7);
      }
    };
    var divide = function(n) {
      var i = 6;
      var c = 0;
      while (--i >= 0) {
        c += data[i];
        data[i] = floor(c / n);
        c = c % n * 1e7;
      }
    };
    var numToString = function() {
      var i = 6;
      var s = "";
      while (--i >= 0) {
        if (s !== "" || i === 0 || data[i] !== 0) {
          var t = String(data[i]);
          s = s === "" ? t : s + repeat.call(ZERO, 7 - t.length) + t;
        }
      }
      return s;
    };
    var pow = function(x, n, acc) {
      return n === 0 ? acc : n % 2 === 1 ? pow(x, n - 1, acc * x) : pow(x * x, n / 2, acc);
    };
    var log = function(x) {
      var n = 0;
      var x2 = x;
      while (x2 >= 4096) {
        n += 12;
        x2 /= 4096;
      }
      while (x2 >= 2) {
        n += 1;
        x2 /= 2;
      }
      return n;
    };
    $export($export.P + $export.F * (!!$toFixed && (8e-5 .toFixed(3) !== "0.000" || 0.9 .toFixed(0) !== "1" || 1.255 .toFixed(2) !== "1.25" || 1000000000000000100 .toFixed(0) !== "1000000000000000128") || !require_fails()(function() {
      $toFixed.call({});
    })), "Number", {
      toFixed: function toFixed(fractionDigits) {
        var x = aNumberValue(this, ERROR);
        var f = toInteger(fractionDigits);
        var s = "";
        var m = ZERO;
        var e, z, j, k;
        if (f < 0 || f > 20)
          throw RangeError(ERROR);
        if (x != x)
          return "NaN";
        if (x <= -1e21 || x >= 1e21)
          return String(x);
        if (x < 0) {
          s = "-";
          x = -x;
        }
        if (x > 1e-21) {
          e = log(x * pow(2, 69, 1)) - 69;
          z = e < 0 ? x * pow(2, -e, 1) : x / pow(2, e, 1);
          z *= 4503599627370496;
          e = 52 - e;
          if (e > 0) {
            multiply(0, z);
            j = f;
            while (j >= 7) {
              multiply(1e7, 0);
              j -= 7;
            }
            multiply(pow(10, j, 1), 0);
            j = e - 1;
            while (j >= 23) {
              divide(1 << 23);
              j -= 23;
            }
            divide(1 << j);
            multiply(1, 1);
            divide(2);
            m = numToString();
          } else {
            multiply(0, z);
            multiply(1 << -e, 0);
            m = numToString() + repeat.call(ZERO, f);
          }
        }
        if (f > 0) {
          k = m.length;
          m = s + (k <= f ? "0." + repeat.call(ZERO, f - k) + m : m.slice(0, k - f) + "." + m.slice(k - f));
        } else {
          m = s + m;
        }
        return m;
      }
    });
  });

  // node_modules/core-js/modules/es6.number.to-precision.js
  var require_es6_number_to_precision = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $fails = require_fails();
    var aNumberValue = require_a_number_value();
    var $toPrecision = 1 .toPrecision;
    $export($export.P + $export.F * ($fails(function() {
      return $toPrecision.call(1, void 0) !== "1";
    }) || !$fails(function() {
      $toPrecision.call({});
    })), "Number", {
      toPrecision: function toPrecision(precision) {
        var that = aNumberValue(this, "Number#toPrecision: incorrect invocation!");
        return precision === void 0 ? $toPrecision.call(that) : $toPrecision.call(that, precision);
      }
    });
  });

  // node_modules/core-js/modules/es6.number.epsilon.js
  var require_es6_number_epsilon = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Number", {EPSILON: Math.pow(2, -52)});
  });

  // node_modules/core-js/modules/es6.number.is-finite.js
  var require_es6_number_is_finite = __commonJS(() => {
    var $export = require_export();
    var _isFinite = require_global().isFinite;
    $export($export.S, "Number", {
      isFinite: function isFinite2(it) {
        return typeof it == "number" && _isFinite(it);
      }
    });
  });

  // node_modules/core-js/modules/_is-integer.js
  var require_is_integer = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var floor = Math.floor;
    module2.exports = function isInteger(it) {
      return !isObject(it) && isFinite(it) && floor(it) === it;
    };
  });

  // node_modules/core-js/modules/es6.number.is-integer.js
  var require_es6_number_is_integer = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Number", {isInteger: require_is_integer()});
  });

  // node_modules/core-js/modules/es6.number.is-nan.js
  var require_es6_number_is_nan = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Number", {
      isNaN: function isNaN2(number) {
        return number != number;
      }
    });
  });

  // node_modules/core-js/modules/es6.number.is-safe-integer.js
  var require_es6_number_is_safe_integer = __commonJS(() => {
    var $export = require_export();
    var isInteger = require_is_integer();
    var abs = Math.abs;
    $export($export.S, "Number", {
      isSafeInteger: function isSafeInteger(number) {
        return isInteger(number) && abs(number) <= 9007199254740991;
      }
    });
  });

  // node_modules/core-js/modules/es6.number.max-safe-integer.js
  var require_es6_number_max_safe_integer = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Number", {MAX_SAFE_INTEGER: 9007199254740991});
  });

  // node_modules/core-js/modules/es6.number.min-safe-integer.js
  var require_es6_number_min_safe_integer = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Number", {MIN_SAFE_INTEGER: -9007199254740991});
  });

  // node_modules/core-js/modules/es6.number.parse-float.js
  var require_es6_number_parse_float = __commonJS(() => {
    var $export = require_export();
    var $parseFloat = require_parse_float();
    $export($export.S + $export.F * (Number.parseFloat != $parseFloat), "Number", {parseFloat: $parseFloat});
  });

  // node_modules/core-js/modules/es6.number.parse-int.js
  var require_es6_number_parse_int = __commonJS(() => {
    var $export = require_export();
    var $parseInt = require_parse_int();
    $export($export.S + $export.F * (Number.parseInt != $parseInt), "Number", {parseInt: $parseInt});
  });

  // node_modules/core-js/modules/_math-log1p.js
  var require_math_log1p = __commonJS((exports2, module2) => {
    module2.exports = Math.log1p || function log1p(x) {
      return (x = +x) > -1e-8 && x < 1e-8 ? x - x * x / 2 : Math.log(1 + x);
    };
  });

  // node_modules/core-js/modules/es6.math.acosh.js
  var require_es6_math_acosh = __commonJS(() => {
    var $export = require_export();
    var log1p = require_math_log1p();
    var sqrt = Math.sqrt;
    var $acosh = Math.acosh;
    $export($export.S + $export.F * !($acosh && Math.floor($acosh(Number.MAX_VALUE)) == 710 && $acosh(Infinity) == Infinity), "Math", {
      acosh: function acosh(x) {
        return (x = +x) < 1 ? NaN : x > 9490626562425156e-8 ? Math.log(x) + Math.LN2 : log1p(x - 1 + sqrt(x - 1) * sqrt(x + 1));
      }
    });
  });

  // node_modules/core-js/modules/es6.math.asinh.js
  var require_es6_math_asinh = __commonJS(() => {
    var $export = require_export();
    var $asinh = Math.asinh;
    function asinh(x) {
      return !isFinite(x = +x) || x == 0 ? x : x < 0 ? -asinh(-x) : Math.log(x + Math.sqrt(x * x + 1));
    }
    $export($export.S + $export.F * !($asinh && 1 / $asinh(0) > 0), "Math", {asinh});
  });

  // node_modules/core-js/modules/es6.math.atanh.js
  var require_es6_math_atanh = __commonJS(() => {
    var $export = require_export();
    var $atanh = Math.atanh;
    $export($export.S + $export.F * !($atanh && 1 / $atanh(-0) < 0), "Math", {
      atanh: function atanh(x) {
        return (x = +x) == 0 ? x : Math.log((1 + x) / (1 - x)) / 2;
      }
    });
  });

  // node_modules/core-js/modules/_math-sign.js
  var require_math_sign = __commonJS((exports2, module2) => {
    module2.exports = Math.sign || function sign(x) {
      return (x = +x) == 0 || x != x ? x : x < 0 ? -1 : 1;
    };
  });

  // node_modules/core-js/modules/es6.math.cbrt.js
  var require_es6_math_cbrt = __commonJS(() => {
    var $export = require_export();
    var sign = require_math_sign();
    $export($export.S, "Math", {
      cbrt: function cbrt(x) {
        return sign(x = +x) * Math.pow(Math.abs(x), 1 / 3);
      }
    });
  });

  // node_modules/core-js/modules/es6.math.clz32.js
  var require_es6_math_clz32 = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      clz32: function clz32(x) {
        return (x >>>= 0) ? 31 - Math.floor(Math.log(x + 0.5) * Math.LOG2E) : 32;
      }
    });
  });

  // node_modules/core-js/modules/es6.math.cosh.js
  var require_es6_math_cosh = __commonJS(() => {
    var $export = require_export();
    var exp = Math.exp;
    $export($export.S, "Math", {
      cosh: function cosh(x) {
        return (exp(x = +x) + exp(-x)) / 2;
      }
    });
  });

  // node_modules/core-js/modules/_math-expm1.js
  var require_math_expm1 = __commonJS((exports2, module2) => {
    var $expm1 = Math.expm1;
    module2.exports = !$expm1 || $expm1(10) > 22025.465794806718 || $expm1(10) < 22025.465794806718 || $expm1(-2e-17) != -2e-17 ? function expm1(x) {
      return (x = +x) == 0 ? x : x > -1e-6 && x < 1e-6 ? x + x * x / 2 : Math.exp(x) - 1;
    } : $expm1;
  });

  // node_modules/core-js/modules/es6.math.expm1.js
  var require_es6_math_expm1 = __commonJS(() => {
    var $export = require_export();
    var $expm1 = require_math_expm1();
    $export($export.S + $export.F * ($expm1 != Math.expm1), "Math", {expm1: $expm1});
  });

  // node_modules/core-js/modules/_math-fround.js
  var require_math_fround = __commonJS((exports2, module2) => {
    var sign = require_math_sign();
    var pow = Math.pow;
    var EPSILON = pow(2, -52);
    var EPSILON32 = pow(2, -23);
    var MAX32 = pow(2, 127) * (2 - EPSILON32);
    var MIN32 = pow(2, -126);
    var roundTiesToEven = function(n) {
      return n + 1 / EPSILON - 1 / EPSILON;
    };
    module2.exports = Math.fround || function fround(x) {
      var $abs = Math.abs(x);
      var $sign = sign(x);
      var a, result;
      if ($abs < MIN32)
        return $sign * roundTiesToEven($abs / MIN32 / EPSILON32) * MIN32 * EPSILON32;
      a = (1 + EPSILON32 / EPSILON) * $abs;
      result = a - (a - $abs);
      if (result > MAX32 || result != result)
        return $sign * Infinity;
      return $sign * result;
    };
  });

  // node_modules/core-js/modules/es6.math.fround.js
  var require_es6_math_fround = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {fround: require_math_fround()});
  });

  // node_modules/core-js/modules/es6.math.hypot.js
  var require_es6_math_hypot = __commonJS(() => {
    var $export = require_export();
    var abs = Math.abs;
    $export($export.S, "Math", {
      hypot: function hypot(value1, value2) {
        var sum = 0;
        var i = 0;
        var aLen = arguments.length;
        var larg = 0;
        var arg, div;
        while (i < aLen) {
          arg = abs(arguments[i++]);
          if (larg < arg) {
            div = larg / arg;
            sum = sum * div * div + 1;
            larg = arg;
          } else if (arg > 0) {
            div = arg / larg;
            sum += div * div;
          } else
            sum += arg;
        }
        return larg === Infinity ? Infinity : larg * Math.sqrt(sum);
      }
    });
  });

  // node_modules/core-js/modules/es6.math.imul.js
  var require_es6_math_imul = __commonJS(() => {
    var $export = require_export();
    var $imul = Math.imul;
    $export($export.S + $export.F * require_fails()(function() {
      return $imul(4294967295, 5) != -5 || $imul.length != 2;
    }), "Math", {
      imul: function imul(x, y) {
        var UINT16 = 65535;
        var xn = +x;
        var yn = +y;
        var xl = UINT16 & xn;
        var yl = UINT16 & yn;
        return 0 | xl * yl + ((UINT16 & xn >>> 16) * yl + xl * (UINT16 & yn >>> 16) << 16 >>> 0);
      }
    });
  });

  // node_modules/core-js/modules/es6.math.log10.js
  var require_es6_math_log10 = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      log10: function log10(x) {
        return Math.log(x) * Math.LOG10E;
      }
    });
  });

  // node_modules/core-js/modules/es6.math.log1p.js
  var require_es6_math_log1p = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {log1p: require_math_log1p()});
  });

  // node_modules/core-js/modules/es6.math.log2.js
  var require_es6_math_log2 = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      log2: function log2(x) {
        return Math.log(x) / Math.LN2;
      }
    });
  });

  // node_modules/core-js/modules/es6.math.sign.js
  var require_es6_math_sign = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {sign: require_math_sign()});
  });

  // node_modules/core-js/modules/es6.math.sinh.js
  var require_es6_math_sinh = __commonJS(() => {
    var $export = require_export();
    var expm1 = require_math_expm1();
    var exp = Math.exp;
    $export($export.S + $export.F * require_fails()(function() {
      return !Math.sinh(-2e-17) != -2e-17;
    }), "Math", {
      sinh: function sinh(x) {
        return Math.abs(x = +x) < 1 ? (expm1(x) - expm1(-x)) / 2 : (exp(x - 1) - exp(-x - 1)) * (Math.E / 2);
      }
    });
  });

  // node_modules/core-js/modules/es6.math.tanh.js
  var require_es6_math_tanh = __commonJS(() => {
    var $export = require_export();
    var expm1 = require_math_expm1();
    var exp = Math.exp;
    $export($export.S, "Math", {
      tanh: function tanh(x) {
        var a = expm1(x = +x);
        var b = expm1(-x);
        return a == Infinity ? 1 : b == Infinity ? -1 : (a - b) / (exp(x) + exp(-x));
      }
    });
  });

  // node_modules/core-js/modules/es6.math.trunc.js
  var require_es6_math_trunc = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      trunc: function trunc(it) {
        return (it > 0 ? Math.floor : Math.ceil)(it);
      }
    });
  });

  // node_modules/core-js/modules/es6.string.from-code-point.js
  var require_es6_string_from_code_point = __commonJS(() => {
    var $export = require_export();
    var toAbsoluteIndex = require_to_absolute_index();
    var fromCharCode = String.fromCharCode;
    var $fromCodePoint = String.fromCodePoint;
    $export($export.S + $export.F * (!!$fromCodePoint && $fromCodePoint.length != 1), "String", {
      fromCodePoint: function fromCodePoint(x) {
        var res = [];
        var aLen = arguments.length;
        var i = 0;
        var code;
        while (aLen > i) {
          code = +arguments[i++];
          if (toAbsoluteIndex(code, 1114111) !== code)
            throw RangeError(code + " is not a valid code point");
          res.push(code < 65536 ? fromCharCode(code) : fromCharCode(((code -= 65536) >> 10) + 55296, code % 1024 + 56320));
        }
        return res.join("");
      }
    });
  });

  // node_modules/core-js/modules/es6.string.raw.js
  var require_es6_string_raw = __commonJS(() => {
    var $export = require_export();
    var toIObject = require_to_iobject();
    var toLength = require_to_length();
    $export($export.S, "String", {
      raw: function raw(callSite) {
        var tpl = toIObject(callSite.raw);
        var len = toLength(tpl.length);
        var aLen = arguments.length;
        var res = [];
        var i = 0;
        while (len > i) {
          res.push(String(tpl[i++]));
          if (i < aLen)
            res.push(String(arguments[i]));
        }
        return res.join("");
      }
    });
  });

  // node_modules/core-js/modules/es6.string.trim.js
  var require_es6_string_trim = __commonJS(() => {
    "use strict";
    require_string_trim()("trim", function($trim) {
      return function trim() {
        return $trim(this, 3);
      };
    });
  });

  // node_modules/core-js/modules/_string-at.js
  var require_string_at = __commonJS((exports2, module2) => {
    var toInteger = require_to_integer();
    var defined = require_defined();
    module2.exports = function(TO_STRING) {
      return function(that, pos) {
        var s = String(defined(that));
        var i = toInteger(pos);
        var l = s.length;
        var a, b;
        if (i < 0 || i >= l)
          return TO_STRING ? "" : void 0;
        a = s.charCodeAt(i);
        return a < 55296 || a > 56319 || i + 1 === l || (b = s.charCodeAt(i + 1)) < 56320 || b > 57343 ? TO_STRING ? s.charAt(i) : a : TO_STRING ? s.slice(i, i + 2) : (a - 55296 << 10) + (b - 56320) + 65536;
      };
    };
  });

  // node_modules/core-js/modules/_iterators.js
  var require_iterators = __commonJS((exports2, module2) => {
    module2.exports = {};
  });

  // node_modules/core-js/modules/_iter-create.js
  var require_iter_create = __commonJS((exports2, module2) => {
    "use strict";
    var create = require_object_create();
    var descriptor = require_property_desc();
    var setToStringTag = require_set_to_string_tag();
    var IteratorPrototype = {};
    require_hide()(IteratorPrototype, require_wks()("iterator"), function() {
      return this;
    });
    module2.exports = function(Constructor, NAME, next) {
      Constructor.prototype = create(IteratorPrototype, {next: descriptor(1, next)});
      setToStringTag(Constructor, NAME + " Iterator");
    };
  });

  // node_modules/core-js/modules/_iter-define.js
  var require_iter_define = __commonJS((exports2, module2) => {
    "use strict";
    var LIBRARY = require_library();
    var $export = require_export();
    var redefine = require_redefine();
    var hide = require_hide();
    var Iterators = require_iterators();
    var $iterCreate = require_iter_create();
    var setToStringTag = require_set_to_string_tag();
    var getPrototypeOf = require_object_gpo();
    var ITERATOR = require_wks()("iterator");
    var BUGGY = !([].keys && "next" in [].keys());
    var FF_ITERATOR = "@@iterator";
    var KEYS = "keys";
    var VALUES = "values";
    var returnThis = function() {
      return this;
    };
    module2.exports = function(Base, NAME, Constructor, next, DEFAULT, IS_SET, FORCED) {
      $iterCreate(Constructor, NAME, next);
      var getMethod = function(kind) {
        if (!BUGGY && kind in proto)
          return proto[kind];
        switch (kind) {
          case KEYS:
            return function keys() {
              return new Constructor(this, kind);
            };
          case VALUES:
            return function values() {
              return new Constructor(this, kind);
            };
        }
        return function entries() {
          return new Constructor(this, kind);
        };
      };
      var TAG = NAME + " Iterator";
      var DEF_VALUES = DEFAULT == VALUES;
      var VALUES_BUG = false;
      var proto = Base.prototype;
      var $native = proto[ITERATOR] || proto[FF_ITERATOR] || DEFAULT && proto[DEFAULT];
      var $default = $native || getMethod(DEFAULT);
      var $entries = DEFAULT ? !DEF_VALUES ? $default : getMethod("entries") : void 0;
      var $anyNative = NAME == "Array" ? proto.entries || $native : $native;
      var methods, key, IteratorPrototype;
      if ($anyNative) {
        IteratorPrototype = getPrototypeOf($anyNative.call(new Base()));
        if (IteratorPrototype !== Object.prototype && IteratorPrototype.next) {
          setToStringTag(IteratorPrototype, TAG, true);
          if (!LIBRARY && typeof IteratorPrototype[ITERATOR] != "function")
            hide(IteratorPrototype, ITERATOR, returnThis);
        }
      }
      if (DEF_VALUES && $native && $native.name !== VALUES) {
        VALUES_BUG = true;
        $default = function values() {
          return $native.call(this);
        };
      }
      if ((!LIBRARY || FORCED) && (BUGGY || VALUES_BUG || !proto[ITERATOR])) {
        hide(proto, ITERATOR, $default);
      }
      Iterators[NAME] = $default;
      Iterators[TAG] = returnThis;
      if (DEFAULT) {
        methods = {
          values: DEF_VALUES ? $default : getMethod(VALUES),
          keys: IS_SET ? $default : getMethod(KEYS),
          entries: $entries
        };
        if (FORCED)
          for (key in methods) {
            if (!(key in proto))
              redefine(proto, key, methods[key]);
          }
        else
          $export($export.P + $export.F * (BUGGY || VALUES_BUG), NAME, methods);
      }
      return methods;
    };
  });

  // node_modules/core-js/modules/es6.string.iterator.js
  var require_es6_string_iterator = __commonJS(() => {
    "use strict";
    var $at = require_string_at()(true);
    require_iter_define()(String, "String", function(iterated) {
      this._t = String(iterated);
      this._i = 0;
    }, function() {
      var O = this._t;
      var index = this._i;
      var point;
      if (index >= O.length)
        return {value: void 0, done: true};
      point = $at(O, index);
      this._i += point.length;
      return {value: point, done: false};
    });
  });

  // node_modules/core-js/modules/es6.string.code-point-at.js
  var require_es6_string_code_point_at = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $at = require_string_at()(false);
    $export($export.P, "String", {
      codePointAt: function codePointAt(pos) {
        return $at(this, pos);
      }
    });
  });

  // node_modules/core-js/modules/_is-regexp.js
  var require_is_regexp = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var cof = require_cof();
    var MATCH = require_wks()("match");
    module2.exports = function(it) {
      var isRegExp;
      return isObject(it) && ((isRegExp = it[MATCH]) !== void 0 ? !!isRegExp : cof(it) == "RegExp");
    };
  });

  // node_modules/core-js/modules/_string-context.js
  var require_string_context = __commonJS((exports2, module2) => {
    var isRegExp = require_is_regexp();
    var defined = require_defined();
    module2.exports = function(that, searchString, NAME) {
      if (isRegExp(searchString))
        throw TypeError("String#" + NAME + " doesn't accept regex!");
      return String(defined(that));
    };
  });

  // node_modules/core-js/modules/_fails-is-regexp.js
  var require_fails_is_regexp = __commonJS((exports2, module2) => {
    var MATCH = require_wks()("match");
    module2.exports = function(KEY) {
      var re = /./;
      try {
        "/./"[KEY](re);
      } catch (e) {
        try {
          re[MATCH] = false;
          return !"/./"[KEY](re);
        } catch (f) {
        }
      }
      return true;
    };
  });

  // node_modules/core-js/modules/es6.string.ends-with.js
  var require_es6_string_ends_with = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toLength = require_to_length();
    var context = require_string_context();
    var ENDS_WITH = "endsWith";
    var $endsWith = ""[ENDS_WITH];
    $export($export.P + $export.F * require_fails_is_regexp()(ENDS_WITH), "String", {
      endsWith: function endsWith(searchString) {
        var that = context(this, searchString, ENDS_WITH);
        var endPosition = arguments.length > 1 ? arguments[1] : void 0;
        var len = toLength(that.length);
        var end = endPosition === void 0 ? len : Math.min(toLength(endPosition), len);
        var search = String(searchString);
        return $endsWith ? $endsWith.call(that, search, end) : that.slice(end - search.length, end) === search;
      }
    });
  });

  // node_modules/core-js/modules/es6.string.includes.js
  var require_es6_string_includes = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var context = require_string_context();
    var INCLUDES = "includes";
    $export($export.P + $export.F * require_fails_is_regexp()(INCLUDES), "String", {
      includes: function includes(searchString) {
        return !!~context(this, searchString, INCLUDES).indexOf(searchString, arguments.length > 1 ? arguments[1] : void 0);
      }
    });
  });

  // node_modules/core-js/modules/es6.string.repeat.js
  var require_es6_string_repeat = __commonJS(() => {
    var $export = require_export();
    $export($export.P, "String", {
      repeat: require_string_repeat()
    });
  });

  // node_modules/core-js/modules/es6.string.starts-with.js
  var require_es6_string_starts_with = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toLength = require_to_length();
    var context = require_string_context();
    var STARTS_WITH = "startsWith";
    var $startsWith = ""[STARTS_WITH];
    $export($export.P + $export.F * require_fails_is_regexp()(STARTS_WITH), "String", {
      startsWith: function startsWith(searchString) {
        var that = context(this, searchString, STARTS_WITH);
        var index = toLength(Math.min(arguments.length > 1 ? arguments[1] : void 0, that.length));
        var search = String(searchString);
        return $startsWith ? $startsWith.call(that, search, index) : that.slice(index, index + search.length) === search;
      }
    });
  });

  // node_modules/core-js/modules/_string-html.js
  var require_string_html = __commonJS((exports2, module2) => {
    var $export = require_export();
    var fails = require_fails();
    var defined = require_defined();
    var quot = /"/g;
    var createHTML = function(string, tag, attribute, value) {
      var S = String(defined(string));
      var p1 = "<" + tag;
      if (attribute !== "")
        p1 += " " + attribute + '="' + String(value).replace(quot, "&quot;") + '"';
      return p1 + ">" + S + "</" + tag + ">";
    };
    module2.exports = function(NAME, exec) {
      var O = {};
      O[NAME] = exec(createHTML);
      $export($export.P + $export.F * fails(function() {
        var test = ""[NAME]('"');
        return test !== test.toLowerCase() || test.split('"').length > 3;
      }), "String", O);
    };
  });

  // node_modules/core-js/modules/es6.string.anchor.js
  var require_es6_string_anchor = __commonJS(() => {
    "use strict";
    require_string_html()("anchor", function(createHTML) {
      return function anchor(name) {
        return createHTML(this, "a", "name", name);
      };
    });
  });

  // node_modules/core-js/modules/es6.string.big.js
  var require_es6_string_big = __commonJS(() => {
    "use strict";
    require_string_html()("big", function(createHTML) {
      return function big() {
        return createHTML(this, "big", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.blink.js
  var require_es6_string_blink = __commonJS(() => {
    "use strict";
    require_string_html()("blink", function(createHTML) {
      return function blink() {
        return createHTML(this, "blink", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.bold.js
  var require_es6_string_bold = __commonJS(() => {
    "use strict";
    require_string_html()("bold", function(createHTML) {
      return function bold() {
        return createHTML(this, "b", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.fixed.js
  var require_es6_string_fixed = __commonJS(() => {
    "use strict";
    require_string_html()("fixed", function(createHTML) {
      return function fixed() {
        return createHTML(this, "tt", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.fontcolor.js
  var require_es6_string_fontcolor = __commonJS(() => {
    "use strict";
    require_string_html()("fontcolor", function(createHTML) {
      return function fontcolor(color) {
        return createHTML(this, "font", "color", color);
      };
    });
  });

  // node_modules/core-js/modules/es6.string.fontsize.js
  var require_es6_string_fontsize = __commonJS(() => {
    "use strict";
    require_string_html()("fontsize", function(createHTML) {
      return function fontsize(size) {
        return createHTML(this, "font", "size", size);
      };
    });
  });

  // node_modules/core-js/modules/es6.string.italics.js
  var require_es6_string_italics = __commonJS(() => {
    "use strict";
    require_string_html()("italics", function(createHTML) {
      return function italics() {
        return createHTML(this, "i", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.link.js
  var require_es6_string_link = __commonJS(() => {
    "use strict";
    require_string_html()("link", function(createHTML) {
      return function link(url) {
        return createHTML(this, "a", "href", url);
      };
    });
  });

  // node_modules/core-js/modules/es6.string.small.js
  var require_es6_string_small = __commonJS(() => {
    "use strict";
    require_string_html()("small", function(createHTML) {
      return function small() {
        return createHTML(this, "small", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.strike.js
  var require_es6_string_strike = __commonJS(() => {
    "use strict";
    require_string_html()("strike", function(createHTML) {
      return function strike() {
        return createHTML(this, "strike", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.sub.js
  var require_es6_string_sub = __commonJS(() => {
    "use strict";
    require_string_html()("sub", function(createHTML) {
      return function sub() {
        return createHTML(this, "sub", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.string.sup.js
  var require_es6_string_sup = __commonJS(() => {
    "use strict";
    require_string_html()("sup", function(createHTML) {
      return function sup() {
        return createHTML(this, "sup", "", "");
      };
    });
  });

  // node_modules/core-js/modules/es6.date.now.js
  var require_es6_date_now = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Date", {now: function() {
      return new Date().getTime();
    }});
  });

  // node_modules/core-js/modules/es6.date.to-json.js
  var require_es6_date_to_json = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toObject = require_to_object();
    var toPrimitive = require_to_primitive();
    $export($export.P + $export.F * require_fails()(function() {
      return new Date(NaN).toJSON() !== null || Date.prototype.toJSON.call({toISOString: function() {
        return 1;
      }}) !== 1;
    }), "Date", {
      toJSON: function toJSON(key) {
        var O = toObject(this);
        var pv = toPrimitive(O);
        return typeof pv == "number" && !isFinite(pv) ? null : O.toISOString();
      }
    });
  });

  // node_modules/core-js/modules/_date-to-iso-string.js
  var require_date_to_iso_string = __commonJS((exports2, module2) => {
    "use strict";
    var fails = require_fails();
    var getTime = Date.prototype.getTime;
    var $toISOString = Date.prototype.toISOString;
    var lz = function(num) {
      return num > 9 ? num : "0" + num;
    };
    module2.exports = fails(function() {
      return $toISOString.call(new Date(-5e13 - 1)) != "0385-07-25T07:06:39.999Z";
    }) || !fails(function() {
      $toISOString.call(new Date(NaN));
    }) ? function toISOString() {
      if (!isFinite(getTime.call(this)))
        throw RangeError("Invalid time value");
      var d = this;
      var y = d.getUTCFullYear();
      var m = d.getUTCMilliseconds();
      var s = y < 0 ? "-" : y > 9999 ? "+" : "";
      return s + ("00000" + Math.abs(y)).slice(s ? -6 : -4) + "-" + lz(d.getUTCMonth() + 1) + "-" + lz(d.getUTCDate()) + "T" + lz(d.getUTCHours()) + ":" + lz(d.getUTCMinutes()) + ":" + lz(d.getUTCSeconds()) + "." + (m > 99 ? m : "0" + lz(m)) + "Z";
    } : $toISOString;
  });

  // node_modules/core-js/modules/es6.date.to-iso-string.js
  var require_es6_date_to_iso_string = __commonJS(() => {
    var $export = require_export();
    var toISOString = require_date_to_iso_string();
    $export($export.P + $export.F * (Date.prototype.toISOString !== toISOString), "Date", {
      toISOString
    });
  });

  // node_modules/core-js/modules/es6.date.to-string.js
  var require_es6_date_to_string = __commonJS(() => {
    var DateProto = Date.prototype;
    var INVALID_DATE = "Invalid Date";
    var TO_STRING = "toString";
    var $toString = DateProto[TO_STRING];
    var getTime = DateProto.getTime;
    if (new Date(NaN) + "" != INVALID_DATE) {
      require_redefine()(DateProto, TO_STRING, function toString() {
        var value = getTime.call(this);
        return value === value ? $toString.call(this) : INVALID_DATE;
      });
    }
  });

  // node_modules/core-js/modules/_date-to-primitive.js
  var require_date_to_primitive = __commonJS((exports2, module2) => {
    "use strict";
    var anObject = require_an_object();
    var toPrimitive = require_to_primitive();
    var NUMBER = "number";
    module2.exports = function(hint) {
      if (hint !== "string" && hint !== NUMBER && hint !== "default")
        throw TypeError("Incorrect hint");
      return toPrimitive(anObject(this), hint != NUMBER);
    };
  });

  // node_modules/core-js/modules/es6.date.to-primitive.js
  var require_es6_date_to_primitive = __commonJS(() => {
    var TO_PRIMITIVE = require_wks()("toPrimitive");
    var proto = Date.prototype;
    if (!(TO_PRIMITIVE in proto))
      require_hide()(proto, TO_PRIMITIVE, require_date_to_primitive());
  });

  // node_modules/core-js/modules/es6.array.is-array.js
  var require_es6_array_is_array = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Array", {isArray: require_is_array()});
  });

  // node_modules/core-js/modules/_iter-call.js
  var require_iter_call = __commonJS((exports2, module2) => {
    var anObject = require_an_object();
    module2.exports = function(iterator, fn, value, entries) {
      try {
        return entries ? fn(anObject(value)[0], value[1]) : fn(value);
      } catch (e) {
        var ret = iterator["return"];
        if (ret !== void 0)
          anObject(ret.call(iterator));
        throw e;
      }
    };
  });

  // node_modules/core-js/modules/_is-array-iter.js
  var require_is_array_iter = __commonJS((exports2, module2) => {
    var Iterators = require_iterators();
    var ITERATOR = require_wks()("iterator");
    var ArrayProto = Array.prototype;
    module2.exports = function(it) {
      return it !== void 0 && (Iterators.Array === it || ArrayProto[ITERATOR] === it);
    };
  });

  // node_modules/core-js/modules/_create-property.js
  var require_create_property = __commonJS((exports2, module2) => {
    "use strict";
    var $defineProperty = require_object_dp();
    var createDesc = require_property_desc();
    module2.exports = function(object, index, value) {
      if (index in object)
        $defineProperty.f(object, index, createDesc(0, value));
      else
        object[index] = value;
    };
  });

  // node_modules/core-js/modules/core.get-iterator-method.js
  var require_core_get_iterator_method = __commonJS((exports2, module2) => {
    var classof = require_classof();
    var ITERATOR = require_wks()("iterator");
    var Iterators = require_iterators();
    module2.exports = require_core().getIteratorMethod = function(it) {
      if (it != void 0)
        return it[ITERATOR] || it["@@iterator"] || Iterators[classof(it)];
    };
  });

  // node_modules/core-js/modules/_iter-detect.js
  var require_iter_detect = __commonJS((exports2, module2) => {
    var ITERATOR = require_wks()("iterator");
    var SAFE_CLOSING = false;
    try {
      riter = [7][ITERATOR]();
      riter["return"] = function() {
        SAFE_CLOSING = true;
      };
      Array.from(riter, function() {
        throw 2;
      });
    } catch (e) {
    }
    var riter;
    module2.exports = function(exec, skipClosing) {
      if (!skipClosing && !SAFE_CLOSING)
        return false;
      var safe = false;
      try {
        var arr = [7];
        var iter = arr[ITERATOR]();
        iter.next = function() {
          return {done: safe = true};
        };
        arr[ITERATOR] = function() {
          return iter;
        };
        exec(arr);
      } catch (e) {
      }
      return safe;
    };
  });

  // node_modules/core-js/modules/es6.array.from.js
  var require_es6_array_from = __commonJS(() => {
    "use strict";
    var ctx = require_ctx();
    var $export = require_export();
    var toObject = require_to_object();
    var call = require_iter_call();
    var isArrayIter = require_is_array_iter();
    var toLength = require_to_length();
    var createProperty = require_create_property();
    var getIterFn = require_core_get_iterator_method();
    $export($export.S + $export.F * !require_iter_detect()(function(iter) {
      Array.from(iter);
    }), "Array", {
      from: function from(arrayLike) {
        var O = toObject(arrayLike);
        var C = typeof this == "function" ? this : Array;
        var aLen = arguments.length;
        var mapfn = aLen > 1 ? arguments[1] : void 0;
        var mapping = mapfn !== void 0;
        var index = 0;
        var iterFn = getIterFn(O);
        var length, result, step, iterator;
        if (mapping)
          mapfn = ctx(mapfn, aLen > 2 ? arguments[2] : void 0, 2);
        if (iterFn != void 0 && !(C == Array && isArrayIter(iterFn))) {
          for (iterator = iterFn.call(O), result = new C(); !(step = iterator.next()).done; index++) {
            createProperty(result, index, mapping ? call(iterator, mapfn, [step.value, index], true) : step.value);
          }
        } else {
          length = toLength(O.length);
          for (result = new C(length); length > index; index++) {
            createProperty(result, index, mapping ? mapfn(O[index], index) : O[index]);
          }
        }
        result.length = index;
        return result;
      }
    });
  });

  // node_modules/core-js/modules/es6.array.of.js
  var require_es6_array_of = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var createProperty = require_create_property();
    $export($export.S + $export.F * require_fails()(function() {
      function F() {
      }
      return !(Array.of.call(F) instanceof F);
    }), "Array", {
      of: function of() {
        var index = 0;
        var aLen = arguments.length;
        var result = new (typeof this == "function" ? this : Array)(aLen);
        while (aLen > index)
          createProperty(result, index, arguments[index++]);
        result.length = aLen;
        return result;
      }
    });
  });

  // node_modules/core-js/modules/_strict-method.js
  var require_strict_method = __commonJS((exports2, module2) => {
    "use strict";
    var fails = require_fails();
    module2.exports = function(method, arg) {
      return !!method && fails(function() {
        arg ? method.call(null, function() {
        }, 1) : method.call(null);
      });
    };
  });

  // node_modules/core-js/modules/es6.array.join.js
  var require_es6_array_join = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toIObject = require_to_iobject();
    var arrayJoin = [].join;
    $export($export.P + $export.F * (require_iobject() != Object || !require_strict_method()(arrayJoin)), "Array", {
      join: function join(separator) {
        return arrayJoin.call(toIObject(this), separator === void 0 ? "," : separator);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.slice.js
  var require_es6_array_slice = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var html = require_html();
    var cof = require_cof();
    var toAbsoluteIndex = require_to_absolute_index();
    var toLength = require_to_length();
    var arraySlice = [].slice;
    $export($export.P + $export.F * require_fails()(function() {
      if (html)
        arraySlice.call(html);
    }), "Array", {
      slice: function slice(begin, end) {
        var len = toLength(this.length);
        var klass = cof(this);
        end = end === void 0 ? len : end;
        if (klass == "Array")
          return arraySlice.call(this, begin, end);
        var start = toAbsoluteIndex(begin, len);
        var upTo = toAbsoluteIndex(end, len);
        var size = toLength(upTo - start);
        var cloned = new Array(size);
        var i = 0;
        for (; i < size; i++)
          cloned[i] = klass == "String" ? this.charAt(start + i) : this[start + i];
        return cloned;
      }
    });
  });

  // node_modules/core-js/modules/es6.array.sort.js
  var require_es6_array_sort = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var aFunction = require_a_function();
    var toObject = require_to_object();
    var fails = require_fails();
    var $sort = [].sort;
    var test = [1, 2, 3];
    $export($export.P + $export.F * (fails(function() {
      test.sort(void 0);
    }) || !fails(function() {
      test.sort(null);
    }) || !require_strict_method()($sort)), "Array", {
      sort: function sort(comparefn) {
        return comparefn === void 0 ? $sort.call(toObject(this)) : $sort.call(toObject(this), aFunction(comparefn));
      }
    });
  });

  // node_modules/core-js/modules/_array-species-constructor.js
  var require_array_species_constructor = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    var isArray = require_is_array();
    var SPECIES = require_wks()("species");
    module2.exports = function(original) {
      var C;
      if (isArray(original)) {
        C = original.constructor;
        if (typeof C == "function" && (C === Array || isArray(C.prototype)))
          C = void 0;
        if (isObject(C)) {
          C = C[SPECIES];
          if (C === null)
            C = void 0;
        }
      }
      return C === void 0 ? Array : C;
    };
  });

  // node_modules/core-js/modules/_array-species-create.js
  var require_array_species_create = __commonJS((exports2, module2) => {
    var speciesConstructor = require_array_species_constructor();
    module2.exports = function(original, length) {
      return new (speciesConstructor(original))(length);
    };
  });

  // node_modules/core-js/modules/_array-methods.js
  var require_array_methods = __commonJS((exports2, module2) => {
    var ctx = require_ctx();
    var IObject = require_iobject();
    var toObject = require_to_object();
    var toLength = require_to_length();
    var asc = require_array_species_create();
    module2.exports = function(TYPE, $create) {
      var IS_MAP = TYPE == 1;
      var IS_FILTER = TYPE == 2;
      var IS_SOME = TYPE == 3;
      var IS_EVERY = TYPE == 4;
      var IS_FIND_INDEX = TYPE == 6;
      var NO_HOLES = TYPE == 5 || IS_FIND_INDEX;
      var create = $create || asc;
      return function($this, callbackfn, that) {
        var O = toObject($this);
        var self2 = IObject(O);
        var f = ctx(callbackfn, that, 3);
        var length = toLength(self2.length);
        var index = 0;
        var result = IS_MAP ? create($this, length) : IS_FILTER ? create($this, 0) : void 0;
        var val, res;
        for (; length > index; index++)
          if (NO_HOLES || index in self2) {
            val = self2[index];
            res = f(val, index, O);
            if (TYPE) {
              if (IS_MAP)
                result[index] = res;
              else if (res)
                switch (TYPE) {
                  case 3:
                    return true;
                  case 5:
                    return val;
                  case 6:
                    return index;
                  case 2:
                    result.push(val);
                }
              else if (IS_EVERY)
                return false;
            }
          }
        return IS_FIND_INDEX ? -1 : IS_SOME || IS_EVERY ? IS_EVERY : result;
      };
    };
  });

  // node_modules/core-js/modules/es6.array.for-each.js
  var require_es6_array_for_each = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $forEach = require_array_methods()(0);
    var STRICT = require_strict_method()([].forEach, true);
    $export($export.P + $export.F * !STRICT, "Array", {
      forEach: function forEach(callbackfn) {
        return $forEach(this, callbackfn, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.map.js
  var require_es6_array_map = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $map = require_array_methods()(1);
    $export($export.P + $export.F * !require_strict_method()([].map, true), "Array", {
      map: function map(callbackfn) {
        return $map(this, callbackfn, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.filter.js
  var require_es6_array_filter = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $filter = require_array_methods()(2);
    $export($export.P + $export.F * !require_strict_method()([].filter, true), "Array", {
      filter: function filter(callbackfn) {
        return $filter(this, callbackfn, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.some.js
  var require_es6_array_some = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $some = require_array_methods()(3);
    $export($export.P + $export.F * !require_strict_method()([].some, true), "Array", {
      some: function some(callbackfn) {
        return $some(this, callbackfn, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.every.js
  var require_es6_array_every = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $every = require_array_methods()(4);
    $export($export.P + $export.F * !require_strict_method()([].every, true), "Array", {
      every: function every(callbackfn) {
        return $every(this, callbackfn, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/_array-reduce.js
  var require_array_reduce = __commonJS((exports2, module2) => {
    var aFunction = require_a_function();
    var toObject = require_to_object();
    var IObject = require_iobject();
    var toLength = require_to_length();
    module2.exports = function(that, callbackfn, aLen, memo, isRight) {
      aFunction(callbackfn);
      var O = toObject(that);
      var self2 = IObject(O);
      var length = toLength(O.length);
      var index = isRight ? length - 1 : 0;
      var i = isRight ? -1 : 1;
      if (aLen < 2)
        for (; ; ) {
          if (index in self2) {
            memo = self2[index];
            index += i;
            break;
          }
          index += i;
          if (isRight ? index < 0 : length <= index) {
            throw TypeError("Reduce of empty array with no initial value");
          }
        }
      for (; isRight ? index >= 0 : length > index; index += i)
        if (index in self2) {
          memo = callbackfn(memo, self2[index], index, O);
        }
      return memo;
    };
  });

  // node_modules/core-js/modules/es6.array.reduce.js
  var require_es6_array_reduce = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $reduce = require_array_reduce();
    $export($export.P + $export.F * !require_strict_method()([].reduce, true), "Array", {
      reduce: function reduce(callbackfn) {
        return $reduce(this, callbackfn, arguments.length, arguments[1], false);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.reduce-right.js
  var require_es6_array_reduce_right = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $reduce = require_array_reduce();
    $export($export.P + $export.F * !require_strict_method()([].reduceRight, true), "Array", {
      reduceRight: function reduceRight(callbackfn) {
        return $reduce(this, callbackfn, arguments.length, arguments[1], true);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.index-of.js
  var require_es6_array_index_of = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $indexOf = require_array_includes()(false);
    var $native = [].indexOf;
    var NEGATIVE_ZERO = !!$native && 1 / [1].indexOf(1, -0) < 0;
    $export($export.P + $export.F * (NEGATIVE_ZERO || !require_strict_method()($native)), "Array", {
      indexOf: function indexOf(searchElement) {
        return NEGATIVE_ZERO ? $native.apply(this, arguments) || 0 : $indexOf(this, searchElement, arguments[1]);
      }
    });
  });

  // node_modules/core-js/modules/es6.array.last-index-of.js
  var require_es6_array_last_index_of = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toIObject = require_to_iobject();
    var toInteger = require_to_integer();
    var toLength = require_to_length();
    var $native = [].lastIndexOf;
    var NEGATIVE_ZERO = !!$native && 1 / [1].lastIndexOf(1, -0) < 0;
    $export($export.P + $export.F * (NEGATIVE_ZERO || !require_strict_method()($native)), "Array", {
      lastIndexOf: function lastIndexOf(searchElement) {
        if (NEGATIVE_ZERO)
          return $native.apply(this, arguments) || 0;
        var O = toIObject(this);
        var length = toLength(O.length);
        var index = length - 1;
        if (arguments.length > 1)
          index = Math.min(index, toInteger(arguments[1]));
        if (index < 0)
          index = length + index;
        for (; index >= 0; index--)
          if (index in O) {
            if (O[index] === searchElement)
              return index || 0;
          }
        return -1;
      }
    });
  });

  // node_modules/core-js/modules/_array-copy-within.js
  var require_array_copy_within = __commonJS((exports2, module2) => {
    "use strict";
    var toObject = require_to_object();
    var toAbsoluteIndex = require_to_absolute_index();
    var toLength = require_to_length();
    module2.exports = [].copyWithin || function copyWithin(target, start) {
      var O = toObject(this);
      var len = toLength(O.length);
      var to = toAbsoluteIndex(target, len);
      var from = toAbsoluteIndex(start, len);
      var end = arguments.length > 2 ? arguments[2] : void 0;
      var count = Math.min((end === void 0 ? len : toAbsoluteIndex(end, len)) - from, len - to);
      var inc = 1;
      if (from < to && to < from + count) {
        inc = -1;
        from += count - 1;
        to += count - 1;
      }
      while (count-- > 0) {
        if (from in O)
          O[to] = O[from];
        else
          delete O[to];
        to += inc;
        from += inc;
      }
      return O;
    };
  });

  // node_modules/core-js/modules/_add-to-unscopables.js
  var require_add_to_unscopables = __commonJS((exports2, module2) => {
    var UNSCOPABLES = require_wks()("unscopables");
    var ArrayProto = Array.prototype;
    if (ArrayProto[UNSCOPABLES] == void 0)
      require_hide()(ArrayProto, UNSCOPABLES, {});
    module2.exports = function(key) {
      ArrayProto[UNSCOPABLES][key] = true;
    };
  });

  // node_modules/core-js/modules/es6.array.copy-within.js
  var require_es6_array_copy_within = __commonJS(() => {
    var $export = require_export();
    $export($export.P, "Array", {copyWithin: require_array_copy_within()});
    require_add_to_unscopables()("copyWithin");
  });

  // node_modules/core-js/modules/_array-fill.js
  var require_array_fill = __commonJS((exports2, module2) => {
    "use strict";
    var toObject = require_to_object();
    var toAbsoluteIndex = require_to_absolute_index();
    var toLength = require_to_length();
    module2.exports = function fill(value) {
      var O = toObject(this);
      var length = toLength(O.length);
      var aLen = arguments.length;
      var index = toAbsoluteIndex(aLen > 1 ? arguments[1] : void 0, length);
      var end = aLen > 2 ? arguments[2] : void 0;
      var endPos = end === void 0 ? length : toAbsoluteIndex(end, length);
      while (endPos > index)
        O[index++] = value;
      return O;
    };
  });

  // node_modules/core-js/modules/es6.array.fill.js
  var require_es6_array_fill = __commonJS(() => {
    var $export = require_export();
    $export($export.P, "Array", {fill: require_array_fill()});
    require_add_to_unscopables()("fill");
  });

  // node_modules/core-js/modules/es6.array.find.js
  var require_es6_array_find = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $find = require_array_methods()(5);
    var KEY = "find";
    var forced = true;
    if (KEY in [])
      Array(1)[KEY](function() {
        forced = false;
      });
    $export($export.P + $export.F * forced, "Array", {
      find: function find(callbackfn) {
        return $find(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
      }
    });
    require_add_to_unscopables()(KEY);
  });

  // node_modules/core-js/modules/es6.array.find-index.js
  var require_es6_array_find_index = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $find = require_array_methods()(6);
    var KEY = "findIndex";
    var forced = true;
    if (KEY in [])
      Array(1)[KEY](function() {
        forced = false;
      });
    $export($export.P + $export.F * forced, "Array", {
      findIndex: function findIndex(callbackfn) {
        return $find(this, callbackfn, arguments.length > 1 ? arguments[1] : void 0);
      }
    });
    require_add_to_unscopables()(KEY);
  });

  // node_modules/core-js/modules/_set-species.js
  var require_set_species = __commonJS((exports2, module2) => {
    "use strict";
    var global2 = require_global();
    var dP = require_object_dp();
    var DESCRIPTORS = require_descriptors();
    var SPECIES = require_wks()("species");
    module2.exports = function(KEY) {
      var C = global2[KEY];
      if (DESCRIPTORS && C && !C[SPECIES])
        dP.f(C, SPECIES, {
          configurable: true,
          get: function() {
            return this;
          }
        });
    };
  });

  // node_modules/core-js/modules/es6.array.species.js
  var require_es6_array_species = __commonJS(() => {
    require_set_species()("Array");
  });

  // node_modules/core-js/modules/_iter-step.js
  var require_iter_step = __commonJS((exports2, module2) => {
    module2.exports = function(done, value) {
      return {value, done: !!done};
    };
  });

  // node_modules/core-js/modules/es6.array.iterator.js
  var require_es6_array_iterator = __commonJS((exports2, module2) => {
    "use strict";
    var addToUnscopables = require_add_to_unscopables();
    var step = require_iter_step();
    var Iterators = require_iterators();
    var toIObject = require_to_iobject();
    module2.exports = require_iter_define()(Array, "Array", function(iterated, kind) {
      this._t = toIObject(iterated);
      this._i = 0;
      this._k = kind;
    }, function() {
      var O = this._t;
      var kind = this._k;
      var index = this._i++;
      if (!O || index >= O.length) {
        this._t = void 0;
        return step(1);
      }
      if (kind == "keys")
        return step(0, index);
      if (kind == "values")
        return step(0, O[index]);
      return step(0, [index, O[index]]);
    }, "values");
    Iterators.Arguments = Iterators.Array;
    addToUnscopables("keys");
    addToUnscopables("values");
    addToUnscopables("entries");
  });

  // node_modules/core-js/modules/_flags.js
  var require_flags = __commonJS((exports2, module2) => {
    "use strict";
    var anObject = require_an_object();
    module2.exports = function() {
      var that = anObject(this);
      var result = "";
      if (that.global)
        result += "g";
      if (that.ignoreCase)
        result += "i";
      if (that.multiline)
        result += "m";
      if (that.unicode)
        result += "u";
      if (that.sticky)
        result += "y";
      return result;
    };
  });

  // node_modules/core-js/modules/es6.regexp.constructor.js
  var require_es6_regexp_constructor = __commonJS(() => {
    var global2 = require_global();
    var inheritIfRequired = require_inherit_if_required();
    var dP = require_object_dp().f;
    var gOPN = require_object_gopn().f;
    var isRegExp = require_is_regexp();
    var $flags = require_flags();
    var $RegExp = global2.RegExp;
    var Base = $RegExp;
    var proto = $RegExp.prototype;
    var re1 = /a/g;
    var re2 = /a/g;
    var CORRECT_NEW = new $RegExp(re1) !== re1;
    if (require_descriptors() && (!CORRECT_NEW || require_fails()(function() {
      re2[require_wks()("match")] = false;
      return $RegExp(re1) != re1 || $RegExp(re2) == re2 || $RegExp(re1, "i") != "/a/i";
    }))) {
      $RegExp = function RegExp2(p, f) {
        var tiRE = this instanceof $RegExp;
        var piRE = isRegExp(p);
        var fiU = f === void 0;
        return !tiRE && piRE && p.constructor === $RegExp && fiU ? p : inheritIfRequired(CORRECT_NEW ? new Base(piRE && !fiU ? p.source : p, f) : Base((piRE = p instanceof $RegExp) ? p.source : p, piRE && fiU ? $flags.call(p) : f), tiRE ? this : proto, $RegExp);
      };
      proxy = function(key) {
        key in $RegExp || dP($RegExp, key, {
          configurable: true,
          get: function() {
            return Base[key];
          },
          set: function(it) {
            Base[key] = it;
          }
        });
      };
      for (keys = gOPN(Base), i = 0; keys.length > i; )
        proxy(keys[i++]);
      proto.constructor = $RegExp;
      $RegExp.prototype = proto;
      require_redefine()(global2, "RegExp", $RegExp);
    }
    var proxy;
    var keys;
    var i;
    require_set_species()("RegExp");
  });

  // node_modules/core-js/modules/_regexp-exec.js
  var require_regexp_exec = __commonJS((exports2, module2) => {
    "use strict";
    var regexpFlags = require_flags();
    var nativeExec = RegExp.prototype.exec;
    var nativeReplace = String.prototype.replace;
    var patchedExec = nativeExec;
    var LAST_INDEX = "lastIndex";
    var UPDATES_LAST_INDEX_WRONG = function() {
      var re1 = /a/, re2 = /b*/g;
      nativeExec.call(re1, "a");
      nativeExec.call(re2, "a");
      return re1[LAST_INDEX] !== 0 || re2[LAST_INDEX] !== 0;
    }();
    var NPCG_INCLUDED = /()??/.exec("")[1] !== void 0;
    var PATCH = UPDATES_LAST_INDEX_WRONG || NPCG_INCLUDED;
    if (PATCH) {
      patchedExec = function exec(str) {
        var re = this;
        var lastIndex, reCopy, match, i;
        if (NPCG_INCLUDED) {
          reCopy = new RegExp("^" + re.source + "$(?!\\s)", regexpFlags.call(re));
        }
        if (UPDATES_LAST_INDEX_WRONG)
          lastIndex = re[LAST_INDEX];
        match = nativeExec.call(re, str);
        if (UPDATES_LAST_INDEX_WRONG && match) {
          re[LAST_INDEX] = re.global ? match.index + match[0].length : lastIndex;
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

  // node_modules/core-js/modules/es6.regexp.exec.js
  var require_es6_regexp_exec = __commonJS(() => {
    "use strict";
    var regexpExec = require_regexp_exec();
    require_export()({
      target: "RegExp",
      proto: true,
      forced: regexpExec !== /./.exec
    }, {
      exec: regexpExec
    });
  });

  // node_modules/core-js/modules/es6.regexp.flags.js
  var require_es6_regexp_flags = __commonJS(() => {
    if (require_descriptors() && /./g.flags != "g")
      require_object_dp().f(RegExp.prototype, "flags", {
        configurable: true,
        get: require_flags()
      });
  });

  // node_modules/core-js/modules/es6.regexp.to-string.js
  var require_es6_regexp_to_string = __commonJS(() => {
    "use strict";
    require_es6_regexp_flags();
    var anObject = require_an_object();
    var $flags = require_flags();
    var DESCRIPTORS = require_descriptors();
    var TO_STRING = "toString";
    var $toString = /./[TO_STRING];
    var define2 = function(fn) {
      require_redefine()(RegExp.prototype, TO_STRING, fn, true);
    };
    if (require_fails()(function() {
      return $toString.call({source: "a", flags: "b"}) != "/a/b";
    })) {
      define2(function toString() {
        var R = anObject(this);
        return "/".concat(R.source, "/", "flags" in R ? R.flags : !DESCRIPTORS && R instanceof RegExp ? $flags.call(R) : void 0);
      });
    } else if ($toString.name != TO_STRING) {
      define2(function toString() {
        return $toString.call(this);
      });
    }
  });

  // node_modules/core-js/modules/_advance-string-index.js
  var require_advance_string_index = __commonJS((exports2, module2) => {
    "use strict";
    var at = require_string_at()(true);
    module2.exports = function(S, index, unicode) {
      return index + (unicode ? at(S, index).length : 1);
    };
  });

  // node_modules/core-js/modules/_regexp-exec-abstract.js
  var require_regexp_exec_abstract = __commonJS((exports2, module2) => {
    "use strict";
    var classof = require_classof();
    var builtinExec = RegExp.prototype.exec;
    module2.exports = function(R, S) {
      var exec = R.exec;
      if (typeof exec === "function") {
        var result = exec.call(R, S);
        if (typeof result !== "object") {
          throw new TypeError("RegExp exec method returned something other than an Object or null");
        }
        return result;
      }
      if (classof(R) !== "RegExp") {
        throw new TypeError("RegExp#exec called on incompatible receiver");
      }
      return builtinExec.call(R, S);
    };
  });

  // node_modules/core-js/modules/_fix-re-wks.js
  var require_fix_re_wks = __commonJS((exports2, module2) => {
    "use strict";
    require_es6_regexp_exec();
    var redefine = require_redefine();
    var hide = require_hide();
    var fails = require_fails();
    var defined = require_defined();
    var wks = require_wks();
    var regexpExec = require_regexp_exec();
    var SPECIES = wks("species");
    var REPLACE_SUPPORTS_NAMED_GROUPS = !fails(function() {
      var re = /./;
      re.exec = function() {
        var result = [];
        result.groups = {a: "7"};
        return result;
      };
      return "".replace(re, "$<a>") !== "7";
    });
    var SPLIT_WORKS_WITH_OVERWRITTEN_EXEC = function() {
      var re = /(?:)/;
      var originalExec = re.exec;
      re.exec = function() {
        return originalExec.apply(this, arguments);
      };
      var result = "ab".split(re);
      return result.length === 2 && result[0] === "a" && result[1] === "b";
    }();
    module2.exports = function(KEY, length, exec) {
      var SYMBOL = wks(KEY);
      var DELEGATES_TO_SYMBOL = !fails(function() {
        var O = {};
        O[SYMBOL] = function() {
          return 7;
        };
        return ""[KEY](O) != 7;
      });
      var DELEGATES_TO_EXEC = DELEGATES_TO_SYMBOL ? !fails(function() {
        var execCalled = false;
        var re = /a/;
        re.exec = function() {
          execCalled = true;
          return null;
        };
        if (KEY === "split") {
          re.constructor = {};
          re.constructor[SPECIES] = function() {
            return re;
          };
        }
        re[SYMBOL]("");
        return !execCalled;
      }) : void 0;
      if (!DELEGATES_TO_SYMBOL || !DELEGATES_TO_EXEC || KEY === "replace" && !REPLACE_SUPPORTS_NAMED_GROUPS || KEY === "split" && !SPLIT_WORKS_WITH_OVERWRITTEN_EXEC) {
        var nativeRegExpMethod = /./[SYMBOL];
        var fns = exec(defined, SYMBOL, ""[KEY], function maybeCallNative(nativeMethod, regexp, str, arg2, forceStringMethod) {
          if (regexp.exec === regexpExec) {
            if (DELEGATES_TO_SYMBOL && !forceStringMethod) {
              return {done: true, value: nativeRegExpMethod.call(regexp, str, arg2)};
            }
            return {done: true, value: nativeMethod.call(str, regexp, arg2)};
          }
          return {done: false};
        });
        var strfn = fns[0];
        var rxfn = fns[1];
        redefine(String.prototype, KEY, strfn);
        hide(RegExp.prototype, SYMBOL, length == 2 ? function(string, arg) {
          return rxfn.call(string, this, arg);
        } : function(string) {
          return rxfn.call(string, this);
        });
      }
    };
  });

  // node_modules/core-js/modules/es6.regexp.match.js
  var require_es6_regexp_match = __commonJS(() => {
    "use strict";
    var anObject = require_an_object();
    var toLength = require_to_length();
    var advanceStringIndex = require_advance_string_index();
    var regExpExec = require_regexp_exec_abstract();
    require_fix_re_wks()("match", 1, function(defined, MATCH, $match, maybeCallNative) {
      return [
        function match(regexp) {
          var O = defined(this);
          var fn = regexp == void 0 ? void 0 : regexp[MATCH];
          return fn !== void 0 ? fn.call(regexp, O) : new RegExp(regexp)[MATCH](String(O));
        },
        function(regexp) {
          var res = maybeCallNative($match, regexp, this);
          if (res.done)
            return res.value;
          var rx = anObject(regexp);
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
              rx.lastIndex = advanceStringIndex(S, toLength(rx.lastIndex), fullUnicode);
            n++;
          }
          return n === 0 ? null : A;
        }
      ];
    });
  });

  // node_modules/core-js/modules/es6.regexp.replace.js
  var require_es6_regexp_replace = __commonJS(() => {
    "use strict";
    var anObject = require_an_object();
    var toObject = require_to_object();
    var toLength = require_to_length();
    var toInteger = require_to_integer();
    var advanceStringIndex = require_advance_string_index();
    var regExpExec = require_regexp_exec_abstract();
    var max = Math.max;
    var min = Math.min;
    var floor = Math.floor;
    var SUBSTITUTION_SYMBOLS = /\$([$&`']|\d\d?|<[^>]*>)/g;
    var SUBSTITUTION_SYMBOLS_NO_NAMED = /\$([$&`']|\d\d?)/g;
    var maybeToString = function(it) {
      return it === void 0 ? it : String(it);
    };
    require_fix_re_wks()("replace", 2, function(defined, REPLACE, $replace, maybeCallNative) {
      return [
        function replace(searchValue, replaceValue) {
          var O = defined(this);
          var fn = searchValue == void 0 ? void 0 : searchValue[REPLACE];
          return fn !== void 0 ? fn.call(searchValue, O, replaceValue) : $replace.call(String(O), searchValue, replaceValue);
        },
        function(regexp, replaceValue) {
          var res = maybeCallNative($replace, regexp, this, replaceValue);
          if (res.done)
            return res.value;
          var rx = anObject(regexp);
          var S = String(this);
          var functionalReplace = typeof replaceValue === "function";
          if (!functionalReplace)
            replaceValue = String(replaceValue);
          var global2 = rx.global;
          if (global2) {
            var fullUnicode = rx.unicode;
            rx.lastIndex = 0;
          }
          var results = [];
          while (true) {
            var result = regExpExec(rx, S);
            if (result === null)
              break;
            results.push(result);
            if (!global2)
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
      function getSubstitution(matched, str, position, captures, namedCaptures, replacement) {
        var tailPos = position + matched.length;
        var m = captures.length;
        var symbols = SUBSTITUTION_SYMBOLS_NO_NAMED;
        if (namedCaptures !== void 0) {
          namedCaptures = toObject(namedCaptures);
          symbols = SUBSTITUTION_SYMBOLS;
        }
        return $replace.call(replacement, symbols, function(match, ch) {
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
      }
    });
  });

  // node_modules/core-js/modules/es6.regexp.search.js
  var require_es6_regexp_search = __commonJS(() => {
    "use strict";
    var anObject = require_an_object();
    var sameValue = require_same_value();
    var regExpExec = require_regexp_exec_abstract();
    require_fix_re_wks()("search", 1, function(defined, SEARCH, $search, maybeCallNative) {
      return [
        function search(regexp) {
          var O = defined(this);
          var fn = regexp == void 0 ? void 0 : regexp[SEARCH];
          return fn !== void 0 ? fn.call(regexp, O) : new RegExp(regexp)[SEARCH](String(O));
        },
        function(regexp) {
          var res = maybeCallNative($search, regexp, this);
          if (res.done)
            return res.value;
          var rx = anObject(regexp);
          var S = String(this);
          var previousLastIndex = rx.lastIndex;
          if (!sameValue(previousLastIndex, 0))
            rx.lastIndex = 0;
          var result = regExpExec(rx, S);
          if (!sameValue(rx.lastIndex, previousLastIndex))
            rx.lastIndex = previousLastIndex;
          return result === null ? -1 : result.index;
        }
      ];
    });
  });

  // node_modules/core-js/modules/_species-constructor.js
  var require_species_constructor = __commonJS((exports2, module2) => {
    var anObject = require_an_object();
    var aFunction = require_a_function();
    var SPECIES = require_wks()("species");
    module2.exports = function(O, D) {
      var C = anObject(O).constructor;
      var S;
      return C === void 0 || (S = anObject(C)[SPECIES]) == void 0 ? D : aFunction(S);
    };
  });

  // node_modules/core-js/modules/es6.regexp.split.js
  var require_es6_regexp_split = __commonJS(() => {
    "use strict";
    var isRegExp = require_is_regexp();
    var anObject = require_an_object();
    var speciesConstructor = require_species_constructor();
    var advanceStringIndex = require_advance_string_index();
    var toLength = require_to_length();
    var callRegExpExec = require_regexp_exec_abstract();
    var regexpExec = require_regexp_exec();
    var fails = require_fails();
    var $min = Math.min;
    var $push = [].push;
    var $SPLIT = "split";
    var LENGTH = "length";
    var LAST_INDEX = "lastIndex";
    var MAX_UINT32 = 4294967295;
    var SUPPORTS_Y = !fails(function() {
      RegExp(MAX_UINT32, "y");
    });
    require_fix_re_wks()("split", 2, function(defined, SPLIT, $split, maybeCallNative) {
      var internalSplit;
      if ("abbc"[$SPLIT](/(b)*/)[1] == "c" || "test"[$SPLIT](/(?:)/, -1)[LENGTH] != 4 || "ab"[$SPLIT](/(?:ab)*/)[LENGTH] != 2 || "."[$SPLIT](/(.?)(.?)/)[LENGTH] != 4 || "."[$SPLIT](/()()/)[LENGTH] > 1 || ""[$SPLIT](/.?/)[LENGTH]) {
        internalSplit = function(separator, limit) {
          var string = String(this);
          if (separator === void 0 && limit === 0)
            return [];
          if (!isRegExp(separator))
            return $split.call(string, separator, limit);
          var output = [];
          var flags = (separator.ignoreCase ? "i" : "") + (separator.multiline ? "m" : "") + (separator.unicode ? "u" : "") + (separator.sticky ? "y" : "");
          var lastLastIndex = 0;
          var splitLimit = limit === void 0 ? MAX_UINT32 : limit >>> 0;
          var separatorCopy = new RegExp(separator.source, flags + "g");
          var match, lastIndex, lastLength;
          while (match = regexpExec.call(separatorCopy, string)) {
            lastIndex = separatorCopy[LAST_INDEX];
            if (lastIndex > lastLastIndex) {
              output.push(string.slice(lastLastIndex, match.index));
              if (match[LENGTH] > 1 && match.index < string[LENGTH])
                $push.apply(output, match.slice(1));
              lastLength = match[0][LENGTH];
              lastLastIndex = lastIndex;
              if (output[LENGTH] >= splitLimit)
                break;
            }
            if (separatorCopy[LAST_INDEX] === match.index)
              separatorCopy[LAST_INDEX]++;
          }
          if (lastLastIndex === string[LENGTH]) {
            if (lastLength || !separatorCopy.test(""))
              output.push("");
          } else
            output.push(string.slice(lastLastIndex));
          return output[LENGTH] > splitLimit ? output.slice(0, splitLimit) : output;
        };
      } else if ("0"[$SPLIT](void 0, 0)[LENGTH]) {
        internalSplit = function(separator, limit) {
          return separator === void 0 && limit === 0 ? [] : $split.call(this, separator, limit);
        };
      } else {
        internalSplit = $split;
      }
      return [
        function split(separator, limit) {
          var O = defined(this);
          var splitter = separator == void 0 ? void 0 : separator[SPLIT];
          return splitter !== void 0 ? splitter.call(separator, O, limit) : internalSplit.call(String(O), separator, limit);
        },
        function(regexp, limit) {
          var res = maybeCallNative(internalSplit, regexp, this, limit, internalSplit !== $split);
          if (res.done)
            return res.value;
          var rx = anObject(regexp);
          var S = String(this);
          var C = speciesConstructor(rx, RegExp);
          var unicodeMatching = rx.unicode;
          var flags = (rx.ignoreCase ? "i" : "") + (rx.multiline ? "m" : "") + (rx.unicode ? "u" : "") + (SUPPORTS_Y ? "y" : "g");
          var splitter = new C(SUPPORTS_Y ? rx : "^(?:" + rx.source + ")", flags);
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
            if (z === null || (e = $min(toLength(splitter.lastIndex + (SUPPORTS_Y ? 0 : q)), S.length)) === p) {
              q = advanceStringIndex(S, q, unicodeMatching);
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
    });
  });

  // node_modules/core-js/modules/_an-instance.js
  var require_an_instance = __commonJS((exports2, module2) => {
    module2.exports = function(it, Constructor, name, forbiddenField) {
      if (!(it instanceof Constructor) || forbiddenField !== void 0 && forbiddenField in it) {
        throw TypeError(name + ": incorrect invocation!");
      }
      return it;
    };
  });

  // node_modules/core-js/modules/_for-of.js
  var require_for_of = __commonJS((exports2, module2) => {
    var ctx = require_ctx();
    var call = require_iter_call();
    var isArrayIter = require_is_array_iter();
    var anObject = require_an_object();
    var toLength = require_to_length();
    var getIterFn = require_core_get_iterator_method();
    var BREAK = {};
    var RETURN = {};
    var exports2 = module2.exports = function(iterable, entries, fn, that, ITERATOR) {
      var iterFn = ITERATOR ? function() {
        return iterable;
      } : getIterFn(iterable);
      var f = ctx(fn, that, entries ? 2 : 1);
      var index = 0;
      var length, step, iterator, result;
      if (typeof iterFn != "function")
        throw TypeError(iterable + " is not iterable!");
      if (isArrayIter(iterFn))
        for (length = toLength(iterable.length); length > index; index++) {
          result = entries ? f(anObject(step = iterable[index])[0], step[1]) : f(iterable[index]);
          if (result === BREAK || result === RETURN)
            return result;
        }
      else
        for (iterator = iterFn.call(iterable); !(step = iterator.next()).done; ) {
          result = call(iterator, f, step.value, entries);
          if (result === BREAK || result === RETURN)
            return result;
        }
    };
    exports2.BREAK = BREAK;
    exports2.RETURN = RETURN;
  });

  // node_modules/core-js/modules/_task.js
  var require_task = __commonJS((exports2, module2) => {
    var ctx = require_ctx();
    var invoke = require_invoke();
    var html = require_html();
    var cel = require_dom_create();
    var global2 = require_global();
    var process = global2.process;
    var setTask = global2.setImmediate;
    var clearTask = global2.clearImmediate;
    var MessageChannel = global2.MessageChannel;
    var Dispatch = global2.Dispatch;
    var counter = 0;
    var queue = {};
    var ONREADYSTATECHANGE = "onreadystatechange";
    var defer;
    var channel;
    var port;
    var run = function() {
      var id = +this;
      if (queue.hasOwnProperty(id)) {
        var fn = queue[id];
        delete queue[id];
        fn();
      }
    };
    var listener = function(event) {
      run.call(event.data);
    };
    if (!setTask || !clearTask) {
      setTask = function setImmediate(fn) {
        var args = [];
        var i = 1;
        while (arguments.length > i)
          args.push(arguments[i++]);
        queue[++counter] = function() {
          invoke(typeof fn == "function" ? fn : Function(fn), args);
        };
        defer(counter);
        return counter;
      };
      clearTask = function clearImmediate(id) {
        delete queue[id];
      };
      if (require_cof()(process) == "process") {
        defer = function(id) {
          process.nextTick(ctx(run, id, 1));
        };
      } else if (Dispatch && Dispatch.now) {
        defer = function(id) {
          Dispatch.now(ctx(run, id, 1));
        };
      } else if (MessageChannel) {
        channel = new MessageChannel();
        port = channel.port2;
        channel.port1.onmessage = listener;
        defer = ctx(port.postMessage, port, 1);
      } else if (global2.addEventListener && typeof postMessage == "function" && !global2.importScripts) {
        defer = function(id) {
          global2.postMessage(id + "", "*");
        };
        global2.addEventListener("message", listener, false);
      } else if (ONREADYSTATECHANGE in cel("script")) {
        defer = function(id) {
          html.appendChild(cel("script"))[ONREADYSTATECHANGE] = function() {
            html.removeChild(this);
            run.call(id);
          };
        };
      } else {
        defer = function(id) {
          setTimeout(ctx(run, id, 1), 0);
        };
      }
    }
    module2.exports = {
      set: setTask,
      clear: clearTask
    };
  });

  // node_modules/core-js/modules/_microtask.js
  var require_microtask = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var macrotask = require_task().set;
    var Observer = global2.MutationObserver || global2.WebKitMutationObserver;
    var process = global2.process;
    var Promise2 = global2.Promise;
    var isNode = require_cof()(process) == "process";
    module2.exports = function() {
      var head, last, notify;
      var flush = function() {
        var parent, fn;
        if (isNode && (parent = process.domain))
          parent.exit();
        while (head) {
          fn = head.fn;
          head = head.next;
          try {
            fn();
          } catch (e) {
            if (head)
              notify();
            else
              last = void 0;
            throw e;
          }
        }
        last = void 0;
        if (parent)
          parent.enter();
      };
      if (isNode) {
        notify = function() {
          process.nextTick(flush);
        };
      } else if (Observer && !(global2.navigator && global2.navigator.standalone)) {
        var toggle = true;
        var node = document.createTextNode("");
        new Observer(flush).observe(node, {characterData: true});
        notify = function() {
          node.data = toggle = !toggle;
        };
      } else if (Promise2 && Promise2.resolve) {
        var promise = Promise2.resolve(void 0);
        notify = function() {
          promise.then(flush);
        };
      } else {
        notify = function() {
          macrotask.call(global2, flush);
        };
      }
      return function(fn) {
        var task = {fn, next: void 0};
        if (last)
          last.next = task;
        if (!head) {
          head = task;
          notify();
        }
        last = task;
      };
    };
  });

  // node_modules/core-js/modules/_new-promise-capability.js
  var require_new_promise_capability = __commonJS((exports2, module2) => {
    "use strict";
    var aFunction = require_a_function();
    function PromiseCapability(C) {
      var resolve, reject;
      this.promise = new C(function($$resolve, $$reject) {
        if (resolve !== void 0 || reject !== void 0)
          throw TypeError("Bad Promise constructor");
        resolve = $$resolve;
        reject = $$reject;
      });
      this.resolve = aFunction(resolve);
      this.reject = aFunction(reject);
    }
    module2.exports.f = function(C) {
      return new PromiseCapability(C);
    };
  });

  // node_modules/core-js/modules/_perform.js
  var require_perform = __commonJS((exports2, module2) => {
    module2.exports = function(exec) {
      try {
        return {e: false, v: exec()};
      } catch (e) {
        return {e: true, v: e};
      }
    };
  });

  // node_modules/core-js/modules/_user-agent.js
  var require_user_agent = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var navigator = global2.navigator;
    module2.exports = navigator && navigator.userAgent || "";
  });

  // node_modules/core-js/modules/_promise-resolve.js
  var require_promise_resolve = __commonJS((exports2, module2) => {
    var anObject = require_an_object();
    var isObject = require_is_object();
    var newPromiseCapability = require_new_promise_capability();
    module2.exports = function(C, x) {
      anObject(C);
      if (isObject(x) && x.constructor === C)
        return x;
      var promiseCapability = newPromiseCapability.f(C);
      var resolve = promiseCapability.resolve;
      resolve(x);
      return promiseCapability.promise;
    };
  });

  // node_modules/core-js/modules/_redefine-all.js
  var require_redefine_all = __commonJS((exports2, module2) => {
    var redefine = require_redefine();
    module2.exports = function(target, src, safe) {
      for (var key in src)
        redefine(target, key, src[key], safe);
      return target;
    };
  });

  // node_modules/core-js/modules/es6.promise.js
  var require_es6_promise = __commonJS(() => {
    "use strict";
    var LIBRARY = require_library();
    var global2 = require_global();
    var ctx = require_ctx();
    var classof = require_classof();
    var $export = require_export();
    var isObject = require_is_object();
    var aFunction = require_a_function();
    var anInstance = require_an_instance();
    var forOf = require_for_of();
    var speciesConstructor = require_species_constructor();
    var task = require_task().set;
    var microtask = require_microtask()();
    var newPromiseCapabilityModule = require_new_promise_capability();
    var perform = require_perform();
    var userAgent2 = require_user_agent();
    var promiseResolve = require_promise_resolve();
    var PROMISE = "Promise";
    var TypeError2 = global2.TypeError;
    var process = global2.process;
    var versions = process && process.versions;
    var v8 = versions && versions.v8 || "";
    var $Promise = global2[PROMISE];
    var isNode = classof(process) == "process";
    var empty = function() {
    };
    var Internal;
    var newGenericPromiseCapability;
    var OwnPromiseCapability;
    var Wrapper;
    var newPromiseCapability = newGenericPromiseCapability = newPromiseCapabilityModule.f;
    var USE_NATIVE = !!function() {
      try {
        var promise = $Promise.resolve(1);
        var FakePromise = (promise.constructor = {})[require_wks()("species")] = function(exec) {
          exec(empty, empty);
        };
        return (isNode || typeof PromiseRejectionEvent == "function") && promise.then(empty) instanceof FakePromise && v8.indexOf("6.6") !== 0 && userAgent2.indexOf("Chrome/66") === -1;
      } catch (e) {
      }
    }();
    var isThenable = function(it) {
      var then;
      return isObject(it) && typeof (then = it.then) == "function" ? then : false;
    };
    var notify = function(promise, isReject) {
      if (promise._n)
        return;
      promise._n = true;
      var chain = promise._c;
      microtask(function() {
        var value = promise._v;
        var ok = promise._s == 1;
        var i = 0;
        var run = function(reaction) {
          var handler = ok ? reaction.ok : reaction.fail;
          var resolve = reaction.resolve;
          var reject = reaction.reject;
          var domain = reaction.domain;
          var result, then, exited;
          try {
            if (handler) {
              if (!ok) {
                if (promise._h == 2)
                  onHandleUnhandled(promise);
                promise._h = 1;
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
                reject(TypeError2("Promise-chain cycle"));
              } else if (then = isThenable(result)) {
                then.call(result, resolve, reject);
              } else
                resolve(result);
            } else
              reject(value);
          } catch (e) {
            if (domain && !exited)
              domain.exit();
            reject(e);
          }
        };
        while (chain.length > i)
          run(chain[i++]);
        promise._c = [];
        promise._n = false;
        if (isReject && !promise._h)
          onUnhandled(promise);
      });
    };
    var onUnhandled = function(promise) {
      task.call(global2, function() {
        var value = promise._v;
        var unhandled = isUnhandled(promise);
        var result, handler, console2;
        if (unhandled) {
          result = perform(function() {
            if (isNode) {
              process.emit("unhandledRejection", value, promise);
            } else if (handler = global2.onunhandledrejection) {
              handler({promise, reason: value});
            } else if ((console2 = global2.console) && console2.error) {
              console2.error("Unhandled promise rejection", value);
            }
          });
          promise._h = isNode || isUnhandled(promise) ? 2 : 1;
        }
        promise._a = void 0;
        if (unhandled && result.e)
          throw result.v;
      });
    };
    var isUnhandled = function(promise) {
      return promise._h !== 1 && (promise._a || promise._c).length === 0;
    };
    var onHandleUnhandled = function(promise) {
      task.call(global2, function() {
        var handler;
        if (isNode) {
          process.emit("rejectionHandled", promise);
        } else if (handler = global2.onrejectionhandled) {
          handler({promise, reason: promise._v});
        }
      });
    };
    var $reject = function(value) {
      var promise = this;
      if (promise._d)
        return;
      promise._d = true;
      promise = promise._w || promise;
      promise._v = value;
      promise._s = 2;
      if (!promise._a)
        promise._a = promise._c.slice();
      notify(promise, true);
    };
    var $resolve = function(value) {
      var promise = this;
      var then;
      if (promise._d)
        return;
      promise._d = true;
      promise = promise._w || promise;
      try {
        if (promise === value)
          throw TypeError2("Promise can't be resolved itself");
        if (then = isThenable(value)) {
          microtask(function() {
            var wrapper = {_w: promise, _d: false};
            try {
              then.call(value, ctx($resolve, wrapper, 1), ctx($reject, wrapper, 1));
            } catch (e) {
              $reject.call(wrapper, e);
            }
          });
        } else {
          promise._v = value;
          promise._s = 1;
          notify(promise, false);
        }
      } catch (e) {
        $reject.call({_w: promise, _d: false}, e);
      }
    };
    if (!USE_NATIVE) {
      $Promise = function Promise2(executor) {
        anInstance(this, $Promise, PROMISE, "_h");
        aFunction(executor);
        Internal.call(this);
        try {
          executor(ctx($resolve, this, 1), ctx($reject, this, 1));
        } catch (err) {
          $reject.call(this, err);
        }
      };
      Internal = function Promise2(executor) {
        this._c = [];
        this._a = void 0;
        this._s = 0;
        this._d = false;
        this._v = void 0;
        this._h = 0;
        this._n = false;
      };
      Internal.prototype = require_redefine_all()($Promise.prototype, {
        then: function then(onFulfilled, onRejected) {
          var reaction = newPromiseCapability(speciesConstructor(this, $Promise));
          reaction.ok = typeof onFulfilled == "function" ? onFulfilled : true;
          reaction.fail = typeof onRejected == "function" && onRejected;
          reaction.domain = isNode ? process.domain : void 0;
          this._c.push(reaction);
          if (this._a)
            this._a.push(reaction);
          if (this._s)
            notify(this, false);
          return reaction.promise;
        },
        catch: function(onRejected) {
          return this.then(void 0, onRejected);
        }
      });
      OwnPromiseCapability = function() {
        var promise = new Internal();
        this.promise = promise;
        this.resolve = ctx($resolve, promise, 1);
        this.reject = ctx($reject, promise, 1);
      };
      newPromiseCapabilityModule.f = newPromiseCapability = function(C) {
        return C === $Promise || C === Wrapper ? new OwnPromiseCapability(C) : newGenericPromiseCapability(C);
      };
    }
    $export($export.G + $export.W + $export.F * !USE_NATIVE, {Promise: $Promise});
    require_set_to_string_tag()($Promise, PROMISE);
    require_set_species()(PROMISE);
    Wrapper = require_core()[PROMISE];
    $export($export.S + $export.F * !USE_NATIVE, PROMISE, {
      reject: function reject(r) {
        var capability = newPromiseCapability(this);
        var $$reject = capability.reject;
        $$reject(r);
        return capability.promise;
      }
    });
    $export($export.S + $export.F * (LIBRARY || !USE_NATIVE), PROMISE, {
      resolve: function resolve(x) {
        return promiseResolve(LIBRARY && this === Wrapper ? $Promise : this, x);
      }
    });
    $export($export.S + $export.F * !(USE_NATIVE && require_iter_detect()(function(iter) {
      $Promise.all(iter)["catch"](empty);
    })), PROMISE, {
      all: function all(iterable) {
        var C = this;
        var capability = newPromiseCapability(C);
        var resolve = capability.resolve;
        var reject = capability.reject;
        var result = perform(function() {
          var values = [];
          var index = 0;
          var remaining = 1;
          forOf(iterable, false, function(promise) {
            var $index = index++;
            var alreadyCalled = false;
            values.push(void 0);
            remaining++;
            C.resolve(promise).then(function(value) {
              if (alreadyCalled)
                return;
              alreadyCalled = true;
              values[$index] = value;
              --remaining || resolve(values);
            }, reject);
          });
          --remaining || resolve(values);
        });
        if (result.e)
          reject(result.v);
        return capability.promise;
      },
      race: function race(iterable) {
        var C = this;
        var capability = newPromiseCapability(C);
        var reject = capability.reject;
        var result = perform(function() {
          forOf(iterable, false, function(promise) {
            C.resolve(promise).then(capability.resolve, reject);
          });
        });
        if (result.e)
          reject(result.v);
        return capability.promise;
      }
    });
  });

  // node_modules/core-js/modules/_validate-collection.js
  var require_validate_collection = __commonJS((exports2, module2) => {
    var isObject = require_is_object();
    module2.exports = function(it, TYPE) {
      if (!isObject(it) || it._t !== TYPE)
        throw TypeError("Incompatible receiver, " + TYPE + " required!");
      return it;
    };
  });

  // node_modules/core-js/modules/_collection-strong.js
  var require_collection_strong = __commonJS((exports2, module2) => {
    "use strict";
    var dP = require_object_dp().f;
    var create = require_object_create();
    var redefineAll = require_redefine_all();
    var ctx = require_ctx();
    var anInstance = require_an_instance();
    var forOf = require_for_of();
    var $iterDefine = require_iter_define();
    var step = require_iter_step();
    var setSpecies = require_set_species();
    var DESCRIPTORS = require_descriptors();
    var fastKey = require_meta().fastKey;
    var validate = require_validate_collection();
    var SIZE = DESCRIPTORS ? "_s" : "size";
    var getEntry = function(that, key) {
      var index = fastKey(key);
      var entry;
      if (index !== "F")
        return that._i[index];
      for (entry = that._f; entry; entry = entry.n) {
        if (entry.k == key)
          return entry;
      }
    };
    module2.exports = {
      getConstructor: function(wrapper, NAME, IS_MAP, ADDER) {
        var C = wrapper(function(that, iterable) {
          anInstance(that, C, NAME, "_i");
          that._t = NAME;
          that._i = create(null);
          that._f = void 0;
          that._l = void 0;
          that[SIZE] = 0;
          if (iterable != void 0)
            forOf(iterable, IS_MAP, that[ADDER], that);
        });
        redefineAll(C.prototype, {
          clear: function clear() {
            for (var that = validate(this, NAME), data = that._i, entry = that._f; entry; entry = entry.n) {
              entry.r = true;
              if (entry.p)
                entry.p = entry.p.n = void 0;
              delete data[entry.i];
            }
            that._f = that._l = void 0;
            that[SIZE] = 0;
          },
          delete: function(key) {
            var that = validate(this, NAME);
            var entry = getEntry(that, key);
            if (entry) {
              var next = entry.n;
              var prev = entry.p;
              delete that._i[entry.i];
              entry.r = true;
              if (prev)
                prev.n = next;
              if (next)
                next.p = prev;
              if (that._f == entry)
                that._f = next;
              if (that._l == entry)
                that._l = prev;
              that[SIZE]--;
            }
            return !!entry;
          },
          forEach: function forEach(callbackfn) {
            validate(this, NAME);
            var f = ctx(callbackfn, arguments.length > 1 ? arguments[1] : void 0, 3);
            var entry;
            while (entry = entry ? entry.n : this._f) {
              f(entry.v, entry.k, this);
              while (entry && entry.r)
                entry = entry.p;
            }
          },
          has: function has(key) {
            return !!getEntry(validate(this, NAME), key);
          }
        });
        if (DESCRIPTORS)
          dP(C.prototype, "size", {
            get: function() {
              return validate(this, NAME)[SIZE];
            }
          });
        return C;
      },
      def: function(that, key, value) {
        var entry = getEntry(that, key);
        var prev, index;
        if (entry) {
          entry.v = value;
        } else {
          that._l = entry = {
            i: index = fastKey(key, true),
            k: key,
            v: value,
            p: prev = that._l,
            n: void 0,
            r: false
          };
          if (!that._f)
            that._f = entry;
          if (prev)
            prev.n = entry;
          that[SIZE]++;
          if (index !== "F")
            that._i[index] = entry;
        }
        return that;
      },
      getEntry,
      setStrong: function(C, NAME, IS_MAP) {
        $iterDefine(C, NAME, function(iterated, kind) {
          this._t = validate(iterated, NAME);
          this._k = kind;
          this._l = void 0;
        }, function() {
          var that = this;
          var kind = that._k;
          var entry = that._l;
          while (entry && entry.r)
            entry = entry.p;
          if (!that._t || !(that._l = entry = entry ? entry.n : that._t._f)) {
            that._t = void 0;
            return step(1);
          }
          if (kind == "keys")
            return step(0, entry.k);
          if (kind == "values")
            return step(0, entry.v);
          return step(0, [entry.k, entry.v]);
        }, IS_MAP ? "entries" : "values", !IS_MAP, true);
        setSpecies(NAME);
      }
    };
  });

  // node_modules/core-js/modules/_collection.js
  var require_collection = __commonJS((exports2, module2) => {
    "use strict";
    var global2 = require_global();
    var $export = require_export();
    var redefine = require_redefine();
    var redefineAll = require_redefine_all();
    var meta = require_meta();
    var forOf = require_for_of();
    var anInstance = require_an_instance();
    var isObject = require_is_object();
    var fails = require_fails();
    var $iterDetect = require_iter_detect();
    var setToStringTag = require_set_to_string_tag();
    var inheritIfRequired = require_inherit_if_required();
    module2.exports = function(NAME, wrapper, methods, common, IS_MAP, IS_WEAK) {
      var Base = global2[NAME];
      var C = Base;
      var ADDER = IS_MAP ? "set" : "add";
      var proto = C && C.prototype;
      var O = {};
      var fixMethod = function(KEY) {
        var fn = proto[KEY];
        redefine(proto, KEY, KEY == "delete" ? function(a) {
          return IS_WEAK && !isObject(a) ? false : fn.call(this, a === 0 ? 0 : a);
        } : KEY == "has" ? function has(a) {
          return IS_WEAK && !isObject(a) ? false : fn.call(this, a === 0 ? 0 : a);
        } : KEY == "get" ? function get(a) {
          return IS_WEAK && !isObject(a) ? void 0 : fn.call(this, a === 0 ? 0 : a);
        } : KEY == "add" ? function add(a) {
          fn.call(this, a === 0 ? 0 : a);
          return this;
        } : function set(a, b) {
          fn.call(this, a === 0 ? 0 : a, b);
          return this;
        });
      };
      if (typeof C != "function" || !(IS_WEAK || proto.forEach && !fails(function() {
        new C().entries().next();
      }))) {
        C = common.getConstructor(wrapper, NAME, IS_MAP, ADDER);
        redefineAll(C.prototype, methods);
        meta.NEED = true;
      } else {
        var instance = new C();
        var HASNT_CHAINING = instance[ADDER](IS_WEAK ? {} : -0, 1) != instance;
        var THROWS_ON_PRIMITIVES = fails(function() {
          instance.has(1);
        });
        var ACCEPT_ITERABLES = $iterDetect(function(iter) {
          new C(iter);
        });
        var BUGGY_ZERO = !IS_WEAK && fails(function() {
          var $instance = new C();
          var index = 5;
          while (index--)
            $instance[ADDER](index, index);
          return !$instance.has(-0);
        });
        if (!ACCEPT_ITERABLES) {
          C = wrapper(function(target, iterable) {
            anInstance(target, C, NAME);
            var that = inheritIfRequired(new Base(), target, C);
            if (iterable != void 0)
              forOf(iterable, IS_MAP, that[ADDER], that);
            return that;
          });
          C.prototype = proto;
          proto.constructor = C;
        }
        if (THROWS_ON_PRIMITIVES || BUGGY_ZERO) {
          fixMethod("delete");
          fixMethod("has");
          IS_MAP && fixMethod("get");
        }
        if (BUGGY_ZERO || HASNT_CHAINING)
          fixMethod(ADDER);
        if (IS_WEAK && proto.clear)
          delete proto.clear;
      }
      setToStringTag(C, NAME);
      O[NAME] = C;
      $export($export.G + $export.W + $export.F * (C != Base), O);
      if (!IS_WEAK)
        common.setStrong(C, NAME, IS_MAP);
      return C;
    };
  });

  // node_modules/core-js/modules/es6.map.js
  var require_es6_map = __commonJS((exports2, module2) => {
    "use strict";
    var strong = require_collection_strong();
    var validate = require_validate_collection();
    var MAP = "Map";
    module2.exports = require_collection()(MAP, function(get) {
      return function Map() {
        return get(this, arguments.length > 0 ? arguments[0] : void 0);
      };
    }, {
      get: function get(key) {
        var entry = strong.getEntry(validate(this, MAP), key);
        return entry && entry.v;
      },
      set: function set(key, value) {
        return strong.def(validate(this, MAP), key === 0 ? 0 : key, value);
      }
    }, strong, true);
  });

  // node_modules/core-js/modules/es6.set.js
  var require_es6_set = __commonJS((exports2, module2) => {
    "use strict";
    var strong = require_collection_strong();
    var validate = require_validate_collection();
    var SET = "Set";
    module2.exports = require_collection()(SET, function(get) {
      return function Set() {
        return get(this, arguments.length > 0 ? arguments[0] : void 0);
      };
    }, {
      add: function add(value) {
        return strong.def(validate(this, SET), value = value === 0 ? 0 : value, value);
      }
    }, strong);
  });

  // node_modules/core-js/modules/_collection-weak.js
  var require_collection_weak = __commonJS((exports2, module2) => {
    "use strict";
    var redefineAll = require_redefine_all();
    var getWeak = require_meta().getWeak;
    var anObject = require_an_object();
    var isObject = require_is_object();
    var anInstance = require_an_instance();
    var forOf = require_for_of();
    var createArrayMethod = require_array_methods();
    var $has = require_has();
    var validate = require_validate_collection();
    var arrayFind = createArrayMethod(5);
    var arrayFindIndex = createArrayMethod(6);
    var id = 0;
    var uncaughtFrozenStore = function(that) {
      return that._l || (that._l = new UncaughtFrozenStore());
    };
    var UncaughtFrozenStore = function() {
      this.a = [];
    };
    var findUncaughtFrozen = function(store, key) {
      return arrayFind(store.a, function(it) {
        return it[0] === key;
      });
    };
    UncaughtFrozenStore.prototype = {
      get: function(key) {
        var entry = findUncaughtFrozen(this, key);
        if (entry)
          return entry[1];
      },
      has: function(key) {
        return !!findUncaughtFrozen(this, key);
      },
      set: function(key, value) {
        var entry = findUncaughtFrozen(this, key);
        if (entry)
          entry[1] = value;
        else
          this.a.push([key, value]);
      },
      delete: function(key) {
        var index = arrayFindIndex(this.a, function(it) {
          return it[0] === key;
        });
        if (~index)
          this.a.splice(index, 1);
        return !!~index;
      }
    };
    module2.exports = {
      getConstructor: function(wrapper, NAME, IS_MAP, ADDER) {
        var C = wrapper(function(that, iterable) {
          anInstance(that, C, NAME, "_i");
          that._t = NAME;
          that._i = id++;
          that._l = void 0;
          if (iterable != void 0)
            forOf(iterable, IS_MAP, that[ADDER], that);
        });
        redefineAll(C.prototype, {
          delete: function(key) {
            if (!isObject(key))
              return false;
            var data = getWeak(key);
            if (data === true)
              return uncaughtFrozenStore(validate(this, NAME))["delete"](key);
            return data && $has(data, this._i) && delete data[this._i];
          },
          has: function has(key) {
            if (!isObject(key))
              return false;
            var data = getWeak(key);
            if (data === true)
              return uncaughtFrozenStore(validate(this, NAME)).has(key);
            return data && $has(data, this._i);
          }
        });
        return C;
      },
      def: function(that, key, value) {
        var data = getWeak(anObject(key), true);
        if (data === true)
          uncaughtFrozenStore(that).set(key, value);
        else
          data[that._i] = value;
        return that;
      },
      ufstore: uncaughtFrozenStore
    };
  });

  // node_modules/core-js/modules/es6.weak-map.js
  var require_es6_weak_map = __commonJS((exports2, module2) => {
    "use strict";
    var global2 = require_global();
    var each = require_array_methods()(0);
    var redefine = require_redefine();
    var meta = require_meta();
    var assign = require_object_assign();
    var weak = require_collection_weak();
    var isObject = require_is_object();
    var validate = require_validate_collection();
    var NATIVE_WEAK_MAP = require_validate_collection();
    var IS_IE11 = !global2.ActiveXObject && "ActiveXObject" in global2;
    var WEAK_MAP = "WeakMap";
    var getWeak = meta.getWeak;
    var isExtensible = Object.isExtensible;
    var uncaughtFrozenStore = weak.ufstore;
    var InternalMap;
    var wrapper = function(get) {
      return function WeakMap() {
        return get(this, arguments.length > 0 ? arguments[0] : void 0);
      };
    };
    var methods = {
      get: function get(key) {
        if (isObject(key)) {
          var data = getWeak(key);
          if (data === true)
            return uncaughtFrozenStore(validate(this, WEAK_MAP)).get(key);
          return data ? data[this._i] : void 0;
        }
      },
      set: function set(key, value) {
        return weak.def(validate(this, WEAK_MAP), key, value);
      }
    };
    var $WeakMap = module2.exports = require_collection()(WEAK_MAP, wrapper, methods, weak, true, true);
    if (NATIVE_WEAK_MAP && IS_IE11) {
      InternalMap = weak.getConstructor(wrapper, WEAK_MAP);
      assign(InternalMap.prototype, methods);
      meta.NEED = true;
      each(["delete", "has", "get", "set"], function(key) {
        var proto = $WeakMap.prototype;
        var method = proto[key];
        redefine(proto, key, function(a, b) {
          if (isObject(a) && !isExtensible(a)) {
            if (!this._f)
              this._f = new InternalMap();
            var result = this._f[key](a, b);
            return key == "set" ? this : result;
          }
          return method.call(this, a, b);
        });
      });
    }
  });

  // node_modules/core-js/modules/es6.weak-set.js
  var require_es6_weak_set = __commonJS(() => {
    "use strict";
    var weak = require_collection_weak();
    var validate = require_validate_collection();
    var WEAK_SET = "WeakSet";
    require_collection()(WEAK_SET, function(get) {
      return function WeakSet() {
        return get(this, arguments.length > 0 ? arguments[0] : void 0);
      };
    }, {
      add: function add(value) {
        return weak.def(validate(this, WEAK_SET), value, true);
      }
    }, weak, false, true);
  });

  // node_modules/core-js/modules/_typed.js
  var require_typed = __commonJS((exports2, module2) => {
    var global2 = require_global();
    var hide = require_hide();
    var uid = require_uid();
    var TYPED = uid("typed_array");
    var VIEW = uid("view");
    var ABV = !!(global2.ArrayBuffer && global2.DataView);
    var CONSTR = ABV;
    var i = 0;
    var l = 9;
    var Typed;
    var TypedArrayConstructors = "Int8Array,Uint8Array,Uint8ClampedArray,Int16Array,Uint16Array,Int32Array,Uint32Array,Float32Array,Float64Array".split(",");
    while (i < l) {
      if (Typed = global2[TypedArrayConstructors[i++]]) {
        hide(Typed.prototype, TYPED, true);
        hide(Typed.prototype, VIEW, true);
      } else
        CONSTR = false;
    }
    module2.exports = {
      ABV,
      CONSTR,
      TYPED,
      VIEW
    };
  });

  // node_modules/core-js/modules/_to-index.js
  var require_to_index = __commonJS((exports2, module2) => {
    var toInteger = require_to_integer();
    var toLength = require_to_length();
    module2.exports = function(it) {
      if (it === void 0)
        return 0;
      var number = toInteger(it);
      var length = toLength(number);
      if (number !== length)
        throw RangeError("Wrong length!");
      return length;
    };
  });

  // node_modules/core-js/modules/_typed-buffer.js
  var require_typed_buffer = __commonJS((exports2) => {
    "use strict";
    var global2 = require_global();
    var DESCRIPTORS = require_descriptors();
    var LIBRARY = require_library();
    var $typed = require_typed();
    var hide = require_hide();
    var redefineAll = require_redefine_all();
    var fails = require_fails();
    var anInstance = require_an_instance();
    var toInteger = require_to_integer();
    var toLength = require_to_length();
    var toIndex = require_to_index();
    var gOPN = require_object_gopn().f;
    var dP = require_object_dp().f;
    var arrayFill = require_array_fill();
    var setToStringTag = require_set_to_string_tag();
    var ARRAY_BUFFER = "ArrayBuffer";
    var DATA_VIEW = "DataView";
    var PROTOTYPE = "prototype";
    var WRONG_LENGTH = "Wrong length!";
    var WRONG_INDEX = "Wrong index!";
    var $ArrayBuffer = global2[ARRAY_BUFFER];
    var $DataView = global2[DATA_VIEW];
    var Math2 = global2.Math;
    var RangeError2 = global2.RangeError;
    var Infinity2 = global2.Infinity;
    var BaseBuffer = $ArrayBuffer;
    var abs = Math2.abs;
    var pow = Math2.pow;
    var floor = Math2.floor;
    var log = Math2.log;
    var LN2 = Math2.LN2;
    var BUFFER = "buffer";
    var BYTE_LENGTH = "byteLength";
    var BYTE_OFFSET = "byteOffset";
    var $BUFFER = DESCRIPTORS ? "_b" : BUFFER;
    var $LENGTH = DESCRIPTORS ? "_l" : BYTE_LENGTH;
    var $OFFSET = DESCRIPTORS ? "_o" : BYTE_OFFSET;
    function packIEEE754(value, mLen, nBytes) {
      var buffer = new Array(nBytes);
      var eLen = nBytes * 8 - mLen - 1;
      var eMax = (1 << eLen) - 1;
      var eBias = eMax >> 1;
      var rt = mLen === 23 ? pow(2, -24) - pow(2, -77) : 0;
      var i = 0;
      var s = value < 0 || value === 0 && 1 / value < 0 ? 1 : 0;
      var e, m, c;
      value = abs(value);
      if (value != value || value === Infinity2) {
        m = value != value ? 1 : 0;
        e = eMax;
      } else {
        e = floor(log(value) / LN2);
        if (value * (c = pow(2, -e)) < 1) {
          e--;
          c *= 2;
        }
        if (e + eBias >= 1) {
          value += rt / c;
        } else {
          value += rt * pow(2, 1 - eBias);
        }
        if (value * c >= 2) {
          e++;
          c /= 2;
        }
        if (e + eBias >= eMax) {
          m = 0;
          e = eMax;
        } else if (e + eBias >= 1) {
          m = (value * c - 1) * pow(2, mLen);
          e = e + eBias;
        } else {
          m = value * pow(2, eBias - 1) * pow(2, mLen);
          e = 0;
        }
      }
      for (; mLen >= 8; buffer[i++] = m & 255, m /= 256, mLen -= 8)
        ;
      e = e << mLen | m;
      eLen += mLen;
      for (; eLen > 0; buffer[i++] = e & 255, e /= 256, eLen -= 8)
        ;
      buffer[--i] |= s * 128;
      return buffer;
    }
    function unpackIEEE754(buffer, mLen, nBytes) {
      var eLen = nBytes * 8 - mLen - 1;
      var eMax = (1 << eLen) - 1;
      var eBias = eMax >> 1;
      var nBits = eLen - 7;
      var i = nBytes - 1;
      var s = buffer[i--];
      var e = s & 127;
      var m;
      s >>= 7;
      for (; nBits > 0; e = e * 256 + buffer[i], i--, nBits -= 8)
        ;
      m = e & (1 << -nBits) - 1;
      e >>= -nBits;
      nBits += mLen;
      for (; nBits > 0; m = m * 256 + buffer[i], i--, nBits -= 8)
        ;
      if (e === 0) {
        e = 1 - eBias;
      } else if (e === eMax) {
        return m ? NaN : s ? -Infinity2 : Infinity2;
      } else {
        m = m + pow(2, mLen);
        e = e - eBias;
      }
      return (s ? -1 : 1) * m * pow(2, e - mLen);
    }
    function unpackI32(bytes) {
      return bytes[3] << 24 | bytes[2] << 16 | bytes[1] << 8 | bytes[0];
    }
    function packI8(it) {
      return [it & 255];
    }
    function packI16(it) {
      return [it & 255, it >> 8 & 255];
    }
    function packI32(it) {
      return [it & 255, it >> 8 & 255, it >> 16 & 255, it >> 24 & 255];
    }
    function packF64(it) {
      return packIEEE754(it, 52, 8);
    }
    function packF32(it) {
      return packIEEE754(it, 23, 4);
    }
    function addGetter(C, key2, internal) {
      dP(C[PROTOTYPE], key2, {get: function() {
        return this[internal];
      }});
    }
    function get(view2, bytes, index, isLittleEndian) {
      var numIndex = +index;
      var intIndex = toIndex(numIndex);
      if (intIndex + bytes > view2[$LENGTH])
        throw RangeError2(WRONG_INDEX);
      var store = view2[$BUFFER]._b;
      var start = intIndex + view2[$OFFSET];
      var pack = store.slice(start, start + bytes);
      return isLittleEndian ? pack : pack.reverse();
    }
    function set(view2, bytes, index, conversion, value, isLittleEndian) {
      var numIndex = +index;
      var intIndex = toIndex(numIndex);
      if (intIndex + bytes > view2[$LENGTH])
        throw RangeError2(WRONG_INDEX);
      var store = view2[$BUFFER]._b;
      var start = intIndex + view2[$OFFSET];
      var pack = conversion(+value);
      for (var i = 0; i < bytes; i++)
        store[start + i] = pack[isLittleEndian ? i : bytes - i - 1];
    }
    if (!$typed.ABV) {
      $ArrayBuffer = function ArrayBuffer2(length) {
        anInstance(this, $ArrayBuffer, ARRAY_BUFFER);
        var byteLength = toIndex(length);
        this._b = arrayFill.call(new Array(byteLength), 0);
        this[$LENGTH] = byteLength;
      };
      $DataView = function DataView2(buffer, byteOffset, byteLength) {
        anInstance(this, $DataView, DATA_VIEW);
        anInstance(buffer, $ArrayBuffer, DATA_VIEW);
        var bufferLength = buffer[$LENGTH];
        var offset = toInteger(byteOffset);
        if (offset < 0 || offset > bufferLength)
          throw RangeError2("Wrong offset!");
        byteLength = byteLength === void 0 ? bufferLength - offset : toLength(byteLength);
        if (offset + byteLength > bufferLength)
          throw RangeError2(WRONG_LENGTH);
        this[$BUFFER] = buffer;
        this[$OFFSET] = offset;
        this[$LENGTH] = byteLength;
      };
      if (DESCRIPTORS) {
        addGetter($ArrayBuffer, BYTE_LENGTH, "_l");
        addGetter($DataView, BUFFER, "_b");
        addGetter($DataView, BYTE_LENGTH, "_l");
        addGetter($DataView, BYTE_OFFSET, "_o");
      }
      redefineAll($DataView[PROTOTYPE], {
        getInt8: function getInt8(byteOffset) {
          return get(this, 1, byteOffset)[0] << 24 >> 24;
        },
        getUint8: function getUint8(byteOffset) {
          return get(this, 1, byteOffset)[0];
        },
        getInt16: function getInt16(byteOffset) {
          var bytes = get(this, 2, byteOffset, arguments[1]);
          return (bytes[1] << 8 | bytes[0]) << 16 >> 16;
        },
        getUint16: function getUint16(byteOffset) {
          var bytes = get(this, 2, byteOffset, arguments[1]);
          return bytes[1] << 8 | bytes[0];
        },
        getInt32: function getInt32(byteOffset) {
          return unpackI32(get(this, 4, byteOffset, arguments[1]));
        },
        getUint32: function getUint32(byteOffset) {
          return unpackI32(get(this, 4, byteOffset, arguments[1])) >>> 0;
        },
        getFloat32: function getFloat32(byteOffset) {
          return unpackIEEE754(get(this, 4, byteOffset, arguments[1]), 23, 4);
        },
        getFloat64: function getFloat64(byteOffset) {
          return unpackIEEE754(get(this, 8, byteOffset, arguments[1]), 52, 8);
        },
        setInt8: function setInt8(byteOffset, value) {
          set(this, 1, byteOffset, packI8, value);
        },
        setUint8: function setUint8(byteOffset, value) {
          set(this, 1, byteOffset, packI8, value);
        },
        setInt16: function setInt16(byteOffset, value) {
          set(this, 2, byteOffset, packI16, value, arguments[2]);
        },
        setUint16: function setUint16(byteOffset, value) {
          set(this, 2, byteOffset, packI16, value, arguments[2]);
        },
        setInt32: function setInt32(byteOffset, value) {
          set(this, 4, byteOffset, packI32, value, arguments[2]);
        },
        setUint32: function setUint32(byteOffset, value) {
          set(this, 4, byteOffset, packI32, value, arguments[2]);
        },
        setFloat32: function setFloat32(byteOffset, value) {
          set(this, 4, byteOffset, packF32, value, arguments[2]);
        },
        setFloat64: function setFloat64(byteOffset, value) {
          set(this, 8, byteOffset, packF64, value, arguments[2]);
        }
      });
    } else {
      if (!fails(function() {
        $ArrayBuffer(1);
      }) || !fails(function() {
        new $ArrayBuffer(-1);
      }) || fails(function() {
        new $ArrayBuffer();
        new $ArrayBuffer(1.5);
        new $ArrayBuffer(NaN);
        return $ArrayBuffer.name != ARRAY_BUFFER;
      })) {
        $ArrayBuffer = function ArrayBuffer2(length) {
          anInstance(this, $ArrayBuffer);
          return new BaseBuffer(toIndex(length));
        };
        ArrayBufferProto = $ArrayBuffer[PROTOTYPE] = BaseBuffer[PROTOTYPE];
        for (keys = gOPN(BaseBuffer), j = 0; keys.length > j; ) {
          if (!((key = keys[j++]) in $ArrayBuffer))
            hide($ArrayBuffer, key, BaseBuffer[key]);
        }
        if (!LIBRARY)
          ArrayBufferProto.constructor = $ArrayBuffer;
      }
      view = new $DataView(new $ArrayBuffer(2));
      $setInt8 = $DataView[PROTOTYPE].setInt8;
      view.setInt8(0, 2147483648);
      view.setInt8(1, 2147483649);
      if (view.getInt8(0) || !view.getInt8(1))
        redefineAll($DataView[PROTOTYPE], {
          setInt8: function setInt8(byteOffset, value) {
            $setInt8.call(this, byteOffset, value << 24 >> 24);
          },
          setUint8: function setUint8(byteOffset, value) {
            $setInt8.call(this, byteOffset, value << 24 >> 24);
          }
        }, true);
    }
    var ArrayBufferProto;
    var keys;
    var j;
    var key;
    var view;
    var $setInt8;
    setToStringTag($ArrayBuffer, ARRAY_BUFFER);
    setToStringTag($DataView, DATA_VIEW);
    hide($DataView[PROTOTYPE], $typed.VIEW, true);
    exports2[ARRAY_BUFFER] = $ArrayBuffer;
    exports2[DATA_VIEW] = $DataView;
  });

  // node_modules/core-js/modules/es6.typed.array-buffer.js
  var require_es6_typed_array_buffer = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $typed = require_typed();
    var buffer = require_typed_buffer();
    var anObject = require_an_object();
    var toAbsoluteIndex = require_to_absolute_index();
    var toLength = require_to_length();
    var isObject = require_is_object();
    var ArrayBuffer2 = require_global().ArrayBuffer;
    var speciesConstructor = require_species_constructor();
    var $ArrayBuffer = buffer.ArrayBuffer;
    var $DataView = buffer.DataView;
    var $isView = $typed.ABV && ArrayBuffer2.isView;
    var $slice = $ArrayBuffer.prototype.slice;
    var VIEW = $typed.VIEW;
    var ARRAY_BUFFER = "ArrayBuffer";
    $export($export.G + $export.W + $export.F * (ArrayBuffer2 !== $ArrayBuffer), {ArrayBuffer: $ArrayBuffer});
    $export($export.S + $export.F * !$typed.CONSTR, ARRAY_BUFFER, {
      isView: function isView(it) {
        return $isView && $isView(it) || isObject(it) && VIEW in it;
      }
    });
    $export($export.P + $export.U + $export.F * require_fails()(function() {
      return !new $ArrayBuffer(2).slice(1, void 0).byteLength;
    }), ARRAY_BUFFER, {
      slice: function slice(start, end) {
        if ($slice !== void 0 && end === void 0)
          return $slice.call(anObject(this), start);
        var len = anObject(this).byteLength;
        var first = toAbsoluteIndex(start, len);
        var fin = toAbsoluteIndex(end === void 0 ? len : end, len);
        var result = new (speciesConstructor(this, $ArrayBuffer))(toLength(fin - first));
        var viewS = new $DataView(this);
        var viewT = new $DataView(result);
        var index = 0;
        while (first < fin) {
          viewT.setUint8(index++, viewS.getUint8(first++));
        }
        return result;
      }
    });
    require_set_species()(ARRAY_BUFFER);
  });

  // node_modules/core-js/modules/es6.typed.data-view.js
  var require_es6_typed_data_view = __commonJS(() => {
    var $export = require_export();
    $export($export.G + $export.W + $export.F * !require_typed().ABV, {
      DataView: require_typed_buffer().DataView
    });
  });

  // node_modules/core-js/modules/_typed-array.js
  var require_typed_array = __commonJS((exports2, module2) => {
    "use strict";
    if (require_descriptors()) {
      LIBRARY = require_library();
      global2 = require_global();
      fails = require_fails();
      $export = require_export();
      $typed = require_typed();
      $buffer = require_typed_buffer();
      ctx = require_ctx();
      anInstance = require_an_instance();
      propertyDesc = require_property_desc();
      hide = require_hide();
      redefineAll = require_redefine_all();
      toInteger = require_to_integer();
      toLength = require_to_length();
      toIndex = require_to_index();
      toAbsoluteIndex = require_to_absolute_index();
      toPrimitive = require_to_primitive();
      has = require_has();
      classof = require_classof();
      isObject = require_is_object();
      toObject = require_to_object();
      isArrayIter = require_is_array_iter();
      create = require_object_create();
      getPrototypeOf = require_object_gpo();
      gOPN = require_object_gopn().f;
      getIterFn = require_core_get_iterator_method();
      uid = require_uid();
      wks = require_wks();
      createArrayMethod = require_array_methods();
      createArrayIncludes = require_array_includes();
      speciesConstructor = require_species_constructor();
      ArrayIterators = require_es6_array_iterator();
      Iterators = require_iterators();
      $iterDetect = require_iter_detect();
      setSpecies = require_set_species();
      arrayFill = require_array_fill();
      arrayCopyWithin = require_array_copy_within();
      $DP = require_object_dp();
      $GOPD = require_object_gopd();
      dP = $DP.f;
      gOPD = $GOPD.f;
      RangeError2 = global2.RangeError;
      TypeError2 = global2.TypeError;
      Uint8Array2 = global2.Uint8Array;
      ARRAY_BUFFER = "ArrayBuffer";
      SHARED_BUFFER = "Shared" + ARRAY_BUFFER;
      BYTES_PER_ELEMENT = "BYTES_PER_ELEMENT";
      PROTOTYPE = "prototype";
      ArrayProto = Array[PROTOTYPE];
      $ArrayBuffer = $buffer.ArrayBuffer;
      $DataView = $buffer.DataView;
      arrayForEach = createArrayMethod(0);
      arrayFilter = createArrayMethod(2);
      arraySome = createArrayMethod(3);
      arrayEvery = createArrayMethod(4);
      arrayFind = createArrayMethod(5);
      arrayFindIndex = createArrayMethod(6);
      arrayIncludes = createArrayIncludes(true);
      arrayIndexOf = createArrayIncludes(false);
      arrayValues = ArrayIterators.values;
      arrayKeys = ArrayIterators.keys;
      arrayEntries = ArrayIterators.entries;
      arrayLastIndexOf = ArrayProto.lastIndexOf;
      arrayReduce = ArrayProto.reduce;
      arrayReduceRight = ArrayProto.reduceRight;
      arrayJoin = ArrayProto.join;
      arraySort = ArrayProto.sort;
      arraySlice = ArrayProto.slice;
      arrayToString = ArrayProto.toString;
      arrayToLocaleString = ArrayProto.toLocaleString;
      ITERATOR = wks("iterator");
      TAG = wks("toStringTag");
      TYPED_CONSTRUCTOR = uid("typed_constructor");
      DEF_CONSTRUCTOR = uid("def_constructor");
      ALL_CONSTRUCTORS = $typed.CONSTR;
      TYPED_ARRAY = $typed.TYPED;
      VIEW = $typed.VIEW;
      WRONG_LENGTH = "Wrong length!";
      $map = createArrayMethod(1, function(O, length) {
        return allocate(speciesConstructor(O, O[DEF_CONSTRUCTOR]), length);
      });
      LITTLE_ENDIAN = fails(function() {
        return new Uint8Array2(new Uint16Array([1]).buffer)[0] === 1;
      });
      FORCED_SET = !!Uint8Array2 && !!Uint8Array2[PROTOTYPE].set && fails(function() {
        new Uint8Array2(1).set({});
      });
      toOffset = function(it, BYTES) {
        var offset = toInteger(it);
        if (offset < 0 || offset % BYTES)
          throw RangeError2("Wrong offset!");
        return offset;
      };
      validate = function(it) {
        if (isObject(it) && TYPED_ARRAY in it)
          return it;
        throw TypeError2(it + " is not a typed array!");
      };
      allocate = function(C, length) {
        if (!(isObject(C) && TYPED_CONSTRUCTOR in C)) {
          throw TypeError2("It is not a typed array constructor!");
        }
        return new C(length);
      };
      speciesFromList = function(O, list) {
        return fromList(speciesConstructor(O, O[DEF_CONSTRUCTOR]), list);
      };
      fromList = function(C, list) {
        var index = 0;
        var length = list.length;
        var result = allocate(C, length);
        while (length > index)
          result[index] = list[index++];
        return result;
      };
      addGetter = function(it, key, internal) {
        dP(it, key, {get: function() {
          return this._d[internal];
        }});
      };
      $from = function from(source) {
        var O = toObject(source);
        var aLen = arguments.length;
        var mapfn = aLen > 1 ? arguments[1] : void 0;
        var mapping = mapfn !== void 0;
        var iterFn = getIterFn(O);
        var i, length, values, result, step, iterator;
        if (iterFn != void 0 && !isArrayIter(iterFn)) {
          for (iterator = iterFn.call(O), values = [], i = 0; !(step = iterator.next()).done; i++) {
            values.push(step.value);
          }
          O = values;
        }
        if (mapping && aLen > 2)
          mapfn = ctx(mapfn, arguments[2], 2);
        for (i = 0, length = toLength(O.length), result = allocate(this, length); length > i; i++) {
          result[i] = mapping ? mapfn(O[i], i) : O[i];
        }
        return result;
      };
      $of = function of() {
        var index = 0;
        var length = arguments.length;
        var result = allocate(this, length);
        while (length > index)
          result[index] = arguments[index++];
        return result;
      };
      TO_LOCALE_BUG = !!Uint8Array2 && fails(function() {
        arrayToLocaleString.call(new Uint8Array2(1));
      });
      $toLocaleString = function toLocaleString() {
        return arrayToLocaleString.apply(TO_LOCALE_BUG ? arraySlice.call(validate(this)) : validate(this), arguments);
      };
      proto = {
        copyWithin: function copyWithin(target, start) {
          return arrayCopyWithin.call(validate(this), target, start, arguments.length > 2 ? arguments[2] : void 0);
        },
        every: function every(callbackfn) {
          return arrayEvery(validate(this), callbackfn, arguments.length > 1 ? arguments[1] : void 0);
        },
        fill: function fill(value) {
          return arrayFill.apply(validate(this), arguments);
        },
        filter: function filter(callbackfn) {
          return speciesFromList(this, arrayFilter(validate(this), callbackfn, arguments.length > 1 ? arguments[1] : void 0));
        },
        find: function find(predicate) {
          return arrayFind(validate(this), predicate, arguments.length > 1 ? arguments[1] : void 0);
        },
        findIndex: function findIndex(predicate) {
          return arrayFindIndex(validate(this), predicate, arguments.length > 1 ? arguments[1] : void 0);
        },
        forEach: function forEach(callbackfn) {
          arrayForEach(validate(this), callbackfn, arguments.length > 1 ? arguments[1] : void 0);
        },
        indexOf: function indexOf(searchElement) {
          return arrayIndexOf(validate(this), searchElement, arguments.length > 1 ? arguments[1] : void 0);
        },
        includes: function includes(searchElement) {
          return arrayIncludes(validate(this), searchElement, arguments.length > 1 ? arguments[1] : void 0);
        },
        join: function join(separator) {
          return arrayJoin.apply(validate(this), arguments);
        },
        lastIndexOf: function lastIndexOf(searchElement) {
          return arrayLastIndexOf.apply(validate(this), arguments);
        },
        map: function map(mapfn) {
          return $map(validate(this), mapfn, arguments.length > 1 ? arguments[1] : void 0);
        },
        reduce: function reduce(callbackfn) {
          return arrayReduce.apply(validate(this), arguments);
        },
        reduceRight: function reduceRight(callbackfn) {
          return arrayReduceRight.apply(validate(this), arguments);
        },
        reverse: function reverse() {
          var that = this;
          var length = validate(that).length;
          var middle = Math.floor(length / 2);
          var index = 0;
          var value;
          while (index < middle) {
            value = that[index];
            that[index++] = that[--length];
            that[length] = value;
          }
          return that;
        },
        some: function some(callbackfn) {
          return arraySome(validate(this), callbackfn, arguments.length > 1 ? arguments[1] : void 0);
        },
        sort: function sort(comparefn) {
          return arraySort.call(validate(this), comparefn);
        },
        subarray: function subarray(begin, end) {
          var O = validate(this);
          var length = O.length;
          var $begin = toAbsoluteIndex(begin, length);
          return new (speciesConstructor(O, O[DEF_CONSTRUCTOR]))(O.buffer, O.byteOffset + $begin * O.BYTES_PER_ELEMENT, toLength((end === void 0 ? length : toAbsoluteIndex(end, length)) - $begin));
        }
      };
      $slice = function slice(start, end) {
        return speciesFromList(this, arraySlice.call(validate(this), start, end));
      };
      $set = function set(arrayLike) {
        validate(this);
        var offset = toOffset(arguments[1], 1);
        var length = this.length;
        var src = toObject(arrayLike);
        var len = toLength(src.length);
        var index = 0;
        if (len + offset > length)
          throw RangeError2(WRONG_LENGTH);
        while (index < len)
          this[offset + index] = src[index++];
      };
      $iterators = {
        entries: function entries() {
          return arrayEntries.call(validate(this));
        },
        keys: function keys() {
          return arrayKeys.call(validate(this));
        },
        values: function values() {
          return arrayValues.call(validate(this));
        }
      };
      isTAIndex = function(target, key) {
        return isObject(target) && target[TYPED_ARRAY] && typeof key != "symbol" && key in target && String(+key) == String(key);
      };
      $getDesc = function getOwnPropertyDescriptor(target, key) {
        return isTAIndex(target, key = toPrimitive(key, true)) ? propertyDesc(2, target[key]) : gOPD(target, key);
      };
      $setDesc = function defineProperty(target, key, desc) {
        if (isTAIndex(target, key = toPrimitive(key, true)) && isObject(desc) && has(desc, "value") && !has(desc, "get") && !has(desc, "set") && !desc.configurable && (!has(desc, "writable") || desc.writable) && (!has(desc, "enumerable") || desc.enumerable)) {
          target[key] = desc.value;
          return target;
        }
        return dP(target, key, desc);
      };
      if (!ALL_CONSTRUCTORS) {
        $GOPD.f = $getDesc;
        $DP.f = $setDesc;
      }
      $export($export.S + $export.F * !ALL_CONSTRUCTORS, "Object", {
        getOwnPropertyDescriptor: $getDesc,
        defineProperty: $setDesc
      });
      if (fails(function() {
        arrayToString.call({});
      })) {
        arrayToString = arrayToLocaleString = function toString() {
          return arrayJoin.call(this);
        };
      }
      $TypedArrayPrototype$ = redefineAll({}, proto);
      redefineAll($TypedArrayPrototype$, $iterators);
      hide($TypedArrayPrototype$, ITERATOR, $iterators.values);
      redefineAll($TypedArrayPrototype$, {
        slice: $slice,
        set: $set,
        constructor: function() {
        },
        toString: arrayToString,
        toLocaleString: $toLocaleString
      });
      addGetter($TypedArrayPrototype$, "buffer", "b");
      addGetter($TypedArrayPrototype$, "byteOffset", "o");
      addGetter($TypedArrayPrototype$, "byteLength", "l");
      addGetter($TypedArrayPrototype$, "length", "e");
      dP($TypedArrayPrototype$, TAG, {
        get: function() {
          return this[TYPED_ARRAY];
        }
      });
      module2.exports = function(KEY, BYTES, wrapper, CLAMPED) {
        CLAMPED = !!CLAMPED;
        var NAME = KEY + (CLAMPED ? "Clamped" : "") + "Array";
        var GETTER = "get" + KEY;
        var SETTER = "set" + KEY;
        var TypedArray = global2[NAME];
        var Base = TypedArray || {};
        var TAC = TypedArray && getPrototypeOf(TypedArray);
        var FORCED = !TypedArray || !$typed.ABV;
        var O = {};
        var TypedArrayPrototype = TypedArray && TypedArray[PROTOTYPE];
        var getter = function(that, index) {
          var data = that._d;
          return data.v[GETTER](index * BYTES + data.o, LITTLE_ENDIAN);
        };
        var setter = function(that, index, value) {
          var data = that._d;
          if (CLAMPED)
            value = (value = Math.round(value)) < 0 ? 0 : value > 255 ? 255 : value & 255;
          data.v[SETTER](index * BYTES + data.o, value, LITTLE_ENDIAN);
        };
        var addElement = function(that, index) {
          dP(that, index, {
            get: function() {
              return getter(this, index);
            },
            set: function(value) {
              return setter(this, index, value);
            },
            enumerable: true
          });
        };
        if (FORCED) {
          TypedArray = wrapper(function(that, data, $offset, $length) {
            anInstance(that, TypedArray, NAME, "_d");
            var index = 0;
            var offset = 0;
            var buffer, byteLength, length, klass;
            if (!isObject(data)) {
              length = toIndex(data);
              byteLength = length * BYTES;
              buffer = new $ArrayBuffer(byteLength);
            } else if (data instanceof $ArrayBuffer || (klass = classof(data)) == ARRAY_BUFFER || klass == SHARED_BUFFER) {
              buffer = data;
              offset = toOffset($offset, BYTES);
              var $len = data.byteLength;
              if ($length === void 0) {
                if ($len % BYTES)
                  throw RangeError2(WRONG_LENGTH);
                byteLength = $len - offset;
                if (byteLength < 0)
                  throw RangeError2(WRONG_LENGTH);
              } else {
                byteLength = toLength($length) * BYTES;
                if (byteLength + offset > $len)
                  throw RangeError2(WRONG_LENGTH);
              }
              length = byteLength / BYTES;
            } else if (TYPED_ARRAY in data) {
              return fromList(TypedArray, data);
            } else {
              return $from.call(TypedArray, data);
            }
            hide(that, "_d", {
              b: buffer,
              o: offset,
              l: byteLength,
              e: length,
              v: new $DataView(buffer)
            });
            while (index < length)
              addElement(that, index++);
          });
          TypedArrayPrototype = TypedArray[PROTOTYPE] = create($TypedArrayPrototype$);
          hide(TypedArrayPrototype, "constructor", TypedArray);
        } else if (!fails(function() {
          TypedArray(1);
        }) || !fails(function() {
          new TypedArray(-1);
        }) || !$iterDetect(function(iter) {
          new TypedArray();
          new TypedArray(null);
          new TypedArray(1.5);
          new TypedArray(iter);
        }, true)) {
          TypedArray = wrapper(function(that, data, $offset, $length) {
            anInstance(that, TypedArray, NAME);
            var klass;
            if (!isObject(data))
              return new Base(toIndex(data));
            if (data instanceof $ArrayBuffer || (klass = classof(data)) == ARRAY_BUFFER || klass == SHARED_BUFFER) {
              return $length !== void 0 ? new Base(data, toOffset($offset, BYTES), $length) : $offset !== void 0 ? new Base(data, toOffset($offset, BYTES)) : new Base(data);
            }
            if (TYPED_ARRAY in data)
              return fromList(TypedArray, data);
            return $from.call(TypedArray, data);
          });
          arrayForEach(TAC !== Function.prototype ? gOPN(Base).concat(gOPN(TAC)) : gOPN(Base), function(key) {
            if (!(key in TypedArray))
              hide(TypedArray, key, Base[key]);
          });
          TypedArray[PROTOTYPE] = TypedArrayPrototype;
          if (!LIBRARY)
            TypedArrayPrototype.constructor = TypedArray;
        }
        var $nativeIterator = TypedArrayPrototype[ITERATOR];
        var CORRECT_ITER_NAME = !!$nativeIterator && ($nativeIterator.name == "values" || $nativeIterator.name == void 0);
        var $iterator = $iterators.values;
        hide(TypedArray, TYPED_CONSTRUCTOR, true);
        hide(TypedArrayPrototype, TYPED_ARRAY, NAME);
        hide(TypedArrayPrototype, VIEW, true);
        hide(TypedArrayPrototype, DEF_CONSTRUCTOR, TypedArray);
        if (CLAMPED ? new TypedArray(1)[TAG] != NAME : !(TAG in TypedArrayPrototype)) {
          dP(TypedArrayPrototype, TAG, {
            get: function() {
              return NAME;
            }
          });
        }
        O[NAME] = TypedArray;
        $export($export.G + $export.W + $export.F * (TypedArray != Base), O);
        $export($export.S, NAME, {
          BYTES_PER_ELEMENT: BYTES
        });
        $export($export.S + $export.F * fails(function() {
          Base.of.call(TypedArray, 1);
        }), NAME, {
          from: $from,
          of: $of
        });
        if (!(BYTES_PER_ELEMENT in TypedArrayPrototype))
          hide(TypedArrayPrototype, BYTES_PER_ELEMENT, BYTES);
        $export($export.P, NAME, proto);
        setSpecies(NAME);
        $export($export.P + $export.F * FORCED_SET, NAME, {set: $set});
        $export($export.P + $export.F * !CORRECT_ITER_NAME, NAME, $iterators);
        if (!LIBRARY && TypedArrayPrototype.toString != arrayToString)
          TypedArrayPrototype.toString = arrayToString;
        $export($export.P + $export.F * fails(function() {
          new TypedArray(1).slice();
        }), NAME, {slice: $slice});
        $export($export.P + $export.F * (fails(function() {
          return [1, 2].toLocaleString() != new TypedArray([1, 2]).toLocaleString();
        }) || !fails(function() {
          TypedArrayPrototype.toLocaleString.call([1, 2]);
        })), NAME, {toLocaleString: $toLocaleString});
        Iterators[NAME] = CORRECT_ITER_NAME ? $nativeIterator : $iterator;
        if (!LIBRARY && !CORRECT_ITER_NAME)
          hide(TypedArrayPrototype, ITERATOR, $iterator);
      };
    } else
      module2.exports = function() {
      };
    var LIBRARY;
    var global2;
    var fails;
    var $export;
    var $typed;
    var $buffer;
    var ctx;
    var anInstance;
    var propertyDesc;
    var hide;
    var redefineAll;
    var toInteger;
    var toLength;
    var toIndex;
    var toAbsoluteIndex;
    var toPrimitive;
    var has;
    var classof;
    var isObject;
    var toObject;
    var isArrayIter;
    var create;
    var getPrototypeOf;
    var gOPN;
    var getIterFn;
    var uid;
    var wks;
    var createArrayMethod;
    var createArrayIncludes;
    var speciesConstructor;
    var ArrayIterators;
    var Iterators;
    var $iterDetect;
    var setSpecies;
    var arrayFill;
    var arrayCopyWithin;
    var $DP;
    var $GOPD;
    var dP;
    var gOPD;
    var RangeError2;
    var TypeError2;
    var Uint8Array2;
    var ARRAY_BUFFER;
    var SHARED_BUFFER;
    var BYTES_PER_ELEMENT;
    var PROTOTYPE;
    var ArrayProto;
    var $ArrayBuffer;
    var $DataView;
    var arrayForEach;
    var arrayFilter;
    var arraySome;
    var arrayEvery;
    var arrayFind;
    var arrayFindIndex;
    var arrayIncludes;
    var arrayIndexOf;
    var arrayValues;
    var arrayKeys;
    var arrayEntries;
    var arrayLastIndexOf;
    var arrayReduce;
    var arrayReduceRight;
    var arrayJoin;
    var arraySort;
    var arraySlice;
    var arrayToString;
    var arrayToLocaleString;
    var ITERATOR;
    var TAG;
    var TYPED_CONSTRUCTOR;
    var DEF_CONSTRUCTOR;
    var ALL_CONSTRUCTORS;
    var TYPED_ARRAY;
    var VIEW;
    var WRONG_LENGTH;
    var $map;
    var LITTLE_ENDIAN;
    var FORCED_SET;
    var toOffset;
    var validate;
    var allocate;
    var speciesFromList;
    var fromList;
    var addGetter;
    var $from;
    var $of;
    var TO_LOCALE_BUG;
    var $toLocaleString;
    var proto;
    var $slice;
    var $set;
    var $iterators;
    var isTAIndex;
    var $getDesc;
    var $setDesc;
    var $TypedArrayPrototype$;
  });

  // node_modules/core-js/modules/es6.typed.int8-array.js
  var require_es6_typed_int8_array = __commonJS(() => {
    require_typed_array()("Int8", 1, function(init) {
      return function Int8Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.uint8-array.js
  var require_es6_typed_uint8_array = __commonJS(() => {
    require_typed_array()("Uint8", 1, function(init) {
      return function Uint8Array2(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.uint8-clamped-array.js
  var require_es6_typed_uint8_clamped_array = __commonJS(() => {
    require_typed_array()("Uint8", 1, function(init) {
      return function Uint8ClampedArray(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    }, true);
  });

  // node_modules/core-js/modules/es6.typed.int16-array.js
  var require_es6_typed_int16_array = __commonJS(() => {
    require_typed_array()("Int16", 2, function(init) {
      return function Int16Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.uint16-array.js
  var require_es6_typed_uint16_array = __commonJS(() => {
    require_typed_array()("Uint16", 2, function(init) {
      return function Uint16Array2(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.int32-array.js
  var require_es6_typed_int32_array = __commonJS(() => {
    require_typed_array()("Int32", 4, function(init) {
      return function Int32Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.uint32-array.js
  var require_es6_typed_uint32_array = __commonJS(() => {
    require_typed_array()("Uint32", 4, function(init) {
      return function Uint32Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.float32-array.js
  var require_es6_typed_float32_array = __commonJS(() => {
    require_typed_array()("Float32", 4, function(init) {
      return function Float32Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.typed.float64-array.js
  var require_es6_typed_float64_array = __commonJS(() => {
    require_typed_array()("Float64", 8, function(init) {
      return function Float64Array(data, byteOffset, length) {
        return init(this, data, byteOffset, length);
      };
    });
  });

  // node_modules/core-js/modules/es6.reflect.apply.js
  var require_es6_reflect_apply = __commonJS(() => {
    var $export = require_export();
    var aFunction = require_a_function();
    var anObject = require_an_object();
    var rApply = (require_global().Reflect || {}).apply;
    var fApply = Function.apply;
    $export($export.S + $export.F * !require_fails()(function() {
      rApply(function() {
      });
    }), "Reflect", {
      apply: function apply(target, thisArgument, argumentsList) {
        var T = aFunction(target);
        var L = anObject(argumentsList);
        return rApply ? rApply(T, thisArgument, L) : fApply.call(T, thisArgument, L);
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.construct.js
  var require_es6_reflect_construct = __commonJS(() => {
    var $export = require_export();
    var create = require_object_create();
    var aFunction = require_a_function();
    var anObject = require_an_object();
    var isObject = require_is_object();
    var fails = require_fails();
    var bind = require_bind();
    var rConstruct = (require_global().Reflect || {}).construct;
    var NEW_TARGET_BUG = fails(function() {
      function F() {
      }
      return !(rConstruct(function() {
      }, [], F) instanceof F);
    });
    var ARGS_BUG = !fails(function() {
      rConstruct(function() {
      });
    });
    $export($export.S + $export.F * (NEW_TARGET_BUG || ARGS_BUG), "Reflect", {
      construct: function construct(Target, args) {
        aFunction(Target);
        anObject(args);
        var newTarget = arguments.length < 3 ? Target : aFunction(arguments[2]);
        if (ARGS_BUG && !NEW_TARGET_BUG)
          return rConstruct(Target, args, newTarget);
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
        var instance = create(isObject(proto) ? proto : Object.prototype);
        var result = Function.apply.call(Target, instance, args);
        return isObject(result) ? result : instance;
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.define-property.js
  var require_es6_reflect_define_property = __commonJS(() => {
    var dP = require_object_dp();
    var $export = require_export();
    var anObject = require_an_object();
    var toPrimitive = require_to_primitive();
    $export($export.S + $export.F * require_fails()(function() {
      Reflect.defineProperty(dP.f({}, 1, {value: 1}), 1, {value: 2});
    }), "Reflect", {
      defineProperty: function defineProperty(target, propertyKey, attributes) {
        anObject(target);
        propertyKey = toPrimitive(propertyKey, true);
        anObject(attributes);
        try {
          dP.f(target, propertyKey, attributes);
          return true;
        } catch (e) {
          return false;
        }
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.delete-property.js
  var require_es6_reflect_delete_property = __commonJS(() => {
    var $export = require_export();
    var gOPD = require_object_gopd().f;
    var anObject = require_an_object();
    $export($export.S, "Reflect", {
      deleteProperty: function deleteProperty(target, propertyKey) {
        var desc = gOPD(anObject(target), propertyKey);
        return desc && !desc.configurable ? false : delete target[propertyKey];
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.enumerate.js
  var require_es6_reflect_enumerate = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var anObject = require_an_object();
    var Enumerate = function(iterated) {
      this._t = anObject(iterated);
      this._i = 0;
      var keys = this._k = [];
      var key;
      for (key in iterated)
        keys.push(key);
    };
    require_iter_create()(Enumerate, "Object", function() {
      var that = this;
      var keys = that._k;
      var key;
      do {
        if (that._i >= keys.length)
          return {value: void 0, done: true};
      } while (!((key = keys[that._i++]) in that._t));
      return {value: key, done: false};
    });
    $export($export.S, "Reflect", {
      enumerate: function enumerate(target) {
        return new Enumerate(target);
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.get.js
  var require_es6_reflect_get = __commonJS(() => {
    var gOPD = require_object_gopd();
    var getPrototypeOf = require_object_gpo();
    var has = require_has();
    var $export = require_export();
    var isObject = require_is_object();
    var anObject = require_an_object();
    function get(target, propertyKey) {
      var receiver = arguments.length < 3 ? target : arguments[2];
      var desc, proto;
      if (anObject(target) === receiver)
        return target[propertyKey];
      if (desc = gOPD.f(target, propertyKey))
        return has(desc, "value") ? desc.value : desc.get !== void 0 ? desc.get.call(receiver) : void 0;
      if (isObject(proto = getPrototypeOf(target)))
        return get(proto, propertyKey, receiver);
    }
    $export($export.S, "Reflect", {get});
  });

  // node_modules/core-js/modules/es6.reflect.get-own-property-descriptor.js
  var require_es6_reflect_get_own_property_descriptor = __commonJS(() => {
    var gOPD = require_object_gopd();
    var $export = require_export();
    var anObject = require_an_object();
    $export($export.S, "Reflect", {
      getOwnPropertyDescriptor: function getOwnPropertyDescriptor(target, propertyKey) {
        return gOPD.f(anObject(target), propertyKey);
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.get-prototype-of.js
  var require_es6_reflect_get_prototype_of = __commonJS(() => {
    var $export = require_export();
    var getProto = require_object_gpo();
    var anObject = require_an_object();
    $export($export.S, "Reflect", {
      getPrototypeOf: function getPrototypeOf(target) {
        return getProto(anObject(target));
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.has.js
  var require_es6_reflect_has = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Reflect", {
      has: function has(target, propertyKey) {
        return propertyKey in target;
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.is-extensible.js
  var require_es6_reflect_is_extensible = __commonJS(() => {
    var $export = require_export();
    var anObject = require_an_object();
    var $isExtensible = Object.isExtensible;
    $export($export.S, "Reflect", {
      isExtensible: function isExtensible(target) {
        anObject(target);
        return $isExtensible ? $isExtensible(target) : true;
      }
    });
  });

  // node_modules/core-js/modules/_own-keys.js
  var require_own_keys = __commonJS((exports2, module2) => {
    var gOPN = require_object_gopn();
    var gOPS = require_object_gops();
    var anObject = require_an_object();
    var Reflect2 = require_global().Reflect;
    module2.exports = Reflect2 && Reflect2.ownKeys || function ownKeys(it) {
      var keys = gOPN.f(anObject(it));
      var getSymbols = gOPS.f;
      return getSymbols ? keys.concat(getSymbols(it)) : keys;
    };
  });

  // node_modules/core-js/modules/es6.reflect.own-keys.js
  var require_es6_reflect_own_keys = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Reflect", {ownKeys: require_own_keys()});
  });

  // node_modules/core-js/modules/es6.reflect.prevent-extensions.js
  var require_es6_reflect_prevent_extensions = __commonJS(() => {
    var $export = require_export();
    var anObject = require_an_object();
    var $preventExtensions = Object.preventExtensions;
    $export($export.S, "Reflect", {
      preventExtensions: function preventExtensions(target) {
        anObject(target);
        try {
          if ($preventExtensions)
            $preventExtensions(target);
          return true;
        } catch (e) {
          return false;
        }
      }
    });
  });

  // node_modules/core-js/modules/es6.reflect.set.js
  var require_es6_reflect_set = __commonJS(() => {
    var dP = require_object_dp();
    var gOPD = require_object_gopd();
    var getPrototypeOf = require_object_gpo();
    var has = require_has();
    var $export = require_export();
    var createDesc = require_property_desc();
    var anObject = require_an_object();
    var isObject = require_is_object();
    function set(target, propertyKey, V) {
      var receiver = arguments.length < 4 ? target : arguments[3];
      var ownDesc = gOPD.f(anObject(target), propertyKey);
      var existingDescriptor, proto;
      if (!ownDesc) {
        if (isObject(proto = getPrototypeOf(target))) {
          return set(proto, propertyKey, V, receiver);
        }
        ownDesc = createDesc(0);
      }
      if (has(ownDesc, "value")) {
        if (ownDesc.writable === false || !isObject(receiver))
          return false;
        if (existingDescriptor = gOPD.f(receiver, propertyKey)) {
          if (existingDescriptor.get || existingDescriptor.set || existingDescriptor.writable === false)
            return false;
          existingDescriptor.value = V;
          dP.f(receiver, propertyKey, existingDescriptor);
        } else
          dP.f(receiver, propertyKey, createDesc(0, V));
        return true;
      }
      return ownDesc.set === void 0 ? false : (ownDesc.set.call(receiver, V), true);
    }
    $export($export.S, "Reflect", {set});
  });

  // node_modules/core-js/modules/es6.reflect.set-prototype-of.js
  var require_es6_reflect_set_prototype_of = __commonJS(() => {
    var $export = require_export();
    var setProto = require_set_proto();
    if (setProto)
      $export($export.S, "Reflect", {
        setPrototypeOf: function setPrototypeOf(target, proto) {
          setProto.check(target, proto);
          try {
            setProto.set(target, proto);
            return true;
          } catch (e) {
            return false;
          }
        }
      });
  });

  // node_modules/core-js/modules/es7.array.includes.js
  var require_es7_array_includes = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $includes = require_array_includes()(true);
    $export($export.P, "Array", {
      includes: function includes(el) {
        return $includes(this, el, arguments.length > 1 ? arguments[1] : void 0);
      }
    });
    require_add_to_unscopables()("includes");
  });

  // node_modules/core-js/modules/_flatten-into-array.js
  var require_flatten_into_array = __commonJS((exports2, module2) => {
    "use strict";
    var isArray = require_is_array();
    var isObject = require_is_object();
    var toLength = require_to_length();
    var ctx = require_ctx();
    var IS_CONCAT_SPREADABLE = require_wks()("isConcatSpreadable");
    function flattenIntoArray(target, original, source, sourceLen, start, depth, mapper, thisArg) {
      var targetIndex = start;
      var sourceIndex = 0;
      var mapFn = mapper ? ctx(mapper, thisArg, 3) : false;
      var element, spreadable;
      while (sourceIndex < sourceLen) {
        if (sourceIndex in source) {
          element = mapFn ? mapFn(source[sourceIndex], sourceIndex, original) : source[sourceIndex];
          spreadable = false;
          if (isObject(element)) {
            spreadable = element[IS_CONCAT_SPREADABLE];
            spreadable = spreadable !== void 0 ? !!spreadable : isArray(element);
          }
          if (spreadable && depth > 0) {
            targetIndex = flattenIntoArray(target, original, element, toLength(element.length), targetIndex, depth - 1) - 1;
          } else {
            if (targetIndex >= 9007199254740991)
              throw TypeError();
            target[targetIndex] = element;
          }
          targetIndex++;
        }
        sourceIndex++;
      }
      return targetIndex;
    }
    module2.exports = flattenIntoArray;
  });

  // node_modules/core-js/modules/es7.array.flat-map.js
  var require_es7_array_flat_map = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var flattenIntoArray = require_flatten_into_array();
    var toObject = require_to_object();
    var toLength = require_to_length();
    var aFunction = require_a_function();
    var arraySpeciesCreate = require_array_species_create();
    $export($export.P, "Array", {
      flatMap: function flatMap(callbackfn) {
        var O = toObject(this);
        var sourceLen, A;
        aFunction(callbackfn);
        sourceLen = toLength(O.length);
        A = arraySpeciesCreate(O, 0);
        flattenIntoArray(A, O, O, sourceLen, 0, 1, callbackfn, arguments[1]);
        return A;
      }
    });
    require_add_to_unscopables()("flatMap");
  });

  // node_modules/core-js/modules/es7.array.flatten.js
  var require_es7_array_flatten = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var flattenIntoArray = require_flatten_into_array();
    var toObject = require_to_object();
    var toLength = require_to_length();
    var toInteger = require_to_integer();
    var arraySpeciesCreate = require_array_species_create();
    $export($export.P, "Array", {
      flatten: function flatten() {
        var depthArg = arguments[0];
        var O = toObject(this);
        var sourceLen = toLength(O.length);
        var A = arraySpeciesCreate(O, 0);
        flattenIntoArray(A, O, O, sourceLen, 0, depthArg === void 0 ? 1 : toInteger(depthArg));
        return A;
      }
    });
    require_add_to_unscopables()("flatten");
  });

  // node_modules/core-js/modules/es7.string.at.js
  var require_es7_string_at = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $at = require_string_at()(true);
    var $fails = require_fails();
    var FORCED = $fails(function() {
      return "\u{20BB7}".at(0) !== "\u{20BB7}";
    });
    $export($export.P + $export.F * FORCED, "String", {
      at: function at(pos) {
        return $at(this, pos);
      }
    });
  });

  // node_modules/core-js/modules/_string-pad.js
  var require_string_pad = __commonJS((exports2, module2) => {
    var toLength = require_to_length();
    var repeat = require_string_repeat();
    var defined = require_defined();
    module2.exports = function(that, maxLength, fillString, left) {
      var S = String(defined(that));
      var stringLength = S.length;
      var fillStr = fillString === void 0 ? " " : String(fillString);
      var intMaxLength = toLength(maxLength);
      if (intMaxLength <= stringLength || fillStr == "")
        return S;
      var fillLen = intMaxLength - stringLength;
      var stringFiller = repeat.call(fillStr, Math.ceil(fillLen / fillStr.length));
      if (stringFiller.length > fillLen)
        stringFiller = stringFiller.slice(0, fillLen);
      return left ? stringFiller + S : S + stringFiller;
    };
  });

  // node_modules/core-js/modules/es7.string.pad-start.js
  var require_es7_string_pad_start = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $pad = require_string_pad();
    var userAgent2 = require_user_agent();
    var WEBKIT_BUG = /Version\/10\.\d+(\.\d+)?( Mobile\/\w+)? Safari\//.test(userAgent2);
    $export($export.P + $export.F * WEBKIT_BUG, "String", {
      padStart: function padStart(maxLength) {
        return $pad(this, maxLength, arguments.length > 1 ? arguments[1] : void 0, true);
      }
    });
  });

  // node_modules/core-js/modules/es7.string.pad-end.js
  var require_es7_string_pad_end = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var $pad = require_string_pad();
    var userAgent2 = require_user_agent();
    var WEBKIT_BUG = /Version\/10\.\d+(\.\d+)?( Mobile\/\w+)? Safari\//.test(userAgent2);
    $export($export.P + $export.F * WEBKIT_BUG, "String", {
      padEnd: function padEnd(maxLength) {
        return $pad(this, maxLength, arguments.length > 1 ? arguments[1] : void 0, false);
      }
    });
  });

  // node_modules/core-js/modules/es7.string.trim-left.js
  var require_es7_string_trim_left = __commonJS(() => {
    "use strict";
    require_string_trim()("trimLeft", function($trim) {
      return function trimLeft() {
        return $trim(this, 1);
      };
    }, "trimStart");
  });

  // node_modules/core-js/modules/es7.string.trim-right.js
  var require_es7_string_trim_right = __commonJS(() => {
    "use strict";
    require_string_trim()("trimRight", function($trim) {
      return function trimRight() {
        return $trim(this, 2);
      };
    }, "trimEnd");
  });

  // node_modules/core-js/modules/es7.string.match-all.js
  var require_es7_string_match_all = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var defined = require_defined();
    var toLength = require_to_length();
    var isRegExp = require_is_regexp();
    var getFlags = require_flags();
    var RegExpProto = RegExp.prototype;
    var $RegExpStringIterator = function(regexp, string) {
      this._r = regexp;
      this._s = string;
    };
    require_iter_create()($RegExpStringIterator, "RegExp String", function next() {
      var match = this._r.exec(this._s);
      return {value: match, done: match === null};
    });
    $export($export.P, "String", {
      matchAll: function matchAll(regexp) {
        defined(this);
        if (!isRegExp(regexp))
          throw TypeError(regexp + " is not a regexp!");
        var S = String(this);
        var flags = "flags" in RegExpProto ? String(regexp.flags) : getFlags.call(regexp);
        var rx = new RegExp(regexp.source, ~flags.indexOf("g") ? flags : "g" + flags);
        rx.lastIndex = toLength(regexp.lastIndex);
        return new $RegExpStringIterator(rx, S);
      }
    });
  });

  // node_modules/core-js/modules/es7.symbol.async-iterator.js
  var require_es7_symbol_async_iterator = __commonJS(() => {
    require_wks_define()("asyncIterator");
  });

  // node_modules/core-js/modules/es7.symbol.observable.js
  var require_es7_symbol_observable = __commonJS(() => {
    require_wks_define()("observable");
  });

  // node_modules/core-js/modules/es7.object.get-own-property-descriptors.js
  var require_es7_object_get_own_property_descriptors = __commonJS(() => {
    var $export = require_export();
    var ownKeys = require_own_keys();
    var toIObject = require_to_iobject();
    var gOPD = require_object_gopd();
    var createProperty = require_create_property();
    $export($export.S, "Object", {
      getOwnPropertyDescriptors: function getOwnPropertyDescriptors(object) {
        var O = toIObject(object);
        var getDesc = gOPD.f;
        var keys = ownKeys(O);
        var result = {};
        var i = 0;
        var key, desc;
        while (keys.length > i) {
          desc = getDesc(O, key = keys[i++]);
          if (desc !== void 0)
            createProperty(result, key, desc);
        }
        return result;
      }
    });
  });

  // node_modules/core-js/modules/_object-to-array.js
  var require_object_to_array = __commonJS((exports2, module2) => {
    var DESCRIPTORS = require_descriptors();
    var getKeys = require_object_keys();
    var toIObject = require_to_iobject();
    var isEnum = require_object_pie().f;
    module2.exports = function(isEntries) {
      return function(it) {
        var O = toIObject(it);
        var keys = getKeys(O);
        var length = keys.length;
        var i = 0;
        var result = [];
        var key;
        while (length > i) {
          key = keys[i++];
          if (!DESCRIPTORS || isEnum.call(O, key)) {
            result.push(isEntries ? [key, O[key]] : O[key]);
          }
        }
        return result;
      };
    };
  });

  // node_modules/core-js/modules/es7.object.values.js
  var require_es7_object_values = __commonJS(() => {
    var $export = require_export();
    var $values = require_object_to_array()(false);
    $export($export.S, "Object", {
      values: function values(it) {
        return $values(it);
      }
    });
  });

  // node_modules/core-js/modules/es7.object.entries.js
  var require_es7_object_entries = __commonJS(() => {
    var $export = require_export();
    var $entries = require_object_to_array()(true);
    $export($export.S, "Object", {
      entries: function entries(it) {
        return $entries(it);
      }
    });
  });

  // node_modules/core-js/modules/_object-forced-pam.js
  var require_object_forced_pam = __commonJS((exports2, module2) => {
    "use strict";
    module2.exports = require_library() || !require_fails()(function() {
      var K = Math.random();
      __defineSetter__.call(null, K, function() {
      });
      delete require_global()[K];
    });
  });

  // node_modules/core-js/modules/es7.object.define-getter.js
  var require_es7_object_define_getter = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toObject = require_to_object();
    var aFunction = require_a_function();
    var $defineProperty = require_object_dp();
    require_descriptors() && $export($export.P + require_object_forced_pam(), "Object", {
      __defineGetter__: function __defineGetter__(P, getter) {
        $defineProperty.f(toObject(this), P, {get: aFunction(getter), enumerable: true, configurable: true});
      }
    });
  });

  // node_modules/core-js/modules/es7.object.define-setter.js
  var require_es7_object_define_setter = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toObject = require_to_object();
    var aFunction = require_a_function();
    var $defineProperty = require_object_dp();
    require_descriptors() && $export($export.P + require_object_forced_pam(), "Object", {
      __defineSetter__: function __defineSetter__2(P, setter) {
        $defineProperty.f(toObject(this), P, {set: aFunction(setter), enumerable: true, configurable: true});
      }
    });
  });

  // node_modules/core-js/modules/es7.object.lookup-getter.js
  var require_es7_object_lookup_getter = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toObject = require_to_object();
    var toPrimitive = require_to_primitive();
    var getPrototypeOf = require_object_gpo();
    var getOwnPropertyDescriptor = require_object_gopd().f;
    require_descriptors() && $export($export.P + require_object_forced_pam(), "Object", {
      __lookupGetter__: function __lookupGetter__(P) {
        var O = toObject(this);
        var K = toPrimitive(P, true);
        var D;
        do {
          if (D = getOwnPropertyDescriptor(O, K))
            return D.get;
        } while (O = getPrototypeOf(O));
      }
    });
  });

  // node_modules/core-js/modules/es7.object.lookup-setter.js
  var require_es7_object_lookup_setter = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var toObject = require_to_object();
    var toPrimitive = require_to_primitive();
    var getPrototypeOf = require_object_gpo();
    var getOwnPropertyDescriptor = require_object_gopd().f;
    require_descriptors() && $export($export.P + require_object_forced_pam(), "Object", {
      __lookupSetter__: function __lookupSetter__(P) {
        var O = toObject(this);
        var K = toPrimitive(P, true);
        var D;
        do {
          if (D = getOwnPropertyDescriptor(O, K))
            return D.set;
        } while (O = getPrototypeOf(O));
      }
    });
  });

  // node_modules/core-js/modules/_array-from-iterable.js
  var require_array_from_iterable = __commonJS((exports2, module2) => {
    var forOf = require_for_of();
    module2.exports = function(iter, ITERATOR) {
      var result = [];
      forOf(iter, false, result.push, result, ITERATOR);
      return result;
    };
  });

  // node_modules/core-js/modules/_collection-to-json.js
  var require_collection_to_json = __commonJS((exports2, module2) => {
    var classof = require_classof();
    var from = require_array_from_iterable();
    module2.exports = function(NAME) {
      return function toJSON() {
        if (classof(this) != NAME)
          throw TypeError(NAME + "#toJSON isn't generic");
        return from(this);
      };
    };
  });

  // node_modules/core-js/modules/es7.map.to-json.js
  var require_es7_map_to_json = __commonJS(() => {
    var $export = require_export();
    $export($export.P + $export.R, "Map", {toJSON: require_collection_to_json()("Map")});
  });

  // node_modules/core-js/modules/es7.set.to-json.js
  var require_es7_set_to_json = __commonJS(() => {
    var $export = require_export();
    $export($export.P + $export.R, "Set", {toJSON: require_collection_to_json()("Set")});
  });

  // node_modules/core-js/modules/_set-collection-of.js
  var require_set_collection_of = __commonJS((exports2, module2) => {
    "use strict";
    var $export = require_export();
    module2.exports = function(COLLECTION) {
      $export($export.S, COLLECTION, {of: function of() {
        var length = arguments.length;
        var A = new Array(length);
        while (length--)
          A[length] = arguments[length];
        return new this(A);
      }});
    };
  });

  // node_modules/core-js/modules/es7.map.of.js
  var require_es7_map_of = __commonJS(() => {
    require_set_collection_of()("Map");
  });

  // node_modules/core-js/modules/es7.set.of.js
  var require_es7_set_of = __commonJS(() => {
    require_set_collection_of()("Set");
  });

  // node_modules/core-js/modules/es7.weak-map.of.js
  var require_es7_weak_map_of = __commonJS(() => {
    require_set_collection_of()("WeakMap");
  });

  // node_modules/core-js/modules/es7.weak-set.of.js
  var require_es7_weak_set_of = __commonJS(() => {
    require_set_collection_of()("WeakSet");
  });

  // node_modules/core-js/modules/_set-collection-from.js
  var require_set_collection_from = __commonJS((exports2, module2) => {
    "use strict";
    var $export = require_export();
    var aFunction = require_a_function();
    var ctx = require_ctx();
    var forOf = require_for_of();
    module2.exports = function(COLLECTION) {
      $export($export.S, COLLECTION, {from: function from(source) {
        var mapFn = arguments[1];
        var mapping, A, n, cb;
        aFunction(this);
        mapping = mapFn !== void 0;
        if (mapping)
          aFunction(mapFn);
        if (source == void 0)
          return new this();
        A = [];
        if (mapping) {
          n = 0;
          cb = ctx(mapFn, arguments[2], 2);
          forOf(source, false, function(nextItem) {
            A.push(cb(nextItem, n++));
          });
        } else {
          forOf(source, false, A.push, A);
        }
        return new this(A);
      }});
    };
  });

  // node_modules/core-js/modules/es7.map.from.js
  var require_es7_map_from = __commonJS(() => {
    require_set_collection_from()("Map");
  });

  // node_modules/core-js/modules/es7.set.from.js
  var require_es7_set_from = __commonJS(() => {
    require_set_collection_from()("Set");
  });

  // node_modules/core-js/modules/es7.weak-map.from.js
  var require_es7_weak_map_from = __commonJS(() => {
    require_set_collection_from()("WeakMap");
  });

  // node_modules/core-js/modules/es7.weak-set.from.js
  var require_es7_weak_set_from = __commonJS(() => {
    require_set_collection_from()("WeakSet");
  });

  // node_modules/core-js/modules/es7.global.js
  var require_es7_global = __commonJS(() => {
    var $export = require_export();
    $export($export.G, {global: require_global()});
  });

  // node_modules/core-js/modules/es7.system.global.js
  var require_es7_system_global = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "System", {global: require_global()});
  });

  // node_modules/core-js/modules/es7.error.is-error.js
  var require_es7_error_is_error = __commonJS(() => {
    var $export = require_export();
    var cof = require_cof();
    $export($export.S, "Error", {
      isError: function isError(it) {
        return cof(it) === "Error";
      }
    });
  });

  // node_modules/core-js/modules/es7.math.clamp.js
  var require_es7_math_clamp = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      clamp: function clamp(x, lower, upper) {
        return Math.min(upper, Math.max(lower, x));
      }
    });
  });

  // node_modules/core-js/modules/es7.math.deg-per-rad.js
  var require_es7_math_deg_per_rad = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {DEG_PER_RAD: Math.PI / 180});
  });

  // node_modules/core-js/modules/es7.math.degrees.js
  var require_es7_math_degrees = __commonJS(() => {
    var $export = require_export();
    var RAD_PER_DEG = 180 / Math.PI;
    $export($export.S, "Math", {
      degrees: function degrees(radians) {
        return radians * RAD_PER_DEG;
      }
    });
  });

  // node_modules/core-js/modules/_math-scale.js
  var require_math_scale = __commonJS((exports2, module2) => {
    module2.exports = Math.scale || function scale(x, inLow, inHigh, outLow, outHigh) {
      if (arguments.length === 0 || x != x || inLow != inLow || inHigh != inHigh || outLow != outLow || outHigh != outHigh)
        return NaN;
      if (x === Infinity || x === -Infinity)
        return x;
      return (x - inLow) * (outHigh - outLow) / (inHigh - inLow) + outLow;
    };
  });

  // node_modules/core-js/modules/es7.math.fscale.js
  var require_es7_math_fscale = __commonJS(() => {
    var $export = require_export();
    var scale = require_math_scale();
    var fround = require_math_fround();
    $export($export.S, "Math", {
      fscale: function fscale(x, inLow, inHigh, outLow, outHigh) {
        return fround(scale(x, inLow, inHigh, outLow, outHigh));
      }
    });
  });

  // node_modules/core-js/modules/es7.math.iaddh.js
  var require_es7_math_iaddh = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      iaddh: function iaddh(x0, x1, y0, y1) {
        var $x0 = x0 >>> 0;
        var $x1 = x1 >>> 0;
        var $y0 = y0 >>> 0;
        return $x1 + (y1 >>> 0) + (($x0 & $y0 | ($x0 | $y0) & ~($x0 + $y0 >>> 0)) >>> 31) | 0;
      }
    });
  });

  // node_modules/core-js/modules/es7.math.isubh.js
  var require_es7_math_isubh = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      isubh: function isubh(x0, x1, y0, y1) {
        var $x0 = x0 >>> 0;
        var $x1 = x1 >>> 0;
        var $y0 = y0 >>> 0;
        return $x1 - (y1 >>> 0) - ((~$x0 & $y0 | ~($x0 ^ $y0) & $x0 - $y0 >>> 0) >>> 31) | 0;
      }
    });
  });

  // node_modules/core-js/modules/es7.math.imulh.js
  var require_es7_math_imulh = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      imulh: function imulh(u, v) {
        var UINT16 = 65535;
        var $u = +u;
        var $v = +v;
        var u0 = $u & UINT16;
        var v0 = $v & UINT16;
        var u1 = $u >> 16;
        var v1 = $v >> 16;
        var t = (u1 * v0 >>> 0) + (u0 * v0 >>> 16);
        return u1 * v1 + (t >> 16) + ((u0 * v1 >>> 0) + (t & UINT16) >> 16);
      }
    });
  });

  // node_modules/core-js/modules/es7.math.rad-per-deg.js
  var require_es7_math_rad_per_deg = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {RAD_PER_DEG: 180 / Math.PI});
  });

  // node_modules/core-js/modules/es7.math.radians.js
  var require_es7_math_radians = __commonJS(() => {
    var $export = require_export();
    var DEG_PER_RAD = Math.PI / 180;
    $export($export.S, "Math", {
      radians: function radians(degrees) {
        return degrees * DEG_PER_RAD;
      }
    });
  });

  // node_modules/core-js/modules/es7.math.scale.js
  var require_es7_math_scale = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {scale: require_math_scale()});
  });

  // node_modules/core-js/modules/es7.math.umulh.js
  var require_es7_math_umulh = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {
      umulh: function umulh(u, v) {
        var UINT16 = 65535;
        var $u = +u;
        var $v = +v;
        var u0 = $u & UINT16;
        var v0 = $v & UINT16;
        var u1 = $u >>> 16;
        var v1 = $v >>> 16;
        var t = (u1 * v0 >>> 0) + (u0 * v0 >>> 16);
        return u1 * v1 + (t >>> 16) + ((u0 * v1 >>> 0) + (t & UINT16) >>> 16);
      }
    });
  });

  // node_modules/core-js/modules/es7.math.signbit.js
  var require_es7_math_signbit = __commonJS(() => {
    var $export = require_export();
    $export($export.S, "Math", {signbit: function signbit(x) {
      return (x = +x) != x ? x : x == 0 ? 1 / x == Infinity : x > 0;
    }});
  });

  // node_modules/core-js/modules/es7.promise.finally.js
  var require_es7_promise_finally = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var core = require_core();
    var global2 = require_global();
    var speciesConstructor = require_species_constructor();
    var promiseResolve = require_promise_resolve();
    $export($export.P + $export.R, "Promise", {finally: function(onFinally) {
      var C = speciesConstructor(this, core.Promise || global2.Promise);
      var isFunction = typeof onFinally == "function";
      return this.then(isFunction ? function(x) {
        return promiseResolve(C, onFinally()).then(function() {
          return x;
        });
      } : onFinally, isFunction ? function(e) {
        return promiseResolve(C, onFinally()).then(function() {
          throw e;
        });
      } : onFinally);
    }});
  });

  // node_modules/core-js/modules/es7.promise.try.js
  var require_es7_promise_try = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var newPromiseCapability = require_new_promise_capability();
    var perform = require_perform();
    $export($export.S, "Promise", {try: function(callbackfn) {
      var promiseCapability = newPromiseCapability.f(this);
      var result = perform(callbackfn);
      (result.e ? promiseCapability.reject : promiseCapability.resolve)(result.v);
      return promiseCapability.promise;
    }});
  });

  // node_modules/core-js/modules/_metadata.js
  var require_metadata = __commonJS((exports2, module2) => {
    var Map = require_es6_map();
    var $export = require_export();
    var shared = require_shared()("metadata");
    var store = shared.store || (shared.store = new (require_es6_weak_map())());
    var getOrCreateMetadataMap = function(target, targetKey, create) {
      var targetMetadata = store.get(target);
      if (!targetMetadata) {
        if (!create)
          return void 0;
        store.set(target, targetMetadata = new Map());
      }
      var keyMetadata = targetMetadata.get(targetKey);
      if (!keyMetadata) {
        if (!create)
          return void 0;
        targetMetadata.set(targetKey, keyMetadata = new Map());
      }
      return keyMetadata;
    };
    var ordinaryHasOwnMetadata = function(MetadataKey, O, P) {
      var metadataMap = getOrCreateMetadataMap(O, P, false);
      return metadataMap === void 0 ? false : metadataMap.has(MetadataKey);
    };
    var ordinaryGetOwnMetadata = function(MetadataKey, O, P) {
      var metadataMap = getOrCreateMetadataMap(O, P, false);
      return metadataMap === void 0 ? void 0 : metadataMap.get(MetadataKey);
    };
    var ordinaryDefineOwnMetadata = function(MetadataKey, MetadataValue, O, P) {
      getOrCreateMetadataMap(O, P, true).set(MetadataKey, MetadataValue);
    };
    var ordinaryOwnMetadataKeys = function(target, targetKey) {
      var metadataMap = getOrCreateMetadataMap(target, targetKey, false);
      var keys = [];
      if (metadataMap)
        metadataMap.forEach(function(_, key) {
          keys.push(key);
        });
      return keys;
    };
    var toMetaKey = function(it) {
      return it === void 0 || typeof it == "symbol" ? it : String(it);
    };
    var exp = function(O) {
      $export($export.S, "Reflect", O);
    };
    module2.exports = {
      store,
      map: getOrCreateMetadataMap,
      has: ordinaryHasOwnMetadata,
      get: ordinaryGetOwnMetadata,
      set: ordinaryDefineOwnMetadata,
      keys: ordinaryOwnMetadataKeys,
      key: toMetaKey,
      exp
    };
  });

  // node_modules/core-js/modules/es7.reflect.define-metadata.js
  var require_es7_reflect_define_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var toMetaKey = metadata.key;
    var ordinaryDefineOwnMetadata = metadata.set;
    metadata.exp({defineMetadata: function defineMetadata(metadataKey, metadataValue, target, targetKey) {
      ordinaryDefineOwnMetadata(metadataKey, metadataValue, anObject(target), toMetaKey(targetKey));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.delete-metadata.js
  var require_es7_reflect_delete_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var toMetaKey = metadata.key;
    var getOrCreateMetadataMap = metadata.map;
    var store = metadata.store;
    metadata.exp({deleteMetadata: function deleteMetadata(metadataKey, target) {
      var targetKey = arguments.length < 3 ? void 0 : toMetaKey(arguments[2]);
      var metadataMap = getOrCreateMetadataMap(anObject(target), targetKey, false);
      if (metadataMap === void 0 || !metadataMap["delete"](metadataKey))
        return false;
      if (metadataMap.size)
        return true;
      var targetMetadata = store.get(target);
      targetMetadata["delete"](targetKey);
      return !!targetMetadata.size || store["delete"](target);
    }});
  });

  // node_modules/core-js/modules/es7.reflect.get-metadata.js
  var require_es7_reflect_get_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var getPrototypeOf = require_object_gpo();
    var ordinaryHasOwnMetadata = metadata.has;
    var ordinaryGetOwnMetadata = metadata.get;
    var toMetaKey = metadata.key;
    var ordinaryGetMetadata = function(MetadataKey, O, P) {
      var hasOwn = ordinaryHasOwnMetadata(MetadataKey, O, P);
      if (hasOwn)
        return ordinaryGetOwnMetadata(MetadataKey, O, P);
      var parent = getPrototypeOf(O);
      return parent !== null ? ordinaryGetMetadata(MetadataKey, parent, P) : void 0;
    };
    metadata.exp({getMetadata: function getMetadata(metadataKey, target) {
      return ordinaryGetMetadata(metadataKey, anObject(target), arguments.length < 3 ? void 0 : toMetaKey(arguments[2]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.get-metadata-keys.js
  var require_es7_reflect_get_metadata_keys = __commonJS(() => {
    var Set = require_es6_set();
    var from = require_array_from_iterable();
    var metadata = require_metadata();
    var anObject = require_an_object();
    var getPrototypeOf = require_object_gpo();
    var ordinaryOwnMetadataKeys = metadata.keys;
    var toMetaKey = metadata.key;
    var ordinaryMetadataKeys = function(O, P) {
      var oKeys = ordinaryOwnMetadataKeys(O, P);
      var parent = getPrototypeOf(O);
      if (parent === null)
        return oKeys;
      var pKeys = ordinaryMetadataKeys(parent, P);
      return pKeys.length ? oKeys.length ? from(new Set(oKeys.concat(pKeys))) : pKeys : oKeys;
    };
    metadata.exp({getMetadataKeys: function getMetadataKeys(target) {
      return ordinaryMetadataKeys(anObject(target), arguments.length < 2 ? void 0 : toMetaKey(arguments[1]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.get-own-metadata.js
  var require_es7_reflect_get_own_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var ordinaryGetOwnMetadata = metadata.get;
    var toMetaKey = metadata.key;
    metadata.exp({getOwnMetadata: function getOwnMetadata(metadataKey, target) {
      return ordinaryGetOwnMetadata(metadataKey, anObject(target), arguments.length < 3 ? void 0 : toMetaKey(arguments[2]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.get-own-metadata-keys.js
  var require_es7_reflect_get_own_metadata_keys = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var ordinaryOwnMetadataKeys = metadata.keys;
    var toMetaKey = metadata.key;
    metadata.exp({getOwnMetadataKeys: function getOwnMetadataKeys(target) {
      return ordinaryOwnMetadataKeys(anObject(target), arguments.length < 2 ? void 0 : toMetaKey(arguments[1]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.has-metadata.js
  var require_es7_reflect_has_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var getPrototypeOf = require_object_gpo();
    var ordinaryHasOwnMetadata = metadata.has;
    var toMetaKey = metadata.key;
    var ordinaryHasMetadata = function(MetadataKey, O, P) {
      var hasOwn = ordinaryHasOwnMetadata(MetadataKey, O, P);
      if (hasOwn)
        return true;
      var parent = getPrototypeOf(O);
      return parent !== null ? ordinaryHasMetadata(MetadataKey, parent, P) : false;
    };
    metadata.exp({hasMetadata: function hasMetadata(metadataKey, target) {
      return ordinaryHasMetadata(metadataKey, anObject(target), arguments.length < 3 ? void 0 : toMetaKey(arguments[2]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.has-own-metadata.js
  var require_es7_reflect_has_own_metadata = __commonJS(() => {
    var metadata = require_metadata();
    var anObject = require_an_object();
    var ordinaryHasOwnMetadata = metadata.has;
    var toMetaKey = metadata.key;
    metadata.exp({hasOwnMetadata: function hasOwnMetadata(metadataKey, target) {
      return ordinaryHasOwnMetadata(metadataKey, anObject(target), arguments.length < 3 ? void 0 : toMetaKey(arguments[2]));
    }});
  });

  // node_modules/core-js/modules/es7.reflect.metadata.js
  var require_es7_reflect_metadata = __commonJS(() => {
    var $metadata = require_metadata();
    var anObject = require_an_object();
    var aFunction = require_a_function();
    var toMetaKey = $metadata.key;
    var ordinaryDefineOwnMetadata = $metadata.set;
    $metadata.exp({metadata: function metadata(metadataKey, metadataValue) {
      return function decorator(target, targetKey) {
        ordinaryDefineOwnMetadata(metadataKey, metadataValue, (targetKey !== void 0 ? anObject : aFunction)(target), toMetaKey(targetKey));
      };
    }});
  });

  // node_modules/core-js/modules/es7.asap.js
  var require_es7_asap = __commonJS(() => {
    var $export = require_export();
    var microtask = require_microtask()();
    var process = require_global().process;
    var isNode = require_cof()(process) == "process";
    $export($export.G, {
      asap: function asap(fn) {
        var domain = isNode && process.domain;
        microtask(domain ? domain.bind(fn) : fn);
      }
    });
  });

  // node_modules/core-js/modules/es7.observable.js
  var require_es7_observable = __commonJS(() => {
    "use strict";
    var $export = require_export();
    var global2 = require_global();
    var core = require_core();
    var microtask = require_microtask()();
    var OBSERVABLE = require_wks()("observable");
    var aFunction = require_a_function();
    var anObject = require_an_object();
    var anInstance = require_an_instance();
    var redefineAll = require_redefine_all();
    var hide = require_hide();
    var forOf = require_for_of();
    var RETURN = forOf.RETURN;
    var getMethod = function(fn) {
      return fn == null ? void 0 : aFunction(fn);
    };
    var cleanupSubscription = function(subscription) {
      var cleanup = subscription._c;
      if (cleanup) {
        subscription._c = void 0;
        cleanup();
      }
    };
    var subscriptionClosed = function(subscription) {
      return subscription._o === void 0;
    };
    var closeSubscription = function(subscription) {
      if (!subscriptionClosed(subscription)) {
        subscription._o = void 0;
        cleanupSubscription(subscription);
      }
    };
    var Subscription = function(observer, subscriber) {
      anObject(observer);
      this._c = void 0;
      this._o = observer;
      observer = new SubscriptionObserver(this);
      try {
        var cleanup = subscriber(observer);
        var subscription = cleanup;
        if (cleanup != null) {
          if (typeof cleanup.unsubscribe === "function")
            cleanup = function() {
              subscription.unsubscribe();
            };
          else
            aFunction(cleanup);
          this._c = cleanup;
        }
      } catch (e) {
        observer.error(e);
        return;
      }
      if (subscriptionClosed(this))
        cleanupSubscription(this);
    };
    Subscription.prototype = redefineAll({}, {
      unsubscribe: function unsubscribe() {
        closeSubscription(this);
      }
    });
    var SubscriptionObserver = function(subscription) {
      this._s = subscription;
    };
    SubscriptionObserver.prototype = redefineAll({}, {
      next: function next(value) {
        var subscription = this._s;
        if (!subscriptionClosed(subscription)) {
          var observer = subscription._o;
          try {
            var m = getMethod(observer.next);
            if (m)
              return m.call(observer, value);
          } catch (e) {
            try {
              closeSubscription(subscription);
            } finally {
              throw e;
            }
          }
        }
      },
      error: function error(value) {
        var subscription = this._s;
        if (subscriptionClosed(subscription))
          throw value;
        var observer = subscription._o;
        subscription._o = void 0;
        try {
          var m = getMethod(observer.error);
          if (!m)
            throw value;
          value = m.call(observer, value);
        } catch (e) {
          try {
            cleanupSubscription(subscription);
          } finally {
            throw e;
          }
        }
        cleanupSubscription(subscription);
        return value;
      },
      complete: function complete(value) {
        var subscription = this._s;
        if (!subscriptionClosed(subscription)) {
          var observer = subscription._o;
          subscription._o = void 0;
          try {
            var m = getMethod(observer.complete);
            value = m ? m.call(observer, value) : void 0;
          } catch (e) {
            try {
              cleanupSubscription(subscription);
            } finally {
              throw e;
            }
          }
          cleanupSubscription(subscription);
          return value;
        }
      }
    });
    var $Observable = function Observable(subscriber) {
      anInstance(this, $Observable, "Observable", "_f")._f = aFunction(subscriber);
    };
    redefineAll($Observable.prototype, {
      subscribe: function subscribe(observer) {
        return new Subscription(observer, this._f);
      },
      forEach: function forEach(fn) {
        var that = this;
        return new (core.Promise || global2.Promise)(function(resolve, reject) {
          aFunction(fn);
          var subscription = that.subscribe({
            next: function(value) {
              try {
                return fn(value);
              } catch (e) {
                reject(e);
                subscription.unsubscribe();
              }
            },
            error: reject,
            complete: resolve
          });
        });
      }
    });
    redefineAll($Observable, {
      from: function from(x) {
        var C = typeof this === "function" ? this : $Observable;
        var method = getMethod(anObject(x)[OBSERVABLE]);
        if (method) {
          var observable = anObject(method.call(x));
          return observable.constructor === C ? observable : new C(function(observer) {
            return observable.subscribe(observer);
          });
        }
        return new C(function(observer) {
          var done = false;
          microtask(function() {
            if (!done) {
              try {
                if (forOf(x, false, function(it) {
                  observer.next(it);
                  if (done)
                    return RETURN;
                }) === RETURN)
                  return;
              } catch (e) {
                if (done)
                  throw e;
                observer.error(e);
                return;
              }
              observer.complete();
            }
          });
          return function() {
            done = true;
          };
        });
      },
      of: function of() {
        for (var i = 0, l = arguments.length, items = new Array(l); i < l; )
          items[i] = arguments[i++];
        return new (typeof this === "function" ? this : $Observable)(function(observer) {
          var done = false;
          microtask(function() {
            if (!done) {
              for (var j = 0; j < items.length; ++j) {
                observer.next(items[j]);
                if (done)
                  return;
              }
              observer.complete();
            }
          });
          return function() {
            done = true;
          };
        });
      }
    });
    hide($Observable.prototype, OBSERVABLE, function() {
      return this;
    });
    $export($export.G, {Observable: $Observable});
    require_set_species()("Observable");
  });

  // node_modules/core-js/modules/web.timers.js
  var require_web_timers = __commonJS(() => {
    var global2 = require_global();
    var $export = require_export();
    var userAgent2 = require_user_agent();
    var slice = [].slice;
    var MSIE = /MSIE .\./.test(userAgent2);
    var wrap = function(set) {
      return function(fn, time) {
        var boundArgs = arguments.length > 2;
        var args = boundArgs ? slice.call(arguments, 2) : false;
        return set(boundArgs ? function() {
          (typeof fn == "function" ? fn : Function(fn)).apply(this, args);
        } : fn, time);
      };
    };
    $export($export.G + $export.B + $export.F * MSIE, {
      setTimeout: wrap(global2.setTimeout),
      setInterval: wrap(global2.setInterval)
    });
  });

  // node_modules/core-js/modules/web.immediate.js
  var require_web_immediate = __commonJS(() => {
    var $export = require_export();
    var $task = require_task();
    $export($export.G + $export.B, {
      setImmediate: $task.set,
      clearImmediate: $task.clear
    });
  });

  // node_modules/core-js/modules/web.dom.iterable.js
  var require_web_dom_iterable = __commonJS(() => {
    var $iterators = require_es6_array_iterator();
    var getKeys = require_object_keys();
    var redefine = require_redefine();
    var global2 = require_global();
    var hide = require_hide();
    var Iterators = require_iterators();
    var wks = require_wks();
    var ITERATOR = wks("iterator");
    var TO_STRING_TAG = wks("toStringTag");
    var ArrayValues = Iterators.Array;
    var DOMIterables = {
      CSSRuleList: true,
      CSSStyleDeclaration: false,
      CSSValueList: false,
      ClientRectList: false,
      DOMRectList: false,
      DOMStringList: false,
      DOMTokenList: true,
      DataTransferItemList: false,
      FileList: false,
      HTMLAllCollection: false,
      HTMLCollection: false,
      HTMLFormElement: false,
      HTMLSelectElement: false,
      MediaList: true,
      MimeTypeArray: false,
      NamedNodeMap: false,
      NodeList: true,
      PaintRequestList: false,
      Plugin: false,
      PluginArray: false,
      SVGLengthList: false,
      SVGNumberList: false,
      SVGPathSegList: false,
      SVGPointList: false,
      SVGStringList: false,
      SVGTransformList: false,
      SourceBufferList: false,
      StyleSheetList: true,
      TextTrackCueList: false,
      TextTrackList: false,
      TouchList: false
    };
    for (var collections = getKeys(DOMIterables), i = 0; i < collections.length; i++) {
      NAME = collections[i];
      explicit = DOMIterables[NAME];
      Collection = global2[NAME];
      proto = Collection && Collection.prototype;
      if (proto) {
        if (!proto[ITERATOR])
          hide(proto, ITERATOR, ArrayValues);
        if (!proto[TO_STRING_TAG])
          hide(proto, TO_STRING_TAG, NAME);
        Iterators[NAME] = ArrayValues;
        if (explicit) {
          for (key in $iterators)
            if (!proto[key])
              redefine(proto, key, $iterators[key], true);
        }
      }
    }
    var NAME;
    var explicit;
    var Collection;
    var proto;
    var key;
  });

  // node_modules/core-js/shim.js
  var require_shim = __commonJS((exports2, module2) => {
    require_es6_symbol();
    require_es6_object_create();
    require_es6_object_define_property();
    require_es6_object_define_properties();
    require_es6_object_get_own_property_descriptor();
    require_es6_object_get_prototype_of();
    require_es6_object_keys();
    require_es6_object_get_own_property_names();
    require_es6_object_freeze();
    require_es6_object_seal();
    require_es6_object_prevent_extensions();
    require_es6_object_is_frozen();
    require_es6_object_is_sealed();
    require_es6_object_is_extensible();
    require_es6_object_assign();
    require_es6_object_is();
    require_es6_object_set_prototype_of();
    require_es6_object_to_string();
    require_es6_function_bind();
    require_es6_function_name();
    require_es6_function_has_instance();
    require_es6_parse_int();
    require_es6_parse_float();
    require_es6_number_constructor();
    require_es6_number_to_fixed();
    require_es6_number_to_precision();
    require_es6_number_epsilon();
    require_es6_number_is_finite();
    require_es6_number_is_integer();
    require_es6_number_is_nan();
    require_es6_number_is_safe_integer();
    require_es6_number_max_safe_integer();
    require_es6_number_min_safe_integer();
    require_es6_number_parse_float();
    require_es6_number_parse_int();
    require_es6_math_acosh();
    require_es6_math_asinh();
    require_es6_math_atanh();
    require_es6_math_cbrt();
    require_es6_math_clz32();
    require_es6_math_cosh();
    require_es6_math_expm1();
    require_es6_math_fround();
    require_es6_math_hypot();
    require_es6_math_imul();
    require_es6_math_log10();
    require_es6_math_log1p();
    require_es6_math_log2();
    require_es6_math_sign();
    require_es6_math_sinh();
    require_es6_math_tanh();
    require_es6_math_trunc();
    require_es6_string_from_code_point();
    require_es6_string_raw();
    require_es6_string_trim();
    require_es6_string_iterator();
    require_es6_string_code_point_at();
    require_es6_string_ends_with();
    require_es6_string_includes();
    require_es6_string_repeat();
    require_es6_string_starts_with();
    require_es6_string_anchor();
    require_es6_string_big();
    require_es6_string_blink();
    require_es6_string_bold();
    require_es6_string_fixed();
    require_es6_string_fontcolor();
    require_es6_string_fontsize();
    require_es6_string_italics();
    require_es6_string_link();
    require_es6_string_small();
    require_es6_string_strike();
    require_es6_string_sub();
    require_es6_string_sup();
    require_es6_date_now();
    require_es6_date_to_json();
    require_es6_date_to_iso_string();
    require_es6_date_to_string();
    require_es6_date_to_primitive();
    require_es6_array_is_array();
    require_es6_array_from();
    require_es6_array_of();
    require_es6_array_join();
    require_es6_array_slice();
    require_es6_array_sort();
    require_es6_array_for_each();
    require_es6_array_map();
    require_es6_array_filter();
    require_es6_array_some();
    require_es6_array_every();
    require_es6_array_reduce();
    require_es6_array_reduce_right();
    require_es6_array_index_of();
    require_es6_array_last_index_of();
    require_es6_array_copy_within();
    require_es6_array_fill();
    require_es6_array_find();
    require_es6_array_find_index();
    require_es6_array_species();
    require_es6_array_iterator();
    require_es6_regexp_constructor();
    require_es6_regexp_exec();
    require_es6_regexp_to_string();
    require_es6_regexp_flags();
    require_es6_regexp_match();
    require_es6_regexp_replace();
    require_es6_regexp_search();
    require_es6_regexp_split();
    require_es6_promise();
    require_es6_map();
    require_es6_set();
    require_es6_weak_map();
    require_es6_weak_set();
    require_es6_typed_array_buffer();
    require_es6_typed_data_view();
    require_es6_typed_int8_array();
    require_es6_typed_uint8_array();
    require_es6_typed_uint8_clamped_array();
    require_es6_typed_int16_array();
    require_es6_typed_uint16_array();
    require_es6_typed_int32_array();
    require_es6_typed_uint32_array();
    require_es6_typed_float32_array();
    require_es6_typed_float64_array();
    require_es6_reflect_apply();
    require_es6_reflect_construct();
    require_es6_reflect_define_property();
    require_es6_reflect_delete_property();
    require_es6_reflect_enumerate();
    require_es6_reflect_get();
    require_es6_reflect_get_own_property_descriptor();
    require_es6_reflect_get_prototype_of();
    require_es6_reflect_has();
    require_es6_reflect_is_extensible();
    require_es6_reflect_own_keys();
    require_es6_reflect_prevent_extensions();
    require_es6_reflect_set();
    require_es6_reflect_set_prototype_of();
    require_es7_array_includes();
    require_es7_array_flat_map();
    require_es7_array_flatten();
    require_es7_string_at();
    require_es7_string_pad_start();
    require_es7_string_pad_end();
    require_es7_string_trim_left();
    require_es7_string_trim_right();
    require_es7_string_match_all();
    require_es7_symbol_async_iterator();
    require_es7_symbol_observable();
    require_es7_object_get_own_property_descriptors();
    require_es7_object_values();
    require_es7_object_entries();
    require_es7_object_define_getter();
    require_es7_object_define_setter();
    require_es7_object_lookup_getter();
    require_es7_object_lookup_setter();
    require_es7_map_to_json();
    require_es7_set_to_json();
    require_es7_map_of();
    require_es7_set_of();
    require_es7_weak_map_of();
    require_es7_weak_set_of();
    require_es7_map_from();
    require_es7_set_from();
    require_es7_weak_map_from();
    require_es7_weak_set_from();
    require_es7_global();
    require_es7_system_global();
    require_es7_error_is_error();
    require_es7_math_clamp();
    require_es7_math_deg_per_rad();
    require_es7_math_degrees();
    require_es7_math_fscale();
    require_es7_math_iaddh();
    require_es7_math_isubh();
    require_es7_math_imulh();
    require_es7_math_rad_per_deg();
    require_es7_math_radians();
    require_es7_math_scale();
    require_es7_math_umulh();
    require_es7_math_signbit();
    require_es7_promise_finally();
    require_es7_promise_try();
    require_es7_reflect_define_metadata();
    require_es7_reflect_delete_metadata();
    require_es7_reflect_get_metadata();
    require_es7_reflect_get_metadata_keys();
    require_es7_reflect_get_own_metadata();
    require_es7_reflect_get_own_metadata_keys();
    require_es7_reflect_has_metadata();
    require_es7_reflect_has_own_metadata();
    require_es7_reflect_metadata();
    require_es7_asap();
    require_es7_observable();
    require_web_timers();
    require_web_immediate();
    require_web_dom_iterable();
    module2.exports = require_core();
  });

  // node_modules/regenerator-runtime/runtime.js
  var require_runtime = __commonJS((exports2, module2) => {
    !function(global2) {
      "use strict";
      var Op = Object.prototype;
      var hasOwn = Op.hasOwnProperty;
      var undefined2;
      var $Symbol = typeof Symbol === "function" ? Symbol : {};
      var iteratorSymbol = $Symbol.iterator || "@@iterator";
      var asyncIteratorSymbol = $Symbol.asyncIterator || "@@asyncIterator";
      var toStringTagSymbol = $Symbol.toStringTag || "@@toStringTag";
      var inModule = typeof module2 === "object";
      var runtime = global2.regeneratorRuntime;
      if (runtime) {
        if (inModule) {
          module2.exports = runtime;
        }
        return;
      }
      runtime = global2.regeneratorRuntime = inModule ? module2.exports : {};
      function wrap(innerFn, outerFn, self2, tryLocsList) {
        var protoGenerator = outerFn && outerFn.prototype instanceof Generator ? outerFn : Generator;
        var generator = Object.create(protoGenerator.prototype);
        var context = new Context(tryLocsList || []);
        generator._invoke = makeInvokeMethod(innerFn, self2, context);
        return generator;
      }
      runtime.wrap = wrap;
      function tryCatch(fn, obj, arg) {
        try {
          return {type: "normal", arg: fn.call(obj, arg)};
        } catch (err) {
          return {type: "throw", arg: err};
        }
      }
      var GenStateSuspendedStart = "suspendedStart";
      var GenStateSuspendedYield = "suspendedYield";
      var GenStateExecuting = "executing";
      var GenStateCompleted = "completed";
      var ContinueSentinel = {};
      function Generator() {
      }
      function GeneratorFunction() {
      }
      function GeneratorFunctionPrototype() {
      }
      var IteratorPrototype = {};
      IteratorPrototype[iteratorSymbol] = function() {
        return this;
      };
      var getProto = Object.getPrototypeOf;
      var NativeIteratorPrototype = getProto && getProto(getProto(values([])));
      if (NativeIteratorPrototype && NativeIteratorPrototype !== Op && hasOwn.call(NativeIteratorPrototype, iteratorSymbol)) {
        IteratorPrototype = NativeIteratorPrototype;
      }
      var Gp = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(IteratorPrototype);
      GeneratorFunction.prototype = Gp.constructor = GeneratorFunctionPrototype;
      GeneratorFunctionPrototype.constructor = GeneratorFunction;
      GeneratorFunctionPrototype[toStringTagSymbol] = GeneratorFunction.displayName = "GeneratorFunction";
      function defineIteratorMethods(prototype) {
        ["next", "throw", "return"].forEach(function(method) {
          prototype[method] = function(arg) {
            return this._invoke(method, arg);
          };
        });
      }
      runtime.isGeneratorFunction = function(genFun) {
        var ctor = typeof genFun === "function" && genFun.constructor;
        return ctor ? ctor === GeneratorFunction || (ctor.displayName || ctor.name) === "GeneratorFunction" : false;
      };
      runtime.mark = function(genFun) {
        if (Object.setPrototypeOf) {
          Object.setPrototypeOf(genFun, GeneratorFunctionPrototype);
        } else {
          genFun.__proto__ = GeneratorFunctionPrototype;
          if (!(toStringTagSymbol in genFun)) {
            genFun[toStringTagSymbol] = "GeneratorFunction";
          }
        }
        genFun.prototype = Object.create(Gp);
        return genFun;
      };
      runtime.awrap = function(arg) {
        return {__await: arg};
      };
      function AsyncIterator(generator) {
        function invoke(method, arg, resolve, reject) {
          var record = tryCatch(generator[method], generator, arg);
          if (record.type === "throw") {
            reject(record.arg);
          } else {
            var result = record.arg;
            var value = result.value;
            if (value && typeof value === "object" && hasOwn.call(value, "__await")) {
              return Promise.resolve(value.__await).then(function(value2) {
                invoke("next", value2, resolve, reject);
              }, function(err) {
                invoke("throw", err, resolve, reject);
              });
            }
            return Promise.resolve(value).then(function(unwrapped) {
              result.value = unwrapped;
              resolve(result);
            }, reject);
          }
        }
        if (typeof global2.process === "object" && global2.process.domain) {
          invoke = global2.process.domain.bind(invoke);
        }
        var previousPromise;
        function enqueue(method, arg) {
          function callInvokeWithMethodAndArg() {
            return new Promise(function(resolve, reject) {
              invoke(method, arg, resolve, reject);
            });
          }
          return previousPromise = previousPromise ? previousPromise.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
        }
        this._invoke = enqueue;
      }
      defineIteratorMethods(AsyncIterator.prototype);
      AsyncIterator.prototype[asyncIteratorSymbol] = function() {
        return this;
      };
      runtime.AsyncIterator = AsyncIterator;
      runtime.async = function(innerFn, outerFn, self2, tryLocsList) {
        var iter = new AsyncIterator(wrap(innerFn, outerFn, self2, tryLocsList));
        return runtime.isGeneratorFunction(outerFn) ? iter : iter.next().then(function(result) {
          return result.done ? result.value : iter.next();
        });
      };
      function makeInvokeMethod(innerFn, self2, context) {
        var state = GenStateSuspendedStart;
        return function invoke(method, arg) {
          if (state === GenStateExecuting) {
            throw new Error("Generator is already running");
          }
          if (state === GenStateCompleted) {
            if (method === "throw") {
              throw arg;
            }
            return doneResult();
          }
          context.method = method;
          context.arg = arg;
          while (true) {
            var delegate = context.delegate;
            if (delegate) {
              var delegateResult = maybeInvokeDelegate(delegate, context);
              if (delegateResult) {
                if (delegateResult === ContinueSentinel)
                  continue;
                return delegateResult;
              }
            }
            if (context.method === "next") {
              context.sent = context._sent = context.arg;
            } else if (context.method === "throw") {
              if (state === GenStateSuspendedStart) {
                state = GenStateCompleted;
                throw context.arg;
              }
              context.dispatchException(context.arg);
            } else if (context.method === "return") {
              context.abrupt("return", context.arg);
            }
            state = GenStateExecuting;
            var record = tryCatch(innerFn, self2, context);
            if (record.type === "normal") {
              state = context.done ? GenStateCompleted : GenStateSuspendedYield;
              if (record.arg === ContinueSentinel) {
                continue;
              }
              return {
                value: record.arg,
                done: context.done
              };
            } else if (record.type === "throw") {
              state = GenStateCompleted;
              context.method = "throw";
              context.arg = record.arg;
            }
          }
        };
      }
      function maybeInvokeDelegate(delegate, context) {
        var method = delegate.iterator[context.method];
        if (method === undefined2) {
          context.delegate = null;
          if (context.method === "throw") {
            if (delegate.iterator.return) {
              context.method = "return";
              context.arg = undefined2;
              maybeInvokeDelegate(delegate, context);
              if (context.method === "throw") {
                return ContinueSentinel;
              }
            }
            context.method = "throw";
            context.arg = new TypeError("The iterator does not provide a 'throw' method");
          }
          return ContinueSentinel;
        }
        var record = tryCatch(method, delegate.iterator, context.arg);
        if (record.type === "throw") {
          context.method = "throw";
          context.arg = record.arg;
          context.delegate = null;
          return ContinueSentinel;
        }
        var info = record.arg;
        if (!info) {
          context.method = "throw";
          context.arg = new TypeError("iterator result is not an object");
          context.delegate = null;
          return ContinueSentinel;
        }
        if (info.done) {
          context[delegate.resultName] = info.value;
          context.next = delegate.nextLoc;
          if (context.method !== "return") {
            context.method = "next";
            context.arg = undefined2;
          }
        } else {
          return info;
        }
        context.delegate = null;
        return ContinueSentinel;
      }
      defineIteratorMethods(Gp);
      Gp[toStringTagSymbol] = "Generator";
      Gp[iteratorSymbol] = function() {
        return this;
      };
      Gp.toString = function() {
        return "[object Generator]";
      };
      function pushTryEntry(locs) {
        var entry = {tryLoc: locs[0]};
        if (1 in locs) {
          entry.catchLoc = locs[1];
        }
        if (2 in locs) {
          entry.finallyLoc = locs[2];
          entry.afterLoc = locs[3];
        }
        this.tryEntries.push(entry);
      }
      function resetTryEntry(entry) {
        var record = entry.completion || {};
        record.type = "normal";
        delete record.arg;
        entry.completion = record;
      }
      function Context(tryLocsList) {
        this.tryEntries = [{tryLoc: "root"}];
        tryLocsList.forEach(pushTryEntry, this);
        this.reset(true);
      }
      runtime.keys = function(object) {
        var keys = [];
        for (var key in object) {
          keys.push(key);
        }
        keys.reverse();
        return function next() {
          while (keys.length) {
            var key2 = keys.pop();
            if (key2 in object) {
              next.value = key2;
              next.done = false;
              return next;
            }
          }
          next.done = true;
          return next;
        };
      };
      function values(iterable) {
        if (iterable) {
          var iteratorMethod = iterable[iteratorSymbol];
          if (iteratorMethod) {
            return iteratorMethod.call(iterable);
          }
          if (typeof iterable.next === "function") {
            return iterable;
          }
          if (!isNaN(iterable.length)) {
            var i = -1, next = function next2() {
              while (++i < iterable.length) {
                if (hasOwn.call(iterable, i)) {
                  next2.value = iterable[i];
                  next2.done = false;
                  return next2;
                }
              }
              next2.value = undefined2;
              next2.done = true;
              return next2;
            };
            return next.next = next;
          }
        }
        return {next: doneResult};
      }
      runtime.values = values;
      function doneResult() {
        return {value: undefined2, done: true};
      }
      Context.prototype = {
        constructor: Context,
        reset: function(skipTempReset) {
          this.prev = 0;
          this.next = 0;
          this.sent = this._sent = undefined2;
          this.done = false;
          this.delegate = null;
          this.method = "next";
          this.arg = undefined2;
          this.tryEntries.forEach(resetTryEntry);
          if (!skipTempReset) {
            for (var name in this) {
              if (name.charAt(0) === "t" && hasOwn.call(this, name) && !isNaN(+name.slice(1))) {
                this[name] = undefined2;
              }
            }
          }
        },
        stop: function() {
          this.done = true;
          var rootEntry = this.tryEntries[0];
          var rootRecord = rootEntry.completion;
          if (rootRecord.type === "throw") {
            throw rootRecord.arg;
          }
          return this.rval;
        },
        dispatchException: function(exception) {
          if (this.done) {
            throw exception;
          }
          var context = this;
          function handle(loc, caught) {
            record.type = "throw";
            record.arg = exception;
            context.next = loc;
            if (caught) {
              context.method = "next";
              context.arg = undefined2;
            }
            return !!caught;
          }
          for (var i = this.tryEntries.length - 1; i >= 0; --i) {
            var entry = this.tryEntries[i];
            var record = entry.completion;
            if (entry.tryLoc === "root") {
              return handle("end");
            }
            if (entry.tryLoc <= this.prev) {
              var hasCatch = hasOwn.call(entry, "catchLoc");
              var hasFinally = hasOwn.call(entry, "finallyLoc");
              if (hasCatch && hasFinally) {
                if (this.prev < entry.catchLoc) {
                  return handle(entry.catchLoc, true);
                } else if (this.prev < entry.finallyLoc) {
                  return handle(entry.finallyLoc);
                }
              } else if (hasCatch) {
                if (this.prev < entry.catchLoc) {
                  return handle(entry.catchLoc, true);
                }
              } else if (hasFinally) {
                if (this.prev < entry.finallyLoc) {
                  return handle(entry.finallyLoc);
                }
              } else {
                throw new Error("try statement without catch or finally");
              }
            }
          }
        },
        abrupt: function(type, arg) {
          for (var i = this.tryEntries.length - 1; i >= 0; --i) {
            var entry = this.tryEntries[i];
            if (entry.tryLoc <= this.prev && hasOwn.call(entry, "finallyLoc") && this.prev < entry.finallyLoc) {
              var finallyEntry = entry;
              break;
            }
          }
          if (finallyEntry && (type === "break" || type === "continue") && finallyEntry.tryLoc <= arg && arg <= finallyEntry.finallyLoc) {
            finallyEntry = null;
          }
          var record = finallyEntry ? finallyEntry.completion : {};
          record.type = type;
          record.arg = arg;
          if (finallyEntry) {
            this.method = "next";
            this.next = finallyEntry.finallyLoc;
            return ContinueSentinel;
          }
          return this.complete(record);
        },
        complete: function(record, afterLoc) {
          if (record.type === "throw") {
            throw record.arg;
          }
          if (record.type === "break" || record.type === "continue") {
            this.next = record.arg;
          } else if (record.type === "return") {
            this.rval = this.arg = record.arg;
            this.method = "return";
            this.next = "end";
          } else if (record.type === "normal" && afterLoc) {
            this.next = afterLoc;
          }
          return ContinueSentinel;
        },
        finish: function(finallyLoc) {
          for (var i = this.tryEntries.length - 1; i >= 0; --i) {
            var entry = this.tryEntries[i];
            if (entry.finallyLoc === finallyLoc) {
              this.complete(entry.completion, entry.afterLoc);
              resetTryEntry(entry);
              return ContinueSentinel;
            }
          }
        },
        catch: function(tryLoc) {
          for (var i = this.tryEntries.length - 1; i >= 0; --i) {
            var entry = this.tryEntries[i];
            if (entry.tryLoc === tryLoc) {
              var record = entry.completion;
              if (record.type === "throw") {
                var thrown = record.arg;
                resetTryEntry(entry);
              }
              return thrown;
            }
          }
          throw new Error("illegal catch attempt");
        },
        delegateYield: function(iterable, resultName, nextLoc) {
          this.delegate = {
            iterator: values(iterable),
            resultName,
            nextLoc
          };
          if (this.method === "next") {
            this.arg = undefined2;
          }
          return ContinueSentinel;
        }
      };
    }(typeof global === "object" ? global : typeof window === "object" ? window : typeof self === "object" ? self : exports2);
  });

  // node_modules/core-js/modules/_replacer.js
  var require_replacer = __commonJS((exports2, module2) => {
    module2.exports = function(regExp, replace) {
      var replacer = replace === Object(replace) ? function(part) {
        return replace[part];
      } : replace;
      return function(it) {
        return String(it).replace(regExp, replacer);
      };
    };
  });

  // node_modules/core-js/modules/core.regexp.escape.js
  var require_core_regexp_escape = __commonJS(() => {
    var $export = require_export();
    var $re = require_replacer()(/[\\^$*+?.()|[\]{}]/g, "\\$&");
    $export($export.S, "RegExp", {escape: function escape(it) {
      return $re(it);
    }});
  });

  // node_modules/core-js/fn/regexp/escape.js
  var require_escape = __commonJS((exports2, module2) => {
    require_core_regexp_escape();
    module2.exports = require_core().RegExp.escape;
  });

  // src/utils/index.ts
  var require_utils = __commonJS((exports2) => {
    __markAsModule(exports2);
    __export(exports2, {
      $escape: () => $escape3,
      _equal: () => _equal2,
      asArray: () => asArray2,
      blob: () => blob_exports,
      compareVersion: () => compareVersion3,
      equal: () => equal2,
      escapeHTML: () => escapeHTML2,
      formatDateUTC: () => formatDateUTC2,
      getComputedLinkColor: () => getComputedLinkColor2,
      getStyle: () => getStyle2,
      isnan: () => isnan2,
      makeResizeFilter: () => makeResizeFilter2,
      mapValues: () => mapValues2,
      mergeSort: () => mergeSort2,
      padZeros: () => padZeros2,
      parseDate: () => parseDate2,
      pixelRatio: () => pixelRatio2,
      randomId: () => randomId2,
      roundSignif: () => roundSignif2,
      scopeExprToFunc: () => scopeExprToFunc2,
      strToBool: () => strToBool2,
      updateLabel: () => updateLabel2
    });
    function escapeHTML2(str) {
      const escaped = {
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
    function randomId2() {
      return Math.floor(4294967296 + Math.random() * 64424509440).toString(16);
    }
    function strToBool2(str) {
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
    function getStyle2(el, styleProp) {
      let x = void 0;
      if (el.currentStyle)
        x = el.currentStyle[styleProp];
      else if (window.getComputedStyle) {
        const style = document.defaultView.getComputedStyle(el, null);
        if (style)
          x = style.getPropertyValue(styleProp);
      }
      return x;
    }
    function padZeros2(n, digits) {
      let str = n.toString();
      while (str.length < digits)
        str = "0" + str;
      return str;
    }
    function roundSignif2(x, digits = 1) {
      if (digits < 1)
        throw "Significant digits must be at least 1.";
      return parseFloat(x.toPrecision(digits));
    }
    function parseDate2(dateString) {
      let date = new Date(dateString);
      if (date.toString() === "Invalid Date") {
        date = new Date(dateString.replace(/-/g, "/"));
      }
      return date;
    }
    function formatDateUTC2(date) {
      if (date instanceof Date) {
        return date.getUTCFullYear() + "-" + padZeros2(date.getUTCMonth() + 1, 2) + "-" + padZeros2(date.getUTCDate(), 2);
      } else {
        return null;
      }
    }
    function makeResizeFilter2(el, func) {
      let lastSize = {};
      return function() {
        const size = {w: el.offsetWidth, h: el.offsetHeight};
        if (size.w === 0 && size.h === 0)
          return;
        if (size.w === lastSize.w && size.h === lastSize.h)
          return;
        lastSize = size;
        func(size.w, size.h);
      };
    }
    function pixelRatio2() {
      if (devicePixelRatio()) {
        return Math.round(window.devicePixelRatio * 100) / 100;
      } else {
        return 1;
      }
    }
    scopeExprToFunc2.call;
    function scopeExprToFunc2(expr) {
      const expr_escaped = expr.replace(/[\\"']/g, "\\$&").replace(/\u0000/g, "\\0").replace(/\n/g, "\\n").replace(/\r/g, "\\r").replace(/[\b]/g, "\\b");
      let func;
      try {
        func = new Function(`with (this) {
        try {
          return (${expr});
        } catch (e) {
          console.error('Error evaluating expression: ${expr_escaped}');
          throw e;
        }
      }`);
      } catch (e) {
        console.error("Error parsing expression: " + expr);
        throw e;
      }
      return function(scope) {
        return func.call(scope);
      };
    }
    function asArray2(value) {
      if (value === null || value === void 0)
        return [];
      if (Array.isArray(value))
        return value;
      return [value];
    }
    function mergeSort2(list, sortfunc) {
      function merge(sortfunc2, a, b) {
        let ia = 0;
        let ib = 0;
        const sorted = [];
        while (ia < a.length && ib < b.length) {
          if (sortfunc2(a[ia], b[ib]) <= 0) {
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
      for (let chunkSize = 1; chunkSize < list.length; chunkSize *= 2) {
        for (let i = 0; i < list.length; i += chunkSize * 2) {
          const listA = list.slice(i, i + chunkSize);
          const listB = list.slice(i + chunkSize, i + chunkSize * 2);
          const merged = merge(sortfunc, listA, listB);
          const args = [i, merged.length];
          Array.prototype.push.apply(args, merged);
          Array.prototype.splice.apply(list, args);
        }
      }
      return list;
    }
    var $escape3 = exports2.$escape = function(val) {
      return val.replace(/([!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~])/g, "\\$1");
    };
    function mapValues2(obj, f) {
      const newObj = {};
      for (const key in obj) {
        if (obj.hasOwnProperty(key))
          newObj[key] = f(obj[key], key, obj);
      }
      return newObj;
    }
    function isnan2(x) {
      return typeof x === "number" && isNaN(x);
    }
    function _equal2(x, y) {
      if ($.type(x) === "object" && $.type(y) === "object") {
        if (Object.keys(x).length !== Object.keys(y).length)
          return false;
        for (const prop in x) {
          if (!y.hasOwnProperty(prop) || !_equal2(x[prop], y[prop]))
            return false;
        }
        return true;
      } else if ($.type(x) === "array" && $.type(y) === "array") {
        if (x.length !== y.length)
          return false;
        for (let i = 0; i < x.length; i++)
          if (!_equal2(x[i], y[i]))
            return false;
        return true;
      } else {
        return x === y;
      }
    }
    function equal2(...args) {
      if (args.length < 2)
        throw new Error("equal requires at least two arguments.");
      for (let i = 0; i < args.length - 1; i++) {
        if (!_equal2(args[i], args[i + 1]))
          return false;
      }
      return true;
    }
    var compareVersion3 = function(a, op, b) {
      function versionParts(ver) {
        return (ver + "").replace(/-/, ".").replace(/(\.0)+[^.]*$/, "").split(".");
      }
      function cmpVersion(a2, b2) {
        a2 = versionParts(a2);
        b2 = versionParts(b2);
        const len = Math.min(a2.length, b2.length);
        let cmp;
        for (let i = 0; i < len; i++) {
          cmp = parseInt(a2[i], 10) - parseInt(b2[i], 10);
          if (cmp !== 0) {
            return cmp;
          }
        }
        return a2.length - b2.length;
      }
      const diff = cmpVersion(a, b);
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
    function updateLabel2(labelTxt, labelNode) {
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
    function getComputedLinkColor2(el) {
      const a = document.createElement("a");
      a.href = "/";
      const div = document.createElement("div");
      div.style.setProperty("position", "absolute", "important");
      div.style.setProperty("top", "-1000px", "important");
      div.style.setProperty("left", "0", "important");
      div.style.setProperty("width", "30px", "important");
      div.style.setProperty("height", "10px", "important");
      div.appendChild(a);
      el.appendChild(div);
      const linkColor = window.getComputedStyle(a).getPropertyValue("color");
      el.removeChild(div);
      return linkColor;
    }
  });

  // src/main.ts
  var require_main = __commonJS((exports) => {
    __markAsModule(exports);
    __export(exports, {
      main: () => main
    });
    var import_utils2 = __toModule(require_utils());
    function main() {
      const Invoker = function(target, func) {
        this.target = target;
        this.func = func;
      };
      (function() {
        this.normalCall = this.immediateCall = function(...args) {
          this.func.apply(this.target, args);
        };
      }).call(Invoker.prototype);
      const Debouncer = function(target, func, delayMs) {
        this.target = target;
        this.func = func;
        this.delayMs = delayMs;
        this.timerId = null;
        this.args = null;
      };
      (function() {
        this.normalCall = function(...args) {
          const self2 = this;
          this.$clearTimer();
          this.args = args;
          this.timerId = setTimeout(function() {
            if (self2.timerId === null)
              return;
            self2.$clearTimer();
            self2.$invoke();
          }, this.delayMs);
        };
        this.immediateCall = function(...args) {
          this.$clearTimer();
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
      const Throttler = function(target, func, delayMs) {
        this.target = target;
        this.func = func;
        this.delayMs = delayMs;
        this.timerId = null;
        this.args = null;
      };
      (function() {
        this.normalCall = function(...args) {
          const self2 = this;
          this.args = args;
          if (this.timerId === null) {
            this.$invoke();
            this.timerId = setTimeout(function() {
              if (self2.timerId === null)
                return;
              self2.$clearTimer();
              if (args.length > 0)
                self2.normalCall.apply(self2, ...args);
            }, this.delayMs);
          }
        };
        this.immediateCall = function(...args) {
          this.$clearTimer();
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
        let timerId = null;
        let self2, args;
        return function(...args2) {
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
        let executionPending = false;
        let timerId = null;
        let self2, args;
        function throttled(...argumentVals) {
          self2 = null;
          args = null;
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
      const InputBatchSender = function(shinyapp) {
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
            jQuery.each(this.lastChanceCallback, (i, callback) => {
              callback();
            });
            const currentData = this.pendingData;
            this.pendingData = {};
            this.shinyapp.sendInput(currentData);
          } finally {
            this.reentrant = false;
          }
        };
      }).call(InputBatchSender.prototype);
      const InputNoResendDecorator = function(target, initialValues) {
        this.target = target;
        this.lastSentValues = this.reset(initialValues);
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          const {name: inputName, inputType} = splitInputNameType(nameType);
          const jsonValue = JSON.stringify(value);
          if (opts.priority !== "event" && this.lastSentValues[inputName] && this.lastSentValues[inputName].jsonValue === jsonValue && this.lastSentValues[inputName].inputType === inputType) {
            return;
          }
          this.lastSentValues[inputName] = {jsonValue, inputType};
          this.target.setInput(nameType, value, opts);
        };
        this.reset = function(values = {}) {
          const cacheValues = {};
          for (const inputName in values) {
            if (values.hasOwnProperty(inputName)) {
              const {name, inputType} = splitInputNameType(inputName);
              cacheValues[name] = {
                jsonValue: JSON.stringify(values[inputName]),
                inputType
              };
            }
          }
          this.lastSentValues = cacheValues;
        };
        this.forget = function(name) {
          delete this.lastSentValues[name];
        };
      }).call(InputNoResendDecorator.prototype);
      const InputEventDecorator = function(target) {
        this.target = target;
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          const evt = jQuery.Event("shiny:inputchanged");
          const input = splitInputNameType(nameType);
          evt.name = input.name;
          evt.inputType = input.inputType;
          evt.value = value;
          evt.binding = opts.binding;
          evt.el = opts.el;
          evt.priority = opts.priority;
          jQuery(opts.el).trigger(evt);
          if (!evt.isDefaultPrevented()) {
            let name = evt.name;
            if (evt.inputType !== "")
              name += ":" + evt.inputType;
            this.target.setInput(name, evt.value, {priority: opts.priority});
          }
        };
      }).call(InputEventDecorator.prototype);
      const InputRateDecorator = function(target) {
        this.target = target;
        this.inputRatePolicies = {};
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          const {name: inputName} = splitInputNameType(nameType);
          this.$ensureInit(inputName);
          if (opts.priority !== "deferred")
            this.inputRatePolicies[inputName].immediateCall(nameType, value, opts);
          else
            this.inputRatePolicies[inputName].normalCall(nameType, value, opts);
        };
        this.setRatePolicy = function(nameType, mode, millis) {
          const {name: inputName} = splitInputNameType(nameType);
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
      const InputDeferDecorator = function(target) {
        this.target = target;
        this.pendingInput = {};
      };
      (function() {
        this.setInput = function(nameType, value, opts) {
          if (/^\./.test(nameType))
            this.target.setInput(nameType, value, opts);
          else
            this.pendingInput[nameType] = {value, opts};
        };
        this.submit = function() {
          for (const nameType in this.pendingInput) {
            if (this.pendingInput.hasOwnProperty(nameType)) {
              const {value, opts} = this.pendingInput[nameType];
              this.target.setInput(nameType, value, opts);
            }
          }
        };
      }).call(InputDeferDecorator.prototype);
      const InputValidateDecorator = function(target) {
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
        opts = jQuery.extend({
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
        const name2 = nameType.split(":");
        return {
          name: name2[0],
          inputType: name2.length > 1 ? name2[1] : ""
        };
      }
      const ShinyApp = function() {
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
          jQuery.extend(this.$inputValues, initialInput);
          this.$updateConditionals();
        };
        this.isConnected = function() {
          return !!this.$socket;
        };
        let scheduledReconnect = null;
        this.reconnect = function() {
          clearTimeout(scheduledReconnect);
          if (this.isConnected())
            throw "Attempted to reconnect, but already connected.";
          this.$socket = this.createSocket();
          this.$initialInput = jQuery.extend({}, this.$inputValues);
          this.$updateConditionals();
        };
        this.createSocket = function() {
          const self2 = this;
          const createSocketFunc = WindowShiny.createSocket || function() {
            let protocol = "ws:";
            if (window.location.protocol === "https:")
              protocol = "wss:";
            let defaultPath = window.location.pathname;
            if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
              defaultPath = encodeURI(defaultPath);
              if (isQt) {
                defaultPath = encodeURI(defaultPath);
              }
            }
            if (!/\/$/.test(defaultPath))
              defaultPath += "/";
            defaultPath += "websocket/";
            const ws = new WebSocket(protocol + "//" + window.location.host + defaultPath);
            ws.binaryType = "arraybuffer";
            return ws;
          };
          const socket = createSocketFunc();
          let hasOpened = false;
          socket.onopen = function() {
            hasOpened = true;
            jQuery(document).trigger({
              type: "shiny:connected",
              socket
            });
            self2.onConnected();
            socket.send(JSON.stringify({
              method: "init",
              data: self2.$initialInput
            }));
            while (self2.$pendingMessages.length) {
              const msg = self2.$pendingMessages.shift();
              socket.send(msg);
            }
          };
          socket.onmessage = function(e) {
            self2.dispatchMessage(e.data);
          };
          socket.onclose = function() {
            if (hasOpened) {
              jQuery(document).trigger({
                type: "shiny:disconnected",
                socket
              });
              self2.$notifyDisconnected();
            }
            self2.onDisconnected();
            self2.$removeSocket();
          };
          return socket;
        };
        this.sendInput = function(values) {
          const msg = JSON.stringify({
            method: "update",
            data: values
          });
          this.$sendMsg(msg);
          jQuery.extend(this.$inputValues, values);
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
          const self2 = this;
          scheduledReconnect = setTimeout(function() {
            self2.reconnect();
          }, delay);
        };
        const reconnectDelay = function() {
          let attempts = 0;
          const delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];
          return {
            next: function() {
              let i = attempts;
              if (i >= delays.length) {
                i = delays.length - 1;
              }
              attempts++;
              return delays[i];
            },
            reset: function() {
              attempts = 0;
            }
          };
        }();
        this.onDisconnected = function() {
          const $overlay = jQuery("#shiny-disconnected-overlay");
          if ($overlay.length === 0) {
            jQuery(document.body).append('<div id="shiny-disconnected-overlay"></div>');
          }
          if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
            const delay = reconnectDelay.next();
            WindowShiny.showReconnectDialog(delay);
            this.$scheduleReconnect(delay);
          }
        };
        this.onConnected = function() {
          jQuery("#shiny-disconnected-overlay").remove();
          WindowShiny.hideReconnectDialog();
          reconnectDelay.reset();
        };
        this.makeRequest = function(method, args, onSuccess, onError, blobs) {
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
            const uint32_to_buf = function(val) {
              const buffer = new ArrayBuffer(4);
              const view = new DataView(buffer);
              view.setUint32(0, val, true);
              return buffer;
            };
            const payload = [];
            payload.push(uint32_to_buf(16908802));
            const jsonBuf = makeBlob([msg]);
            payload.push(uint32_to_buf(jsonBuf.size));
            payload.push(jsonBuf);
            for (let i = 0; i < blobs.length; i++) {
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
          const binding = this.$bindings[name];
          const evt = jQuery.Event("shiny:error");
          evt.name = name;
          evt.error = error;
          evt.binding = binding;
          jQuery(binding ? binding.el : document).trigger(evt);
          if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
            binding.onValueError(evt.error);
          }
        };
        this.receiveOutput = function(name, value) {
          const binding = this.$bindings[name];
          const evt = jQuery.Event("shiny:value");
          evt.name = name;
          evt.value = value;
          evt.binding = binding;
          if (this.$values[name] === value) {
            jQuery(binding ? binding.el : document).trigger(evt);
            return void 0;
          }
          this.$values[name] = value;
          delete this.$errors[name];
          jQuery(binding ? binding.el : document).trigger(evt);
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
          return Object.keys(scopeComponent).filter((k) => k.indexOf(nsPrefix) === 0).map((k) => ({[k.substring(nsPrefix.length)]: scopeComponent[k]})).reduce((obj, pair) => jQuery.extend(obj, pair), {});
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
          jQuery(document).trigger({
            type: "shiny:conditional"
          });
          const inputs = {};
          for (const name in this.$inputValues) {
            if (this.$inputValues.hasOwnProperty(name)) {
              const shortName = name.replace(/:.*/, "");
              inputs[shortName] = this.$inputValues[name];
            }
          }
          const scope = {input: inputs, output: this.$values};
          const conditionals = jQuery(document).find("[data-display-if]");
          for (let i = 0; i < conditionals.length; i++) {
            const el = jQuery(conditionals[i]);
            let condFunc = el.data("data-display-if-func");
            if (!condFunc) {
              const condExpr = el.attr("data-display-if");
              condFunc = import_utils2.scopeExprToFunc(condExpr);
              el.data("data-display-if-func", condFunc);
            }
            const nsPrefix = el.attr("data-ns-prefix");
            const nsScope = narrowScope(scope, nsPrefix);
            const show = condFunc(nsScope);
            const showing = el.css("display") !== "none";
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
        const messageHandlerOrder = [];
        const messageHandlers = {};
        const customMessageHandlerOrder = [];
        const customMessageHandlers = {};
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
        WindowShiny.addCustomMessageHandler = addCustomMessageHandler;
        this.dispatchMessage = function(data) {
          let msgObj = {};
          if (typeof data === "string") {
            msgObj = JSON.parse(data);
          } else {
            const len = new DataView(data, 0, 1).getUint8(0);
            const typedv = new DataView(data, 1, len);
            const typebuf = [];
            for (let i = 0; i < len; i++) {
              typebuf.push(String.fromCharCode(typedv.getUint8(i)));
            }
            const type = typebuf.join("");
            data = data.slice(len + 1);
            msgObj.custom = {};
            msgObj.custom[type] = data;
          }
          const evt = jQuery.Event("shiny:message");
          evt.message = msgObj;
          jQuery(document).trigger(evt);
          if (evt.isDefaultPrevented())
            return;
          this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);
          this.$updateConditionals();
        };
        this._sendMessagesToHandlers = function(msgObj, handlers, handlerOrder) {
          for (let i = 0; i < handlerOrder.length; i++) {
            const msgType = handlerOrder[i];
            if (msgObj.hasOwnProperty(msgType)) {
              handlers[msgType].call(this, msgObj[msgType]);
            }
          }
        };
        addMessageHandler("values", function(message) {
          for (const name in this.$bindings) {
            if (this.$bindings.hasOwnProperty(name))
              this.$bindings[name].showProgress(false);
          }
          for (const key in message) {
            if (message.hasOwnProperty(key))
              this.receiveOutput(key, message[key]);
          }
        });
        addMessageHandler("errors", function(message) {
          for (const key in message) {
            if (message.hasOwnProperty(key))
              this.receiveError(key, message[key]);
          }
        });
        addMessageHandler("inputMessages", function(message) {
          for (let i = 0; i < message.length; i++) {
            const $obj = jQuery(".shiny-bound-input#" + import_utils2.$escape(message[i].id));
            const inputBinding = $obj.data("shiny-input-binding");
            if ($obj.length > 0) {
              if (!$obj.attr("aria-live"))
                $obj.attr("aria-live", "polite");
              const el = $obj[0];
              const evt = jQuery.Event("shiny:updateinput");
              evt.message = message[i].message;
              evt.binding = inputBinding;
              jQuery(el).trigger(evt);
              if (!evt.isDefaultPrevented())
                inputBinding.receiveMessage(el, evt.message);
            }
          }
        });
        addMessageHandler("javascript", function(message) {
          eval(message);
        });
        addMessageHandler("console", function(message) {
          for (let i = 0; i < message.length; i++) {
            if (console.log)
              console.log(message[i]);
          }
        });
        addMessageHandler("progress", function(message) {
          if (message.type && message.message) {
            const handler = progressHandlers[message.type];
            if (handler)
              handler.call(this, message.message);
          }
        });
        addMessageHandler("notification", function(message) {
          if (message.type === "show")
            WindowShiny.notifications.show(message.message);
          else if (message.type === "remove")
            WindowShiny.notifications.remove(message.message);
          else
            throw "Unkown notification type: " + message.type;
        });
        addMessageHandler("modal", function(message) {
          if (message.type === "show")
            WindowShiny.modal.show(message.message);
          else if (message.type === "remove")
            WindowShiny.modal.remove();
          else
            throw "Unkown modal type: " + message.type;
        });
        addMessageHandler("response", function(message) {
          const requestId = message.tag;
          const request = this.$activeRequests[requestId];
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
          if (WindowShiny.oncustommessage) {
            WindowShiny.oncustommessage(message);
          }
          this._sendMessagesToHandlers(message, customMessageHandlers, customMessageHandlerOrder);
        });
        addMessageHandler("config", function(message) {
          this.config = {
            workerId: message.workerId,
            sessionId: message.sessionId
          };
          if (message.user)
            WindowShiny.user = message.user;
          jQuery(document).trigger("shiny:sessioninitialized");
        });
        addMessageHandler("busy", function(message) {
          if (message === "busy") {
            jQuery(document.documentElement).addClass("shiny-busy");
            jQuery(document).trigger("shiny:busy");
          } else if (message === "idle") {
            jQuery(document.documentElement).removeClass("shiny-busy");
            jQuery(document).trigger("shiny:idle");
          }
        });
        addMessageHandler("recalculating", function(message) {
          if (message.hasOwnProperty("name") && message.hasOwnProperty("status")) {
            const binding = this.$bindings[message.name];
            jQuery(binding ? binding.el : null).trigger({
              type: "shiny:" + message.status
            });
          }
        });
        addMessageHandler("reload", function(message) {
          window.location.reload();
        });
        addMessageHandler("shiny-insert-ui", function(message) {
          const targets = jQuery(message.selector);
          if (targets.length === 0) {
            console.warn('The selector you chose ("' + message.selector + '") could not be found in the DOM.');
            WindowShiny.renderHtml(message.content.html, jQuery([]), message.content.deps);
          } else {
            targets.each(function(i, target) {
              WindowShiny.renderContent(target, message.content, message.where);
              return message.multiple;
            });
          }
        });
        addMessageHandler("shiny-remove-ui", function(message) {
          const els = jQuery(message.selector);
          els.each(function(i, el) {
            WindowShiny.unbindAll(el, true);
            jQuery(el).remove();
            return message.multiple;
          });
        });
        addMessageHandler("frozen", function(message) {
          for (let i = 0; i < message.ids.length; i++) {
            WindowShiny.forgetLastInputValue(message.ids[i]);
          }
        });
        function getTabset(id) {
          const $tabset = jQuery("#" + import_utils2.$escape(id));
          if ($tabset.length === 0)
            throw "There is no tabsetPanel (or navbarPage or navlistPanel) with id equal to '" + id + "'";
          return $tabset;
        }
        function getTabContent($tabset) {
          const tabsetId = $tabset.attr("data-tabsetid");
          const $tabContent = jQuery("div.tab-content[data-tabsetid='" + import_utils2.$escape(tabsetId) + "']");
          return $tabContent;
        }
        function getTargetTabs($tabset, $tabContent, target) {
          const dataValue = "[data-value='" + import_utils2.$escape(target) + "']";
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
            $dropdownLiTags.each(function(i, el) {
              $liTags.push(jQuery(el));
            });
            const selector = "div.tab-pane[id^='tab-" + import_utils2.$escape(dropdownId) + "']";
            const $dropdownDivs = $tabContent.find(selector);
            $dropdownDivs.each(function(i, el) {
              $divTags.push(jQuery(el));
            });
          } else {
            $divTags.push($tabContent.find("div" + dataValue));
          }
          return {$liTag, $liTags, $divTags};
        }
        addMessageHandler("shiny-insert-tab", function(message) {
          const $parentTabset = getTabset(message.inputId);
          let $tabset = $parentTabset;
          const $tabContent = getTabContent($tabset);
          let tabsetId = $parentTabset.attr("data-tabsetid");
          const $divTag = jQuery(message.divTag.html);
          const $liTag = jQuery(message.liTag.html);
          const $aTag = $liTag.find("> a");
          let target = null;
          let $targetLiTag = null;
          if (message.target !== null) {
            target = getTargetTabs($tabset, $tabContent, message.target);
            $targetLiTag = target.$liTag;
          }
          const dropdown = getDropdown();
          if (dropdown !== null) {
            if ($aTag.attr("data-toggle") === "dropdown")
              throw "Cannot insert a navbarMenu inside another one";
            $tabset = dropdown.$tabset;
            tabsetId = dropdown.id;
          }
          if ($aTag.attr("data-toggle") === "tab") {
            const index = getTabIndex($tabset, tabsetId);
            const tabId = "tab-" + tabsetId + "-" + index;
            $liTag.find("> a").attr("href", "#" + tabId);
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
          WindowShiny.renderContent($liTag[0], {
            html: $liTag.html(),
            deps: message.liTag.deps
          });
          WindowShiny.renderContent($tabContent[0], {html: "", deps: message.divTag.deps}, "beforeend");
          $divTag.get().forEach((el) => {
            $tabContent[0].appendChild(el);
            WindowShiny.renderContent(el, el.innerHTML || el.textContent);
          });
          if (message.select) {
            $liTag.find("a").tab("show");
          }
          function getTabIndex($tabset2, tabsetId2) {
            const existingTabIds = [0];
            $tabset2.find("> li").each(function() {
              const $tab = jQuery(this).find("> a[data-toggle='tab']");
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
              const $dropdownATag = jQuery("a.dropdown-toggle[data-value='" + import_utils2.$escape(message.menuName) + "']");
              if ($dropdownATag.length === 0) {
                throw "There is no navbarMenu with menuName equal to '" + message.menuName + "'";
              }
              const $dropdownTabset = $dropdownATag.find("+ ul.dropdown-menu");
              const dropdownId = $dropdownTabset.attr("data-tabsetid");
              return {$tabset: $dropdownTabset, id: dropdownId};
            } else if (message.target !== null) {
              const $uncleTabset = $targetLiTag.parent("ul");
              if ($uncleTabset.hasClass("dropdown-menu")) {
                const uncleId = $uncleTabset.attr("data-tabsetid");
                return {$tabset: $uncleTabset, id: uncleId};
              }
            }
            return null;
          }
        });
        function ensureTabsetHasVisibleTab($tabset) {
          if ($tabset.find("li.active").not(".dropdown").length === 0) {
            const destTabValue = getFirstTab($tabset);
            const inputBinding = $tabset.data("shiny-input-binding");
            const evt = jQuery.Event("shiny:updateinput");
            evt.binding = inputBinding;
            $tabset.trigger(evt);
            inputBinding.setValue($tabset[0], destTabValue);
          }
        }
        function getFirstTab($ul) {
          return $ul.find("li:visible a[data-toggle='tab']").first().attr("data-value") || null;
        }
        function tabApplyFunction(target, func, liTags = false) {
          jQuery.each(target, function(key, el) {
            if (key === "$liTag") {
              func(el);
            } else if (key === "$divTags") {
              jQuery.each(el, function(i, div) {
                func(div);
              });
            } else if (liTags && key === "$liTags") {
              jQuery.each(el, function(i, div) {
                func(div);
              });
            }
          });
        }
        addMessageHandler("shiny-remove-tab", function(message) {
          const $tabset = getTabset(message.inputId);
          const $tabContent = getTabContent($tabset);
          const target = getTargetTabs($tabset, $tabContent, message.target);
          tabApplyFunction(target, removeEl);
          ensureTabsetHasVisibleTab($tabset);
          function removeEl($el) {
            WindowShiny.unbindAll($el, true);
            $el.remove();
          }
        });
        addMessageHandler("shiny-change-tab-visibility", function(message) {
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
        });
        addMessageHandler("updateQueryString", function(message) {
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
            jQuery(document).trigger("hashchange");
        });
        addMessageHandler("resetBrush", function(message) {
          WindowShiny.resetBrush(message.brushId);
        });
        const progressHandlers = {
          binding: function(message) {
            const key = message.id;
            const binding = this.$bindings[key];
            if (binding) {
              jQuery(binding.el).trigger({
                type: "shiny:outputinvalidated",
                binding,
                name: key
              });
              if (binding.showProgress)
                binding.showProgress(true);
            }
          },
          open: function(message) {
            if (message.style === "notification") {
              WindowShiny.notifications.show({
                html: `<div id="shiny-progress-${message.id}" class="shiny-progress-notification"><div class="progress active" style="display: none;"><div class="progress-bar"></div></div><div class="progress-text"><span class="progress-message">message</span> <span class="progress-detail"></span></div></div>`,
                id: message.id,
                duration: null
              });
            } else if (message.style === "old") {
              let $container = jQuery(".shiny-progress-container");
              if ($container.length === 0) {
                $container = jQuery('<div class="shiny-progress-container"></div>');
                jQuery(document.body).append($container);
              }
              const depth = jQuery(".shiny-progress.open").length;
              const $progress = jQuery('<div class="shiny-progress open"><div class="progress active"><div class="progress-bar bar"></div></div><div class="progress-text"><span class="progress-message">message</span><span class="progress-detail"></span></div></div>');
              $progress.attr("id", message.id);
              $container.append($progress);
              const $progressBar = $progress.find(".progress");
              $progressBar.css("top", depth * $progressBar.height() + "px");
              const $progressText = $progress.find(".progress-text");
              $progressText.css("top", 3 * $progressBar.height() + depth * $progressText.outerHeight() + "px");
              $progress.hide();
            }
          },
          update: function(message) {
            if (message.style === "notification") {
              const $progress = jQuery("#shiny-progress-" + message.id);
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
              const $progress = jQuery("#" + message.id + ".shiny-progress");
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
              WindowShiny.notifications.remove(message.id);
            } else if (message.style === "old") {
              const $progress = jQuery("#" + message.id + ".shiny-progress");
              $progress.removeClass("open");
              $progress.fadeOut({
                complete: function() {
                  $progress.remove();
                  if (jQuery(".shiny-progress").length === 0)
                    jQuery(".shiny-progress-container").remove();
                }
              });
            }
          }
        };
        WindowShiny.progressHandlers = progressHandlers;
        this.getTestSnapshotBaseUrl = function({fullUrl = true} = {}) {
          const loc = window.location;
          let url = "";
          if (fullUrl) {
            url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
          }
          url += "/session/" + encodeURIComponent(this.config.sessionId) + "/dataobj/shinytest?w=" + encodeURIComponent(this.config.workerId) + "&nonce=" + import_utils2.randomId();
          return url;
        };
      }).call(ShinyApp.prototype);
      WindowShiny.showReconnectDialog = function() {
        let reconnectTime = null;
        function updateTime() {
          const $time = jQuery("#shiny-reconnect-time");
          if ($time.length === 0)
            return;
          const seconds = Math.floor((reconnectTime - new Date().getTime()) / 1e3);
          if (seconds > 0) {
            $time.text(" in " + seconds + "s");
          } else {
            $time.text("...");
          }
          setTimeout(updateTime, 1e3);
        }
        return function(delay) {
          reconnectTime = new Date().getTime() + delay;
          if (jQuery("#shiny-reconnect-text").length > 0)
            return;
          const html = '<span id="shiny-reconnect-text">Attempting to reconnect</span><span id="shiny-reconnect-time"></span>';
          const action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';
          WindowShiny.notifications.show({
            id: "reconnect",
            html,
            action,
            duration: null,
            closeButton: false,
            type: "warning"
          });
          updateTime();
        };
      }();
      WindowShiny.hideReconnectDialog = function() {
        WindowShiny.notifications.remove("reconnect");
      };
      WindowShiny.notifications = function() {
        const fadeDuration = 250;
        function show({
          html = "",
          action = "",
          deps = [],
          duration = 5e3,
          id = null,
          closeButton = true,
          type = null
        } = {}) {
          if (!id)
            id = import_utils2.randomId();
          _createPanel();
          let $notification = _get(id);
          if ($notification.length === 0)
            $notification = _create(id);
          const newHtml = `<div class="shiny-notification-content-text">${html}</div><div class="shiny-notification-content-action">${action}</div>`;
          const $content = $notification.find(".shiny-notification-content");
          WindowShiny.renderContent($content, {html: newHtml, deps});
          const classes = $notification.attr("class").split(/\s+/).filter((cls) => cls.match(/^shiny-notification-/)).join(" ");
          $notification.removeClass(classes);
          if (type && type !== "default")
            $notification.addClass("shiny-notification-" + type);
          const $close = $notification.find(".shiny-notification-close");
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
            WindowShiny.unbindAll(this);
            jQuery(this).remove();
            if (_ids().length === 0) {
              _getPanel().remove();
            }
          });
        }
        function _get(id) {
          if (!id)
            return null;
          return _getPanel().find("#shiny-notification-" + import_utils2.$escape(id));
        }
        function _ids() {
          return _getPanel().find(".shiny-notification").map(function() {
            return this.id.replace(/shiny-notification-/, "");
          }).get();
        }
        function _getPanel() {
          return jQuery("#shiny-notification-panel");
        }
        function _createPanel() {
          const $panel = _getPanel();
          if ($panel.length > 0)
            return $panel;
          jQuery(document.body).append('<div id="shiny-notification-panel">');
          return $panel;
        }
        function _create(id) {
          let $notification = _get(id);
          if ($notification.length === 0) {
            $notification = jQuery(`<div id="shiny-notification-${id}" class="shiny-notification"><div class="shiny-notification-close">&times;</div><div class="shiny-notification-content"></div></div>`);
            $notification.find(".shiny-notification-close").on("click", (e) => {
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
          const removalCallback = setTimeout(function() {
            remove(id);
          }, delay);
          _get(id).data("removalCallback", removalCallback);
        }
        function _clearRemovalCallback(id) {
          const $notification = _get(id);
          const oldRemovalCallback = $notification.data("removalCallback");
          if (oldRemovalCallback) {
            clearTimeout(oldRemovalCallback);
          }
        }
        return {
          show,
          remove
        };
      }();
      WindowShiny.modal = {
        show: function({html = "", deps = []} = {}) {
          jQuery(".modal-backdrop").remove();
          let $modal = jQuery("#shiny-modal-wrapper");
          if ($modal.length === 0) {
            $modal = jQuery('<div id="shiny-modal-wrapper"></div>');
            jQuery(document.body).append($modal);
            $modal.on("hidden.bs.modal", function(e) {
              if (e.target === jQuery("#shiny-modal")[0]) {
                WindowShiny.unbindAll($modal);
                $modal.remove();
              }
            });
          }
          $modal.on("keydown.shinymodal", function(e) {
            if (jQuery("#shiny-modal").data("keyboard") === false)
              return;
            if (e.keyCode === 27) {
              e.stopPropagation();
              e.preventDefault();
            }
          });
          WindowShiny.renderContent($modal, {html, deps});
        },
        remove: function() {
          const $modal = jQuery("#shiny-modal-wrapper");
          $modal.off("keydown.shinymodal");
          if ($modal.find(".modal").length > 0) {
            $modal.find(".modal").modal("hide");
          } else {
            WindowShiny.unbindAll($modal);
            $modal.remove();
          }
        }
      };
      const BindingRegistry = function() {
        this.bindings = [];
        this.bindingNames = {};
      };
      (function() {
        this.register = function(binding, bindingName, priority) {
          const bindingObj = {binding, priority: priority || 0};
          this.bindings.unshift(bindingObj);
          if (bindingName) {
            this.bindingNames[bindingName] = bindingObj;
            binding.name = bindingName;
          }
        };
        this.setPriority = function(bindingName, priority) {
          const bindingObj = this.bindingNames[bindingName];
          if (!bindingObj)
            throw "Tried to set priority on unknown binding " + bindingName;
          bindingObj.priority = priority || 0;
        };
        this.getPriority = function(bindingName) {
          const bindingObj = this.bindingNames[bindingName];
          if (!bindingObj)
            return false;
          return bindingObj.priority;
        };
        this.getBindings = function() {
          return import_utils2.mergeSort(this.bindings, function(a, b) {
            return b.priority - a.priority;
          });
        };
      }).call(BindingRegistry.prototype);
      const inputBindings = WindowShiny.inputBindings = new BindingRegistry();
      const outputBindings = WindowShiny.outputBindings = new BindingRegistry();
      const OutputBinding = WindowShiny.OutputBinding = function() {
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
            jQuery(el).empty();
            return;
          }
          let errClass = "shiny-output-error";
          if (err.type !== null) {
            errClass = errClass + " " + jQuery.map(import_utils2.asArray(err.type), function(type) {
              return errClass + "-" + type;
            }).join(" ");
          }
          jQuery(el).addClass(errClass).text(err.message);
        };
        this.clearError = function(el) {
          jQuery(el).attr("class", function(i, c) {
            return c.replace(/(^|\s)shiny-output-error\S*/g, "");
          });
        };
        this.showProgress = function(el, show) {
          const RECALC_CLASS = "recalculating";
          if (show)
            jQuery(el).addClass(RECALC_CLASS);
          else
            jQuery(el).removeClass(RECALC_CLASS);
        };
      }).call(OutputBinding.prototype);
      const textOutputBinding = new OutputBinding();
      jQuery.extend(textOutputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-text-output");
        },
        renderValue: function(el, data) {
          jQuery(el).text(data);
        }
      });
      outputBindings.register(textOutputBinding, "shiny.textOutput");
      const imageOutputBinding = new OutputBinding();
      jQuery.extend(imageOutputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-image-output, .shiny-plot-output");
        },
        renderValue: function(el, data) {
          const outputId = this.getId(el);
          const $el = jQuery(el);
          let img;
          let $img = $el.find("img");
          if ($img.length === 0) {
            img = document.createElement("img");
            $el.append(img);
            $img = jQuery(img);
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
          const opts = {
            clickId: $el.data("click-id"),
            clickClip: OR(import_utils2.strToBool($el.data("click-clip")), true),
            dblclickId: $el.data("dblclick-id"),
            dblclickClip: OR(import_utils2.strToBool($el.data("dblclick-clip")), true),
            dblclickDelay: OR($el.data("dblclick-delay"), 400),
            hoverId: $el.data("hover-id"),
            hoverClip: OR(import_utils2.strToBool($el.data("hover-clip")), true),
            hoverDelayType: OR($el.data("hover-delay-type"), "debounce"),
            hoverDelay: OR($el.data("hover-delay"), 300),
            hoverNullOutside: OR(import_utils2.strToBool($el.data("hover-null-outside")), false),
            brushId: $el.data("brush-id"),
            brushClip: OR(import_utils2.strToBool($el.data("brush-clip")), true),
            brushDelayType: OR($el.data("brush-delay-type"), "debounce"),
            brushDelay: OR($el.data("brush-delay"), 300),
            brushFill: OR($el.data("brush-fill"), "#666"),
            brushStroke: OR($el.data("brush-stroke"), "#000"),
            brushOpacity: OR($el.data("brush-opacity"), 0.3),
            brushDirection: OR($el.data("brush-direction"), "xy"),
            brushResetOnNew: OR(import_utils2.strToBool($el.data("brush-reset-on-new")), false),
            coordmap: data.coordmap
          };
          if (opts.brushFill === "auto") {
            opts.brushFill = import_utils2.getComputedLinkColor($el[0]);
          }
          if (opts.brushStroke === "auto") {
            opts.brushStroke = import_utils2.getStyle($el[0], "color");
          }
          jQuery.each(data, function(key, value) {
            if (value === null || key === "coordmap") {
              return;
            }
            if (key === "src" && value === img.getAttribute("src")) {
              img.removeAttribute("src");
            }
            img.setAttribute(key, value);
          });
          for (let i = 0; i < img.attributes.length; i++) {
            const attrib = img.attributes[i];
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
            const clickInfo = imageutils.createClickInfo($el, opts.dblclickId, opts.dblclickDelay);
            $el.on("mousedown.image_output", clickInfo.mousedown);
            if (isIE && IEVersion === 8) {
              $el.on("dblclick.image_output", clickInfo.dblclickIE8);
            }
            if (opts.clickId) {
              imageutils.disableDrag($el, $img);
              const clickHandler = imageutils.createClickHandler(opts.clickId, opts.clickClip, opts.coordmap);
              $el.on("mousedown2.image_output", clickHandler.mousedown);
              $el.on("resize.image_output", clickHandler.onResize);
              $img.on("reset.image_output", clickHandler.onResetImg);
            }
            if (opts.dblclickId) {
              imageutils.disableDrag($el, $img);
              const dblclickHandler = imageutils.createClickHandler(opts.dblclickId, opts.clickClip, opts.coordmap);
              $el.on("dblclick2.image_output", dblclickHandler.mousedown);
              $el.on("resize.image_output", dblclickHandler.onResize);
              $img.on("reset.image_output", dblclickHandler.onResetImg);
            }
            if (opts.hoverId) {
              imageutils.disableDrag($el, $img);
              const hoverHandler = imageutils.createHoverHandler(opts.hoverId, opts.hoverDelay, opts.hoverDelayType, opts.hoverClip, opts.hoverNullOutside, opts.coordmap);
              $el.on("mousemove.image_output", hoverHandler.mousemove);
              $el.on("mouseout.image_output", hoverHandler.mouseout);
              $el.on("resize.image_output", hoverHandler.onResize);
              $img.on("reset.image_output", hoverHandler.onResetImg);
            }
            if (opts.brushId) {
              imageutils.disableDrag($el, $img);
              const brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts, opts.coordmap, outputId);
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
        renderError: function(el, err) {
          jQuery(el).find("img").trigger("reset");
          OutputBinding.prototype.renderError.call(this, el, err);
        },
        clearError: function(el) {
          jQuery(el).contents().filter(function() {
            return this.tagName !== "IMG" && this.id !== el.id + "_brush";
          }).remove();
          OutputBinding.prototype.clearError.call(this, el);
        },
        resize: function(el, width, height) {
          jQuery(el).find("img").trigger("resize");
        }
      });
      outputBindings.register(imageOutputBinding, "shiny.imageOutput");
      const imageutils = {};
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
          const factor = (rangeMax - rangeMin) / (domainMax - domainMin);
          const val = x - domainMin;
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
        function addScaleFuns(panel) {
          const d = panel.domain;
          const r = panel.range;
          const xlog = panel.log && panel.log.x ? panel.log.x : null;
          const ylog = panel.log && panel.log.y ? panel.log.y : null;
          const xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
          const yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);
          panel.scaleDataToImg = function(val, clip) {
            return import_utils2.mapValues(val, (value, key) => {
              const prefix = key.substring(0, 1);
              if (prefix === "x") {
                return xscaler.scale(value, clip);
              } else if (prefix === "y") {
                return yscaler.scale(value, clip);
              }
              return null;
            });
          };
          panel.scaleImgToData = function(val, clip) {
            return import_utils2.mapValues(val, (value, key) => {
              const prefix = key.substring(0, 1);
              if (prefix === "x") {
                return xscaler.scaleInv(value, clip);
              } else if (prefix === "y") {
                return yscaler.scaleInv(value, clip);
              }
              return null;
            });
          };
          panel.clipImg = function(offset_img) {
            const newOffset = {
              x: offset_img.x,
              y: offset_img.y
            };
            const bounds = panel.range;
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
        for (let i = 0; i < panels.length; i++) {
          const panel = panels[i];
          addScaleFuns(panel);
        }
      };
      imageutils.initCoordmap = function($el, coordmap) {
        const $img = $el.find("img");
        const img = $img[0];
        if (coordmap.panels.length === 0) {
          const bounds = {
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
          const img_origin = findOrigin($img);
          return {
            x: mouseEvent.pageX - img_origin.x,
            y: mouseEvent.pageY - img_origin.y
          };
        };
        coordmap.scaleCssToImg = function(offset_css) {
          const pixel_scaling = coordmap.imgToCssScalingRatio();
          const result = import_utils2.mapValues(offset_css, (value, key) => {
            const prefix = key.substring(0, 1);
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
          const pixel_scaling = coordmap.imgToCssScalingRatio();
          const result = import_utils2.mapValues(offset_img, (value, key) => {
            const prefix = key.substring(0, 1);
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
          const img_dims = findDims($img);
          return {
            x: img_dims.x / coordmap.dims.width,
            y: img_dims.y / coordmap.dims.height
          };
        };
        coordmap.cssToImgScalingRatio = function() {
          const res = coordmap.imgToCssScalingRatio();
          return {
            x: 1 / res.x,
            y: 1 / res.y
          };
        };
        coordmap.getPanelCss = function(offset_css, expand = 0) {
          const offset_img = coordmap.scaleCssToImg(offset_css);
          const x = offset_img.x;
          const y = offset_img.y;
          const cssToImgRatio = coordmap.cssToImgScalingRatio();
          const expand_img = {
            x: expand * cssToImgRatio.x,
            y: expand * cssToImgRatio.y
          };
          const matches = [];
          const dists = [];
          let b;
          let i;
          for (i = 0; i < coordmap.panels.length; i++) {
            b = coordmap.panels[i].range;
            if (x <= b.right + expand_img.x && x >= b.left - expand_img.x && y <= b.bottom + expand_img.y && y >= b.top - expand_img.y) {
              matches.push(coordmap.panels[i]);
              let xdist = 0;
              let ydist = 0;
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
            const min_dist = Math.min.apply(null, dists);
            for (i = 0; i < matches.length; i++) {
              if (dists[i] === min_dist) {
                return matches[i];
              }
            }
          }
          return null;
        };
        coordmap.isInPanelCss = function(offset_css, expand = 0) {
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
              WindowShiny.setInputValue(inputId, null);
              return;
            }
            const coords = {};
            const coords_css = coordmap.mouseOffsetCss(e);
            if (!coordmap.isInPanelCss(coords_css)) {
              if (nullOutside) {
                WindowShiny.setInputValue(inputId, null);
                return;
              }
              if (clip)
                return;
              coords.coords_css = coords_css;
              coords.coords_img = coordmap.scaleCssToImg(coords_css);
              WindowShiny.setInputValue(inputId, coords, {priority: "event"});
              return;
            }
            const panel = coordmap.getPanelCss(coords_css);
            const coords_img = coordmap.scaleCssToImg(coords_css);
            const coords_data = panel.scaleImgToData(coords_img);
            coords.x = coords_data.x;
            coords.y = coords_data.y;
            coords.coords_css = coords_css;
            coords.coords_img = coords_img;
            coords.img_css_ratio = coordmap.cssToImgScalingRatio();
            jQuery.extend(coords, panel.panel_vars);
            coords.mapping = panel.mapping;
            coords.domain = panel.domain;
            coords.range = panel.range;
            coords.log = panel.log;
            WindowShiny.setInputValue(inputId, coords, {priority: "event"});
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
      imageutils.shiftToRange = function(vals, min, max) {
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
        for (let i = 0; i < vals.length; i++) {
          newvals[i] = vals[i] + shiftAmount;
        }
        return newvals;
      };
      imageutils.createClickInfo = function($el, dblclickId, dblclickDelay) {
        let clickTimer = null;
        let pending_e = null;
        function triggerEvent(newEventType, e) {
          const e2 = jQuery.Event(newEventType, {
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
          mousedown,
          dblclickIE8
        };
      };
      imageutils.createClickHandler = function(inputId, clip, coordmap) {
        const clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);
        return {
          mousedown: function(e) {
            if (e.which !== 1)
              return;
            clickInfoSender(e);
          },
          onResetImg: function() {
            clickInfoSender(null);
          },
          onResize: null
        };
      };
      imageutils.createHoverHandler = function(inputId, delay, delayType, clip, nullOutside, coordmap) {
        const sendHoverInfo = coordmap.mouseCoordinateSender(inputId, clip, nullOutside);
        let hoverInfoSender;
        if (delayType === "throttle")
          hoverInfoSender = new Throttler(null, sendHoverInfo, delay);
        else
          hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);
        let mouseout;
        if (nullOutside)
          mouseout = function() {
            hoverInfoSender.normalCall(null);
          };
        else
          mouseout = function() {
          };
        return {
          mousemove: function(e) {
            hoverInfoSender.normalCall(e);
          },
          mouseout,
          onResetImg: function() {
            hoverInfoSender.immediateCall(null);
          },
          onResize: null
        };
      };
      imageutils.createBrushHandler = function(inputId, $el, opts, coordmap, outputId) {
        const expandPixels = 20;
        const brush = imageutils.createBrush($el, opts, coordmap, expandPixels);
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
          const coords = brush.boundsData();
          if (isNaN(coords.xmin)) {
            WindowShiny.setInputValue(inputId, null);
            imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
              brushId: inputId,
              outputId: null
            });
            return;
          }
          const panel = brush.getPanel();
          jQuery.extend(coords, panel.panel_vars);
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
          WindowShiny.setInputValue(inputId, coords);
          $el.data("mostRecentBrush", true);
          imageOutputBinding.find(document).trigger("shiny-internal:brushed", coords);
        }
        let brushInfoSender;
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
          const offset_css = coordmap.mouseOffsetCss(e);
          if (opts.brushClip && !coordmap.isInPanelCss(offset_css, expandPixels))
            return;
          brush.up({x: NaN, y: NaN});
          brush.down(offset_css);
          if (brush.isInResizeArea(offset_css)) {
            brush.startResizing(offset_css);
            jQuery(document).on("mousemove.image_brush", mousemoveResizing).on("mouseup.image_brush", mouseupResizing);
          } else if (brush.isInsideBrush(offset_css)) {
            brush.startDragging(offset_css);
            setCursorStyle("grabbing");
            jQuery(document).on("mousemove.image_brush", mousemoveDragging).on("mouseup.image_brush", mouseupDragging);
          } else {
            const panel = coordmap.getPanelCss(offset_css, expandPixels);
            brush.startBrushing(panel.clipImg(coordmap.scaleCssToImg(offset_css)));
            jQuery(document).on("mousemove.image_brush", mousemoveBrushing).on("mouseup.image_brush", mouseupBrushing);
          }
        }
        function mousemove(e) {
          const offset_css = coordmap.mouseOffsetCss(e);
          if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
            if (brush.isInResizeArea(offset_css)) {
              const r = brush.whichResizeSides(offset_css);
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
          jQuery(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
          jQuery(document).off("mousemove.image_brush").off("mouseup.image_brush");
          brush.up(coordmap.mouseOffsetCss(e));
          brush.stopDragging();
          setCursorStyle("grabbable");
          if (brushInfoSender.isPending())
            brushInfoSender.immediateCall();
        }
        function mouseupResizing(e) {
          if (e.which !== 1)
            return;
          jQuery(document).off("mousemove.image_brush").off("mouseup.image_brush");
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
          mousedown,
          mousemove,
          onResetImg,
          onResize
        };
      };
      imageutils.createBrush = function($el, opts, coordmap, expandPixels) {
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
          state.down = {x: NaN, y: NaN};
          state.up = {x: NaN, y: NaN};
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
          const oldDiv = $el.find("#" + el.id + "_brush");
          if (oldDiv.length === 0)
            return;
          const oldBoundsData = oldDiv.data("bounds-data");
          const oldPanel = oldDiv.data("panel");
          if (!oldBoundsData || !oldPanel)
            return;
          for (let i = 0; i < coordmap.panels.length; i++) {
            const curPanel = coordmap.panels[i];
            if (import_utils2.equal(oldPanel.mapping, curPanel.mapping) && import_utils2.equal(oldPanel.panel_vars, curPanel.panel_vars)) {
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
          const bounds_data = boundsData();
          for (const val in bounds_data) {
            if (import_utils2.isnan(bounds_data[val]))
              return;
          }
          boundsData(bounds_data);
          updateDiv();
        }
        function isInsideBrush(offset_css) {
          const bounds = state.boundsCss;
          return offset_css.x <= bounds.xmax && offset_css.x >= bounds.xmin && offset_css.y <= bounds.ymax && offset_css.y >= bounds.ymin;
        }
        function isInResizeArea(offset_css) {
          const sides = whichResizeSides(offset_css);
          return sides.left || sides.right || sides.top || sides.bottom;
        }
        function whichResizeSides(offset_css) {
          const b = state.boundsCss;
          const e = {
            xmin: b.xmin - resizeExpand,
            xmax: b.xmax + resizeExpand,
            ymin: b.ymin - resizeExpand,
            ymax: b.ymax + resizeExpand
          };
          const res = {
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
            return jQuery.extend({}, state.boundsCss);
          }
          let min_css = {x: box_css.xmin, y: box_css.ymin};
          let max_css = {x: box_css.xmax, y: box_css.ymax};
          const panel = state.panel;
          const panelBounds_img = panel.range;
          if (opts.brushClip) {
            min_css = imgToCss(panel.clipImg(cssToImg(min_css)));
            max_css = imgToCss(panel.clipImg(cssToImg(max_css)));
          }
          if (opts.brushDirection === "xy") {
          } else if (opts.brushDirection === "x") {
            min_css.y = imgToCss({y: panelBounds_img.top}).y;
            max_css.y = imgToCss({y: panelBounds_img.bottom}).y;
          } else if (opts.brushDirection === "y") {
            min_css.x = imgToCss({x: panelBounds_img.left}).x;
            max_css.x = imgToCss({x: panelBounds_img.right}).x;
          }
          state.boundsCss = {
            xmin: min_css.x,
            xmax: max_css.x,
            ymin: min_css.y,
            ymax: max_css.y
          };
          const min_data = state.panel.scaleImgToData(cssToImg(min_css));
          const max_data = state.panel.scaleImgToData(cssToImg(max_css));
          state.boundsData = imageutils.findBox(min_data, max_data);
          state.boundsData = import_utils2.mapValues(state.boundsData, (val) => import_utils2.roundSignif(val, 14));
          $div.data("bounds-data", state.boundsData);
          $div.data("panel", state.panel);
          return void 0;
        }
        function boundsData(box_data) {
          if (box_data === void 0) {
            return jQuery.extend({}, state.boundsData);
          }
          let box_css = imgToCss(state.panel.scaleDataToImg(box_data));
          box_css = import_utils2.mapValues(box_css, (val) => import_utils2.roundSignif(val, 13));
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
          $div = jQuery(document.createElement("div")).attr("id", el.id + "_brush").css({
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
          $div.offset({x: 0, y: 0}).width(0).outerHeight(0);
        }
        function updateDiv() {
          const img_offset_css = findOrigin($el.find("img"));
          const b = state.boundsCss;
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
          state.changeStartBounds = jQuery.extend({}, state.boundsCss);
        }
        function dragTo(offset_css) {
          const dx = offset_css.x - state.down.x;
          const dy = offset_css.y - state.down.y;
          const start = state.changeStartBounds;
          let newBounds_css = {
            xmin: start.xmin + dx,
            xmax: start.xmax + dx,
            ymin: start.ymin + dy,
            ymax: start.ymax + dy
          };
          if (opts.brushClip) {
            const panelBounds_img = state.panel.range;
            const newBounds_img = cssToImg(newBounds_css);
            let xvals_img = [newBounds_img.xmin, newBounds_img.xmax];
            let yvals_img = [newBounds_img.ymin, newBounds_img.ymax];
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
          state.changeStartBounds = jQuery.extend({}, state.boundsCss);
          state.resizeSides = whichResizeSides(state.down);
        }
        function resizeTo(offset_css) {
          const d_css = {
            x: offset_css.x - state.down.x,
            y: offset_css.y - state.down.y
          };
          const d_img = cssToImg(d_css);
          const b_img = cssToImg(state.changeStartBounds);
          const panelBounds_img = state.panel.range;
          if (state.resizeSides.left) {
            const xmin_img = imageutils.shiftToRange(b_img.xmin + d_img.x, panelBounds_img.left, b_img.xmax)[0];
            b_img.xmin = xmin_img;
          } else if (state.resizeSides.right) {
            const xmax_img = imageutils.shiftToRange(b_img.xmax + d_img.x, b_img.xmin, panelBounds_img.right)[0];
            b_img.xmax = xmax_img;
          }
          if (state.resizeSides.top) {
            const ymin_img = imageutils.shiftToRange(b_img.ymin + d_img.y, panelBounds_img.top, b_img.ymax)[0];
            b_img.ymin = ymin_img;
          } else if (state.resizeSides.bottom) {
            const ymax_img = imageutils.shiftToRange(b_img.ymax + d_img.y, b_img.ymin, panelBounds_img.bottom)[0];
            b_img.ymax = ymax_img;
          }
          boundsCss(imgToCss(b_img));
          updateDiv();
        }
        function stopResizing() {
          state.resizing = false;
        }
        return {
          reset,
          importOldBrush,
          isInsideBrush,
          isInResizeArea,
          whichResizeSides,
          onResize,
          boundsCss,
          boundsData,
          getPanel,
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
      };
      WindowShiny.resetBrush = function(brushId) {
        WindowShiny.setInputValue(brushId, null);
        imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
          brushId,
          outputId: null
        });
      };
      function findScalingRatio($el) {
        const boundingRect = $el[0].getBoundingClientRect();
        return {
          x: boundingRect.width / $el.outerWidth(),
          y: boundingRect.height / $el.outerHeight()
        };
      }
      function findOrigin($el) {
        const offset = $el.offset();
        const scaling_ratio = findScalingRatio($el);
        const paddingBorder = {
          left: parseInt($el.css("border-left-width")) + parseInt($el.css("padding-left")),
          top: parseInt($el.css("border-top-width")) + parseInt($el.css("padding-top"))
        };
        return {
          x: offset.left + scaling_ratio.x * paddingBorder.left,
          y: offset.top + scaling_ratio.y * paddingBorder.top
        };
      }
      function findDims($el) {
        const content_ratio = {
          x: $el.width() / $el.outerWidth(),
          y: $el.height() / $el.outerHeight()
        };
        const bounding_rect = $el[0].getBoundingClientRect();
        return {
          x: content_ratio.x * bounding_rect.width,
          y: content_ratio.y * bounding_rect.height
        };
      }
      const htmlOutputBinding = new OutputBinding();
      jQuery.extend(htmlOutputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-html-output");
        },
        onValueError: function(el, err) {
          WindowShiny.unbindAll(el);
          this.renderError(el, err);
        },
        renderValue: function(el, data) {
          WindowShiny.renderContent(el, data);
        }
      });
      outputBindings.register(htmlOutputBinding, "shiny.htmlOutput");
      const renderDependencies = WindowShiny.renderDependencies = function(dependencies) {
        if (dependencies) {
          jQuery.each(dependencies, function(i, dep) {
            renderDependency(dep);
          });
        }
      };
      WindowShiny.renderContent = function(el, content, where = "replace") {
        if (where === "replace") {
          WindowShiny.unbindAll(el);
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
        WindowShiny.renderHtml(html, el, dependencies, where);
        let scope = el;
        if (where === "replace") {
          WindowShiny.initializeInputs(el);
          WindowShiny.bindAll(el);
        } else {
          const $parent = jQuery(el).parent();
          if ($parent.length > 0) {
            scope = $parent;
            if (where === "beforeBegin" || where === "afterEnd") {
              const $grandparent = $parent.parent();
              if ($grandparent.length > 0)
                scope = $grandparent;
            }
          }
          WindowShiny.initializeInputs(scope);
          WindowShiny.bindAll(scope);
        }
      };
      WindowShiny.renderHtml = function(html, el, dependencies, where = "replace") {
        renderDependencies(dependencies);
        return singletons.renderHtml(html, el, where);
      };
      const htmlDependencies = {};
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
      function renderDependency(dep) {
        const restyle = needsRestyle(dep);
        if (htmlDependencies.hasOwnProperty(dep.name) && !restyle)
          return false;
        registerDependency(dep.name, dep.version);
        const href = dep.src.href;
        const $head = jQuery("head").first();
        if (dep.meta && !restyle) {
          const metas = jQuery.map(import_utils2.asArray(dep.meta), function(obj, idx) {
            const name = Object.keys(obj)[0];
            return jQuery("<meta>").attr("name", name).attr("content", obj[name]);
          });
          $head.append(metas);
        }
        if (dep.stylesheet) {
          const links = jQuery.map(import_utils2.asArray(dep.stylesheet), function(stylesheet) {
            return jQuery("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
          });
          if (!restyle) {
            $head.append(links);
          } else {
            let refreshStyle = function(href2, oldSheet) {
              const xhr = new XMLHttpRequest();
              xhr.open("GET", href2);
              xhr.onload = function() {
                const id = "shiny_restyle_" + href2.split("?restyle")[0].replace(/\W/g, "_");
                const oldStyle = $head.find("style#" + id);
                const newStyle = jQuery("<style>").attr("id", id).html(xhr.responseText);
                $head.append(newStyle);
                setTimeout(() => oldStyle.remove(), 500);
                setTimeout(() => removeSheet(oldSheet), 500);
              };
              xhr.send();
            };
            let findSheet = function(href2) {
              for (let i = 0; i < document.styleSheets.length; i++) {
                const sheet = document.styleSheets[i];
                if (typeof sheet.href === "string" && sheet.href.indexOf(href2) > -1) {
                  return sheet;
                }
              }
              return null;
            };
            let removeSheet = function(sheet) {
              if (!sheet)
                return;
              sheet.disabled = true;
              if (isIE)
                sheet.cssText = "";
              jQuery(sheet.ownerNode).remove();
            };
            jQuery.map(links, function(link) {
              const oldSheet = findSheet(link.attr("href"));
              const href2 = link.attr("href") + "?restyle=" + new Date().getTime();
              if (isIE) {
                refreshStyle(href2, oldSheet);
              } else {
                link.attr("href", href2);
                link.attr("onload", () => {
                  setTimeout(() => removeSheet(oldSheet), 500);
                });
                $head.append(link);
              }
            });
            const bindDebouncer = new Debouncer(null, WindowShiny.bindAll, 100);
            setTimeout(() => bindDebouncer.normalCall(), 100);
          }
        }
        if (dep.script && !restyle) {
          const scripts = jQuery.map(import_utils2.asArray(dep.script), function(scriptName) {
            return jQuery("<script>").attr("src", href + "/" + encodeURI(scriptName));
          });
          $head.append(scripts);
        }
        if (dep.attachment && !restyle) {
          let attachments = dep.attachment;
          if (typeof attachments === "string")
            attachments = [attachments];
          if (jQuery.isArray(attachments)) {
            const tmp = {};
            jQuery.each(attachments, function(index, attachment) {
              tmp[index + 1 + ""] = attachment;
            });
            attachments = tmp;
          }
          const attach = jQuery.map(attachments, function(attachment, key) {
            return jQuery("<link rel='attachment'>").attr("id", dep.name + "-" + key + "-attachment").attr("href", href + "/" + encodeURI(attachment));
          });
          $head.append(attach);
        }
        if (dep.head && !restyle) {
          const $newHead = jQuery("<head></head>");
          $newHead.html(dep.head);
          $head.append($newHead.children());
        }
        return true;
      }
      const singletons = {
        knownSingletons: {},
        renderHtml: function(html, el, where) {
          const processed = this._processHtml(html);
          this._addToHead(processed.head);
          this.register(processed.singletons);
          if (where === "replace") {
            jQuery(el).html(processed.html);
          } else {
            el.insertAdjacentHTML(where, processed.html);
          }
          return processed;
        },
        register: function(s) {
          jQuery.extend(this.knownSingletons, s);
        },
        registerNames: function(s) {
          if (typeof s === "string") {
            this.knownSingletons[s] = true;
          } else if (s instanceof Array) {
            for (let i = 0; i < s.length; i++) {
              this.knownSingletons[s[i]] = true;
            }
          }
        },
        _addToHead: function(head) {
          if (head.length > 0) {
            const tempDiv = jQuery("<div>" + head + "</div>")[0];
            const $head = jQuery("head");
            while (tempDiv.hasChildNodes()) {
              $head.append(tempDiv.firstChild);
            }
          }
        },
        _processHtml: function(val) {
          const self2 = this;
          const newSingletons = {};
          let newVal;
          const findNewPayload = function(match, p1, sig, payload) {
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
          const heads = [];
          const headAddPayload = function(match, payload) {
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
      const downloadLinkOutputBinding = new OutputBinding();
      jQuery.extend(downloadLinkOutputBinding, {
        find: function(scope) {
          return jQuery(scope).find("a.shiny-download-link");
        },
        renderValue: function(el, data) {
          jQuery(el).attr("href", data);
        }
      });
      outputBindings.register(downloadLinkOutputBinding, "shiny.downloadLink");
      jQuery(document).on("click.shinyDownloadLink", "a.shiny-download-link", function(e) {
        const evt = jQuery.Event("shiny:filedownload");
        evt.name = this.id;
        evt.href = this.href;
        jQuery(document).trigger(evt);
      });
      const datatableOutputBinding = new OutputBinding();
      jQuery.extend(datatableOutputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-datatable-output");
        },
        onValueError: function(el, err) {
          WindowShiny.unbindAll(el);
          this.renderError(el, err);
        },
        renderValue: function(el, data) {
          const $el = jQuery(el).empty();
          if (!data || !data.colnames)
            return;
          const colnames = jQuery.makeArray(data.colnames);
          let header = jQuery.map(colnames, function(x) {
            return "<th>" + x + "</th>";
          }).join("");
          header = "<thead><tr>" + header + "</tr></thead>";
          let footer = "";
          if (data.options === null || data.options.searching !== false) {
            footer = jQuery.map(colnames, function(x) {
              return '<th><input type="text" placeholder="' + import_utils2.escapeHTML(x.replace(/(<([^>]+)>)/gi, "")) + '" /></th>';
            }).join("");
            footer = "<tfoot>" + footer + "</tfoot>";
          }
          const content = '<table class="table table-striped table-hover">' + header + footer + "</table>";
          $el.append(content);
          if (data.evalOptions)
            jQuery.each(data.evalOptions, function(i, x) {
              data.options[x] = eval("(" + data.options[x] + ")");
            });
          const searchCI = data.options === null || typeof data.options.search === "undefined" || data.options.search.caseInsensitive !== false;
          const oTable = jQuery(el).children("table").DataTable(jQuery.extend({
            processing: true,
            serverSide: true,
            order: [],
            orderClasses: false,
            pageLength: 25,
            ajax: {
              url: data.action,
              type: "POST",
              data: function(d) {
                d.search.caseInsensitive = searchCI;
                d.escape = data.escape;
              }
            }
          }, data.options));
          if (typeof data.callback === "string") {
            const callback = eval("(" + data.callback + ")");
            if (typeof callback === "function")
              callback(oTable);
          }
          $el.find("label input").first().unbind("keyup").keyup(debounce(data.searchDelay, function() {
            oTable.search(this.value).draw();
          }));
          const searchInputs = $el.find("tfoot input");
          if (searchInputs.length > 0) {
            jQuery.each(oTable.settings()[0].aoColumns, function(i, x) {
              if (!x.bSearchable)
                searchInputs.eq(i).hide();
            });
            searchInputs.keyup(debounce(data.searchDelay, function() {
              oTable.column(searchInputs.index(this)).search(this.value).draw();
            }));
          }
          $el.parents(".tab-content").css("overflow", "visible");
        }
      });
      outputBindings.register(datatableOutputBinding, "shiny.datatableOutput");
      const OutputBindingAdapter = function(el, binding) {
        this.el = el;
        this.binding = binding;
        if (binding.resize) {
          this.onResize = import_utils2.makeResizeFilter(el, function(width, height) {
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
      const InputBinding = WindowShiny.InputBinding = function() {
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
      const textInputBinding = new InputBinding();
      jQuery.extend(textInputBinding, {
        find: function(scope) {
          const $inputs = jQuery(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
          return $inputs.not('input[type="text"][id$="-selectized"]');
        },
        getId: function(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function(el) {
          return el.value;
        },
        setValue: function(el, value) {
          el.value = value;
        },
        subscribe: function(el, callback) {
          jQuery(el).on("keyup.textInputBinding input.textInputBinding", function(event) {
            callback(true);
          });
          jQuery(el).on("change.textInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".textInputBinding");
        },
        receiveMessage: function(el, data) {
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          if (data.hasOwnProperty("placeholder"))
            el.placeholder = data.placeholder;
          jQuery(el).trigger("change");
        },
        getState: function(el) {
          return {
            label: this._getLabelNode(el).text(),
            value: el.value,
            placeholder: el.placeholder
          };
        },
        getRatePolicy: function() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        _getLabelNode: function(el) {
          return jQuery(el).parent().find('label[for="' + import_utils2.$escape(el.id) + '"]');
        }
      });
      inputBindings.register(textInputBinding, "shiny.textInput");
      const textareaInputBinding = {};
      jQuery.extend(textareaInputBinding, textInputBinding, {
        find: function(scope) {
          return jQuery(scope).find("textarea");
        }
      });
      inputBindings.register(textareaInputBinding, "shiny.textareaInput");
      const passwordInputBinding = {};
      jQuery.extend(passwordInputBinding, textInputBinding, {
        find: function(scope) {
          return jQuery(scope).find('input[type="password"]');
        },
        getType: function(el) {
          return "shiny.password";
        }
      });
      inputBindings.register(passwordInputBinding, "shiny.passwordInput");
      const numberInputBinding = {};
      jQuery.extend(numberInputBinding, textInputBinding, {
        find: function(scope) {
          return jQuery(scope).find('input[type="number"]');
        },
        getValue: function(el) {
          const numberVal = jQuery(el).val();
          if (/^\s*$/.test(numberVal))
            return null;
          else if (!isNaN(numberVal))
            return +numberVal;
          else
            return numberVal;
        },
        setValue: function(el, value) {
          el.value = value;
        },
        getType: function(el) {
          return "shiny.number";
        },
        receiveMessage: function(el, data) {
          if (data.hasOwnProperty("value"))
            el.value = data.value;
          if (data.hasOwnProperty("min"))
            el.min = data.min;
          if (data.hasOwnProperty("max"))
            el.max = data.max;
          if (data.hasOwnProperty("step"))
            el.step = data.step;
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          jQuery(el).trigger("change");
        },
        getState: function(el) {
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            min: Number(el.min),
            max: Number(el.max),
            step: Number(el.step)
          };
        },
        _getLabelNode: function(el) {
          return jQuery(el).parent().find('label[for="' + import_utils2.$escape(el.id) + '"]');
        }
      });
      inputBindings.register(numberInputBinding, "shiny.numberInput");
      const checkboxInputBinding = new InputBinding();
      jQuery.extend(checkboxInputBinding, {
        find: function(scope) {
          return jQuery(scope).find('input[type="checkbox"]');
        },
        getValue: function(el) {
          return el.checked;
        },
        setValue: function(el, value) {
          el.checked = value;
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change.checkboxInputBinding", function(event) {
            callback(true);
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".checkboxInputBinding");
        },
        getState: function(el) {
          return {
            label: jQuery(el).parent().find("span").text(),
            value: el.checked
          };
        },
        receiveMessage: function(el, data) {
          if (data.hasOwnProperty("value"))
            el.checked = data.value;
          if (data.hasOwnProperty("label"))
            jQuery(el).parent().find("span").text(data.label);
          jQuery(el).trigger("change");
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
        let timeFormatter;
        let prettify;
        if (dataType === "date") {
          timeFormatter = strftime.utc();
          prettify = function(num) {
            return timeFormatter(timeFormat, new Date(num));
          };
        } else if (dataType === "datetime") {
          if (timezone)
            timeFormatter = strftime.timezone(timezone);
          else
            timeFormatter = strftime;
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
      const sliderInputBinding = {};
      jQuery.extend(sliderInputBinding, textInputBinding, {
        find: function(scope) {
          if (!jQuery.fn.ionRangeSlider)
            return [];
          return jQuery(scope).find("input.js-range-slider");
        },
        getType: function(el) {
          const dataType = jQuery(el).data("data-type");
          if (dataType === "date")
            return "shiny.date";
          else if (dataType === "datetime")
            return "shiny.datetime";
          else
            return false;
        },
        getValue: function(el) {
          const $el = jQuery(el);
          const result = jQuery(el).data("ionRangeSlider").result;
          let convert;
          const dataType = $el.data("data-type");
          if (dataType === "date") {
            convert = function(val) {
              return import_utils2.formatDateUTC(new Date(+val));
            };
          } else if (dataType === "datetime") {
            convert = function(val) {
              return +val / 1e3;
            };
          } else {
            convert = function(val) {
              return +val;
            };
          }
          if (this._numValues(el) === 2) {
            return [convert(result.from), convert(result.to)];
          } else {
            return convert(result.from);
          }
        },
        setValue: function(el, value) {
          const $el = jQuery(el);
          const slider = $el.data("ionRangeSlider");
          $el.data("immediate", true);
          try {
            if (this._numValues(el) === 2 && value instanceof Array) {
              slider.update({from: value[0], to: value[1]});
            } else {
              slider.update({from: value});
            }
            forceIonSliderUpdate(slider);
          } finally {
            $el.data("immediate", false);
          }
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change.sliderInputBinding", function(event) {
            callback(!jQuery(el).data("immediate") && !jQuery(el).data("animating"));
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".sliderInputBinding");
        },
        receiveMessage: function(el, data) {
          const $el = jQuery(el);
          const slider = $el.data("ionRangeSlider");
          const msg = {};
          if (data.hasOwnProperty("value")) {
            if (this._numValues(el) === 2 && data.value instanceof Array) {
              msg.from = data.value[0];
              msg.to = data.value[1];
            } else {
              msg.from = data.value;
            }
          }
          const sliderFeatures = ["min", "max", "step"];
          for (let i = 0; i < sliderFeatures.length; i++) {
            const feats = sliderFeatures[i];
            if (data.hasOwnProperty(feats)) {
              msg[feats] = data[feats];
            }
          }
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          const domElements = ["data-type", "time-format", "timezone"];
          for (let i = 0; i < domElements.length; i++) {
            const elem = domElements[i];
            if (data.hasOwnProperty(elem)) {
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
        },
        getRatePolicy: function() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        getState: function(el) {
        },
        initialize: function(el) {
          const opts = {};
          const $el = jQuery(el);
          const dataType = $el.data("data-type");
          const timeFormat = $el.data("time-format");
          const timezone = $el.data("timezone");
          opts.prettify = getTypePrettifyer(dataType, timeFormat, timezone);
          $el.ionRangeSlider(opts);
        },
        _getLabelNode: function(el) {
          return jQuery(el).parent().find('label[for="' + import_utils2.$escape(el.id) + '"]');
        },
        _numValues: function(el) {
          if (jQuery(el).data("ionRangeSlider").options.type === "double")
            return 2;
          else
            return 1;
        }
      });
      inputBindings.register(sliderInputBinding, "shiny.sliderInput");
      function formatNumber(num, thousand_sep = ",", decimal_sep = ".") {
        const parts = num.toString().split(".");
        parts[0] = parts[0].replace(/(\d{1,3}(?=(?:\d\d\d)+(?!\d)))/g, "$1" + thousand_sep);
        if (parts.length === 1)
          return parts[0];
        else if (parts.length === 2)
          return parts[0] + decimal_sep + parts[1];
        else
          return "";
      }
      jQuery(document).on("click", ".slider-animate-button", function(evt) {
        evt.preventDefault();
        const self2 = jQuery(this);
        const target = jQuery("#" + import_utils2.$escape(self2.attr("data-target-id")));
        const startLabel = "Play";
        const stopLabel = "Pause";
        const loop = self2.attr("data-loop") !== void 0 && !/^\s*false\s*$/i.test(self2.attr("data-loop"));
        let animInterval = self2.attr("data-interval");
        if (isNaN(animInterval))
          animInterval = 1500;
        else
          animInterval = +animInterval;
        if (!target.data("animTimer")) {
          let slider;
          let timer;
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
            const sliderCanStep = function() {
              if (slider.options.type === "double")
                return slider.result.to < slider.result.max;
              else
                return slider.result.from < slider.result.max;
            };
            const sliderReset = function() {
              const val = {from: slider.result.min};
              if (slider.options.type === "double")
                val.to = val.from + (slider.result.to - slider.result.from);
              slider.update(val);
              forceIonSliderUpdate(slider);
            };
            const sliderStep = function() {
              const val = {
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
      const dateInputBinding = new InputBinding();
      jQuery.extend(dateInputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-date-input");
        },
        getType: function(el) {
          return "shiny.date";
        },
        getValue: function(el) {
          const date = jQuery(el).find("input").bsDatepicker("getUTCDate");
          return import_utils2.formatDateUTC(date);
        },
        setValue: function(el, value) {
          if (value === null) {
            jQuery(el).find("input").val("").bsDatepicker("update");
            return;
          }
          const date = this._newDate(value);
          if (isNaN(date))
            return;
          jQuery(el).find("input").bsDatepicker("setUTCDate", date);
        },
        getState: function(el) {
          const $el = jQuery(el);
          const $input = $el.find("input");
          let min = $input.data("datepicker").startDate;
          let max = $input.data("datepicker").endDate;
          min = min === -Infinity ? null : import_utils2.formatDateUTC(min);
          max = max === Infinity ? null : import_utils2.formatDateUTC(max);
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
        },
        receiveMessage: function(el, data) {
          const $input = jQuery(el).find("input");
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          if (data.hasOwnProperty("min"))
            this._setMin($input[0], data.min);
          if (data.hasOwnProperty("max"))
            this._setMax($input[0], data.max);
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          jQuery(el).trigger("change");
        },
        subscribe: function(el, callback) {
          jQuery(el).on("keyup.dateInputBinding input.dateInputBinding", function(event) {
            callback(true);
          });
          jQuery(el).on("changeDate.dateInputBinding change.dateInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".dateInputBinding");
        },
        getRatePolicy: function() {
          return {
            policy: "debounce",
            delay: 250
          };
        },
        initialize: function(el) {
          const $input = jQuery(el).find("input");
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
        },
        _getLabelNode: function(el) {
          return jQuery(el).find('label[for="' + import_utils2.$escape(el.id) + '"]');
        },
        _formatToString: function(format) {
          let str = "";
          let i;
          for (i = 0; i < format.parts.length; i++) {
            str += format.separators[i] + format.parts[i];
          }
          str += format.separators[i];
          return str;
        },
        _setMin: function(el, date) {
          if (date === void 0)
            return;
          if (date === null) {
            jQuery(el).bsDatepicker("setStartDate", null);
            return;
          }
          date = this._newDate(date);
          if (date === null)
            return;
          if (isNaN(date))
            return;
          const curValue = jQuery(el).bsDatepicker("getUTCDate");
          jQuery(el).bsDatepicker("setStartDate", this._UTCDateAsLocal(date));
          if (date && curValue && date.getTime() > curValue.getTime()) {
            jQuery(el).bsDatepicker("clearDates");
          } else {
            jQuery(el).bsDatepicker("setUTCDate", curValue);
          }
        },
        _setMax: function(el, date) {
          if (date === void 0)
            return;
          if (date === null) {
            jQuery(el).bsDatepicker("setEndDate", null);
            return;
          }
          date = this._newDate(date);
          if (date === null)
            return;
          if (isNaN(date))
            return;
          const curValue = jQuery(el).bsDatepicker("getUTCDate");
          jQuery(el).bsDatepicker("setEndDate", this._UTCDateAsLocal(date));
          if (date && curValue && date.getTime() < curValue.getTime()) {
            jQuery(el).bsDatepicker("clearDates");
          } else {
            jQuery(el).bsDatepicker("setUTCDate", curValue);
          }
        },
        _newDate: function(date) {
          if (date instanceof Date)
            return date;
          if (!date)
            return null;
          const d = import_utils2.parseDate(date);
          if (isNaN(d))
            return null;
          return d;
        },
        _floorDateTime: function(date) {
          date = new Date(date.getTime());
          date.setUTCHours(0, 0, 0, 0);
          return date;
        },
        _dateAsUTC: function(date) {
          return new Date(date.getTime() - date.getTimezoneOffset() * 6e4);
        },
        _UTCDateAsLocal: function(date) {
          return new Date(date.getTime() + date.getTimezoneOffset() * 6e4);
        }
      });
      inputBindings.register(dateInputBinding, "shiny.dateInput");
      const dateRangeInputBinding = {};
      jQuery.extend(dateRangeInputBinding, dateInputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-date-range-input");
        },
        getValue: function(el) {
          const $inputs = jQuery(el).find("input");
          const start = $inputs.eq(0).bsDatepicker("getUTCDate");
          const end = $inputs.eq(1).bsDatepicker("getUTCDate");
          return [import_utils2.formatDateUTC(start), import_utils2.formatDateUTC(end)];
        },
        setValue: function(el, value) {
          if (!(value instanceof Object)) {
            return;
          }
          const $inputs = jQuery(el).find("input");
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
        },
        getState: function(el) {
          const $el = jQuery(el);
          const $inputs = $el.find("input");
          const $startinput = $inputs.eq(0);
          const $endinput = $inputs.eq(1);
          let min = $startinput.bsDatepicker("getStartDate");
          let max = $startinput.bsDatepicker("getEndDate");
          min = min === -Infinity ? null : import_utils2.formatDateUTC(min);
          max = max === Infinity ? null : import_utils2.formatDateUTC(max);
          let startview = $startinput.data("datepicker").startView;
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
            min,
            max,
            weekstart: $startinput.data("datepicker").weekStart,
            format: this._formatToString($startinput.data("datepicker").format),
            language: $startinput.data("datepicker").language,
            startview
          };
        },
        receiveMessage: function(el, data) {
          const $el = jQuery(el);
          const $inputs = $el.find("input");
          const $startinput = $inputs.eq(0);
          const $endinput = $inputs.eq(1);
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
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
        initialize: function(el) {
          const $el = jQuery(el);
          const $inputs = $el.find("input");
          const $startinput = $inputs.eq(0);
          const $endinput = $inputs.eq(1);
          let start = $startinput.data("initial-date");
          let end = $endinput.data("initial-date");
          if (start === void 0 || start === null)
            start = this._dateAsUTC(new Date());
          if (end === void 0 || end === null)
            end = this._dateAsUTC(new Date());
          this.setValue(el, {start, end});
          this._setMin($startinput[0], $startinput.data("min-date"));
          this._setMin($endinput[0], $startinput.data("min-date"));
          this._setMax($startinput[0], $endinput.data("max-date"));
          this._setMax($endinput[0], $endinput.data("max-date"));
        },
        subscribe: function(el, callback) {
          jQuery(el).on("keyup.dateRangeInputBinding input.dateRangeInputBinding", function(event) {
            callback(true);
          });
          jQuery(el).on("changeDate.dateRangeInputBinding change.dateRangeInputBinding", function(event) {
            callback(false);
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".dateRangeInputBinding");
        },
        _getLabelNode: function(el) {
          return jQuery(el).find('label[for="' + import_utils2.$escape(el.id) + '"]');
        }
      });
      inputBindings.register(dateRangeInputBinding, "shiny.dateRangeInput");
      const selectInputBinding = new InputBinding();
      jQuery.extend(selectInputBinding, {
        find: function(scope) {
          return jQuery(scope).find("select");
        },
        getType: function(el) {
          const $el = jQuery(el);
          if (!$el.hasClass("symbol")) {
            return null;
          }
          if ($el.attr("multiple") === "multiple") {
            return "shiny.symbolList";
          } else {
            return "shiny.symbol";
          }
        },
        getId: function(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function(el) {
          return jQuery(el).val();
        },
        setValue: function(el, value) {
          if (!this._is_selectize(el)) {
            jQuery(el).val(value);
          } else {
            const selectize = this._selectize(el);
            if (selectize) {
              selectize.setValue(value);
            }
          }
        },
        getState: function(el) {
          const options = new Array(el.length);
          for (let i = 0; i < el.length; i++) {
            options[i] = {value: el[i].value, label: el[i].label};
          }
          return {
            label: this._getLabelNode(el),
            value: this.getValue(el),
            options
          };
        },
        receiveMessage: function(el, data) {
          let $el = jQuery(el), selectize;
          if (data.hasOwnProperty("options")) {
            selectize = this._selectize(el);
            if (selectize)
              selectize.destroy();
            $el.empty().append(data.options);
            this._selectize(el);
          }
          if (data.hasOwnProperty("config")) {
            $el.parent().find('script[data-for="' + import_utils2.$escape(el.id) + '"]').replaceWith(data.config);
            this._selectize(el, true);
          }
          if (data.hasOwnProperty("url")) {
            selectize = this._selectize(el);
            selectize.clearOptions();
            let loaded = false;
            selectize.settings.load = function(query, callback) {
              const settings = selectize.settings;
              jQuery.ajax({
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
                  jQuery.each(res, function(index, elem) {
                    const optgroupId = elem[settings.optgroupField || "optgroup"];
                    const optgroup = {};
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
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          jQuery(el).trigger("change");
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change.selectInputBinding", (event) => {
            if (el.nonempty && this.getValue(el) === "") {
              return;
            }
            callback();
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".selectInputBinding");
        },
        initialize: function(el) {
          this._selectize(el);
        },
        _getLabelNode: function(el) {
          let escaped_id = import_utils2.$escape(el.id);
          if (this._is_selectize(el)) {
            escaped_id += "-selectized";
          }
          return jQuery(el).parent().parent().find('label[for="' + escaped_id + '"]');
        },
        _is_selectize: function(el) {
          const config = jQuery(el).parent().find('script[data-for="' + import_utils2.$escape(el.id) + '"]');
          return config.length > 0;
        },
        _selectize: function(el, update) {
          if (!jQuery.fn.selectize)
            return void 0;
          const $el = jQuery(el);
          const config = $el.parent().find('script[data-for="' + import_utils2.$escape(el.id) + '"]');
          if (config.length === 0)
            return void 0;
          let options = jQuery.extend({
            labelField: "label",
            valueField: "value",
            searchField: ["label"]
          }, JSON.parse(config.html()));
          if (typeof config.data("nonempty") !== "undefined") {
            el.nonempty = true;
            options = jQuery.extend(options, {
              onItemRemove: function(value) {
                if (this.getValue() === "")
                  jQuery("select#" + import_utils2.$escape(el.id)).empty().append(jQuery("<option/>", {
                    value,
                    selected: true
                  })).trigger("change");
              },
              onDropdownClose: function($dropdown) {
                if (this.getValue() === "")
                  this.setValue(jQuery("select#" + import_utils2.$escape(el.id)).val());
              }
            });
          } else {
            el.nonempty = false;
          }
          if (config.data("eval") instanceof Array)
            jQuery.each(config.data("eval"), function(i, x) {
              options[x] = eval("(" + options[x] + ")");
            });
          let control = $el.selectize(options)[0].selectize;
          if (update) {
            const settings = jQuery.extend(control.settings, options);
            control.destroy();
            control = $el.selectize(settings)[0].selectize;
          }
          return control;
        }
      });
      inputBindings.register(selectInputBinding, "shiny.selectInput");
      const radioInputBinding = new InputBinding();
      jQuery.extend(radioInputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-input-radiogroup");
        },
        getValue: function(el) {
          const checked_items = jQuery('input:radio[name="' + import_utils2.$escape(el.id) + '"]:checked');
          if (checked_items.length === 0) {
            return null;
          }
          return checked_items.val();
        },
        setValue: function(el, value) {
          if (jQuery.isArray(value) && value.length === 0) {
            jQuery('input:radio[name="' + import_utils2.$escape(el.id) + '"]').prop("checked", false);
          } else {
            jQuery('input:radio[name="' + import_utils2.$escape(el.id) + '"][value="' + import_utils2.$escape(value) + '"]').prop("checked", true);
          }
        },
        getState: function(el) {
          const $objs = jQuery('input:radio[name="' + import_utils2.$escape(el.id) + '"]');
          const options = new Array($objs.length);
          for (let i = 0; i < options.length; i++) {
            options[i] = {value: $objs[i].value, label: this._getLabel($objs[i])};
          }
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            options
          };
        },
        receiveMessage: function(el, data) {
          const $el = jQuery(el);
          if (data.hasOwnProperty("options")) {
            $el.find("div.shiny-options-group").remove();
            $el.find("label.radio").remove();
            $el.append(data.options);
          }
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          jQuery(el).trigger("change");
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change.radioInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".radioInputBinding");
        },
        _getLabelNode: function(el) {
          return jQuery(el).parent().find('label[for="' + import_utils2.$escape(el.id) + '"]');
        },
        _getLabel: function(obj) {
          if (obj.parentNode.tagName === "LABEL") {
            return jQuery(obj.parentNode).find("span").text().trim();
          }
          return null;
        },
        _setLabel: function(obj, value) {
          if (obj.parentNode.tagName === "LABEL") {
            jQuery(obj.parentNode).find("span").text(value);
          }
          return null;
        }
      });
      inputBindings.register(radioInputBinding, "shiny.radioInput");
      const checkboxGroupInputBinding = new InputBinding();
      jQuery.extend(checkboxGroupInputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".shiny-input-checkboxgroup");
        },
        getValue: function(el) {
          const $objs = jQuery('input:checkbox[name="' + import_utils2.$escape(el.id) + '"]:checked');
          const values = new Array($objs.length);
          for (let i = 0; i < $objs.length; i++) {
            values[i] = $objs[i].value;
          }
          return values;
        },
        setValue: function(el, value) {
          jQuery('input:checkbox[name="' + import_utils2.$escape(el.id) + '"]').prop("checked", false);
          if (value instanceof Array) {
            for (let i = 0; i < value.length; i++) {
              jQuery('input:checkbox[name="' + import_utils2.$escape(el.id) + '"][value="' + import_utils2.$escape(value[i]) + '"]').prop("checked", true);
            }
          } else {
            jQuery('input:checkbox[name="' + import_utils2.$escape(el.id) + '"][value="' + import_utils2.$escape(value) + '"]').prop("checked", true);
          }
        },
        getState: function(el) {
          const $objs = jQuery('input:checkbox[name="' + import_utils2.$escape(el.id) + '"]');
          const options = new Array($objs.length);
          for (let i = 0; i < options.length; i++) {
            options[i] = {value: $objs[i].value, label: this._getLabel($objs[i])};
          }
          return {
            label: this._getLabelNode(el).text(),
            value: this.getValue(el),
            options
          };
        },
        receiveMessage: function(el, data) {
          const $el = jQuery(el);
          if (data.hasOwnProperty("options")) {
            $el.find("div.shiny-options-group").remove();
            $el.find("label.checkbox").remove();
            $el.append(data.options);
          }
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          import_utils2.updateLabel(data.label, this._getLabelNode(el));
          jQuery(el).trigger("change");
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change.checkboxGroupInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".checkboxGroupInputBinding");
        },
        _getLabelNode: function(el) {
          return jQuery(el).find('label[for="' + import_utils2.$escape(el.id) + '"]');
        },
        _getLabel: function(obj) {
          if (obj.parentNode.tagName === "LABEL") {
            return jQuery(obj.parentNode).find("span").text().trim();
          }
          return null;
        },
        _setLabel: function(obj, value) {
          if (obj.parentNode.tagName === "LABEL") {
            jQuery(obj.parentNode).find("span").text(value);
          }
          return null;
        }
      });
      inputBindings.register(checkboxGroupInputBinding, "shiny.checkboxGroupInput");
      const actionButtonInputBinding = new InputBinding();
      jQuery.extend(actionButtonInputBinding, {
        find: function(scope) {
          return jQuery(scope).find(".action-button");
        },
        getValue: function(el) {
          return jQuery(el).data("val") || 0;
        },
        setValue: function(el, value) {
          jQuery(el).data("val", value);
        },
        getType: function(el) {
          return "shiny.action";
        },
        subscribe: function(el, callback) {
          jQuery(el).on("click.actionButtonInputBinding", function(e) {
            const $el = jQuery(this);
            const val = $el.data("val") || 0;
            $el.data("val", val + 1);
            callback();
          });
        },
        getState: function(el) {
          return {value: this.getValue(el)};
        },
        receiveMessage: function(el, data) {
          const $el = jQuery(el);
          let label = $el.text();
          let icon = "";
          if ($el.find("i[class]").length > 0) {
            const icon_html = $el.find("i[class]")[0];
            if (icon_html === $el.children()[0]) {
              icon = jQuery(icon_html).prop("outerHTML");
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
        unsubscribe: function(el) {
          jQuery(el).off(".actionButtonInputBinding");
        }
      });
      inputBindings.register(actionButtonInputBinding, "shiny.actionButtonInput");
      jQuery(document).on("click", "a.action-button", function(e) {
        e.preventDefault();
      });
      const bootstrapTabInputBinding = new InputBinding();
      jQuery.extend(bootstrapTabInputBinding, {
        find: function(scope) {
          return jQuery(scope).find("ul.nav.shiny-tab-input");
        },
        getValue: function(el) {
          const anchor = jQuery(el).find("li:not(.dropdown).active").children("a");
          if (anchor.length === 1)
            return this._getTabName(anchor);
          return null;
        },
        setValue: function(el, value) {
          const self2 = this;
          let success = false;
          if (value) {
            const anchors = jQuery(el).find("li:not(.dropdown)").children("a");
            anchors.each(function() {
              if (self2._getTabName(jQuery(this)) === value) {
                jQuery(this).tab("show");
                success = true;
                return false;
              }
              return true;
            });
          }
          if (!success) {
            jQuery(el).trigger("change");
          }
        },
        getState: function(el) {
          return {value: this.getValue(el)};
        },
        receiveMessage: function(el, data) {
          if (data.hasOwnProperty("value"))
            this.setValue(el, data.value);
          jQuery(el).trigger("change");
        },
        subscribe: function(el, callback) {
          jQuery(el).on("change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding", function(event) {
            callback();
          });
        },
        unsubscribe: function(el) {
          jQuery(el).off(".bootstrapTabInputBinding");
        },
        _getTabName: function(anchor) {
          return anchor.attr("data-value") || anchor.text();
        }
      });
      inputBindings.register(bootstrapTabInputBinding, "shiny.bootstrapTabInput");
      const FileUploader = function(shinyapp, id, files, el) {
        this.shinyapp = shinyapp;
        this.id = id;
        this.el = el;
        FileProcessor.call(this, files);
      };
      jQuery.extend(FileUploader.prototype, FileProcessor.prototype);
      (function() {
        this.makeRequest = function(method, args, onSuccess, onFailure, blobs) {
          this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
        };
        this.onBegin = function(files, cont) {
          const self2 = this;
          this.$setError(null);
          this.$setActive(true);
          this.$setVisible(true);
          this.onProgress(null, 0);
          this.totalBytes = 0;
          this.progressBytes = 0;
          jQuery.each(files, function(i, file) {
            self2.totalBytes += file.size;
          });
          const fileInfo = jQuery.map(files, function(file, i) {
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
          const self2 = this;
          this.onProgress(file, 0);
          jQuery.ajax(this.uploadUrl, {
            type: "POST",
            cache: false,
            xhr: function() {
              const xhrVal = jQuery.ajaxSettings.xhr();
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
            success: function() {
              self2.progressBytes += file.size;
              cont();
            },
            error: function(jqXHR, textStatus, errorThrown) {
              self2.onError(jqXHR.responseText || textStatus);
            }
          });
        };
        this.onComplete = function() {
          const self2 = this;
          const fileInfo = jQuery.map(this.files, function(file, i) {
            return {
              name: file.name,
              size: file.size,
              type: file.type
            };
          });
          const evt = jQuery.Event("shiny:inputchanged");
          evt.name = this.id;
          evt.value = fileInfo;
          evt.binding = fileInputBinding;
          evt.el = this.el;
          evt.inputType = "shiny.fileupload";
          jQuery(document).trigger(evt);
          this.makeRequest("uploadEnd", [this.jobId, this.id], function(response) {
            self2.$setActive(false);
            self2.onProgress(null, 1);
            self2.$bar().text("Upload complete");
            jQuery(evt.el).val("");
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
          return jQuery("#" + import_utils2.$escape(this.id) + "_progress.shiny-file-input-progress");
        };
        this.$bar = function() {
          return jQuery("#" + import_utils2.$escape(this.id) + "_progress.shiny-file-input-progress .progress-bar");
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
        const $el = jQuery(el);
        abortCurrentUpload($el);
        setFileText($el, files);
        $el.data("currentUploader", new FileUploader(WindowShiny.shinyapp, fileInputBinding.getId(el), files, el));
      }
      function uploadFiles(evt) {
        const $el = jQuery(evt.target);
        abortCurrentUpload($el);
        const files = evt.target.files;
        const id = fileInputBinding.getId(evt.target);
        if (files.length === 0)
          return;
        setFileText($el, files);
        $el.data("currentUploader", new FileUploader(WindowShiny.shinyapp, id, files, evt.target));
      }
      let $fileInputs = jQuery();
      const fileInputBinding = new InputBinding();
      jQuery.extend(fileInputBinding, {
        find: function(scope) {
          return jQuery(scope).find('input[type="file"]');
        },
        getId: function(el) {
          return InputBinding.prototype.getId.call(this, el) || el.name;
        },
        getValue: function(el) {
          let data = jQuery(el).attr("data-restore");
          if (data) {
            data = JSON.parse(data);
            const $fileText = jQuery(el).closest("div.input-group").find("input[type=text]");
            if (data.name.length === 1) {
              $fileText.val(data.name[0]);
            } else {
              $fileText.val(data.name.length + " files");
            }
            const $progress = jQuery(el).closest("div.form-group").find(".progress");
            const $bar = $progress.find(".progress-bar");
            $progress.removeClass("active");
            $bar.width("100%");
            $bar.css("visibility", "visible");
            return data;
          } else {
            return null;
          }
        },
        setValue: function(el, value) {
        },
        getType: function(el) {
          return "shiny.file";
        },
        _zoneOf: function(el) {
          return jQuery(el).closest("div.input-group");
        },
        _enableDraghover: function(el) {
          let $el = jQuery(el), childCounter = 0;
          $el.on({
            "dragenter.draghover": (e) => {
              if (childCounter++ === 0) {
                $el.trigger("draghover:enter", e);
              }
            },
            "dragleave.draghover": (e) => {
              if (--childCounter === 0) {
                $el.trigger("draghover:leave", e);
              }
              if (childCounter < 0) {
                console.error("draghover childCounter is negative somehow");
              }
            },
            "dragover.draghover": (e) => {
              e.preventDefault();
            },
            "drop.draghover": (e) => {
              childCounter = 0;
              $el.trigger("draghover:drop", e);
              e.preventDefault();
            }
          });
          return $el;
        },
        _disableDraghover: function(el) {
          return jQuery(el).off(".draghover");
        },
        _ZoneClass: {
          ACTIVE: "shiny-file-input-active",
          OVER: "shiny-file-input-over"
        },
        _enableDocumentEvents: function() {
          let $doc = jQuery("html"), {ACTIVE, OVER} = this._ZoneClass;
          this._enableDraghover($doc).on({
            "draghover:enter.draghover": (e) => {
              this._zoneOf($fileInputs).addClass(ACTIVE);
            },
            "draghover:leave.draghover": (e) => {
              this._zoneOf($fileInputs).removeClass(ACTIVE);
            },
            "draghover:drop.draghover": (e) => {
              this._zoneOf($fileInputs).removeClass(OVER).removeClass(ACTIVE);
            }
          });
        },
        _disableDocumentEvents: function() {
          const $doc = jQuery("html");
          $doc.off(".draghover");
          this._disableDraghover($doc);
        },
        _canSetFiles: function(fileList) {
          const testEl = document.createElement("input");
          testEl.type = "file";
          try {
            testEl.files = fileList;
          } catch (e) {
            return false;
          }
          return true;
        },
        _handleDrop: function(e, el) {
          const files = e.originalEvent.dataTransfer.files, $el = jQuery(el);
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
        subscribe: function(el, callback) {
          jQuery(el).on("change.fileInputBinding", uploadFiles);
          if ($fileInputs.length === 0)
            this._enableDocumentEvents();
          $fileInputs = $fileInputs.add(el);
          const $zone = this._zoneOf(el), {OVER} = this._ZoneClass;
          this._enableDraghover($zone).on({
            "draghover:enter.draghover": (e) => {
              $zone.addClass(OVER);
            },
            "draghover:leave.draghover": (e) => {
              $zone.removeClass(OVER);
              e.stopPropagation();
            },
            "draghover:drop.draghover": (e, dropEvent) => {
              this._handleDrop(dropEvent, el);
            }
          });
        },
        unsubscribe: function(el) {
          const $el = jQuery(el), $zone = this._zoneOf(el);
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
      function initShiny() {
        const shinyapp = WindowShiny.shinyapp = new ShinyApp();
        function bindOutputs(scope = document) {
          scope = jQuery(scope);
          const bindings = outputBindings.getBindings();
          for (let i = 0; i < bindings.length; i++) {
            const binding = bindings[i].binding;
            const matches = binding.find(scope) || [];
            for (let j = 0; j < matches.length; j++) {
              const el = matches[j];
              const id = binding.getId(el);
              if (!id)
                continue;
              if (!jQuery.contains(document, el))
                continue;
              const $el = jQuery(el);
              if ($el.hasClass("shiny-bound-output")) {
                continue;
              }
              maybeAddThemeObserver(el);
              const bindingAdapter = new OutputBindingAdapter(el, binding);
              shinyapp.bindOutput(id, bindingAdapter);
              $el.data("shiny-output-binding", bindingAdapter);
              $el.addClass("shiny-bound-output");
              if (!$el.attr("aria-live"))
                $el.attr("aria-live", "polite");
              $el.trigger({
                type: "shiny:bound",
                binding,
                bindingType: "output"
              });
            }
          }
          setTimeout(sendImageSize, 0);
          setTimeout(sendOutputHiddenState, 0);
        }
        function unbindOutputs(scope = document, includeSelf = false) {
          const outputs = jQuery(scope).find(".shiny-bound-output");
          if (includeSelf && jQuery(scope).hasClass("shiny-bound-output")) {
            outputs.push(scope);
          }
          for (let i = 0; i < outputs.length; i++) {
            const $el = jQuery(outputs[i]);
            const bindingAdapter = $el.data("shiny-output-binding");
            if (!bindingAdapter)
              continue;
            const id = bindingAdapter.binding.getId(outputs[i]);
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
        const inputBatchSender = new InputBatchSender(shinyapp);
        const inputsNoResend = new InputNoResendDecorator(inputBatchSender);
        const inputsEvent = new InputEventDecorator(inputsNoResend);
        const inputsRate = new InputRateDecorator(inputsEvent);
        const inputsDefer = new InputDeferDecorator(inputsEvent);
        let inputs;
        if (jQuery('input[type="submit"], button[type="submit"]').length > 0) {
          inputs = inputsDefer;
          jQuery('input[type="submit"], button[type="submit"]').each(function() {
            jQuery(this).click(function(event) {
              event.preventDefault();
              inputsDefer.submit();
            });
          });
        } else {
          inputs = inputsRate;
        }
        inputs = new InputValidateDecorator(inputs);
        WindowShiny.setInputValue = WindowShiny.onInputChange = function(name, value, opts) {
          opts = addDefaultInputOpts(opts);
          inputs.setInput(name, value, opts);
        };
        WindowShiny.forgetLastInputValue = function(name) {
          inputsNoResend.forget(name);
        };
        const boundInputs = {};
        function valueChangeCallback(binding, el, allowDeferred) {
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
        function bindInputs(scope = document) {
          const bindings = inputBindings.getBindings();
          const inputItems = {};
          for (let i = 0; i < bindings.length; i++) {
            const binding = bindings[i].binding;
            const matches = binding.find(scope) || [];
            for (let j = 0; j < matches.length; j++) {
              const el = matches[j];
              const id = binding.getId(el);
              if (!id || boundInputs[id])
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
                  valueChangeCallback(thisBinding, thisEl, allowDeferred);
                };
              }();
              binding.subscribe(el, thisCallback);
              jQuery(el).data("shiny-input-binding", binding);
              jQuery(el).addClass("shiny-bound-input");
              const ratePolicy = binding.getRatePolicy(el);
              if (ratePolicy !== null) {
                inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
              }
              boundInputs[id] = {
                binding,
                node: el
              };
              jQuery(el).trigger({
                type: "shiny:bound",
                binding,
                bindingType: "input"
              });
            }
          }
          return inputItems;
        }
        function unbindInputs(scope = document, includeSelf = false) {
          const inputs2 = jQuery(scope).find(".shiny-bound-input");
          if (includeSelf && jQuery(scope).hasClass("shiny-bound-input")) {
            inputs2.push(scope);
          }
          for (let i = 0; i < inputs2.length; i++) {
            const el = inputs2[i];
            const binding = jQuery(el).data("shiny-input-binding");
            if (!binding)
              continue;
            const id = binding.getId(el);
            jQuery(el).removeClass("shiny-bound-input");
            delete boundInputs[id];
            binding.unsubscribe(el);
            jQuery(el).trigger({
              type: "shiny:unbound",
              binding,
              bindingType: "input"
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
        WindowShiny.bindAll = function(scope) {
          const currentInputItems = _bindAll(scope);
          jQuery.each(currentInputItems, function(name, item) {
            inputs.setInput(name, item.value, item.opts);
          });
          initDeferredIframes();
        };
        WindowShiny.unbindAll = unbindAll;
        function initializeInputs(scope = document) {
          const bindings = inputBindings.getBindings();
          for (let i = 0; i < bindings.length; i++) {
            const binding = bindings[i].binding;
            const inputObjects = binding.find(scope) || [];
            for (let j = 0; j < inputObjects.length; j++) {
              if (!inputObjects[j]._shiny_initialized) {
                inputObjects[j]._shiny_initialized = true;
                binding.initialize(inputObjects[j]);
              }
            }
          }
        }
        WindowShiny.initializeInputs = initializeInputs;
        function getIdFromEl(el) {
          const $el = jQuery(el);
          const bindingAdapter = $el.data("shiny-output-binding");
          if (!bindingAdapter)
            return null;
          else
            return bindingAdapter.getId();
        }
        initializeInputs(document);
        const initialValues = import_utils2.mapValues(_bindAll(document), (x) => x.value);
        jQuery(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
          const id = getIdFromEl(this);
          if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
            initialValues[".clientdata_output_" + id + "_width"] = this.offsetWidth;
            initialValues[".clientdata_output_" + id + "_height"] = this.offsetHeight;
          }
        });
        function getComputedBgColor(el) {
          if (!el) {
            return null;
          }
          const bgColor = import_utils2.getStyle(el, "background-color");
          const m = bgColor.match(/^rgba\(\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*,\s*([\d.]+)\s*\)$/);
          if (bgColor === "transparent" || m && parseFloat(m[4]) === 0) {
            const bgImage = import_utils2.getStyle(el, "background-image");
            if (bgImage && bgImage !== "none") {
              return null;
            } else {
              return getComputedBgColor(el.parentElement);
            }
          }
          return bgColor;
        }
        function getComputedFont(el) {
          const fontFamily = import_utils2.getStyle(el, "font-family");
          const fontSize = import_utils2.getStyle(el, "font-size");
          return {
            families: fontFamily.replace(/"/g, "").split(", "),
            size: fontSize
          };
        }
        jQuery(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
          const el = this, id = getIdFromEl(el);
          initialValues[".clientdata_output_" + id + "_bg"] = getComputedBgColor(el);
          initialValues[".clientdata_output_" + id + "_fg"] = import_utils2.getStyle(el, "color");
          initialValues[".clientdata_output_" + id + "_accent"] = import_utils2.getComputedLinkColor(el);
          initialValues[".clientdata_output_" + id + "_font"] = getComputedFont(el);
          maybeAddThemeObserver(el);
        });
        function maybeAddThemeObserver(el) {
          if (!window.MutationObserver) {
            return;
          }
          const cl = el.classList;
          const reportTheme = cl.contains("shiny-image-output") || cl.contains("shiny-plot-output") || cl.contains("shiny-report-theme");
          if (!reportTheme) {
            return;
          }
          const $el = jQuery(el);
          if ($el.data("shiny-theme-observer")) {
            return;
          }
          const observerCallback = new Debouncer(null, () => doSendTheme(el), 100);
          const observer = new MutationObserver(() => observerCallback.normalCall());
          const config = {attributes: true, attributeFilter: ["style", "class"]};
          observer.observe(el, config);
          $el.data("shiny-theme-observer", observer);
        }
        function doSendTheme(el) {
          if (el.classList.contains("shiny-output-error")) {
            return;
          }
          const id = getIdFromEl(el);
          inputs.setInput(".clientdata_output_" + id + "_bg", getComputedBgColor(el));
          inputs.setInput(".clientdata_output_" + id + "_fg", import_utils2.getStyle(el, "color"));
          inputs.setInput(".clientdata_output_" + id + "_accent", import_utils2.getComputedLinkColor(el));
          inputs.setInput(".clientdata_output_" + id + "_font", getComputedFont(el));
        }
        function doSendImageSize() {
          jQuery(".shiny-image-output, .shiny-plot-output, .shiny-report-size").each(function() {
            const id = getIdFromEl(this);
            if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
              inputs.setInput(".clientdata_output_" + id + "_width", this.offsetWidth);
              inputs.setInput(".clientdata_output_" + id + "_height", this.offsetHeight);
            }
          });
          jQuery(".shiny-image-output, .shiny-plot-output, .shiny-report-theme").each(function() {
            doSendTheme(this);
          });
          jQuery(".shiny-bound-output").each(function() {
            const $this = jQuery(this), binding = $this.data("shiny-output-binding");
            $this.trigger({
              type: "shiny:visualchange",
              visible: !isHidden(this),
              binding
            });
            binding.onResize();
          });
        }
        const sendImageSizeDebouncer = new Debouncer(null, doSendImageSize, 0);
        function sendImageSize() {
          sendImageSizeDebouncer.normalCall();
        }
        inputBatchSender.lastChanceCallback.push(function() {
          if (sendImageSizeDebouncer.isPending())
            sendImageSizeDebouncer.immediateCall();
        });
        function isHidden(obj) {
          if (obj === null || obj.offsetWidth !== 0 || obj.offsetHeight !== 0) {
            return false;
          } else if (import_utils2.getStyle(obj, "display") === "none") {
            return true;
          } else {
            return isHidden(obj.parentNode);
          }
        }
        let lastKnownVisibleOutputs = {};
        jQuery(".shiny-bound-output").each(function() {
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
          jQuery(".shiny-bound-output").each(function() {
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
            const $this = jQuery(this);
            evt.binding = $this.data("shiny-output-binding");
            $this.trigger(evt);
          });
          for (const name in lastKnownVisibleOutputs) {
            if (lastKnownVisibleOutputs.hasOwnProperty(name))
              inputs.setInput(".clientdata_output_" + name + "_hidden", true);
          }
          lastKnownVisibleOutputs = visibleOutputs;
        }
        const sendOutputHiddenStateDebouncer = new Debouncer(null, doSendOutputHiddenState, 0);
        function sendOutputHiddenState() {
          sendOutputHiddenStateDebouncer.normalCall();
        }
        inputBatchSender.lastChanceCallback.push(function() {
          if (sendOutputHiddenStateDebouncer.isPending())
            sendOutputHiddenStateDebouncer.immediateCall();
        });
        function filterEventsByNamespace(namespace, handler, ...args) {
          namespace = namespace.split(".");
          return function(e) {
            const eventNamespace = e.namespace.split(".");
            for (let i = 0; i < namespace.length; i++) {
              if (eventNamespace.indexOf(namespace[i]) === -1)
                return;
            }
            handler.apply(this, [namespace, handler, ...args]);
          };
        }
        jQuery(window).resize(debounce(500, sendImageSize));
        const bs3classes = [
          "modal",
          "dropdown",
          "tab",
          "tooltip",
          "popover",
          "collapse"
        ];
        jQuery.each(bs3classes, function(idx, classname) {
          jQuery(document.body).on("shown.bs." + classname + ".sendImageSize", "*", filterEventsByNamespace("bs", sendImageSize));
          jQuery(document.body).on("shown.bs." + classname + ".sendOutputHiddenState hidden.bs." + classname + ".sendOutputHiddenState", "*", filterEventsByNamespace("bs", sendOutputHiddenState));
        });
        jQuery(document.body).on("shown.sendImageSize", "*", sendImageSize);
        jQuery(document.body).on("shown.sendOutputHiddenState hidden.sendOutputHiddenState", "*", sendOutputHiddenState);
        initialValues[".clientdata_pixelratio"] = import_utils2.pixelRatio();
        jQuery(window).resize(function() {
          inputs.setInput(".clientdata_pixelratio", import_utils2.pixelRatio());
        });
        initialValues[".clientdata_url_protocol"] = window.location.protocol;
        initialValues[".clientdata_url_hostname"] = window.location.hostname;
        initialValues[".clientdata_url_port"] = window.location.port;
        initialValues[".clientdata_url_pathname"] = window.location.pathname;
        initialValues[".clientdata_url_search"] = window.location.search;
        jQuery(window).on("pushstate", function(e) {
          inputs.setInput(".clientdata_url_search", window.location.search);
        });
        jQuery(window).on("popstate", function(e) {
          inputs.setInput(".clientdata_url_search", window.location.search);
        });
        initialValues[".clientdata_url_hash_initial"] = window.location.hash;
        initialValues[".clientdata_url_hash"] = window.location.hash;
        jQuery(window).on("hashchange", function(e) {
          inputs.setInput(".clientdata_url_hash", window.location.hash);
        });
        const singletonText = initialValues[".clientdata_singletons"] = jQuery('script[type="application/shiny-singletons"]').text();
        singletons.registerNames(singletonText.split(/,/));
        const dependencyText = jQuery('script[type="application/html-dependencies"]').text();
        jQuery.each(dependencyText.split(/;/), function(i, depStr) {
          const match = /\s*^(.+)\[(.+)\]\s*$/.exec(depStr);
          if (match) {
            registerDependency(match[1], match[2]);
          }
        });
        inputsNoResend.reset(initialValues);
        shinyapp.connect(initialValues);
        jQuery(document).one("shiny:connected", function() {
          initDeferredIframes();
        });
      }
      function initDeferredIframes() {
        if (!window.Shiny || !window.Shiny.shinyapp || !window.Shiny.shinyapp.isConnected()) {
          return;
        }
        jQuery(".shiny-frame-deferred").each(function(i, el) {
          const $el = jQuery(el);
          $el.removeClass("shiny-frame-deferred");
          $el.attr("src", $el.attr("data-deferred-src"));
          $el.attr("data-deferred-src", null);
        });
      }
      jQuery(function() {
        setTimeout(initShiny, 1);
      });
      jQuery(document).on("keydown", function(e) {
        if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey)
          return;
        const url = "reactlog?w=" + window.escape(WindowShiny.shinyapp.config.workerId) + "&s=" + window.escape(WindowShiny.shinyapp.config.sessionId);
        window.open(url);
        e.preventDefault();
      });
      jQuery(document).on("keydown", function(e) {
        if (!(e.which === 115 && (e.ctrlKey || e.metaKey) && !e.shiftKey && !e.altKey || e.which === 114 && (e.ctrlKey || e.metaKey) && e.shiftKey && !e.altKey)) {
          return;
        }
        const url = "reactlog/mark?w=" + window.escape(WindowShiny.shinyapp.config.workerId) + "&s=" + window.escape(WindowShiny.shinyapp.config.sessionId);
        jQuery.get(url, function(result) {
          if (result !== "marked")
            return;
          const html = '<span id="shiny-reactlog-mark-text">Marked time point in reactlog</span>';
          WindowShiny.notifications.show({
            html,
            closeButton: true
          });
        }).fail(function() {
          window.open(url);
        });
        e.preventDefault();
      });
    }
  });

  // node_modules/babel-polyfill/lib/index.js
  "use strict";
  require_shim();
  require_runtime();
  require_escape();
  if (global._babelPolyfill) {
    throw new Error("only one instance of babel-polyfill is allowed");
  }
  global._babelPolyfill = true;
  var DEFINE_PROPERTY = "defineProperty";
  function define(O, key, value) {
    O[key] || Object[DEFINE_PROPERTY](O, key, {
      writable: true,
      configurable: true,
      value
    });
  }
  define(String.prototype, "padLeft", "".padStart);
  define(String.prototype, "padRight", "".padEnd);
  "pop,reverse,shift,keys,values,entries,indexOf,every,some,forEach,map,filter,find,findIndex,includes,join,slice,concat,push,splice,unshift,sort,lastIndexOf,reduce,reduceRight,copyWithin,fill".split(",").forEach(function(key) {
    [][key] && define(Array, key, Function.call.bind([][key]));
  });

  // src/external/globals.ts
  var WindowShiny = window.Shiny = window.Shiny || {};
  var jQuery = window.jQuery;
  var userAgent = window.navigator.userAgent;

  // src/jquery/disable.ts
  function main2() {
    jQuery(document).on("submit", "form:not([action])", function(e) {
      e.preventDefault();
    });
  }

  // src/external/history.ts
  function main3() {
    const origPushState = window.history.pushState;
    window.history.pushState = function(...args) {
      const result = origPushState.apply(this, args);
      $(document).trigger("pushstate");
      return result;
    };
  }

  // src/external/pixelRatio.ts
  function devicePixelRatio() {
    return window.devicePixelRatio;
  }

  // src/utils/blob.ts
  var blob_exports = {};
  __export(blob_exports, {
    makeBlob: () => makeBlob
  });

  // src/external/BlobBuilder.ts
  var BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder || window.MSBlobBuilder;

  // src/utils/blob.ts
  function makeBlob(parts) {
    try {
      return new Blob(parts);
    } catch (e) {
      const blobBuilder = new BlobBuilder();
      $.each(parts, function(i, part) {
        blobBuilder.append(part);
      });
      return blobBuilder.getBlob();
    }
  }

  // src/shiny.ts
  var import_utils = __toModule(require_utils());
  WindowShiny.VERSION = "1.6.0.9000";
  WindowShiny.$escape = import_utils.$escape;
  WindowShiny.compareVersion = import_utils.compareVersion;

  // src/external/browser.ts
  var isQt = false;
  if (/\bQt\//.test(userAgent)) {
    $(document.documentElement).addClass("qt");
    isQt = true;
  }
  if (/\bQt\/5/.test(userAgent) && /Linux/.test(userAgent)) {
    $(document.documentElement).addClass("qt5");
  }
  var isIE = /MSIE|Trident|Edge/.test(userAgent);
  function getIEVersion() {
    const msie = userAgent.indexOf("MSIE ");
    if (isIE && msie > 0) {
      return parseInt(userAgent.substring(msie + 5, userAgent.indexOf(".", msie)), 10);
    }
    const trident = userAgent.indexOf("Trident/");
    if (trident > 0) {
      const rv = userAgent.indexOf("rv:");
      return parseInt(userAgent.substring(rv + 3, userAgent.indexOf(".", rv)), 10);
    }
    return -1;
  }
  var IEVersion = getIEVersion();

  // src/file/FileProcessor.ts
  var FileProcessor = function(files) {
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
      let called = false;
      return () => {
        if (called)
          return;
        called = true;
        this.$run();
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
      const file = this.files[this.fileIndex++];
      this.onFile(file, this.$getRun());
    };
  }).call(FileProcessor.prototype);

  // src/index.ts
  var import_main = __toModule(require_main());
  main3();
  main2();
  import_main.main();
  window.console.log(WindowShiny.version);
})();
//# sourceMappingURL=shiny.js.map
