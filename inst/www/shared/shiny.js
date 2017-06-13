'use strict';

var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

function _objectDestructuringEmpty(obj) { if (obj == null) throw new TypeError("Cannot destructure undefined"); }

//---------------------------------------------------------------------
// Source file: ../srcjs/_start.js

(function () {
  var $ = jQuery;

  var exports = window.Shiny = window.Shiny || {};

  var origPushState = window.history.pushState;
  window.history.pushState = function () {
    var result = origPushState.apply(this, arguments);
    $(document).trigger("pushstate");
    return result;
  };

  $(document).on('submit', 'form:not([action])', function (e) {
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../bundled/lodash_preamble.js

  /* eslint-disable */

  //---------------------------------------------------------------------
  // Source file: ../bundled/lodash.custom.js

  /**
   * @license
   * Lodash (Custom Build) <https://lodash.com/>
   * Build: `lodash include="noConflict,reduce,take,drop,some,partition,groupBy,each,filter,map,mapValues,pickBy,mapKeys,chain,value"`
   * Copyright JS Foundation and other contributors <https://js.foundation/>
   * Released under MIT license <https://lodash.com/license>
   * Based on Underscore.js 1.8.3 <http://underscorejs.org/LICENSE>
   * Copyright Jeremy Ashkenas, DocumentCloud and Investigative Reporters & Editors
   */
  ;(function () {

    /** Used as a safe reference for `undefined` in pre-ES5 environments. */
    var undefined;

    /** Used as the semantic version number. */
    var VERSION = '4.17.4';

    /** Used as the size to enable large array optimizations. */
    var LARGE_ARRAY_SIZE = 200;

    /** Error message constants. */
    var FUNC_ERROR_TEXT = 'Expected a function';

    /** Used to stand-in for `undefined` hash values. */
    var HASH_UNDEFINED = '__lodash_hash_undefined__';

    /** Used as the maximum memoize cache size. */
    var MAX_MEMOIZE_SIZE = 500;

    /** Used as the internal argument placeholder. */
    var PLACEHOLDER = '__lodash_placeholder__';

    /** Used to compose bitmasks for cloning. */
    var CLONE_DEEP_FLAG = 1,
        CLONE_FLAT_FLAG = 2,
        CLONE_SYMBOLS_FLAG = 4;

    /** Used to compose bitmasks for value comparisons. */
    var COMPARE_PARTIAL_FLAG = 1,
        COMPARE_UNORDERED_FLAG = 2;

    /** Used to compose bitmasks for function metadata. */
    var WRAP_BIND_FLAG = 1,
        WRAP_BIND_KEY_FLAG = 2,
        WRAP_CURRY_BOUND_FLAG = 4,
        WRAP_CURRY_FLAG = 8,
        WRAP_CURRY_RIGHT_FLAG = 16,
        WRAP_PARTIAL_FLAG = 32,
        WRAP_PARTIAL_RIGHT_FLAG = 64,
        WRAP_ARY_FLAG = 128,
        WRAP_REARG_FLAG = 256,
        WRAP_FLIP_FLAG = 512;

    /** Used to detect hot functions by number of calls within a span of milliseconds. */
    var HOT_COUNT = 800,
        HOT_SPAN = 16;

    /** Used to indicate the type of lazy iteratees. */
    var LAZY_FILTER_FLAG = 1,
        LAZY_MAP_FLAG = 2,
        LAZY_WHILE_FLAG = 3;

    /** Used as references for various `Number` constants. */
    var INFINITY = 1 / 0,
        MAX_SAFE_INTEGER = 9007199254740991,
        MAX_INTEGER = 1.7976931348623157e+308,
        NAN = 0 / 0;

    /** Used as references for the maximum length and index of an array. */
    var MAX_ARRAY_LENGTH = 4294967295;

    /** Used to associate wrap methods with their bit flags. */
    var wrapFlags = [['ary', WRAP_ARY_FLAG], ['bind', WRAP_BIND_FLAG], ['bindKey', WRAP_BIND_KEY_FLAG], ['curry', WRAP_CURRY_FLAG], ['curryRight', WRAP_CURRY_RIGHT_FLAG], ['flip', WRAP_FLIP_FLAG], ['partial', WRAP_PARTIAL_FLAG], ['partialRight', WRAP_PARTIAL_RIGHT_FLAG], ['rearg', WRAP_REARG_FLAG]];

    /** `Object#toString` result references. */
    var argsTag = '[object Arguments]',
        arrayTag = '[object Array]',
        asyncTag = '[object AsyncFunction]',
        boolTag = '[object Boolean]',
        dateTag = '[object Date]',
        errorTag = '[object Error]',
        funcTag = '[object Function]',
        genTag = '[object GeneratorFunction]',
        mapTag = '[object Map]',
        numberTag = '[object Number]',
        nullTag = '[object Null]',
        objectTag = '[object Object]',
        promiseTag = '[object Promise]',
        proxyTag = '[object Proxy]',
        regexpTag = '[object RegExp]',
        setTag = '[object Set]',
        stringTag = '[object String]',
        symbolTag = '[object Symbol]',
        undefinedTag = '[object Undefined]',
        weakMapTag = '[object WeakMap]';

    var arrayBufferTag = '[object ArrayBuffer]',
        dataViewTag = '[object DataView]',
        float32Tag = '[object Float32Array]',
        float64Tag = '[object Float64Array]',
        int8Tag = '[object Int8Array]',
        int16Tag = '[object Int16Array]',
        int32Tag = '[object Int32Array]',
        uint8Tag = '[object Uint8Array]',
        uint8ClampedTag = '[object Uint8ClampedArray]',
        uint16Tag = '[object Uint16Array]',
        uint32Tag = '[object Uint32Array]';

    /** Used to match property names within property paths. */
    var reIsDeepProp = /\.|\[(?:[^[\]]*|(["'])(?:(?!\1)[^\\]|\\.)*?\1)\]/,
        reIsPlainProp = /^\w*$/,
        reLeadingDot = /^\./,
        rePropName = /[^.[\]]+|\[(?:(-?\d+(?:\.\d+)?)|(["'])((?:(?!\2)[^\\]|\\.)*?)\2)\]|(?=(?:\.|\[\])(?:\.|\[\]|$))/g;

    /**
     * Used to match `RegExp`
     * [syntax characters](http://ecma-international.org/ecma-262/7.0/#sec-patterns).
     */
    var reRegExpChar = /[\\^$.*+?()[\]{}|]/g;

    /** Used to match leading and trailing whitespace. */
    var reTrim = /^\s+|\s+$/g;

    /** Used to match wrap detail comments. */
    var reWrapComment = /\{(?:\n\/\* \[wrapped with .+\] \*\/)?\n?/,
        reWrapDetails = /\{\n\/\* \[wrapped with (.+)\] \*/,
        reSplitDetails = /,? & /;

    /** Used to match backslashes in property paths. */
    var reEscapeChar = /\\(\\)?/g;

    /** Used to match `RegExp` flags from their coerced string values. */
    var reFlags = /\w*$/;

    /** Used to detect bad signed hexadecimal string values. */
    var reIsBadHex = /^[-+]0x[0-9a-f]+$/i;

    /** Used to detect binary string values. */
    var reIsBinary = /^0b[01]+$/i;

    /** Used to detect host constructors (Safari). */
    var reIsHostCtor = /^\[object .+?Constructor\]$/;

    /** Used to detect octal string values. */
    var reIsOctal = /^0o[0-7]+$/i;

    /** Used to detect unsigned integer values. */
    var reIsUint = /^(?:0|[1-9]\d*)$/;

    /** Used to compose unicode character classes. */
    var rsAstralRange = '\\ud800-\\udfff',
        rsComboMarksRange = '\\u0300-\\u036f',
        reComboHalfMarksRange = '\\ufe20-\\ufe2f',
        rsComboSymbolsRange = '\\u20d0-\\u20ff',
        rsComboRange = rsComboMarksRange + reComboHalfMarksRange + rsComboSymbolsRange,
        rsVarRange = '\\ufe0e\\ufe0f';

    /** Used to compose unicode capture groups. */
    var rsAstral = '[' + rsAstralRange + ']',
        rsCombo = '[' + rsComboRange + ']',
        rsFitz = '\\ud83c[\\udffb-\\udfff]',
        rsModifier = '(?:' + rsCombo + '|' + rsFitz + ')',
        rsNonAstral = '[^' + rsAstralRange + ']',
        rsRegional = '(?:\\ud83c[\\udde6-\\uddff]){2}',
        rsSurrPair = '[\\ud800-\\udbff][\\udc00-\\udfff]',
        rsZWJ = '\\u200d';

    /** Used to compose unicode regexes. */
    var reOptMod = rsModifier + '?',
        rsOptVar = '[' + rsVarRange + ']?',
        rsOptJoin = '(?:' + rsZWJ + '(?:' + [rsNonAstral, rsRegional, rsSurrPair].join('|') + ')' + rsOptVar + reOptMod + ')*',
        rsSeq = rsOptVar + reOptMod + rsOptJoin,
        rsSymbol = '(?:' + [rsNonAstral + rsCombo + '?', rsCombo, rsRegional, rsSurrPair, rsAstral].join('|') + ')';

    /** Used to match [string symbols](https://mathiasbynens.be/notes/javascript-unicode). */
    var reUnicode = RegExp(rsFitz + '(?=' + rsFitz + ')|' + rsSymbol + rsSeq, 'g');

    /** Used to detect strings with [zero-width joiners or code points from the astral planes](http://eev.ee/blog/2015/09/12/dark-corners-of-unicode/). */
    var reHasUnicode = RegExp('[' + rsZWJ + rsAstralRange + rsComboRange + rsVarRange + ']');

    /** Used to identify `toStringTag` values of typed arrays. */
    var typedArrayTags = {};
    typedArrayTags[float32Tag] = typedArrayTags[float64Tag] = typedArrayTags[int8Tag] = typedArrayTags[int16Tag] = typedArrayTags[int32Tag] = typedArrayTags[uint8Tag] = typedArrayTags[uint8ClampedTag] = typedArrayTags[uint16Tag] = typedArrayTags[uint32Tag] = true;
    typedArrayTags[argsTag] = typedArrayTags[arrayTag] = typedArrayTags[arrayBufferTag] = typedArrayTags[boolTag] = typedArrayTags[dataViewTag] = typedArrayTags[dateTag] = typedArrayTags[errorTag] = typedArrayTags[funcTag] = typedArrayTags[mapTag] = typedArrayTags[numberTag] = typedArrayTags[objectTag] = typedArrayTags[regexpTag] = typedArrayTags[setTag] = typedArrayTags[stringTag] = typedArrayTags[weakMapTag] = false;

    /** Used to identify `toStringTag` values supported by `_.clone`. */
    var cloneableTags = {};
    cloneableTags[argsTag] = cloneableTags[arrayTag] = cloneableTags[arrayBufferTag] = cloneableTags[dataViewTag] = cloneableTags[boolTag] = cloneableTags[dateTag] = cloneableTags[float32Tag] = cloneableTags[float64Tag] = cloneableTags[int8Tag] = cloneableTags[int16Tag] = cloneableTags[int32Tag] = cloneableTags[mapTag] = cloneableTags[numberTag] = cloneableTags[objectTag] = cloneableTags[regexpTag] = cloneableTags[setTag] = cloneableTags[stringTag] = cloneableTags[symbolTag] = cloneableTags[uint8Tag] = cloneableTags[uint8ClampedTag] = cloneableTags[uint16Tag] = cloneableTags[uint32Tag] = true;
    cloneableTags[errorTag] = cloneableTags[funcTag] = cloneableTags[weakMapTag] = false;

    /** Built-in method references without a dependency on `root`. */
    var freeParseInt = parseInt;

    /** Detect free variable `global` from Node.js. */
    var freeGlobal = (typeof global === 'undefined' ? 'undefined' : _typeof(global)) == 'object' && global && global.Object === Object && global;

    /** Detect free variable `self`. */
    var freeSelf = (typeof self === 'undefined' ? 'undefined' : _typeof(self)) == 'object' && self && self.Object === Object && self;

    /** Used as a reference to the global object. */
    var root = freeGlobal || freeSelf || Function('return this')();

    /** Detect free variable `exports`. */
    var freeExports = (typeof exports === 'undefined' ? 'undefined' : _typeof(exports)) == 'object' && exports && !exports.nodeType && exports;

    /** Detect free variable `module`. */
    var freeModule = freeExports && (typeof module === 'undefined' ? 'undefined' : _typeof(module)) == 'object' && module && !module.nodeType && module;

    /** Detect the popular CommonJS extension `module.exports`. */
    var moduleExports = freeModule && freeModule.exports === freeExports;

    /** Detect free variable `process` from Node.js. */
    var freeProcess = moduleExports && freeGlobal.process;

    /** Used to access faster Node.js helpers. */
    var nodeUtil = function () {
      try {
        return freeProcess && freeProcess.binding && freeProcess.binding('util');
      } catch (e) {}
    }();

    /* Node.js helper references. */
    var nodeIsTypedArray = nodeUtil && nodeUtil.isTypedArray;

    /*--------------------------------------------------------------------------*/

    /**
     * Adds the key-value `pair` to `map`.
     *
     * @private
     * @param {Object} map The map to modify.
     * @param {Array} pair The key-value pair to add.
     * @returns {Object} Returns `map`.
     */
    function addMapEntry(map, pair) {
      // Don't return `map.set` because it's not chainable in IE 11.
      map.set(pair[0], pair[1]);
      return map;
    }

    /**
     * Adds `value` to `set`.
     *
     * @private
     * @param {Object} set The set to modify.
     * @param {*} value The value to add.
     * @returns {Object} Returns `set`.
     */
    function addSetEntry(set, value) {
      // Don't return `set.add` because it's not chainable in IE 11.
      set.add(value);
      return set;
    }

    /**
     * A faster alternative to `Function#apply`, this function invokes `func`
     * with the `this` binding of `thisArg` and the arguments of `args`.
     *
     * @private
     * @param {Function} func The function to invoke.
     * @param {*} thisArg The `this` binding of `func`.
     * @param {Array} args The arguments to invoke `func` with.
     * @returns {*} Returns the result of `func`.
     */
    function apply(func, thisArg, args) {
      switch (args.length) {
        case 0:
          return func.call(thisArg);
        case 1:
          return func.call(thisArg, args[0]);
        case 2:
          return func.call(thisArg, args[0], args[1]);
        case 3:
          return func.call(thisArg, args[0], args[1], args[2]);
      }
      return func.apply(thisArg, args);
    }

    /**
     * A specialized version of `baseAggregator` for arrays.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} setter The function to set `accumulator` values.
     * @param {Function} iteratee The iteratee to transform keys.
     * @param {Object} accumulator The initial aggregated object.
     * @returns {Function} Returns `accumulator`.
     */
    function arrayAggregator(array, setter, iteratee, accumulator) {
      var index = -1,
          length = array == null ? 0 : array.length;

      while (++index < length) {
        var value = array[index];
        setter(accumulator, value, iteratee(value), array);
      }
      return accumulator;
    }

    /**
     * A specialized version of `_.forEach` for arrays without support for
     * iteratee shorthands.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Array} Returns `array`.
     */
    function arrayEach(array, iteratee) {
      var index = -1,
          length = array == null ? 0 : array.length;

      while (++index < length) {
        if (iteratee(array[index], index, array) === false) {
          break;
        }
      }
      return array;
    }

    /**
     * A specialized version of `_.filter` for arrays without support for
     * iteratee shorthands.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} predicate The function invoked per iteration.
     * @returns {Array} Returns the new filtered array.
     */
    function arrayFilter(array, predicate) {
      var index = -1,
          length = array == null ? 0 : array.length,
          resIndex = 0,
          result = [];

      while (++index < length) {
        var value = array[index];
        if (predicate(value, index, array)) {
          result[resIndex++] = value;
        }
      }
      return result;
    }

    /**
     * A specialized version of `_.includes` for arrays without support for
     * specifying an index to search from.
     *
     * @private
     * @param {Array} [array] The array to inspect.
     * @param {*} target The value to search for.
     * @returns {boolean} Returns `true` if `target` is found, else `false`.
     */
    function arrayIncludes(array, value) {
      var length = array == null ? 0 : array.length;
      return !!length && baseIndexOf(array, value, 0) > -1;
    }

    /**
     * A specialized version of `_.map` for arrays without support for iteratee
     * shorthands.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Array} Returns the new mapped array.
     */
    function arrayMap(array, iteratee) {
      var index = -1,
          length = array == null ? 0 : array.length,
          result = Array(length);

      while (++index < length) {
        result[index] = iteratee(array[index], index, array);
      }
      return result;
    }

    /**
     * Appends the elements of `values` to `array`.
     *
     * @private
     * @param {Array} array The array to modify.
     * @param {Array} values The values to append.
     * @returns {Array} Returns `array`.
     */
    function arrayPush(array, values) {
      var index = -1,
          length = values.length,
          offset = array.length;

      while (++index < length) {
        array[offset + index] = values[index];
      }
      return array;
    }

    /**
     * A specialized version of `_.reduce` for arrays without support for
     * iteratee shorthands.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @param {*} [accumulator] The initial value.
     * @param {boolean} [initAccum] Specify using the first element of `array` as
     *  the initial value.
     * @returns {*} Returns the accumulated value.
     */
    function arrayReduce(array, iteratee, accumulator, initAccum) {
      var index = -1,
          length = array == null ? 0 : array.length;

      if (initAccum && length) {
        accumulator = array[++index];
      }
      while (++index < length) {
        accumulator = iteratee(accumulator, array[index], index, array);
      }
      return accumulator;
    }

    /**
     * A specialized version of `_.some` for arrays without support for iteratee
     * shorthands.
     *
     * @private
     * @param {Array} [array] The array to iterate over.
     * @param {Function} predicate The function invoked per iteration.
     * @returns {boolean} Returns `true` if any element passes the predicate check,
     *  else `false`.
     */
    function arraySome(array, predicate) {
      var index = -1,
          length = array == null ? 0 : array.length;

      while (++index < length) {
        if (predicate(array[index], index, array)) {
          return true;
        }
      }
      return false;
    }

    /**
     * Converts an ASCII `string` to an array.
     *
     * @private
     * @param {string} string The string to convert.
     * @returns {Array} Returns the converted array.
     */
    function asciiToArray(string) {
      return string.split('');
    }

    /**
     * The base implementation of `_.findIndex` and `_.findLastIndex` without
     * support for iteratee shorthands.
     *
     * @private
     * @param {Array} array The array to inspect.
     * @param {Function} predicate The function invoked per iteration.
     * @param {number} fromIndex The index to search from.
     * @param {boolean} [fromRight] Specify iterating from right to left.
     * @returns {number} Returns the index of the matched value, else `-1`.
     */
    function baseFindIndex(array, predicate, fromIndex, fromRight) {
      var length = array.length,
          index = fromIndex + (fromRight ? 1 : -1);

      while (fromRight ? index-- : ++index < length) {
        if (predicate(array[index], index, array)) {
          return index;
        }
      }
      return -1;
    }

    /**
     * The base implementation of `_.indexOf` without `fromIndex` bounds checks.
     *
     * @private
     * @param {Array} array The array to inspect.
     * @param {*} value The value to search for.
     * @param {number} fromIndex The index to search from.
     * @returns {number} Returns the index of the matched value, else `-1`.
     */
    function baseIndexOf(array, value, fromIndex) {
      return value === value ? strictIndexOf(array, value, fromIndex) : baseFindIndex(array, baseIsNaN, fromIndex);
    }

    /**
     * The base implementation of `_.isNaN` without support for number objects.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is `NaN`, else `false`.
     */
    function baseIsNaN(value) {
      return value !== value;
    }

    /**
     * The base implementation of `_.property` without support for deep paths.
     *
     * @private
     * @param {string} key The key of the property to get.
     * @returns {Function} Returns the new accessor function.
     */
    function baseProperty(key) {
      return function (object) {
        return object == null ? undefined : object[key];
      };
    }

    /**
     * The base implementation of `_.reduce` and `_.reduceRight`, without support
     * for iteratee shorthands, which iterates over `collection` using `eachFunc`.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @param {*} accumulator The initial value.
     * @param {boolean} initAccum Specify using the first or last element of
     *  `collection` as the initial value.
     * @param {Function} eachFunc The function to iterate over `collection`.
     * @returns {*} Returns the accumulated value.
     */
    function baseReduce(collection, iteratee, accumulator, initAccum, eachFunc) {
      eachFunc(collection, function (value, index, collection) {
        accumulator = initAccum ? (initAccum = false, value) : iteratee(accumulator, value, index, collection);
      });
      return accumulator;
    }

    /**
     * The base implementation of `_.times` without support for iteratee shorthands
     * or max array length checks.
     *
     * @private
     * @param {number} n The number of times to invoke `iteratee`.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Array} Returns the array of results.
     */
    function baseTimes(n, iteratee) {
      var index = -1,
          result = Array(n);

      while (++index < n) {
        result[index] = iteratee(index);
      }
      return result;
    }

    /**
     * The base implementation of `_.unary` without support for storing metadata.
     *
     * @private
     * @param {Function} func The function to cap arguments for.
     * @returns {Function} Returns the new capped function.
     */
    function baseUnary(func) {
      return function (value) {
        return func(value);
      };
    }

    /**
     * The base implementation of `_.values` and `_.valuesIn` which creates an
     * array of `object` property values corresponding to the property names
     * of `props`.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Array} props The property names to get values for.
     * @returns {Object} Returns the array of property values.
     */
    function baseValues(object, props) {
      return arrayMap(props, function (key) {
        return object[key];
      });
    }

    /**
     * Checks if a `cache` value for `key` exists.
     *
     * @private
     * @param {Object} cache The cache to query.
     * @param {string} key The key of the entry to check.
     * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
     */
    function cacheHas(cache, key) {
      return cache.has(key);
    }

    /**
     * Gets the number of `placeholder` occurrences in `array`.
     *
     * @private
     * @param {Array} array The array to inspect.
     * @param {*} placeholder The placeholder to search for.
     * @returns {number} Returns the placeholder count.
     */
    function countHolders(array, placeholder) {
      var length = array.length,
          result = 0;

      while (length--) {
        if (array[length] === placeholder) {
          ++result;
        }
      }
      return result;
    }

    /**
     * Gets the value at `key` of `object`.
     *
     * @private
     * @param {Object} [object] The object to query.
     * @param {string} key The key of the property to get.
     * @returns {*} Returns the property value.
     */
    function getValue(object, key) {
      return object == null ? undefined : object[key];
    }

    /**
     * Checks if `string` contains Unicode symbols.
     *
     * @private
     * @param {string} string The string to inspect.
     * @returns {boolean} Returns `true` if a symbol is found, else `false`.
     */
    function hasUnicode(string) {
      return reHasUnicode.test(string);
    }

    /**
     * Converts `iterator` to an array.
     *
     * @private
     * @param {Object} iterator The iterator to convert.
     * @returns {Array} Returns the converted array.
     */
    function iteratorToArray(iterator) {
      var data,
          result = [];

      while (!(data = iterator.next()).done) {
        result.push(data.value);
      }
      return result;
    }

    /**
     * Converts `map` to its key-value pairs.
     *
     * @private
     * @param {Object} map The map to convert.
     * @returns {Array} Returns the key-value pairs.
     */
    function mapToArray(map) {
      var index = -1,
          result = Array(map.size);

      map.forEach(function (value, key) {
        result[++index] = [key, value];
      });
      return result;
    }

    /**
     * Creates a unary function that invokes `func` with its argument transformed.
     *
     * @private
     * @param {Function} func The function to wrap.
     * @param {Function} transform The argument transform.
     * @returns {Function} Returns the new function.
     */
    function overArg(func, transform) {
      return function (arg) {
        return func(transform(arg));
      };
    }

    /**
     * Replaces all `placeholder` elements in `array` with an internal placeholder
     * and returns an array of their indexes.
     *
     * @private
     * @param {Array} array The array to modify.
     * @param {*} placeholder The placeholder to replace.
     * @returns {Array} Returns the new array of placeholder indexes.
     */
    function replaceHolders(array, placeholder) {
      var index = -1,
          length = array.length,
          resIndex = 0,
          result = [];

      while (++index < length) {
        var value = array[index];
        if (value === placeholder || value === PLACEHOLDER) {
          array[index] = PLACEHOLDER;
          result[resIndex++] = index;
        }
      }
      return result;
    }

    /**
     * Converts `set` to an array of its values.
     *
     * @private
     * @param {Object} set The set to convert.
     * @returns {Array} Returns the values.
     */
    function setToArray(set) {
      var index = -1,
          result = Array(set.size);

      set.forEach(function (value) {
        result[++index] = value;
      });
      return result;
    }

    /**
     * A specialized version of `_.indexOf` which performs strict equality
     * comparisons of values, i.e. `===`.
     *
     * @private
     * @param {Array} array The array to inspect.
     * @param {*} value The value to search for.
     * @param {number} fromIndex The index to search from.
     * @returns {number} Returns the index of the matched value, else `-1`.
     */
    function strictIndexOf(array, value, fromIndex) {
      var index = fromIndex - 1,
          length = array.length;

      while (++index < length) {
        if (array[index] === value) {
          return index;
        }
      }
      return -1;
    }

    /**
     * Converts `string` to an array.
     *
     * @private
     * @param {string} string The string to convert.
     * @returns {Array} Returns the converted array.
     */
    function stringToArray(string) {
      return hasUnicode(string) ? unicodeToArray(string) : asciiToArray(string);
    }

    /**
     * Converts a Unicode `string` to an array.
     *
     * @private
     * @param {string} string The string to convert.
     * @returns {Array} Returns the converted array.
     */
    function unicodeToArray(string) {
      return string.match(reUnicode) || [];
    }

    /*--------------------------------------------------------------------------*/

    /** Used for built-in method references. */
    var arrayProto = Array.prototype,
        funcProto = Function.prototype,
        objectProto = Object.prototype;

    /** Used to detect overreaching core-js shims. */
    var coreJsData = root['__core-js_shared__'];

    /** Used to resolve the decompiled source of functions. */
    var funcToString = funcProto.toString;

    /** Used to check objects for own properties. */
    var hasOwnProperty = objectProto.hasOwnProperty;

    /** Used to detect methods masquerading as native. */
    var maskSrcKey = function () {
      var uid = /[^.]+$/.exec(coreJsData && coreJsData.keys && coreJsData.keys.IE_PROTO || '');
      return uid ? 'Symbol(src)_1.' + uid : '';
    }();

    /**
     * Used to resolve the
     * [`toStringTag`](http://ecma-international.org/ecma-262/7.0/#sec-object.prototype.tostring)
     * of values.
     */
    var nativeObjectToString = objectProto.toString;

    /** Used to restore the original `_` reference in `_.noConflict`. */
    var oldDash = root._;

    /** Used to detect if a method is native. */
    var reIsNative = RegExp('^' + funcToString.call(hasOwnProperty).replace(reRegExpChar, '\\$&').replace(/hasOwnProperty|(function).*?(?=\\\()| for .+?(?=\\\])/g, '$1.*?') + '$');

    /** Built-in value references. */
    var Buffer = moduleExports ? root.Buffer : undefined,
        _Symbol = root.Symbol,
        Uint8Array = root.Uint8Array,
        allocUnsafe = Buffer ? Buffer.allocUnsafe : undefined,
        getPrototype = overArg(Object.getPrototypeOf, Object),
        objectCreate = Object.create,
        propertyIsEnumerable = objectProto.propertyIsEnumerable,
        splice = arrayProto.splice,
        spreadableSymbol = _Symbol ? _Symbol.isConcatSpreadable : undefined,
        symIterator = _Symbol ? _Symbol.iterator : undefined,
        symToStringTag = _Symbol ? _Symbol.toStringTag : undefined;

    var defineProperty = function () {
      try {
        var func = getNative(Object, 'defineProperty');
        func({}, '', {});
        return func;
      } catch (e) {}
    }();

    /* Built-in method references for those with the same name as other `lodash` methods. */
    var nativeGetSymbols = Object.getOwnPropertySymbols,
        nativeIsBuffer = Buffer ? Buffer.isBuffer : undefined,
        nativeKeys = overArg(Object.keys, Object),
        nativeMax = Math.max,
        nativeMin = Math.min,
        nativeNow = Date.now,
        nativeReverse = arrayProto.reverse;

    /* Built-in method references that are verified to be native. */
    var DataView = getNative(root, 'DataView'),
        Map = getNative(root, 'Map'),
        Promise = getNative(root, 'Promise'),
        Set = getNative(root, 'Set'),
        WeakMap = getNative(root, 'WeakMap'),
        nativeCreate = getNative(Object, 'create');

    /** Used to store function metadata. */
    var metaMap = WeakMap && new WeakMap();

    /** Used to lookup unminified function names. */
    var realNames = {};

    /** Used to detect maps, sets, and weakmaps. */
    var dataViewCtorString = toSource(DataView),
        mapCtorString = toSource(Map),
        promiseCtorString = toSource(Promise),
        setCtorString = toSource(Set),
        weakMapCtorString = toSource(WeakMap);

    /** Used to convert symbols to primitives and strings. */
    var symbolProto = _Symbol ? _Symbol.prototype : undefined,
        symbolValueOf = symbolProto ? symbolProto.valueOf : undefined,
        symbolToString = symbolProto ? symbolProto.toString : undefined;

    /*------------------------------------------------------------------------*/

    /**
     * Creates a `lodash` object which wraps `value` to enable implicit method
     * chain sequences. Methods that operate on and return arrays, collections,
     * and functions can be chained together. Methods that retrieve a single value
     * or may return a primitive value will automatically end the chain sequence
     * and return the unwrapped value. Otherwise, the value must be unwrapped
     * with `_#value`.
     *
     * Explicit chain sequences, which must be unwrapped with `_#value`, may be
     * enabled using `_.chain`.
     *
     * The execution of chained methods is lazy, that is, it's deferred until
     * `_#value` is implicitly or explicitly called.
     *
     * Lazy evaluation allows several methods to support shortcut fusion.
     * Shortcut fusion is an optimization to merge iteratee calls; this avoids
     * the creation of intermediate arrays and can greatly reduce the number of
     * iteratee executions. Sections of a chain sequence qualify for shortcut
     * fusion if the section is applied to an array and iteratees accept only
     * one argument. The heuristic for whether a section qualifies for shortcut
     * fusion is subject to change.
     *
     * Chaining is supported in custom builds as long as the `_#value` method is
     * directly or indirectly included in the build.
     *
     * In addition to lodash methods, wrappers have `Array` and `String` methods.
     *
     * The wrapper `Array` methods are:
     * `concat`, `join`, `pop`, `push`, `shift`, `sort`, `splice`, and `unshift`
     *
     * The wrapper `String` methods are:
     * `replace` and `split`
     *
     * The wrapper methods that support shortcut fusion are:
     * `at`, `compact`, `drop`, `dropRight`, `dropWhile`, `filter`, `find`,
     * `findLast`, `head`, `initial`, `last`, `map`, `reject`, `reverse`, `slice`,
     * `tail`, `take`, `takeRight`, `takeRightWhile`, `takeWhile`, and `toArray`
     *
     * The chainable wrapper methods are:
     * `after`, `ary`, `assign`, `assignIn`, `assignInWith`, `assignWith`, `at`,
     * `before`, `bind`, `bindAll`, `bindKey`, `castArray`, `chain`, `chunk`,
     * `commit`, `compact`, `concat`, `conforms`, `constant`, `countBy`, `create`,
     * `curry`, `debounce`, `defaults`, `defaultsDeep`, `defer`, `delay`,
     * `difference`, `differenceBy`, `differenceWith`, `drop`, `dropRight`,
     * `dropRightWhile`, `dropWhile`, `extend`, `extendWith`, `fill`, `filter`,
     * `flatMap`, `flatMapDeep`, `flatMapDepth`, `flatten`, `flattenDeep`,
     * `flattenDepth`, `flip`, `flow`, `flowRight`, `fromPairs`, `functions`,
     * `functionsIn`, `groupBy`, `initial`, `intersection`, `intersectionBy`,
     * `intersectionWith`, `invert`, `invertBy`, `invokeMap`, `iteratee`, `keyBy`,
     * `keys`, `keysIn`, `map`, `mapKeys`, `mapValues`, `matches`, `matchesProperty`,
     * `memoize`, `merge`, `mergeWith`, `method`, `methodOf`, `mixin`, `negate`,
     * `nthArg`, `omit`, `omitBy`, `once`, `orderBy`, `over`, `overArgs`,
     * `overEvery`, `overSome`, `partial`, `partialRight`, `partition`, `pick`,
     * `pickBy`, `plant`, `property`, `propertyOf`, `pull`, `pullAll`, `pullAllBy`,
     * `pullAllWith`, `pullAt`, `push`, `range`, `rangeRight`, `rearg`, `reject`,
     * `remove`, `rest`, `reverse`, `sampleSize`, `set`, `setWith`, `shuffle`,
     * `slice`, `sort`, `sortBy`, `splice`, `spread`, `tail`, `take`, `takeRight`,
     * `takeRightWhile`, `takeWhile`, `tap`, `throttle`, `thru`, `toArray`,
     * `toPairs`, `toPairsIn`, `toPath`, `toPlainObject`, `transform`, `unary`,
     * `union`, `unionBy`, `unionWith`, `uniq`, `uniqBy`, `uniqWith`, `unset`,
     * `unshift`, `unzip`, `unzipWith`, `update`, `updateWith`, `values`,
     * `valuesIn`, `without`, `wrap`, `xor`, `xorBy`, `xorWith`, `zip`,
     * `zipObject`, `zipObjectDeep`, and `zipWith`
     *
     * The wrapper methods that are **not** chainable by default are:
     * `add`, `attempt`, `camelCase`, `capitalize`, `ceil`, `clamp`, `clone`,
     * `cloneDeep`, `cloneDeepWith`, `cloneWith`, `conformsTo`, `deburr`,
     * `defaultTo`, `divide`, `each`, `eachRight`, `endsWith`, `eq`, `escape`,
     * `escapeRegExp`, `every`, `find`, `findIndex`, `findKey`, `findLast`,
     * `findLastIndex`, `findLastKey`, `first`, `floor`, `forEach`, `forEachRight`,
     * `forIn`, `forInRight`, `forOwn`, `forOwnRight`, `get`, `gt`, `gte`, `has`,
     * `hasIn`, `head`, `identity`, `includes`, `indexOf`, `inRange`, `invoke`,
     * `isArguments`, `isArray`, `isArrayBuffer`, `isArrayLike`, `isArrayLikeObject`,
     * `isBoolean`, `isBuffer`, `isDate`, `isElement`, `isEmpty`, `isEqual`,
     * `isEqualWith`, `isError`, `isFinite`, `isFunction`, `isInteger`, `isLength`,
     * `isMap`, `isMatch`, `isMatchWith`, `isNaN`, `isNative`, `isNil`, `isNull`,
     * `isNumber`, `isObject`, `isObjectLike`, `isPlainObject`, `isRegExp`,
     * `isSafeInteger`, `isSet`, `isString`, `isUndefined`, `isTypedArray`,
     * `isWeakMap`, `isWeakSet`, `join`, `kebabCase`, `last`, `lastIndexOf`,
     * `lowerCase`, `lowerFirst`, `lt`, `lte`, `max`, `maxBy`, `mean`, `meanBy`,
     * `min`, `minBy`, `multiply`, `noConflict`, `noop`, `now`, `nth`, `pad`,
     * `padEnd`, `padStart`, `parseInt`, `pop`, `random`, `reduce`, `reduceRight`,
     * `repeat`, `result`, `round`, `runInContext`, `sample`, `shift`, `size`,
     * `snakeCase`, `some`, `sortedIndex`, `sortedIndexBy`, `sortedLastIndex`,
     * `sortedLastIndexBy`, `startCase`, `startsWith`, `stubArray`, `stubFalse`,
     * `stubObject`, `stubString`, `stubTrue`, `subtract`, `sum`, `sumBy`,
     * `template`, `times`, `toFinite`, `toInteger`, `toJSON`, `toLength`,
     * `toLower`, `toNumber`, `toSafeInteger`, `toString`, `toUpper`, `trim`,
     * `trimEnd`, `trimStart`, `truncate`, `unescape`, `uniqueId`, `upperCase`,
     * `upperFirst`, `value`, and `words`
     *
     * @name _
     * @constructor
     * @category Seq
     * @param {*} value The value to wrap in a `lodash` instance.
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * function square(n) {
     *   return n * n;
     * }
     *
     * var wrapped = _([1, 2, 3]);
     *
     * // Returns an unwrapped value.
     * wrapped.reduce(_.add);
     * // => 6
     *
     * // Returns a wrapped value.
     * var squares = wrapped.map(square);
     *
     * _.isArray(squares);
     * // => false
     *
     * _.isArray(squares.value());
     * // => true
     */
    function lodash(value) {
      if (isObjectLike(value) && !isArray(value) && !(value instanceof LazyWrapper)) {
        if (value instanceof LodashWrapper) {
          return value;
        }
        if (hasOwnProperty.call(value, '__wrapped__')) {
          return wrapperClone(value);
        }
      }
      return new LodashWrapper(value);
    }

    /**
     * The base implementation of `_.create` without support for assigning
     * properties to the created object.
     *
     * @private
     * @param {Object} proto The object to inherit from.
     * @returns {Object} Returns the new object.
     */
    var baseCreate = function () {
      function object() {}
      return function (proto) {
        if (!isObject(proto)) {
          return {};
        }
        if (objectCreate) {
          return objectCreate(proto);
        }
        object.prototype = proto;
        var result = new object();
        object.prototype = undefined;
        return result;
      };
    }();

    /**
     * The function whose prototype chain sequence wrappers inherit from.
     *
     * @private
     */
    function baseLodash() {}
    // No operation performed.


    /**
     * The base constructor for creating `lodash` wrapper objects.
     *
     * @private
     * @param {*} value The value to wrap.
     * @param {boolean} [chainAll] Enable explicit method chain sequences.
     */
    function LodashWrapper(value, chainAll) {
      this.__wrapped__ = value;
      this.__actions__ = [];
      this.__chain__ = !!chainAll;
      this.__index__ = 0;
      this.__values__ = undefined;
    }

    // Ensure wrappers are instances of `baseLodash`.
    lodash.prototype = baseLodash.prototype;
    lodash.prototype.constructor = lodash;

    LodashWrapper.prototype = baseCreate(baseLodash.prototype);
    LodashWrapper.prototype.constructor = LodashWrapper;

    /*------------------------------------------------------------------------*/

    /**
     * Creates a lazy wrapper object which wraps `value` to enable lazy evaluation.
     *
     * @private
     * @constructor
     * @param {*} value The value to wrap.
     */
    function LazyWrapper(value) {
      this.__wrapped__ = value;
      this.__actions__ = [];
      this.__dir__ = 1;
      this.__filtered__ = false;
      this.__iteratees__ = [];
      this.__takeCount__ = MAX_ARRAY_LENGTH;
      this.__views__ = [];
    }

    /**
     * Creates a clone of the lazy wrapper object.
     *
     * @private
     * @name clone
     * @memberOf LazyWrapper
     * @returns {Object} Returns the cloned `LazyWrapper` object.
     */
    function lazyClone() {
      var result = new LazyWrapper(this.__wrapped__);
      result.__actions__ = copyArray(this.__actions__);
      result.__dir__ = this.__dir__;
      result.__filtered__ = this.__filtered__;
      result.__iteratees__ = copyArray(this.__iteratees__);
      result.__takeCount__ = this.__takeCount__;
      result.__views__ = copyArray(this.__views__);
      return result;
    }

    /**
     * Reverses the direction of lazy iteration.
     *
     * @private
     * @name reverse
     * @memberOf LazyWrapper
     * @returns {Object} Returns the new reversed `LazyWrapper` object.
     */
    function lazyReverse() {
      if (this.__filtered__) {
        var result = new LazyWrapper(this);
        result.__dir__ = -1;
        result.__filtered__ = true;
      } else {
        result = this.clone();
        result.__dir__ *= -1;
      }
      return result;
    }

    /**
     * Extracts the unwrapped value from its lazy wrapper.
     *
     * @private
     * @name value
     * @memberOf LazyWrapper
     * @returns {*} Returns the unwrapped value.
     */
    function lazyValue() {
      var array = this.__wrapped__.value(),
          dir = this.__dir__,
          isArr = isArray(array),
          isRight = dir < 0,
          arrLength = isArr ? array.length : 0,
          view = getView(0, arrLength, this.__views__),
          start = view.start,
          end = view.end,
          length = end - start,
          index = isRight ? end : start - 1,
          iteratees = this.__iteratees__,
          iterLength = iteratees.length,
          resIndex = 0,
          takeCount = nativeMin(length, this.__takeCount__);

      if (!isArr || !isRight && arrLength == length && takeCount == length) {
        return baseWrapperValue(array, this.__actions__);
      }
      var result = [];

      outer: while (length-- && resIndex < takeCount) {
        index += dir;

        var iterIndex = -1,
            value = array[index];

        while (++iterIndex < iterLength) {
          var data = iteratees[iterIndex],
              iteratee = data.iteratee,
              type = data.type,
              computed = iteratee(value);

          if (type == LAZY_MAP_FLAG) {
            value = computed;
          } else if (!computed) {
            if (type == LAZY_FILTER_FLAG) {
              continue outer;
            } else {
              break outer;
            }
          }
        }
        result[resIndex++] = value;
      }
      return result;
    }

    // Ensure `LazyWrapper` is an instance of `baseLodash`.
    LazyWrapper.prototype = baseCreate(baseLodash.prototype);
    LazyWrapper.prototype.constructor = LazyWrapper;

    /*------------------------------------------------------------------------*/

    /**
     * Creates a hash object.
     *
     * @private
     * @constructor
     * @param {Array} [entries] The key-value pairs to cache.
     */
    function Hash(entries) {
      var index = -1,
          length = entries == null ? 0 : entries.length;

      this.clear();
      while (++index < length) {
        var entry = entries[index];
        this.set(entry[0], entry[1]);
      }
    }

    /**
     * Removes all key-value entries from the hash.
     *
     * @private
     * @name clear
     * @memberOf Hash
     */
    function hashClear() {
      this.__data__ = nativeCreate ? nativeCreate(null) : {};
      this.size = 0;
    }

    /**
     * Removes `key` and its value from the hash.
     *
     * @private
     * @name delete
     * @memberOf Hash
     * @param {Object} hash The hash to modify.
     * @param {string} key The key of the value to remove.
     * @returns {boolean} Returns `true` if the entry was removed, else `false`.
     */
    function hashDelete(key) {
      var result = this.has(key) && delete this.__data__[key];
      this.size -= result ? 1 : 0;
      return result;
    }

    /**
     * Gets the hash value for `key`.
     *
     * @private
     * @name get
     * @memberOf Hash
     * @param {string} key The key of the value to get.
     * @returns {*} Returns the entry value.
     */
    function hashGet(key) {
      var data = this.__data__;
      if (nativeCreate) {
        var result = data[key];
        return result === HASH_UNDEFINED ? undefined : result;
      }
      return hasOwnProperty.call(data, key) ? data[key] : undefined;
    }

    /**
     * Checks if a hash value for `key` exists.
     *
     * @private
     * @name has
     * @memberOf Hash
     * @param {string} key The key of the entry to check.
     * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
     */
    function hashHas(key) {
      var data = this.__data__;
      return nativeCreate ? data[key] !== undefined : hasOwnProperty.call(data, key);
    }

    /**
     * Sets the hash `key` to `value`.
     *
     * @private
     * @name set
     * @memberOf Hash
     * @param {string} key The key of the value to set.
     * @param {*} value The value to set.
     * @returns {Object} Returns the hash instance.
     */
    function hashSet(key, value) {
      var data = this.__data__;
      this.size += this.has(key) ? 0 : 1;
      data[key] = nativeCreate && value === undefined ? HASH_UNDEFINED : value;
      return this;
    }

    // Add methods to `Hash`.
    Hash.prototype.clear = hashClear;
    Hash.prototype['delete'] = hashDelete;
    Hash.prototype.get = hashGet;
    Hash.prototype.has = hashHas;
    Hash.prototype.set = hashSet;

    /*------------------------------------------------------------------------*/

    /**
     * Creates an list cache object.
     *
     * @private
     * @constructor
     * @param {Array} [entries] The key-value pairs to cache.
     */
    function ListCache(entries) {
      var index = -1,
          length = entries == null ? 0 : entries.length;

      this.clear();
      while (++index < length) {
        var entry = entries[index];
        this.set(entry[0], entry[1]);
      }
    }

    /**
     * Removes all key-value entries from the list cache.
     *
     * @private
     * @name clear
     * @memberOf ListCache
     */
    function listCacheClear() {
      this.__data__ = [];
      this.size = 0;
    }

    /**
     * Removes `key` and its value from the list cache.
     *
     * @private
     * @name delete
     * @memberOf ListCache
     * @param {string} key The key of the value to remove.
     * @returns {boolean} Returns `true` if the entry was removed, else `false`.
     */
    function listCacheDelete(key) {
      var data = this.__data__,
          index = assocIndexOf(data, key);

      if (index < 0) {
        return false;
      }
      var lastIndex = data.length - 1;
      if (index == lastIndex) {
        data.pop();
      } else {
        splice.call(data, index, 1);
      }
      --this.size;
      return true;
    }

    /**
     * Gets the list cache value for `key`.
     *
     * @private
     * @name get
     * @memberOf ListCache
     * @param {string} key The key of the value to get.
     * @returns {*} Returns the entry value.
     */
    function listCacheGet(key) {
      var data = this.__data__,
          index = assocIndexOf(data, key);

      return index < 0 ? undefined : data[index][1];
    }

    /**
     * Checks if a list cache value for `key` exists.
     *
     * @private
     * @name has
     * @memberOf ListCache
     * @param {string} key The key of the entry to check.
     * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
     */
    function listCacheHas(key) {
      return assocIndexOf(this.__data__, key) > -1;
    }

    /**
     * Sets the list cache `key` to `value`.
     *
     * @private
     * @name set
     * @memberOf ListCache
     * @param {string} key The key of the value to set.
     * @param {*} value The value to set.
     * @returns {Object} Returns the list cache instance.
     */
    function listCacheSet(key, value) {
      var data = this.__data__,
          index = assocIndexOf(data, key);

      if (index < 0) {
        ++this.size;
        data.push([key, value]);
      } else {
        data[index][1] = value;
      }
      return this;
    }

    // Add methods to `ListCache`.
    ListCache.prototype.clear = listCacheClear;
    ListCache.prototype['delete'] = listCacheDelete;
    ListCache.prototype.get = listCacheGet;
    ListCache.prototype.has = listCacheHas;
    ListCache.prototype.set = listCacheSet;

    /*------------------------------------------------------------------------*/

    /**
     * Creates a map cache object to store key-value pairs.
     *
     * @private
     * @constructor
     * @param {Array} [entries] The key-value pairs to cache.
     */
    function MapCache(entries) {
      var index = -1,
          length = entries == null ? 0 : entries.length;

      this.clear();
      while (++index < length) {
        var entry = entries[index];
        this.set(entry[0], entry[1]);
      }
    }

    /**
     * Removes all key-value entries from the map.
     *
     * @private
     * @name clear
     * @memberOf MapCache
     */
    function mapCacheClear() {
      this.size = 0;
      this.__data__ = {
        'hash': new Hash(),
        'map': new (Map || ListCache)(),
        'string': new Hash()
      };
    }

    /**
     * Removes `key` and its value from the map.
     *
     * @private
     * @name delete
     * @memberOf MapCache
     * @param {string} key The key of the value to remove.
     * @returns {boolean} Returns `true` if the entry was removed, else `false`.
     */
    function mapCacheDelete(key) {
      var result = getMapData(this, key)['delete'](key);
      this.size -= result ? 1 : 0;
      return result;
    }

    /**
     * Gets the map value for `key`.
     *
     * @private
     * @name get
     * @memberOf MapCache
     * @param {string} key The key of the value to get.
     * @returns {*} Returns the entry value.
     */
    function mapCacheGet(key) {
      return getMapData(this, key).get(key);
    }

    /**
     * Checks if a map value for `key` exists.
     *
     * @private
     * @name has
     * @memberOf MapCache
     * @param {string} key The key of the entry to check.
     * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
     */
    function mapCacheHas(key) {
      return getMapData(this, key).has(key);
    }

    /**
     * Sets the map `key` to `value`.
     *
     * @private
     * @name set
     * @memberOf MapCache
     * @param {string} key The key of the value to set.
     * @param {*} value The value to set.
     * @returns {Object} Returns the map cache instance.
     */
    function mapCacheSet(key, value) {
      var data = getMapData(this, key),
          size = data.size;

      data.set(key, value);
      this.size += data.size == size ? 0 : 1;
      return this;
    }

    // Add methods to `MapCache`.
    MapCache.prototype.clear = mapCacheClear;
    MapCache.prototype['delete'] = mapCacheDelete;
    MapCache.prototype.get = mapCacheGet;
    MapCache.prototype.has = mapCacheHas;
    MapCache.prototype.set = mapCacheSet;

    /*------------------------------------------------------------------------*/

    /**
     *
     * Creates an array cache object to store unique values.
     *
     * @private
     * @constructor
     * @param {Array} [values] The values to cache.
     */
    function SetCache(values) {
      var index = -1,
          length = values == null ? 0 : values.length;

      this.__data__ = new MapCache();
      while (++index < length) {
        this.add(values[index]);
      }
    }

    /**
     * Adds `value` to the array cache.
     *
     * @private
     * @name add
     * @memberOf SetCache
     * @alias push
     * @param {*} value The value to cache.
     * @returns {Object} Returns the cache instance.
     */
    function setCacheAdd(value) {
      this.__data__.set(value, HASH_UNDEFINED);
      return this;
    }

    /**
     * Checks if `value` is in the array cache.
     *
     * @private
     * @name has
     * @memberOf SetCache
     * @param {*} value The value to search for.
     * @returns {number} Returns `true` if `value` is found, else `false`.
     */
    function setCacheHas(value) {
      return this.__data__.has(value);
    }

    // Add methods to `SetCache`.
    SetCache.prototype.add = SetCache.prototype.push = setCacheAdd;
    SetCache.prototype.has = setCacheHas;

    /*------------------------------------------------------------------------*/

    /**
     * Creates a stack cache object to store key-value pairs.
     *
     * @private
     * @constructor
     * @param {Array} [entries] The key-value pairs to cache.
     */
    function Stack(entries) {
      var data = this.__data__ = new ListCache(entries);
      this.size = data.size;
    }

    /**
     * Removes all key-value entries from the stack.
     *
     * @private
     * @name clear
     * @memberOf Stack
     */
    function stackClear() {
      this.__data__ = new ListCache();
      this.size = 0;
    }

    /**
     * Removes `key` and its value from the stack.
     *
     * @private
     * @name delete
     * @memberOf Stack
     * @param {string} key The key of the value to remove.
     * @returns {boolean} Returns `true` if the entry was removed, else `false`.
     */
    function stackDelete(key) {
      var data = this.__data__,
          result = data['delete'](key);

      this.size = data.size;
      return result;
    }

    /**
     * Gets the stack value for `key`.
     *
     * @private
     * @name get
     * @memberOf Stack
     * @param {string} key The key of the value to get.
     * @returns {*} Returns the entry value.
     */
    function stackGet(key) {
      return this.__data__.get(key);
    }

    /**
     * Checks if a stack value for `key` exists.
     *
     * @private
     * @name has
     * @memberOf Stack
     * @param {string} key The key of the entry to check.
     * @returns {boolean} Returns `true` if an entry for `key` exists, else `false`.
     */
    function stackHas(key) {
      return this.__data__.has(key);
    }

    /**
     * Sets the stack `key` to `value`.
     *
     * @private
     * @name set
     * @memberOf Stack
     * @param {string} key The key of the value to set.
     * @param {*} value The value to set.
     * @returns {Object} Returns the stack cache instance.
     */
    function stackSet(key, value) {
      var data = this.__data__;
      if (data instanceof ListCache) {
        var pairs = data.__data__;
        if (!Map || pairs.length < LARGE_ARRAY_SIZE - 1) {
          pairs.push([key, value]);
          this.size = ++data.size;
          return this;
        }
        data = this.__data__ = new MapCache(pairs);
      }
      data.set(key, value);
      this.size = data.size;
      return this;
    }

    // Add methods to `Stack`.
    Stack.prototype.clear = stackClear;
    Stack.prototype['delete'] = stackDelete;
    Stack.prototype.get = stackGet;
    Stack.prototype.has = stackHas;
    Stack.prototype.set = stackSet;

    /*------------------------------------------------------------------------*/

    /**
     * Creates an array of the enumerable property names of the array-like `value`.
     *
     * @private
     * @param {*} value The value to query.
     * @param {boolean} inherited Specify returning inherited property names.
     * @returns {Array} Returns the array of property names.
     */
    function arrayLikeKeys(value, inherited) {
      var isArr = isArray(value),
          isArg = !isArr && isArguments(value),
          isBuff = !isArr && !isArg && isBuffer(value),
          isType = !isArr && !isArg && !isBuff && isTypedArray(value),
          skipIndexes = isArr || isArg || isBuff || isType,
          result = skipIndexes ? baseTimes(value.length, String) : [],
          length = result.length;

      for (var key in value) {
        if ((inherited || hasOwnProperty.call(value, key)) && !(skipIndexes && (
        // Safari 9 has enumerable `arguments.length` in strict mode.
        key == 'length' ||
        // Node.js 0.10 has enumerable non-index properties on buffers.
        isBuff && (key == 'offset' || key == 'parent') ||
        // PhantomJS 2 has enumerable non-index properties on typed arrays.
        isType && (key == 'buffer' || key == 'byteLength' || key == 'byteOffset') ||
        // Skip index properties.
        isIndex(key, length)))) {
          result.push(key);
        }
      }
      return result;
    }

    /**
     * Assigns `value` to `key` of `object` if the existing value is not equivalent
     * using [`SameValueZero`](http://ecma-international.org/ecma-262/7.0/#sec-samevaluezero)
     * for equality comparisons.
     *
     * @private
     * @param {Object} object The object to modify.
     * @param {string} key The key of the property to assign.
     * @param {*} value The value to assign.
     */
    function assignValue(object, key, value) {
      var objValue = object[key];
      if (!(hasOwnProperty.call(object, key) && eq(objValue, value)) || value === undefined && !(key in object)) {
        baseAssignValue(object, key, value);
      }
    }

    /**
     * Gets the index at which the `key` is found in `array` of key-value pairs.
     *
     * @private
     * @param {Array} array The array to inspect.
     * @param {*} key The key to search for.
     * @returns {number} Returns the index of the matched value, else `-1`.
     */
    function assocIndexOf(array, key) {
      var length = array.length;
      while (length--) {
        if (eq(array[length][0], key)) {
          return length;
        }
      }
      return -1;
    }

    /**
     * Aggregates elements of `collection` on `accumulator` with keys transformed
     * by `iteratee` and values set by `setter`.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} setter The function to set `accumulator` values.
     * @param {Function} iteratee The iteratee to transform keys.
     * @param {Object} accumulator The initial aggregated object.
     * @returns {Function} Returns `accumulator`.
     */
    function baseAggregator(collection, setter, iteratee, accumulator) {
      baseEach(collection, function (value, key, collection) {
        setter(accumulator, value, iteratee(value), collection);
      });
      return accumulator;
    }

    /**
     * The base implementation of `_.assign` without support for multiple sources
     * or `customizer` functions.
     *
     * @private
     * @param {Object} object The destination object.
     * @param {Object} source The source object.
     * @returns {Object} Returns `object`.
     */
    function baseAssign(object, source) {
      return object && copyObject(source, keys(source), object);
    }

    /**
     * The base implementation of `_.assignIn` without support for multiple sources
     * or `customizer` functions.
     *
     * @private
     * @param {Object} object The destination object.
     * @param {Object} source The source object.
     * @returns {Object} Returns `object`.
     */
    function baseAssignIn(object, source) {
      return object && copyObject(source, keysIn(source), object);
    }

    /**
     * The base implementation of `assignValue` and `assignMergeValue` without
     * value checks.
     *
     * @private
     * @param {Object} object The object to modify.
     * @param {string} key The key of the property to assign.
     * @param {*} value The value to assign.
     */
    function baseAssignValue(object, key, value) {
      if (key == '__proto__' && defineProperty) {
        defineProperty(object, key, {
          'configurable': true,
          'enumerable': true,
          'value': value,
          'writable': true
        });
      } else {
        object[key] = value;
      }
    }

    /**
     * The base implementation of `_.at` without support for individual paths.
     *
     * @private
     * @param {Object} object The object to iterate over.
     * @param {string[]} paths The property paths to pick.
     * @returns {Array} Returns the picked elements.
     */
    function baseAt(object, paths) {
      var index = -1,
          length = paths.length,
          result = Array(length),
          skip = object == null;

      while (++index < length) {
        result[index] = skip ? undefined : get(object, paths[index]);
      }
      return result;
    }

    /**
     * The base implementation of `_.clone` and `_.cloneDeep` which tracks
     * traversed objects.
     *
     * @private
     * @param {*} value The value to clone.
     * @param {boolean} bitmask The bitmask flags.
     *  1 - Deep clone
     *  2 - Flatten inherited properties
     *  4 - Clone symbols
     * @param {Function} [customizer] The function to customize cloning.
     * @param {string} [key] The key of `value`.
     * @param {Object} [object] The parent object of `value`.
     * @param {Object} [stack] Tracks traversed objects and their clone counterparts.
     * @returns {*} Returns the cloned value.
     */
    function baseClone(value, bitmask, customizer, key, object, stack) {
      var result,
          isDeep = bitmask & CLONE_DEEP_FLAG,
          isFlat = bitmask & CLONE_FLAT_FLAG,
          isFull = bitmask & CLONE_SYMBOLS_FLAG;

      if (customizer) {
        result = object ? customizer(value, key, object, stack) : customizer(value);
      }
      if (result !== undefined) {
        return result;
      }
      if (!isObject(value)) {
        return value;
      }
      var isArr = isArray(value);
      if (isArr) {
        result = initCloneArray(value);
        if (!isDeep) {
          return copyArray(value, result);
        }
      } else {
        var tag = getTag(value),
            isFunc = tag == funcTag || tag == genTag;

        if (isBuffer(value)) {
          return cloneBuffer(value, isDeep);
        }
        if (tag == objectTag || tag == argsTag || isFunc && !object) {
          result = isFlat || isFunc ? {} : initCloneObject(value);
          if (!isDeep) {
            return isFlat ? copySymbolsIn(value, baseAssignIn(result, value)) : copySymbols(value, baseAssign(result, value));
          }
        } else {
          if (!cloneableTags[tag]) {
            return object ? value : {};
          }
          result = initCloneByTag(value, tag, baseClone, isDeep);
        }
      }
      // Check for circular references and return its corresponding clone.
      stack || (stack = new Stack());
      var stacked = stack.get(value);
      if (stacked) {
        return stacked;
      }
      stack.set(value, result);

      var keysFunc = isFull ? isFlat ? getAllKeysIn : getAllKeys : isFlat ? keysIn : keys;

      var props = isArr ? undefined : keysFunc(value);
      arrayEach(props || value, function (subValue, key) {
        if (props) {
          key = subValue;
          subValue = value[key];
        }
        // Recursively populate clone (susceptible to call stack limits).
        assignValue(result, key, baseClone(subValue, bitmask, customizer, key, value, stack));
      });
      return result;
    }

    /**
     * The base implementation of `_.forEach` without support for iteratee shorthands.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Array|Object} Returns `collection`.
     */
    var baseEach = createBaseEach(baseForOwn);

    /**
     * The base implementation of `_.filter` without support for iteratee shorthands.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} predicate The function invoked per iteration.
     * @returns {Array} Returns the new filtered array.
     */
    function baseFilter(collection, predicate) {
      var result = [];
      baseEach(collection, function (value, index, collection) {
        if (predicate(value, index, collection)) {
          result.push(value);
        }
      });
      return result;
    }

    /**
     * The base implementation of `_.flatten` with support for restricting flattening.
     *
     * @private
     * @param {Array} array The array to flatten.
     * @param {number} depth The maximum recursion depth.
     * @param {boolean} [predicate=isFlattenable] The function invoked per iteration.
     * @param {boolean} [isStrict] Restrict to values that pass `predicate` checks.
     * @param {Array} [result=[]] The initial result value.
     * @returns {Array} Returns the new flattened array.
     */
    function baseFlatten(array, depth, predicate, isStrict, result) {
      var index = -1,
          length = array.length;

      predicate || (predicate = isFlattenable);
      result || (result = []);

      while (++index < length) {
        var value = array[index];
        if (depth > 0 && predicate(value)) {
          if (depth > 1) {
            // Recursively flatten arrays (susceptible to call stack limits).
            baseFlatten(value, depth - 1, predicate, isStrict, result);
          } else {
            arrayPush(result, value);
          }
        } else if (!isStrict) {
          result[result.length] = value;
        }
      }
      return result;
    }

    /**
     * The base implementation of `baseForOwn` which iterates over `object`
     * properties returned by `keysFunc` and invokes `iteratee` for each property.
     * Iteratee functions may exit iteration early by explicitly returning `false`.
     *
     * @private
     * @param {Object} object The object to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @param {Function} keysFunc The function to get the keys of `object`.
     * @returns {Object} Returns `object`.
     */
    var baseFor = createBaseFor();

    /**
     * The base implementation of `_.forOwn` without support for iteratee shorthands.
     *
     * @private
     * @param {Object} object The object to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Object} Returns `object`.
     */
    function baseForOwn(object, iteratee) {
      return object && baseFor(object, iteratee, keys);
    }

    /**
     * The base implementation of `_.functions` which creates an array of
     * `object` function property names filtered from `props`.
     *
     * @private
     * @param {Object} object The object to inspect.
     * @param {Array} props The property names to filter.
     * @returns {Array} Returns the function names.
     */
    function baseFunctions(object, props) {
      return arrayFilter(props, function (key) {
        return isFunction(object[key]);
      });
    }

    /**
     * The base implementation of `_.get` without support for default values.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Array|string} path The path of the property to get.
     * @returns {*} Returns the resolved value.
     */
    function baseGet(object, path) {
      path = castPath(path, object);

      var index = 0,
          length = path.length;

      while (object != null && index < length) {
        object = object[toKey(path[index++])];
      }
      return index && index == length ? object : undefined;
    }

    /**
     * The base implementation of `getAllKeys` and `getAllKeysIn` which uses
     * `keysFunc` and `symbolsFunc` to get the enumerable property names and
     * symbols of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Function} keysFunc The function to get the keys of `object`.
     * @param {Function} symbolsFunc The function to get the symbols of `object`.
     * @returns {Array} Returns the array of property names and symbols.
     */
    function baseGetAllKeys(object, keysFunc, symbolsFunc) {
      var result = keysFunc(object);
      return isArray(object) ? result : arrayPush(result, symbolsFunc(object));
    }

    /**
     * The base implementation of `getTag` without fallbacks for buggy environments.
     *
     * @private
     * @param {*} value The value to query.
     * @returns {string} Returns the `toStringTag`.
     */
    function baseGetTag(value) {
      if (value == null) {
        return value === undefined ? undefinedTag : nullTag;
      }
      return symToStringTag && symToStringTag in Object(value) ? getRawTag(value) : objectToString(value);
    }

    /**
     * The base implementation of `_.hasIn` without support for deep paths.
     *
     * @private
     * @param {Object} [object] The object to query.
     * @param {Array|string} key The key to check.
     * @returns {boolean} Returns `true` if `key` exists, else `false`.
     */
    function baseHasIn(object, key) {
      return object != null && key in Object(object);
    }

    /**
     * The base implementation of `_.invoke` without support for individual
     * method arguments.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Array|string} path The path of the method to invoke.
     * @param {Array} args The arguments to invoke the method with.
     * @returns {*} Returns the result of the invoked method.
     */
    function baseInvoke(object, path, args) {
      path = castPath(path, object);
      object = parent(object, path);
      var func = object == null ? object : object[toKey(last(path))];
      return func == null ? undefined : apply(func, object, args);
    }

    /**
     * The base implementation of `_.isArguments`.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is an `arguments` object,
     */
    function baseIsArguments(value) {
      return isObjectLike(value) && baseGetTag(value) == argsTag;
    }

    /**
     * The base implementation of `_.isEqual` which supports partial comparisons
     * and tracks traversed objects.
     *
     * @private
     * @param {*} value The value to compare.
     * @param {*} other The other value to compare.
     * @param {boolean} bitmask The bitmask flags.
     *  1 - Unordered comparison
     *  2 - Partial comparison
     * @param {Function} [customizer] The function to customize comparisons.
     * @param {Object} [stack] Tracks traversed `value` and `other` objects.
     * @returns {boolean} Returns `true` if the values are equivalent, else `false`.
     */
    function baseIsEqual(value, other, bitmask, customizer, stack) {
      if (value === other) {
        return true;
      }
      if (value == null || other == null || !isObjectLike(value) && !isObjectLike(other)) {
        return value !== value && other !== other;
      }
      return baseIsEqualDeep(value, other, bitmask, customizer, baseIsEqual, stack);
    }

    /**
     * A specialized version of `baseIsEqual` for arrays and objects which performs
     * deep comparisons and tracks traversed objects enabling objects with circular
     * references to be compared.
     *
     * @private
     * @param {Object} object The object to compare.
     * @param {Object} other The other object to compare.
     * @param {number} bitmask The bitmask flags. See `baseIsEqual` for more details.
     * @param {Function} customizer The function to customize comparisons.
     * @param {Function} equalFunc The function to determine equivalents of values.
     * @param {Object} [stack] Tracks traversed `object` and `other` objects.
     * @returns {boolean} Returns `true` if the objects are equivalent, else `false`.
     */
    function baseIsEqualDeep(object, other, bitmask, customizer, equalFunc, stack) {
      var objIsArr = isArray(object),
          othIsArr = isArray(other),
          objTag = objIsArr ? arrayTag : getTag(object),
          othTag = othIsArr ? arrayTag : getTag(other);

      objTag = objTag == argsTag ? objectTag : objTag;
      othTag = othTag == argsTag ? objectTag : othTag;

      var objIsObj = objTag == objectTag,
          othIsObj = othTag == objectTag,
          isSameTag = objTag == othTag;

      if (isSameTag && isBuffer(object)) {
        if (!isBuffer(other)) {
          return false;
        }
        objIsArr = true;
        objIsObj = false;
      }
      if (isSameTag && !objIsObj) {
        stack || (stack = new Stack());
        return objIsArr || isTypedArray(object) ? equalArrays(object, other, bitmask, customizer, equalFunc, stack) : equalByTag(object, other, objTag, bitmask, customizer, equalFunc, stack);
      }
      if (!(bitmask & COMPARE_PARTIAL_FLAG)) {
        var objIsWrapped = objIsObj && hasOwnProperty.call(object, '__wrapped__'),
            othIsWrapped = othIsObj && hasOwnProperty.call(other, '__wrapped__');

        if (objIsWrapped || othIsWrapped) {
          var objUnwrapped = objIsWrapped ? object.value() : object,
              othUnwrapped = othIsWrapped ? other.value() : other;

          stack || (stack = new Stack());
          return equalFunc(objUnwrapped, othUnwrapped, bitmask, customizer, stack);
        }
      }
      if (!isSameTag) {
        return false;
      }
      stack || (stack = new Stack());
      return equalObjects(object, other, bitmask, customizer, equalFunc, stack);
    }

    /**
     * The base implementation of `_.isMatch` without support for iteratee shorthands.
     *
     * @private
     * @param {Object} object The object to inspect.
     * @param {Object} source The object of property values to match.
     * @param {Array} matchData The property names, values, and compare flags to match.
     * @param {Function} [customizer] The function to customize comparisons.
     * @returns {boolean} Returns `true` if `object` is a match, else `false`.
     */
    function baseIsMatch(object, source, matchData, customizer) {
      var index = matchData.length,
          length = index,
          noCustomizer = !customizer;

      if (object == null) {
        return !length;
      }
      object = Object(object);
      while (index--) {
        var data = matchData[index];
        if (noCustomizer && data[2] ? data[1] !== object[data[0]] : !(data[0] in object)) {
          return false;
        }
      }
      while (++index < length) {
        data = matchData[index];
        var key = data[0],
            objValue = object[key],
            srcValue = data[1];

        if (noCustomizer && data[2]) {
          if (objValue === undefined && !(key in object)) {
            return false;
          }
        } else {
          var stack = new Stack();
          if (customizer) {
            var result = customizer(objValue, srcValue, key, object, source, stack);
          }
          if (!(result === undefined ? baseIsEqual(srcValue, objValue, COMPARE_PARTIAL_FLAG | COMPARE_UNORDERED_FLAG, customizer, stack) : result)) {
            return false;
          }
        }
      }
      return true;
    }

    /**
     * The base implementation of `_.isNative` without bad shim checks.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a native function,
     *  else `false`.
     */
    function baseIsNative(value) {
      if (!isObject(value) || isMasked(value)) {
        return false;
      }
      var pattern = isFunction(value) ? reIsNative : reIsHostCtor;
      return pattern.test(toSource(value));
    }

    /**
     * The base implementation of `_.isTypedArray` without Node.js optimizations.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a typed array, else `false`.
     */
    function baseIsTypedArray(value) {
      return isObjectLike(value) && isLength(value.length) && !!typedArrayTags[baseGetTag(value)];
    }

    /**
     * The base implementation of `_.iteratee`.
     *
     * @private
     * @param {*} [value=_.identity] The value to convert to an iteratee.
     * @returns {Function} Returns the iteratee.
     */
    function baseIteratee(value) {
      // Don't store the `typeof` result in a variable to avoid a JIT bug in Safari 9.
      // See https://bugs.webkit.org/show_bug.cgi?id=156034 for more details.
      if (typeof value == 'function') {
        return value;
      }
      if (value == null) {
        return identity;
      }
      if ((typeof value === 'undefined' ? 'undefined' : _typeof(value)) == 'object') {
        return isArray(value) ? baseMatchesProperty(value[0], value[1]) : baseMatches(value);
      }
      return property(value);
    }

    /**
     * The base implementation of `_.keys` which doesn't treat sparse arrays as dense.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names.
     */
    function baseKeys(object) {
      if (!isPrototype(object)) {
        return nativeKeys(object);
      }
      var result = [];
      for (var key in Object(object)) {
        if (hasOwnProperty.call(object, key) && key != 'constructor') {
          result.push(key);
        }
      }
      return result;
    }

    /**
     * The base implementation of `_.keysIn` which doesn't treat sparse arrays as dense.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names.
     */
    function baseKeysIn(object) {
      if (!isObject(object)) {
        return nativeKeysIn(object);
      }
      var isProto = isPrototype(object),
          result = [];

      for (var key in object) {
        if (!(key == 'constructor' && (isProto || !hasOwnProperty.call(object, key)))) {
          result.push(key);
        }
      }
      return result;
    }

    /**
     * The base implementation of `_.map` without support for iteratee shorthands.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} iteratee The function invoked per iteration.
     * @returns {Array} Returns the new mapped array.
     */
    function baseMap(collection, iteratee) {
      var index = -1,
          result = isArrayLike(collection) ? Array(collection.length) : [];

      baseEach(collection, function (value, key, collection) {
        result[++index] = iteratee(value, key, collection);
      });
      return result;
    }

    /**
     * The base implementation of `_.matches` which doesn't clone `source`.
     *
     * @private
     * @param {Object} source The object of property values to match.
     * @returns {Function} Returns the new spec function.
     */
    function baseMatches(source) {
      var matchData = getMatchData(source);
      if (matchData.length == 1 && matchData[0][2]) {
        return matchesStrictComparable(matchData[0][0], matchData[0][1]);
      }
      return function (object) {
        return object === source || baseIsMatch(object, source, matchData);
      };
    }

    /**
     * The base implementation of `_.matchesProperty` which doesn't clone `srcValue`.
     *
     * @private
     * @param {string} path The path of the property to get.
     * @param {*} srcValue The value to match.
     * @returns {Function} Returns the new spec function.
     */
    function baseMatchesProperty(path, srcValue) {
      if (isKey(path) && isStrictComparable(srcValue)) {
        return matchesStrictComparable(toKey(path), srcValue);
      }
      return function (object) {
        var objValue = get(object, path);
        return objValue === undefined && objValue === srcValue ? hasIn(object, path) : baseIsEqual(srcValue, objValue, COMPARE_PARTIAL_FLAG | COMPARE_UNORDERED_FLAG);
      };
    }

    /**
     * The base implementation of  `_.pickBy` without support for iteratee shorthands.
     *
     * @private
     * @param {Object} object The source object.
     * @param {string[]} paths The property paths to pick.
     * @param {Function} predicate The function invoked per property.
     * @returns {Object} Returns the new object.
     */
    function basePickBy(object, paths, predicate) {
      var index = -1,
          length = paths.length,
          result = {};

      while (++index < length) {
        var path = paths[index],
            value = baseGet(object, path);

        if (predicate(value, path)) {
          baseSet(result, castPath(path, object), value);
        }
      }
      return result;
    }

    /**
     * A specialized version of `baseProperty` which supports deep paths.
     *
     * @private
     * @param {Array|string} path The path of the property to get.
     * @returns {Function} Returns the new accessor function.
     */
    function basePropertyDeep(path) {
      return function (object) {
        return baseGet(object, path);
      };
    }

    /**
     * The base implementation of `_.rest` which doesn't validate or coerce arguments.
     *
     * @private
     * @param {Function} func The function to apply a rest parameter to.
     * @param {number} [start=func.length-1] The start position of the rest parameter.
     * @returns {Function} Returns the new function.
     */
    function baseRest(func, start) {
      return setToString(overRest(func, start, identity), func + '');
    }

    /**
     * The base implementation of `_.set`.
     *
     * @private
     * @param {Object} object The object to modify.
     * @param {Array|string} path The path of the property to set.
     * @param {*} value The value to set.
     * @param {Function} [customizer] The function to customize path creation.
     * @returns {Object} Returns `object`.
     */
    function baseSet(object, path, value, customizer) {
      if (!isObject(object)) {
        return object;
      }
      path = castPath(path, object);

      var index = -1,
          length = path.length,
          lastIndex = length - 1,
          nested = object;

      while (nested != null && ++index < length) {
        var key = toKey(path[index]),
            newValue = value;

        if (index != lastIndex) {
          var objValue = nested[key];
          newValue = customizer ? customizer(objValue, key, nested) : undefined;
          if (newValue === undefined) {
            newValue = isObject(objValue) ? objValue : isIndex(path[index + 1]) ? [] : {};
          }
        }
        assignValue(nested, key, newValue);
        nested = nested[key];
      }
      return object;
    }

    /**
     * The base implementation of `setData` without support for hot loop shorting.
     *
     * @private
     * @param {Function} func The function to associate metadata with.
     * @param {*} data The metadata.
     * @returns {Function} Returns `func`.
     */
    var baseSetData = !metaMap ? identity : function (func, data) {
      metaMap.set(func, data);
      return func;
    };

    /**
     * The base implementation of `setToString` without support for hot loop shorting.
     *
     * @private
     * @param {Function} func The function to modify.
     * @param {Function} string The `toString` result.
     * @returns {Function} Returns `func`.
     */
    var baseSetToString = !defineProperty ? identity : function (func, string) {
      return defineProperty(func, 'toString', {
        'configurable': true,
        'enumerable': false,
        'value': constant(string),
        'writable': true
      });
    };

    /**
     * The base implementation of `_.slice` without an iteratee call guard.
     *
     * @private
     * @param {Array} array The array to slice.
     * @param {number} [start=0] The start position.
     * @param {number} [end=array.length] The end position.
     * @returns {Array} Returns the slice of `array`.
     */
    function baseSlice(array, start, end) {
      var index = -1,
          length = array.length;

      if (start < 0) {
        start = -start > length ? 0 : length + start;
      }
      end = end > length ? length : end;
      if (end < 0) {
        end += length;
      }
      length = start > end ? 0 : end - start >>> 0;
      start >>>= 0;

      var result = Array(length);
      while (++index < length) {
        result[index] = array[index + start];
      }
      return result;
    }

    /**
     * The base implementation of `_.some` without support for iteratee shorthands.
     *
     * @private
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} predicate The function invoked per iteration.
     * @returns {boolean} Returns `true` if any element passes the predicate check,
     *  else `false`.
     */
    function baseSome(collection, predicate) {
      var result;

      baseEach(collection, function (value, index, collection) {
        result = predicate(value, index, collection);
        return !result;
      });
      return !!result;
    }

    /**
     * The base implementation of `_.toString` which doesn't convert nullish
     * values to empty strings.
     *
     * @private
     * @param {*} value The value to process.
     * @returns {string} Returns the string.
     */
    function baseToString(value) {
      // Exit early for strings to avoid a performance hit in some environments.
      if (typeof value == 'string') {
        return value;
      }
      if (isArray(value)) {
        // Recursively convert values (susceptible to call stack limits).
        return arrayMap(value, baseToString) + '';
      }
      if (isSymbol(value)) {
        return symbolToString ? symbolToString.call(value) : '';
      }
      var result = value + '';
      return result == '0' && 1 / value == -INFINITY ? '-0' : result;
    }

    /**
     * The base implementation of `wrapperValue` which returns the result of
     * performing a sequence of actions on the unwrapped `value`, where each
     * successive action is supplied the return value of the previous.
     *
     * @private
     * @param {*} value The unwrapped value.
     * @param {Array} actions Actions to perform to resolve the unwrapped value.
     * @returns {*} Returns the resolved value.
     */
    function baseWrapperValue(value, actions) {
      var result = value;
      if (result instanceof LazyWrapper) {
        result = result.value();
      }
      return arrayReduce(actions, function (result, action) {
        return action.func.apply(action.thisArg, arrayPush([result], action.args));
      }, result);
    }

    /**
     * Casts `value` to a path array if it's not one.
     *
     * @private
     * @param {*} value The value to inspect.
     * @param {Object} [object] The object to query keys on.
     * @returns {Array} Returns the cast property path array.
     */
    function castPath(value, object) {
      if (isArray(value)) {
        return value;
      }
      return isKey(value, object) ? [value] : stringToPath(toString(value));
    }

    /**
     * Creates a clone of  `buffer`.
     *
     * @private
     * @param {Buffer} buffer The buffer to clone.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Buffer} Returns the cloned buffer.
     */
    function cloneBuffer(buffer, isDeep) {
      if (isDeep) {
        return buffer.slice();
      }
      var length = buffer.length,
          result = allocUnsafe ? allocUnsafe(length) : new buffer.constructor(length);

      buffer.copy(result);
      return result;
    }

    /**
     * Creates a clone of `arrayBuffer`.
     *
     * @private
     * @param {ArrayBuffer} arrayBuffer The array buffer to clone.
     * @returns {ArrayBuffer} Returns the cloned array buffer.
     */
    function cloneArrayBuffer(arrayBuffer) {
      var result = new arrayBuffer.constructor(arrayBuffer.byteLength);
      new Uint8Array(result).set(new Uint8Array(arrayBuffer));
      return result;
    }

    /**
     * Creates a clone of `dataView`.
     *
     * @private
     * @param {Object} dataView The data view to clone.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Object} Returns the cloned data view.
     */
    function cloneDataView(dataView, isDeep) {
      var buffer = isDeep ? cloneArrayBuffer(dataView.buffer) : dataView.buffer;
      return new dataView.constructor(buffer, dataView.byteOffset, dataView.byteLength);
    }

    /**
     * Creates a clone of `map`.
     *
     * @private
     * @param {Object} map The map to clone.
     * @param {Function} cloneFunc The function to clone values.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Object} Returns the cloned map.
     */
    function cloneMap(map, isDeep, cloneFunc) {
      var array = isDeep ? cloneFunc(mapToArray(map), CLONE_DEEP_FLAG) : mapToArray(map);
      return arrayReduce(array, addMapEntry, new map.constructor());
    }

    /**
     * Creates a clone of `regexp`.
     *
     * @private
     * @param {Object} regexp The regexp to clone.
     * @returns {Object} Returns the cloned regexp.
     */
    function cloneRegExp(regexp) {
      var result = new regexp.constructor(regexp.source, reFlags.exec(regexp));
      result.lastIndex = regexp.lastIndex;
      return result;
    }

    /**
     * Creates a clone of `set`.
     *
     * @private
     * @param {Object} set The set to clone.
     * @param {Function} cloneFunc The function to clone values.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Object} Returns the cloned set.
     */
    function cloneSet(set, isDeep, cloneFunc) {
      var array = isDeep ? cloneFunc(setToArray(set), CLONE_DEEP_FLAG) : setToArray(set);
      return arrayReduce(array, addSetEntry, new set.constructor());
    }

    /**
     * Creates a clone of the `symbol` object.
     *
     * @private
     * @param {Object} symbol The symbol object to clone.
     * @returns {Object} Returns the cloned symbol object.
     */
    function cloneSymbol(symbol) {
      return symbolValueOf ? Object(symbolValueOf.call(symbol)) : {};
    }

    /**
     * Creates a clone of `typedArray`.
     *
     * @private
     * @param {Object} typedArray The typed array to clone.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Object} Returns the cloned typed array.
     */
    function cloneTypedArray(typedArray, isDeep) {
      var buffer = isDeep ? cloneArrayBuffer(typedArray.buffer) : typedArray.buffer;
      return new typedArray.constructor(buffer, typedArray.byteOffset, typedArray.length);
    }

    /**
     * Creates an array that is the composition of partially applied arguments,
     * placeholders, and provided arguments into a single array of arguments.
     *
     * @private
     * @param {Array} args The provided arguments.
     * @param {Array} partials The arguments to prepend to those provided.
     * @param {Array} holders The `partials` placeholder indexes.
     * @params {boolean} [isCurried] Specify composing for a curried function.
     * @returns {Array} Returns the new array of composed arguments.
     */
    function composeArgs(args, partials, holders, isCurried) {
      var argsIndex = -1,
          argsLength = args.length,
          holdersLength = holders.length,
          leftIndex = -1,
          leftLength = partials.length,
          rangeLength = nativeMax(argsLength - holdersLength, 0),
          result = Array(leftLength + rangeLength),
          isUncurried = !isCurried;

      while (++leftIndex < leftLength) {
        result[leftIndex] = partials[leftIndex];
      }
      while (++argsIndex < holdersLength) {
        if (isUncurried || argsIndex < argsLength) {
          result[holders[argsIndex]] = args[argsIndex];
        }
      }
      while (rangeLength--) {
        result[leftIndex++] = args[argsIndex++];
      }
      return result;
    }

    /**
     * This function is like `composeArgs` except that the arguments composition
     * is tailored for `_.partialRight`.
     *
     * @private
     * @param {Array} args The provided arguments.
     * @param {Array} partials The arguments to append to those provided.
     * @param {Array} holders The `partials` placeholder indexes.
     * @params {boolean} [isCurried] Specify composing for a curried function.
     * @returns {Array} Returns the new array of composed arguments.
     */
    function composeArgsRight(args, partials, holders, isCurried) {
      var argsIndex = -1,
          argsLength = args.length,
          holdersIndex = -1,
          holdersLength = holders.length,
          rightIndex = -1,
          rightLength = partials.length,
          rangeLength = nativeMax(argsLength - holdersLength, 0),
          result = Array(rangeLength + rightLength),
          isUncurried = !isCurried;

      while (++argsIndex < rangeLength) {
        result[argsIndex] = args[argsIndex];
      }
      var offset = argsIndex;
      while (++rightIndex < rightLength) {
        result[offset + rightIndex] = partials[rightIndex];
      }
      while (++holdersIndex < holdersLength) {
        if (isUncurried || argsIndex < argsLength) {
          result[offset + holders[holdersIndex]] = args[argsIndex++];
        }
      }
      return result;
    }

    /**
     * Copies the values of `source` to `array`.
     *
     * @private
     * @param {Array} source The array to copy values from.
     * @param {Array} [array=[]] The array to copy values to.
     * @returns {Array} Returns `array`.
     */
    function copyArray(source, array) {
      var index = -1,
          length = source.length;

      array || (array = Array(length));
      while (++index < length) {
        array[index] = source[index];
      }
      return array;
    }

    /**
     * Copies properties of `source` to `object`.
     *
     * @private
     * @param {Object} source The object to copy properties from.
     * @param {Array} props The property identifiers to copy.
     * @param {Object} [object={}] The object to copy properties to.
     * @param {Function} [customizer] The function to customize copied values.
     * @returns {Object} Returns `object`.
     */
    function copyObject(source, props, object, customizer) {
      var isNew = !object;
      object || (object = {});

      var index = -1,
          length = props.length;

      while (++index < length) {
        var key = props[index];

        var newValue = customizer ? customizer(object[key], source[key], key, object, source) : undefined;

        if (newValue === undefined) {
          newValue = source[key];
        }
        if (isNew) {
          baseAssignValue(object, key, newValue);
        } else {
          assignValue(object, key, newValue);
        }
      }
      return object;
    }

    /**
     * Copies own symbols of `source` to `object`.
     *
     * @private
     * @param {Object} source The object to copy symbols from.
     * @param {Object} [object={}] The object to copy symbols to.
     * @returns {Object} Returns `object`.
     */
    function copySymbols(source, object) {
      return copyObject(source, getSymbols(source), object);
    }

    /**
     * Copies own and inherited symbols of `source` to `object`.
     *
     * @private
     * @param {Object} source The object to copy symbols from.
     * @param {Object} [object={}] The object to copy symbols to.
     * @returns {Object} Returns `object`.
     */
    function copySymbolsIn(source, object) {
      return copyObject(source, getSymbolsIn(source), object);
    }

    /**
     * Creates a function like `_.groupBy`.
     *
     * @private
     * @param {Function} setter The function to set accumulator values.
     * @param {Function} [initializer] The accumulator object initializer.
     * @returns {Function} Returns the new aggregator function.
     */
    function createAggregator(setter, initializer) {
      return function (collection, iteratee) {
        var func = isArray(collection) ? arrayAggregator : baseAggregator,
            accumulator = initializer ? initializer() : {};

        return func(collection, setter, getIteratee(iteratee, 2), accumulator);
      };
    }

    /**
     * Creates a `baseEach` or `baseEachRight` function.
     *
     * @private
     * @param {Function} eachFunc The function to iterate over a collection.
     * @param {boolean} [fromRight] Specify iterating from right to left.
     * @returns {Function} Returns the new base function.
     */
    function createBaseEach(eachFunc, fromRight) {
      return function (collection, iteratee) {
        if (collection == null) {
          return collection;
        }
        if (!isArrayLike(collection)) {
          return eachFunc(collection, iteratee);
        }
        var length = collection.length,
            index = fromRight ? length : -1,
            iterable = Object(collection);

        while (fromRight ? index-- : ++index < length) {
          if (iteratee(iterable[index], index, iterable) === false) {
            break;
          }
        }
        return collection;
      };
    }

    /**
     * Creates a base function for methods like `_.forIn` and `_.forOwn`.
     *
     * @private
     * @param {boolean} [fromRight] Specify iterating from right to left.
     * @returns {Function} Returns the new base function.
     */
    function createBaseFor(fromRight) {
      return function (object, iteratee, keysFunc) {
        var index = -1,
            iterable = Object(object),
            props = keysFunc(object),
            length = props.length;

        while (length--) {
          var key = props[fromRight ? length : ++index];
          if (iteratee(iterable[key], key, iterable) === false) {
            break;
          }
        }
        return object;
      };
    }

    /**
     * Creates a function that produces an instance of `Ctor` regardless of
     * whether it was invoked as part of a `new` expression or by `call` or `apply`.
     *
     * @private
     * @param {Function} Ctor The constructor to wrap.
     * @returns {Function} Returns the new wrapped function.
     */
    function createCtor(Ctor) {
      return function () {
        // Use a `switch` statement to work with class constructors. See
        // http://ecma-international.org/ecma-262/7.0/#sec-ecmascript-function-objects-call-thisargument-argumentslist
        // for more details.
        var args = arguments;
        switch (args.length) {
          case 0:
            return new Ctor();
          case 1:
            return new Ctor(args[0]);
          case 2:
            return new Ctor(args[0], args[1]);
          case 3:
            return new Ctor(args[0], args[1], args[2]);
          case 4:
            return new Ctor(args[0], args[1], args[2], args[3]);
          case 5:
            return new Ctor(args[0], args[1], args[2], args[3], args[4]);
          case 6:
            return new Ctor(args[0], args[1], args[2], args[3], args[4], args[5]);
          case 7:
            return new Ctor(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        }
        var thisBinding = baseCreate(Ctor.prototype),
            result = Ctor.apply(thisBinding, args);

        // Mimic the constructor's `return` behavior.
        // See https://es5.github.io/#x13.2.2 for more details.
        return isObject(result) ? result : thisBinding;
      };
    }

    /**
     * Creates a function that wraps `func` to invoke it with optional `this`
     * binding of `thisArg`, partial application, and currying.
     *
     * @private
     * @param {Function|string} func The function or method name to wrap.
     * @param {number} bitmask The bitmask flags. See `createWrap` for more details.
     * @param {*} [thisArg] The `this` binding of `func`.
     * @param {Array} [partials] The arguments to prepend to those provided to
     *  the new function.
     * @param {Array} [holders] The `partials` placeholder indexes.
     * @param {Array} [partialsRight] The arguments to append to those provided
     *  to the new function.
     * @param {Array} [holdersRight] The `partialsRight` placeholder indexes.
     * @param {Array} [argPos] The argument positions of the new function.
     * @param {number} [ary] The arity cap of `func`.
     * @param {number} [arity] The arity of `func`.
     * @returns {Function} Returns the new wrapped function.
     */
    function createHybrid(func, bitmask, thisArg, partials, holders, partialsRight, holdersRight, argPos, ary, arity) {
      var isAry = bitmask & WRAP_ARY_FLAG,
          isBind = bitmask & WRAP_BIND_FLAG,
          isBindKey = bitmask & WRAP_BIND_KEY_FLAG,
          isCurried = bitmask & (WRAP_CURRY_FLAG | WRAP_CURRY_RIGHT_FLAG),
          isFlip = bitmask & WRAP_FLIP_FLAG,
          Ctor = isBindKey ? undefined : createCtor(func);

      function wrapper() {
        var length = arguments.length,
            args = Array(length),
            index = length;

        while (index--) {
          args[index] = arguments[index];
        }
        if (isCurried) {
          var placeholder = getHolder(wrapper),
              holdersCount = countHolders(args, placeholder);
        }
        if (partials) {
          args = composeArgs(args, partials, holders, isCurried);
        }
        if (partialsRight) {
          args = composeArgsRight(args, partialsRight, holdersRight, isCurried);
        }
        length -= holdersCount;
        if (isCurried && length < arity) {
          var newHolders = replaceHolders(args, placeholder);
          return createRecurry(func, bitmask, createHybrid, wrapper.placeholder, thisArg, args, newHolders, argPos, ary, arity - length);
        }
        var thisBinding = isBind ? thisArg : this,
            fn = isBindKey ? thisBinding[func] : func;

        length = args.length;
        if (argPos) {
          args = reorder(args, argPos);
        } else if (isFlip && length > 1) {
          args.reverse();
        }
        if (isAry && ary < length) {
          args.length = ary;
        }
        if (this && this !== root && this instanceof wrapper) {
          fn = Ctor || createCtor(fn);
        }
        return fn.apply(thisBinding, args);
      }
      return wrapper;
    }

    /**
     * Creates a function that wraps `func` to continue currying.
     *
     * @private
     * @param {Function} func The function to wrap.
     * @param {number} bitmask The bitmask flags. See `createWrap` for more details.
     * @param {Function} wrapFunc The function to create the `func` wrapper.
     * @param {*} placeholder The placeholder value.
     * @param {*} [thisArg] The `this` binding of `func`.
     * @param {Array} [partials] The arguments to prepend to those provided to
     *  the new function.
     * @param {Array} [holders] The `partials` placeholder indexes.
     * @param {Array} [argPos] The argument positions of the new function.
     * @param {number} [ary] The arity cap of `func`.
     * @param {number} [arity] The arity of `func`.
     * @returns {Function} Returns the new wrapped function.
     */
    function createRecurry(func, bitmask, wrapFunc, placeholder, thisArg, partials, holders, argPos, ary, arity) {
      var isCurry = bitmask & WRAP_CURRY_FLAG,
          newHolders = isCurry ? holders : undefined,
          newHoldersRight = isCurry ? undefined : holders,
          newPartials = isCurry ? partials : undefined,
          newPartialsRight = isCurry ? undefined : partials;

      bitmask |= isCurry ? WRAP_PARTIAL_FLAG : WRAP_PARTIAL_RIGHT_FLAG;
      bitmask &= ~(isCurry ? WRAP_PARTIAL_RIGHT_FLAG : WRAP_PARTIAL_FLAG);

      if (!(bitmask & WRAP_CURRY_BOUND_FLAG)) {
        bitmask &= ~(WRAP_BIND_FLAG | WRAP_BIND_KEY_FLAG);
      }
      var newData = [func, bitmask, thisArg, newPartials, newHolders, newPartialsRight, newHoldersRight, argPos, ary, arity];

      var result = wrapFunc.apply(undefined, newData);
      if (isLaziable(func)) {
        setData(result, newData);
      }
      result.placeholder = placeholder;
      return setWrapToString(result, func, bitmask);
    }

    /**
     * A specialized version of `baseIsEqualDeep` for arrays with support for
     * partial deep comparisons.
     *
     * @private
     * @param {Array} array The array to compare.
     * @param {Array} other The other array to compare.
     * @param {number} bitmask The bitmask flags. See `baseIsEqual` for more details.
     * @param {Function} customizer The function to customize comparisons.
     * @param {Function} equalFunc The function to determine equivalents of values.
     * @param {Object} stack Tracks traversed `array` and `other` objects.
     * @returns {boolean} Returns `true` if the arrays are equivalent, else `false`.
     */
    function equalArrays(array, other, bitmask, customizer, equalFunc, stack) {
      var isPartial = bitmask & COMPARE_PARTIAL_FLAG,
          arrLength = array.length,
          othLength = other.length;

      if (arrLength != othLength && !(isPartial && othLength > arrLength)) {
        return false;
      }
      // Assume cyclic values are equal.
      var stacked = stack.get(array);
      if (stacked && stack.get(other)) {
        return stacked == other;
      }
      var index = -1,
          result = true,
          seen = bitmask & COMPARE_UNORDERED_FLAG ? new SetCache() : undefined;

      stack.set(array, other);
      stack.set(other, array);

      // Ignore non-index properties.
      while (++index < arrLength) {
        var arrValue = array[index],
            othValue = other[index];

        if (customizer) {
          var compared = isPartial ? customizer(othValue, arrValue, index, other, array, stack) : customizer(arrValue, othValue, index, array, other, stack);
        }
        if (compared !== undefined) {
          if (compared) {
            continue;
          }
          result = false;
          break;
        }
        // Recursively compare arrays (susceptible to call stack limits).
        if (seen) {
          if (!arraySome(other, function (othValue, othIndex) {
            if (!cacheHas(seen, othIndex) && (arrValue === othValue || equalFunc(arrValue, othValue, bitmask, customizer, stack))) {
              return seen.push(othIndex);
            }
          })) {
            result = false;
            break;
          }
        } else if (!(arrValue === othValue || equalFunc(arrValue, othValue, bitmask, customizer, stack))) {
          result = false;
          break;
        }
      }
      stack['delete'](array);
      stack['delete'](other);
      return result;
    }

    /**
     * A specialized version of `baseIsEqualDeep` for comparing objects of
     * the same `toStringTag`.
     *
     * **Note:** This function only supports comparing values with tags of
     * `Boolean`, `Date`, `Error`, `Number`, `RegExp`, or `String`.
     *
     * @private
     * @param {Object} object The object to compare.
     * @param {Object} other The other object to compare.
     * @param {string} tag The `toStringTag` of the objects to compare.
     * @param {number} bitmask The bitmask flags. See `baseIsEqual` for more details.
     * @param {Function} customizer The function to customize comparisons.
     * @param {Function} equalFunc The function to determine equivalents of values.
     * @param {Object} stack Tracks traversed `object` and `other` objects.
     * @returns {boolean} Returns `true` if the objects are equivalent, else `false`.
     */
    function equalByTag(object, other, tag, bitmask, customizer, equalFunc, stack) {
      switch (tag) {
        case dataViewTag:
          if (object.byteLength != other.byteLength || object.byteOffset != other.byteOffset) {
            return false;
          }
          object = object.buffer;
          other = other.buffer;

        case arrayBufferTag:
          if (object.byteLength != other.byteLength || !equalFunc(new Uint8Array(object), new Uint8Array(other))) {
            return false;
          }
          return true;

        case boolTag:
        case dateTag:
        case numberTag:
          // Coerce booleans to `1` or `0` and dates to milliseconds.
          // Invalid dates are coerced to `NaN`.
          return eq(+object, +other);

        case errorTag:
          return object.name == other.name && object.message == other.message;

        case regexpTag:
        case stringTag:
          // Coerce regexes to strings and treat strings, primitives and objects,
          // as equal. See http://www.ecma-international.org/ecma-262/7.0/#sec-regexp.prototype.tostring
          // for more details.
          return object == other + '';

        case mapTag:
          var convert = mapToArray;

        case setTag:
          var isPartial = bitmask & COMPARE_PARTIAL_FLAG;
          convert || (convert = setToArray);

          if (object.size != other.size && !isPartial) {
            return false;
          }
          // Assume cyclic values are equal.
          var stacked = stack.get(object);
          if (stacked) {
            return stacked == other;
          }
          bitmask |= COMPARE_UNORDERED_FLAG;

          // Recursively compare objects (susceptible to call stack limits).
          stack.set(object, other);
          var result = equalArrays(convert(object), convert(other), bitmask, customizer, equalFunc, stack);
          stack['delete'](object);
          return result;

        case symbolTag:
          if (symbolValueOf) {
            return symbolValueOf.call(object) == symbolValueOf.call(other);
          }
      }
      return false;
    }

    /**
     * A specialized version of `baseIsEqualDeep` for objects with support for
     * partial deep comparisons.
     *
     * @private
     * @param {Object} object The object to compare.
     * @param {Object} other The other object to compare.
     * @param {number} bitmask The bitmask flags. See `baseIsEqual` for more details.
     * @param {Function} customizer The function to customize comparisons.
     * @param {Function} equalFunc The function to determine equivalents of values.
     * @param {Object} stack Tracks traversed `object` and `other` objects.
     * @returns {boolean} Returns `true` if the objects are equivalent, else `false`.
     */
    function equalObjects(object, other, bitmask, customizer, equalFunc, stack) {
      var isPartial = bitmask & COMPARE_PARTIAL_FLAG,
          objProps = getAllKeys(object),
          objLength = objProps.length,
          othProps = getAllKeys(other),
          othLength = othProps.length;

      if (objLength != othLength && !isPartial) {
        return false;
      }
      var index = objLength;
      while (index--) {
        var key = objProps[index];
        if (!(isPartial ? key in other : hasOwnProperty.call(other, key))) {
          return false;
        }
      }
      // Assume cyclic values are equal.
      var stacked = stack.get(object);
      if (stacked && stack.get(other)) {
        return stacked == other;
      }
      var result = true;
      stack.set(object, other);
      stack.set(other, object);

      var skipCtor = isPartial;
      while (++index < objLength) {
        key = objProps[index];
        var objValue = object[key],
            othValue = other[key];

        if (customizer) {
          var compared = isPartial ? customizer(othValue, objValue, key, other, object, stack) : customizer(objValue, othValue, key, object, other, stack);
        }
        // Recursively compare objects (susceptible to call stack limits).
        if (!(compared === undefined ? objValue === othValue || equalFunc(objValue, othValue, bitmask, customizer, stack) : compared)) {
          result = false;
          break;
        }
        skipCtor || (skipCtor = key == 'constructor');
      }
      if (result && !skipCtor) {
        var objCtor = object.constructor,
            othCtor = other.constructor;

        // Non `Object` object instances with different constructors are not equal.
        if (objCtor != othCtor && 'constructor' in object && 'constructor' in other && !(typeof objCtor == 'function' && objCtor instanceof objCtor && typeof othCtor == 'function' && othCtor instanceof othCtor)) {
          result = false;
        }
      }
      stack['delete'](object);
      stack['delete'](other);
      return result;
    }

    /**
     * A specialized version of `baseRest` which flattens the rest array.
     *
     * @private
     * @param {Function} func The function to apply a rest parameter to.
     * @returns {Function} Returns the new function.
     */
    function flatRest(func) {
      return setToString(overRest(func, undefined, flatten), func + '');
    }

    /**
     * Creates an array of own enumerable property names and symbols of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names and symbols.
     */
    function getAllKeys(object) {
      return baseGetAllKeys(object, keys, getSymbols);
    }

    /**
     * Creates an array of own and inherited enumerable property names and
     * symbols of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names and symbols.
     */
    function getAllKeysIn(object) {
      return baseGetAllKeys(object, keysIn, getSymbolsIn);
    }

    /**
     * Gets metadata for `func`.
     *
     * @private
     * @param {Function} func The function to query.
     * @returns {*} Returns the metadata for `func`.
     */
    var getData = !metaMap ? noop : function (func) {
      return metaMap.get(func);
    };

    /**
     * Gets the name of `func`.
     *
     * @private
     * @param {Function} func The function to query.
     * @returns {string} Returns the function name.
     */
    function getFuncName(func) {
      var result = func.name + '',
          array = realNames[result],
          length = hasOwnProperty.call(realNames, result) ? array.length : 0;

      while (length--) {
        var data = array[length],
            otherFunc = data.func;
        if (otherFunc == null || otherFunc == func) {
          return data.name;
        }
      }
      return result;
    }

    /**
     * Gets the argument placeholder value for `func`.
     *
     * @private
     * @param {Function} func The function to inspect.
     * @returns {*} Returns the placeholder value.
     */
    function getHolder(func) {
      var object = hasOwnProperty.call(lodash, 'placeholder') ? lodash : func;
      return object.placeholder;
    }

    /**
     * Gets the appropriate "iteratee" function. If `_.iteratee` is customized,
     * this function returns the custom method, otherwise it returns `baseIteratee`.
     * If arguments are provided, the chosen function is invoked with them and
     * its result is returned.
     *
     * @private
     * @param {*} [value] The value to convert to an iteratee.
     * @param {number} [arity] The arity of the created iteratee.
     * @returns {Function} Returns the chosen function or its result.
     */
    function getIteratee() {
      var result = lodash.iteratee || iteratee;
      result = result === iteratee ? baseIteratee : result;
      return arguments.length ? result(arguments[0], arguments[1]) : result;
    }

    /**
     * Gets the data for `map`.
     *
     * @private
     * @param {Object} map The map to query.
     * @param {string} key The reference key.
     * @returns {*} Returns the map data.
     */
    function getMapData(map, key) {
      var data = map.__data__;
      return isKeyable(key) ? data[typeof key == 'string' ? 'string' : 'hash'] : data.map;
    }

    /**
     * Gets the property names, values, and compare flags of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the match data of `object`.
     */
    function getMatchData(object) {
      var result = keys(object),
          length = result.length;

      while (length--) {
        var key = result[length],
            value = object[key];

        result[length] = [key, value, isStrictComparable(value)];
      }
      return result;
    }

    /**
     * Gets the native function at `key` of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {string} key The key of the method to get.
     * @returns {*} Returns the function if it's native, else `undefined`.
     */
    function getNative(object, key) {
      var value = getValue(object, key);
      return baseIsNative(value) ? value : undefined;
    }

    /**
     * A specialized version of `baseGetTag` which ignores `Symbol.toStringTag` values.
     *
     * @private
     * @param {*} value The value to query.
     * @returns {string} Returns the raw `toStringTag`.
     */
    function getRawTag(value) {
      var isOwn = hasOwnProperty.call(value, symToStringTag),
          tag = value[symToStringTag];

      try {
        value[symToStringTag] = undefined;
        var unmasked = true;
      } catch (e) {}

      var result = nativeObjectToString.call(value);
      if (unmasked) {
        if (isOwn) {
          value[symToStringTag] = tag;
        } else {
          delete value[symToStringTag];
        }
      }
      return result;
    }

    /**
     * Creates an array of the own enumerable symbols of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of symbols.
     */
    var getSymbols = !nativeGetSymbols ? stubArray : function (object) {
      if (object == null) {
        return [];
      }
      object = Object(object);
      return arrayFilter(nativeGetSymbols(object), function (symbol) {
        return propertyIsEnumerable.call(object, symbol);
      });
    };

    /**
     * Creates an array of the own and inherited enumerable symbols of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of symbols.
     */
    var getSymbolsIn = !nativeGetSymbols ? stubArray : function (object) {
      var result = [];
      while (object) {
        arrayPush(result, getSymbols(object));
        object = getPrototype(object);
      }
      return result;
    };

    /**
     * Gets the `toStringTag` of `value`.
     *
     * @private
     * @param {*} value The value to query.
     * @returns {string} Returns the `toStringTag`.
     */
    var getTag = baseGetTag;

    // Fallback for data views, maps, sets, and weak maps in IE 11 and promises in Node.js < 6.
    if (DataView && getTag(new DataView(new ArrayBuffer(1))) != dataViewTag || Map && getTag(new Map()) != mapTag || Promise && getTag(Promise.resolve()) != promiseTag || Set && getTag(new Set()) != setTag || WeakMap && getTag(new WeakMap()) != weakMapTag) {
      getTag = function getTag(value) {
        var result = baseGetTag(value),
            Ctor = result == objectTag ? value.constructor : undefined,
            ctorString = Ctor ? toSource(Ctor) : '';

        if (ctorString) {
          switch (ctorString) {
            case dataViewCtorString:
              return dataViewTag;
            case mapCtorString:
              return mapTag;
            case promiseCtorString:
              return promiseTag;
            case setCtorString:
              return setTag;
            case weakMapCtorString:
              return weakMapTag;
          }
        }
        return result;
      };
    }

    /**
     * Gets the view, applying any `transforms` to the `start` and `end` positions.
     *
     * @private
     * @param {number} start The start of the view.
     * @param {number} end The end of the view.
     * @param {Array} transforms The transformations to apply to the view.
     * @returns {Object} Returns an object containing the `start` and `end`
     *  positions of the view.
     */
    function getView(start, end, transforms) {
      var index = -1,
          length = transforms.length;

      while (++index < length) {
        var data = transforms[index],
            size = data.size;

        switch (data.type) {
          case 'drop':
            start += size;break;
          case 'dropRight':
            end -= size;break;
          case 'take':
            end = nativeMin(end, start + size);break;
          case 'takeRight':
            start = nativeMax(start, end - size);break;
        }
      }
      return { 'start': start, 'end': end };
    }

    /**
     * Extracts wrapper details from the `source` body comment.
     *
     * @private
     * @param {string} source The source to inspect.
     * @returns {Array} Returns the wrapper details.
     */
    function getWrapDetails(source) {
      var match = source.match(reWrapDetails);
      return match ? match[1].split(reSplitDetails) : [];
    }

    /**
     * Checks if `path` exists on `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Array|string} path The path to check.
     * @param {Function} hasFunc The function to check properties.
     * @returns {boolean} Returns `true` if `path` exists, else `false`.
     */
    function hasPath(object, path, hasFunc) {
      path = castPath(path, object);

      var index = -1,
          length = path.length,
          result = false;

      while (++index < length) {
        var key = toKey(path[index]);
        if (!(result = object != null && hasFunc(object, key))) {
          break;
        }
        object = object[key];
      }
      if (result || ++index != length) {
        return result;
      }
      length = object == null ? 0 : object.length;
      return !!length && isLength(length) && isIndex(key, length) && (isArray(object) || isArguments(object));
    }

    /**
     * Initializes an array clone.
     *
     * @private
     * @param {Array} array The array to clone.
     * @returns {Array} Returns the initialized clone.
     */
    function initCloneArray(array) {
      var length = array.length,
          result = array.constructor(length);

      // Add properties assigned by `RegExp#exec`.
      if (length && typeof array[0] == 'string' && hasOwnProperty.call(array, 'index')) {
        result.index = array.index;
        result.input = array.input;
      }
      return result;
    }

    /**
     * Initializes an object clone.
     *
     * @private
     * @param {Object} object The object to clone.
     * @returns {Object} Returns the initialized clone.
     */
    function initCloneObject(object) {
      return typeof object.constructor == 'function' && !isPrototype(object) ? baseCreate(getPrototype(object)) : {};
    }

    /**
     * Initializes an object clone based on its `toStringTag`.
     *
     * **Note:** This function only supports cloning values with tags of
     * `Boolean`, `Date`, `Error`, `Number`, `RegExp`, or `String`.
     *
     * @private
     * @param {Object} object The object to clone.
     * @param {string} tag The `toStringTag` of the object to clone.
     * @param {Function} cloneFunc The function to clone values.
     * @param {boolean} [isDeep] Specify a deep clone.
     * @returns {Object} Returns the initialized clone.
     */
    function initCloneByTag(object, tag, cloneFunc, isDeep) {
      var Ctor = object.constructor;
      switch (tag) {
        case arrayBufferTag:
          return cloneArrayBuffer(object);

        case boolTag:
        case dateTag:
          return new Ctor(+object);

        case dataViewTag:
          return cloneDataView(object, isDeep);

        case float32Tag:case float64Tag:
        case int8Tag:case int16Tag:case int32Tag:
        case uint8Tag:case uint8ClampedTag:case uint16Tag:case uint32Tag:
          return cloneTypedArray(object, isDeep);

        case mapTag:
          return cloneMap(object, isDeep, cloneFunc);

        case numberTag:
        case stringTag:
          return new Ctor(object);

        case regexpTag:
          return cloneRegExp(object);

        case setTag:
          return cloneSet(object, isDeep, cloneFunc);

        case symbolTag:
          return cloneSymbol(object);
      }
    }

    /**
     * Inserts wrapper `details` in a comment at the top of the `source` body.
     *
     * @private
     * @param {string} source The source to modify.
     * @returns {Array} details The details to insert.
     * @returns {string} Returns the modified source.
     */
    function insertWrapDetails(source, details) {
      var length = details.length;
      if (!length) {
        return source;
      }
      var lastIndex = length - 1;
      details[lastIndex] = (length > 1 ? '& ' : '') + details[lastIndex];
      details = details.join(length > 2 ? ', ' : ' ');
      return source.replace(reWrapComment, '{\n/* [wrapped with ' + details + '] */\n');
    }

    /**
     * Checks if `value` is a flattenable `arguments` object or array.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is flattenable, else `false`.
     */
    function isFlattenable(value) {
      return isArray(value) || isArguments(value) || !!(spreadableSymbol && value && value[spreadableSymbol]);
    }

    /**
     * Checks if `value` is a valid array-like index.
     *
     * @private
     * @param {*} value The value to check.
     * @param {number} [length=MAX_SAFE_INTEGER] The upper bounds of a valid index.
     * @returns {boolean} Returns `true` if `value` is a valid index, else `false`.
     */
    function isIndex(value, length) {
      length = length == null ? MAX_SAFE_INTEGER : length;
      return !!length && (typeof value == 'number' || reIsUint.test(value)) && value > -1 && value % 1 == 0 && value < length;
    }

    /**
     * Checks if the given arguments are from an iteratee call.
     *
     * @private
     * @param {*} value The potential iteratee value argument.
     * @param {*} index The potential iteratee index or key argument.
     * @param {*} object The potential iteratee object argument.
     * @returns {boolean} Returns `true` if the arguments are from an iteratee call,
     *  else `false`.
     */
    function isIterateeCall(value, index, object) {
      if (!isObject(object)) {
        return false;
      }
      var type = typeof index === 'undefined' ? 'undefined' : _typeof(index);
      if (type == 'number' ? isArrayLike(object) && isIndex(index, object.length) : type == 'string' && index in object) {
        return eq(object[index], value);
      }
      return false;
    }

    /**
     * Checks if `value` is a property name and not a property path.
     *
     * @private
     * @param {*} value The value to check.
     * @param {Object} [object] The object to query keys on.
     * @returns {boolean} Returns `true` if `value` is a property name, else `false`.
     */
    function isKey(value, object) {
      if (isArray(value)) {
        return false;
      }
      var type = typeof value === 'undefined' ? 'undefined' : _typeof(value);
      if (type == 'number' || type == 'symbol' || type == 'boolean' || value == null || isSymbol(value)) {
        return true;
      }
      return reIsPlainProp.test(value) || !reIsDeepProp.test(value) || object != null && value in Object(object);
    }

    /**
     * Checks if `value` is suitable for use as unique object key.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is suitable, else `false`.
     */
    function isKeyable(value) {
      var type = typeof value === 'undefined' ? 'undefined' : _typeof(value);
      return type == 'string' || type == 'number' || type == 'symbol' || type == 'boolean' ? value !== '__proto__' : value === null;
    }

    /**
     * Checks if `func` has a lazy counterpart.
     *
     * @private
     * @param {Function} func The function to check.
     * @returns {boolean} Returns `true` if `func` has a lazy counterpart,
     *  else `false`.
     */
    function isLaziable(func) {
      var funcName = getFuncName(func),
          other = lodash[funcName];

      if (typeof other != 'function' || !(funcName in LazyWrapper.prototype)) {
        return false;
      }
      if (func === other) {
        return true;
      }
      var data = getData(other);
      return !!data && func === data[0];
    }

    /**
     * Checks if `func` has its source masked.
     *
     * @private
     * @param {Function} func The function to check.
     * @returns {boolean} Returns `true` if `func` is masked, else `false`.
     */
    function isMasked(func) {
      return !!maskSrcKey && maskSrcKey in func;
    }

    /**
     * Checks if `value` is likely a prototype object.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a prototype, else `false`.
     */
    function isPrototype(value) {
      var Ctor = value && value.constructor,
          proto = typeof Ctor == 'function' && Ctor.prototype || objectProto;

      return value === proto;
    }

    /**
     * Checks if `value` is suitable for strict equality comparisons, i.e. `===`.
     *
     * @private
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` if suitable for strict
     *  equality comparisons, else `false`.
     */
    function isStrictComparable(value) {
      return value === value && !isObject(value);
    }

    /**
     * A specialized version of `matchesProperty` for source values suitable
     * for strict equality comparisons, i.e. `===`.
     *
     * @private
     * @param {string} key The key of the property to get.
     * @param {*} srcValue The value to match.
     * @returns {Function} Returns the new spec function.
     */
    function matchesStrictComparable(key, srcValue) {
      return function (object) {
        if (object == null) {
          return false;
        }
        return object[key] === srcValue && (srcValue !== undefined || key in Object(object));
      };
    }

    /**
     * A specialized version of `_.memoize` which clears the memoized function's
     * cache when it exceeds `MAX_MEMOIZE_SIZE`.
     *
     * @private
     * @param {Function} func The function to have its output memoized.
     * @returns {Function} Returns the new memoized function.
     */
    function memoizeCapped(func) {
      var result = memoize(func, function (key) {
        if (cache.size === MAX_MEMOIZE_SIZE) {
          cache.clear();
        }
        return key;
      });

      var cache = result.cache;
      return result;
    }

    /**
     * This function is like
     * [`Object.keys`](http://ecma-international.org/ecma-262/7.0/#sec-object.keys)
     * except that it includes inherited enumerable properties.
     *
     * @private
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names.
     */
    function nativeKeysIn(object) {
      var result = [];
      if (object != null) {
        for (var key in Object(object)) {
          result.push(key);
        }
      }
      return result;
    }

    /**
     * Converts `value` to a string using `Object.prototype.toString`.
     *
     * @private
     * @param {*} value The value to convert.
     * @returns {string} Returns the converted string.
     */
    function objectToString(value) {
      return nativeObjectToString.call(value);
    }

    /**
     * A specialized version of `baseRest` which transforms the rest array.
     *
     * @private
     * @param {Function} func The function to apply a rest parameter to.
     * @param {number} [start=func.length-1] The start position of the rest parameter.
     * @param {Function} transform The rest array transform.
     * @returns {Function} Returns the new function.
     */
    function overRest(func, start, transform) {
      start = nativeMax(start === undefined ? func.length - 1 : start, 0);
      return function () {
        var args = arguments,
            index = -1,
            length = nativeMax(args.length - start, 0),
            array = Array(length);

        while (++index < length) {
          array[index] = args[start + index];
        }
        index = -1;
        var otherArgs = Array(start + 1);
        while (++index < start) {
          otherArgs[index] = args[index];
        }
        otherArgs[start] = transform(array);
        return apply(func, this, otherArgs);
      };
    }

    /**
     * Gets the parent value at `path` of `object`.
     *
     * @private
     * @param {Object} object The object to query.
     * @param {Array} path The path to get the parent value of.
     * @returns {*} Returns the parent value.
     */
    function parent(object, path) {
      return path.length < 2 ? object : baseGet(object, baseSlice(path, 0, -1));
    }

    /**
     * Reorder `array` according to the specified indexes where the element at
     * the first index is assigned as the first element, the element at
     * the second index is assigned as the second element, and so on.
     *
     * @private
     * @param {Array} array The array to reorder.
     * @param {Array} indexes The arranged array indexes.
     * @returns {Array} Returns `array`.
     */
    function reorder(array, indexes) {
      var arrLength = array.length,
          length = nativeMin(indexes.length, arrLength),
          oldArray = copyArray(array);

      while (length--) {
        var index = indexes[length];
        array[length] = isIndex(index, arrLength) ? oldArray[index] : undefined;
      }
      return array;
    }

    /**
     * Sets metadata for `func`.
     *
     * **Note:** If this function becomes hot, i.e. is invoked a lot in a short
     * period of time, it will trip its breaker and transition to an identity
     * function to avoid garbage collection pauses in V8. See
     * [V8 issue 2070](https://bugs.chromium.org/p/v8/issues/detail?id=2070)
     * for more details.
     *
     * @private
     * @param {Function} func The function to associate metadata with.
     * @param {*} data The metadata.
     * @returns {Function} Returns `func`.
     */
    var setData = shortOut(baseSetData);

    /**
     * Sets the `toString` method of `func` to return `string`.
     *
     * @private
     * @param {Function} func The function to modify.
     * @param {Function} string The `toString` result.
     * @returns {Function} Returns `func`.
     */
    var setToString = shortOut(baseSetToString);

    /**
     * Sets the `toString` method of `wrapper` to mimic the source of `reference`
     * with wrapper details in a comment at the top of the source body.
     *
     * @private
     * @param {Function} wrapper The function to modify.
     * @param {Function} reference The reference function.
     * @param {number} bitmask The bitmask flags. See `createWrap` for more details.
     * @returns {Function} Returns `wrapper`.
     */
    function setWrapToString(wrapper, reference, bitmask) {
      var source = reference + '';
      return setToString(wrapper, insertWrapDetails(source, updateWrapDetails(getWrapDetails(source), bitmask)));
    }

    /**
     * Creates a function that'll short out and invoke `identity` instead
     * of `func` when it's called `HOT_COUNT` or more times in `HOT_SPAN`
     * milliseconds.
     *
     * @private
     * @param {Function} func The function to restrict.
     * @returns {Function} Returns the new shortable function.
     */
    function shortOut(func) {
      var count = 0,
          lastCalled = 0;

      return function () {
        var stamp = nativeNow(),
            remaining = HOT_SPAN - (stamp - lastCalled);

        lastCalled = stamp;
        if (remaining > 0) {
          if (++count >= HOT_COUNT) {
            return arguments[0];
          }
        } else {
          count = 0;
        }
        return func.apply(undefined, arguments);
      };
    }

    /**
     * Converts `string` to a property path array.
     *
     * @private
     * @param {string} string The string to convert.
     * @returns {Array} Returns the property path array.
     */
    var stringToPath = memoizeCapped(function (string) {
      var result = [];
      if (reLeadingDot.test(string)) {
        result.push('');
      }
      string.replace(rePropName, function (match, number, quote, string) {
        result.push(quote ? string.replace(reEscapeChar, '$1') : number || match);
      });
      return result;
    });

    /**
     * Converts `value` to a string key if it's not a string or symbol.
     *
     * @private
     * @param {*} value The value to inspect.
     * @returns {string|symbol} Returns the key.
     */
    function toKey(value) {
      if (typeof value == 'string' || isSymbol(value)) {
        return value;
      }
      var result = value + '';
      return result == '0' && 1 / value == -INFINITY ? '-0' : result;
    }

    /**
     * Converts `func` to its source code.
     *
     * @private
     * @param {Function} func The function to convert.
     * @returns {string} Returns the source code.
     */
    function toSource(func) {
      if (func != null) {
        try {
          return funcToString.call(func);
        } catch (e) {}
        try {
          return func + '';
        } catch (e) {}
      }
      return '';
    }

    /**
     * Updates wrapper `details` based on `bitmask` flags.
     *
     * @private
     * @returns {Array} details The details to modify.
     * @param {number} bitmask The bitmask flags. See `createWrap` for more details.
     * @returns {Array} Returns `details`.
     */
    function updateWrapDetails(details, bitmask) {
      arrayEach(wrapFlags, function (pair) {
        var value = '_.' + pair[0];
        if (bitmask & pair[1] && !arrayIncludes(details, value)) {
          details.push(value);
        }
      });
      return details.sort();
    }

    /**
     * Creates a clone of `wrapper`.
     *
     * @private
     * @param {Object} wrapper The wrapper to clone.
     * @returns {Object} Returns the cloned wrapper.
     */
    function wrapperClone(wrapper) {
      if (wrapper instanceof LazyWrapper) {
        return wrapper.clone();
      }
      var result = new LodashWrapper(wrapper.__wrapped__, wrapper.__chain__);
      result.__actions__ = copyArray(wrapper.__actions__);
      result.__index__ = wrapper.__index__;
      result.__values__ = wrapper.__values__;
      return result;
    }

    /*------------------------------------------------------------------------*/

    /**
     * Creates a slice of `array` with `n` elements dropped from the beginning.
     *
     * @static
     * @memberOf _
     * @since 0.5.0
     * @category Array
     * @param {Array} array The array to query.
     * @param {number} [n=1] The number of elements to drop.
     * @param- {Object} [guard] Enables use as an iteratee for methods like `_.map`.
     * @returns {Array} Returns the slice of `array`.
     * @example
     *
     * _.drop([1, 2, 3]);
     * // => [2, 3]
     *
     * _.drop([1, 2, 3], 2);
     * // => [3]
     *
     * _.drop([1, 2, 3], 5);
     * // => []
     *
     * _.drop([1, 2, 3], 0);
     * // => [1, 2, 3]
     */
    function drop(array, n, guard) {
      var length = array == null ? 0 : array.length;
      if (!length) {
        return [];
      }
      n = guard || n === undefined ? 1 : toInteger(n);
      return baseSlice(array, n < 0 ? 0 : n, length);
    }

    /**
     * Flattens `array` a single level deep.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Array
     * @param {Array} array The array to flatten.
     * @returns {Array} Returns the new flattened array.
     * @example
     *
     * _.flatten([1, [2, [3, [4]], 5]]);
     * // => [1, 2, [3, [4]], 5]
     */
    function flatten(array) {
      var length = array == null ? 0 : array.length;
      return length ? baseFlatten(array, 1) : [];
    }

    /**
     * Gets the last element of `array`.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Array
     * @param {Array} array The array to query.
     * @returns {*} Returns the last element of `array`.
     * @example
     *
     * _.last([1, 2, 3]);
     * // => 3
     */
    function last(array) {
      var length = array == null ? 0 : array.length;
      return length ? array[length - 1] : undefined;
    }

    /**
     * Reverses `array` so that the first element becomes the last, the second
     * element becomes the second to last, and so on.
     *
     * **Note:** This method mutates `array` and is based on
     * [`Array#reverse`](https://mdn.io/Array/reverse).
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Array
     * @param {Array} array The array to modify.
     * @returns {Array} Returns `array`.
     * @example
     *
     * var array = [1, 2, 3];
     *
     * _.reverse(array);
     * // => [3, 2, 1]
     *
     * console.log(array);
     * // => [3, 2, 1]
     */
    function reverse(array) {
      return array == null ? array : nativeReverse.call(array);
    }

    /**
     * Creates a slice of `array` with `n` elements taken from the beginning.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Array
     * @param {Array} array The array to query.
     * @param {number} [n=1] The number of elements to take.
     * @param- {Object} [guard] Enables use as an iteratee for methods like `_.map`.
     * @returns {Array} Returns the slice of `array`.
     * @example
     *
     * _.take([1, 2, 3]);
     * // => [1]
     *
     * _.take([1, 2, 3], 2);
     * // => [1, 2]
     *
     * _.take([1, 2, 3], 5);
     * // => [1, 2, 3]
     *
     * _.take([1, 2, 3], 0);
     * // => []
     */
    function take(array, n, guard) {
      if (!(array && array.length)) {
        return [];
      }
      n = guard || n === undefined ? 1 : toInteger(n);
      return baseSlice(array, 0, n < 0 ? 0 : n);
    }

    /*------------------------------------------------------------------------*/

    /**
     * Creates a `lodash` wrapper instance that wraps `value` with explicit method
     * chain sequences enabled. The result of such sequences must be unwrapped
     * with `_#value`.
     *
     * @static
     * @memberOf _
     * @since 1.3.0
     * @category Seq
     * @param {*} value The value to wrap.
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * var users = [
     *   { 'user': 'barney',  'age': 36 },
     *   { 'user': 'fred',    'age': 40 },
     *   { 'user': 'pebbles', 'age': 1 }
     * ];
     *
     * var youngest = _
     *   .chain(users)
     *   .sortBy('age')
     *   .map(function(o) {
     *     return o.user + ' is ' + o.age;
     *   })
     *   .head()
     *   .value();
     * // => 'pebbles is 1'
     */
    function chain(value) {
      var result = lodash(value);
      result.__chain__ = true;
      return result;
    }

    /**
     * This method invokes `interceptor` and returns `value`. The interceptor
     * is invoked with one argument; (value). The purpose of this method is to
     * "tap into" a method chain sequence in order to modify intermediate results.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Seq
     * @param {*} value The value to provide to `interceptor`.
     * @param {Function} interceptor The function to invoke.
     * @returns {*} Returns `value`.
     * @example
     *
     * _([1, 2, 3])
     *  .tap(function(array) {
     *    // Mutate input array.
     *    array.pop();
     *  })
     *  .reverse()
     *  .value();
     * // => [2, 1]
     */
    function tap(value, interceptor) {
      interceptor(value);
      return value;
    }

    /**
     * This method is like `_.tap` except that it returns the result of `interceptor`.
     * The purpose of this method is to "pass thru" values replacing intermediate
     * results in a method chain sequence.
     *
     * @static
     * @memberOf _
     * @since 3.0.0
     * @category Seq
     * @param {*} value The value to provide to `interceptor`.
     * @param {Function} interceptor The function to invoke.
     * @returns {*} Returns the result of `interceptor`.
     * @example
     *
     * _('  abc  ')
     *  .chain()
     *  .trim()
     *  .thru(function(value) {
     *    return [value];
     *  })
     *  .value();
     * // => ['abc']
     */
    function thru(value, interceptor) {
      return interceptor(value);
    }

    /**
     * This method is the wrapper version of `_.at`.
     *
     * @name at
     * @memberOf _
     * @since 1.0.0
     * @category Seq
     * @param {...(string|string[])} [paths] The property paths to pick.
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * var object = { 'a': [{ 'b': { 'c': 3 } }, 4] };
     *
     * _(object).at(['a[0].b.c', 'a[1]']).value();
     * // => [3, 4]
     */
    var wrapperAt = flatRest(function (paths) {
      var length = paths.length,
          start = length ? paths[0] : 0,
          value = this.__wrapped__,
          interceptor = function interceptor(object) {
        return baseAt(object, paths);
      };

      if (length > 1 || this.__actions__.length || !(value instanceof LazyWrapper) || !isIndex(start)) {
        return this.thru(interceptor);
      }
      value = value.slice(start, +start + (length ? 1 : 0));
      value.__actions__.push({
        'func': thru,
        'args': [interceptor],
        'thisArg': undefined
      });
      return new LodashWrapper(value, this.__chain__).thru(function (array) {
        if (length && !array.length) {
          array.push(undefined);
        }
        return array;
      });
    });

    /**
     * Creates a `lodash` wrapper instance with explicit method chain sequences enabled.
     *
     * @name chain
     * @memberOf _
     * @since 0.1.0
     * @category Seq
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * var users = [
     *   { 'user': 'barney', 'age': 36 },
     *   { 'user': 'fred',   'age': 40 }
     * ];
     *
     * // A sequence without explicit chaining.
     * _(users).head();
     * // => { 'user': 'barney', 'age': 36 }
     *
     * // A sequence with explicit chaining.
     * _(users)
     *   .chain()
     *   .head()
     *   .pick('user')
     *   .value();
     * // => { 'user': 'barney' }
     */
    function wrapperChain() {
      return chain(this);
    }

    /**
     * Executes the chain sequence and returns the wrapped result.
     *
     * @name commit
     * @memberOf _
     * @since 3.2.0
     * @category Seq
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * var array = [1, 2];
     * var wrapped = _(array).push(3);
     *
     * console.log(array);
     * // => [1, 2]
     *
     * wrapped = wrapped.commit();
     * console.log(array);
     * // => [1, 2, 3]
     *
     * wrapped.last();
     * // => 3
     *
     * console.log(array);
     * // => [1, 2, 3]
     */
    function wrapperCommit() {
      return new LodashWrapper(this.value(), this.__chain__);
    }

    /**
     * Gets the next value on a wrapped object following the
     * [iterator protocol](https://mdn.io/iteration_protocols#iterator).
     *
     * @name next
     * @memberOf _
     * @since 4.0.0
     * @category Seq
     * @returns {Object} Returns the next iterator value.
     * @example
     *
     * var wrapped = _([1, 2]);
     *
     * wrapped.next();
     * // => { 'done': false, 'value': 1 }
     *
     * wrapped.next();
     * // => { 'done': false, 'value': 2 }
     *
     * wrapped.next();
     * // => { 'done': true, 'value': undefined }
     */
    function wrapperNext() {
      if (this.__values__ === undefined) {
        this.__values__ = toArray(this.value());
      }
      var done = this.__index__ >= this.__values__.length,
          value = done ? undefined : this.__values__[this.__index__++];

      return { 'done': done, 'value': value };
    }

    /**
     * Enables the wrapper to be iterable.
     *
     * @name Symbol.iterator
     * @memberOf _
     * @since 4.0.0
     * @category Seq
     * @returns {Object} Returns the wrapper object.
     * @example
     *
     * var wrapped = _([1, 2]);
     *
     * wrapped[Symbol.iterator]() === wrapped;
     * // => true
     *
     * Array.from(wrapped);
     * // => [1, 2]
     */
    function wrapperToIterator() {
      return this;
    }

    /**
     * Creates a clone of the chain sequence planting `value` as the wrapped value.
     *
     * @name plant
     * @memberOf _
     * @since 3.2.0
     * @category Seq
     * @param {*} value The value to plant.
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * function square(n) {
     *   return n * n;
     * }
     *
     * var wrapped = _([1, 2]).map(square);
     * var other = wrapped.plant([3, 4]);
     *
     * other.value();
     * // => [9, 16]
     *
     * wrapped.value();
     * // => [1, 4]
     */
    function wrapperPlant(value) {
      var result,
          parent = this;

      while (parent instanceof baseLodash) {
        var clone = wrapperClone(parent);
        clone.__index__ = 0;
        clone.__values__ = undefined;
        if (result) {
          previous.__wrapped__ = clone;
        } else {
          result = clone;
        }
        var previous = clone;
        parent = parent.__wrapped__;
      }
      previous.__wrapped__ = value;
      return result;
    }

    /**
     * This method is the wrapper version of `_.reverse`.
     *
     * **Note:** This method mutates the wrapped array.
     *
     * @name reverse
     * @memberOf _
     * @since 0.1.0
     * @category Seq
     * @returns {Object} Returns the new `lodash` wrapper instance.
     * @example
     *
     * var array = [1, 2, 3];
     *
     * _(array).reverse().value()
     * // => [3, 2, 1]
     *
     * console.log(array);
     * // => [3, 2, 1]
     */
    function wrapperReverse() {
      var value = this.__wrapped__;
      if (value instanceof LazyWrapper) {
        var wrapped = value;
        if (this.__actions__.length) {
          wrapped = new LazyWrapper(this);
        }
        wrapped = wrapped.reverse();
        wrapped.__actions__.push({
          'func': thru,
          'args': [reverse],
          'thisArg': undefined
        });
        return new LodashWrapper(wrapped, this.__chain__);
      }
      return this.thru(reverse);
    }

    /**
     * Executes the chain sequence to resolve the unwrapped value.
     *
     * @name value
     * @memberOf _
     * @since 0.1.0
     * @alias toJSON, valueOf
     * @category Seq
     * @returns {*} Returns the resolved unwrapped value.
     * @example
     *
     * _([1, 2, 3]).value();
     * // => [1, 2, 3]
     */
    function wrapperValue() {
      return baseWrapperValue(this.__wrapped__, this.__actions__);
    }

    /*------------------------------------------------------------------------*/

    /**
     * Iterates over elements of `collection`, returning an array of all elements
     * `predicate` returns truthy for. The predicate is invoked with three
     * arguments: (value, index|key, collection).
     *
     * **Note:** Unlike `_.remove`, this method returns a new array.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [predicate=_.identity] The function invoked per iteration.
     * @returns {Array} Returns the new filtered array.
     * @see _.reject
     * @example
     *
     * var users = [
     *   { 'user': 'barney', 'age': 36, 'active': true },
     *   { 'user': 'fred',   'age': 40, 'active': false }
     * ];
     *
     * _.filter(users, function(o) { return !o.active; });
     * // => objects for ['fred']
     *
     * // The `_.matches` iteratee shorthand.
     * _.filter(users, { 'age': 36, 'active': true });
     * // => objects for ['barney']
     *
     * // The `_.matchesProperty` iteratee shorthand.
     * _.filter(users, ['active', false]);
     * // => objects for ['fred']
     *
     * // The `_.property` iteratee shorthand.
     * _.filter(users, 'active');
     * // => objects for ['barney']
     */
    function filter(collection, predicate) {
      var func = isArray(collection) ? arrayFilter : baseFilter;
      return func(collection, getIteratee(predicate, 3));
    }

    /**
     * Iterates over elements of `collection` and invokes `iteratee` for each element.
     * The iteratee is invoked with three arguments: (value, index|key, collection).
     * Iteratee functions may exit iteration early by explicitly returning `false`.
     *
     * **Note:** As with other "Collections" methods, objects with a "length"
     * property are iterated like arrays. To avoid this behavior use `_.forIn`
     * or `_.forOwn` for object iteration.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @alias each
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [iteratee=_.identity] The function invoked per iteration.
     * @returns {Array|Object} Returns `collection`.
     * @see _.forEachRight
     * @example
     *
     * _.forEach([1, 2], function(value) {
     *   console.log(value);
     * });
     * // => Logs `1` then `2`.
     *
     * _.forEach({ 'a': 1, 'b': 2 }, function(value, key) {
     *   console.log(key);
     * });
     * // => Logs 'a' then 'b' (iteration order is not guaranteed).
     */
    function forEach(collection, iteratee) {
      var func = isArray(collection) ? arrayEach : baseEach;
      return func(collection, getIteratee(iteratee, 3));
    }

    /**
     * Creates an object composed of keys generated from the results of running
     * each element of `collection` thru `iteratee`. The order of grouped values
     * is determined by the order they occur in `collection`. The corresponding
     * value of each key is an array of elements responsible for generating the
     * key. The iteratee is invoked with one argument: (value).
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [iteratee=_.identity] The iteratee to transform keys.
     * @returns {Object} Returns the composed aggregate object.
     * @example
     *
     * _.groupBy([6.1, 4.2, 6.3], Math.floor);
     * // => { '4': [4.2], '6': [6.1, 6.3] }
     *
     * // The `_.property` iteratee shorthand.
     * _.groupBy(['one', 'two', 'three'], 'length');
     * // => { '3': ['one', 'two'], '5': ['three'] }
     */
    var groupBy = createAggregator(function (result, value, key) {
      if (hasOwnProperty.call(result, key)) {
        result[key].push(value);
      } else {
        baseAssignValue(result, key, [value]);
      }
    });

    /**
     * Creates an array of values by running each element in `collection` thru
     * `iteratee`. The iteratee is invoked with three arguments:
     * (value, index|key, collection).
     *
     * Many lodash methods are guarded to work as iteratees for methods like
     * `_.every`, `_.filter`, `_.map`, `_.mapValues`, `_.reject`, and `_.some`.
     *
     * The guarded methods are:
     * `ary`, `chunk`, `curry`, `curryRight`, `drop`, `dropRight`, `every`,
     * `fill`, `invert`, `parseInt`, `random`, `range`, `rangeRight`, `repeat`,
     * `sampleSize`, `slice`, `some`, `sortBy`, `split`, `take`, `takeRight`,
     * `template`, `trim`, `trimEnd`, `trimStart`, and `words`
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [iteratee=_.identity] The function invoked per iteration.
     * @returns {Array} Returns the new mapped array.
     * @example
     *
     * function square(n) {
     *   return n * n;
     * }
     *
     * _.map([4, 8], square);
     * // => [16, 64]
     *
     * _.map({ 'a': 4, 'b': 8 }, square);
     * // => [16, 64] (iteration order is not guaranteed)
     *
     * var users = [
     *   { 'user': 'barney' },
     *   { 'user': 'fred' }
     * ];
     *
     * // The `_.property` iteratee shorthand.
     * _.map(users, 'user');
     * // => ['barney', 'fred']
     */
    function map(collection, iteratee) {
      var func = isArray(collection) ? arrayMap : baseMap;
      return func(collection, getIteratee(iteratee, 3));
    }

    /**
     * Creates an array of elements split into two groups, the first of which
     * contains elements `predicate` returns truthy for, the second of which
     * contains elements `predicate` returns falsey for. The predicate is
     * invoked with one argument: (value).
     *
     * @static
     * @memberOf _
     * @since 3.0.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [predicate=_.identity] The function invoked per iteration.
     * @returns {Array} Returns the array of grouped elements.
     * @example
     *
     * var users = [
     *   { 'user': 'barney',  'age': 36, 'active': false },
     *   { 'user': 'fred',    'age': 40, 'active': true },
     *   { 'user': 'pebbles', 'age': 1,  'active': false }
     * ];
     *
     * _.partition(users, function(o) { return o.active; });
     * // => objects for [['fred'], ['barney', 'pebbles']]
     *
     * // The `_.matches` iteratee shorthand.
     * _.partition(users, { 'age': 1, 'active': false });
     * // => objects for [['pebbles'], ['barney', 'fred']]
     *
     * // The `_.matchesProperty` iteratee shorthand.
     * _.partition(users, ['active', false]);
     * // => objects for [['barney', 'pebbles'], ['fred']]
     *
     * // The `_.property` iteratee shorthand.
     * _.partition(users, 'active');
     * // => objects for [['fred'], ['barney', 'pebbles']]
     */
    var partition = createAggregator(function (result, value, key) {
      result[key ? 0 : 1].push(value);
    }, function () {
      return [[], []];
    });

    /**
     * Reduces `collection` to a value which is the accumulated result of running
     * each element in `collection` thru `iteratee`, where each successive
     * invocation is supplied the return value of the previous. If `accumulator`
     * is not given, the first element of `collection` is used as the initial
     * value. The iteratee is invoked with four arguments:
     * (accumulator, value, index|key, collection).
     *
     * Many lodash methods are guarded to work as iteratees for methods like
     * `_.reduce`, `_.reduceRight`, and `_.transform`.
     *
     * The guarded methods are:
     * `assign`, `defaults`, `defaultsDeep`, `includes`, `merge`, `orderBy`,
     * and `sortBy`
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [iteratee=_.identity] The function invoked per iteration.
     * @param {*} [accumulator] The initial value.
     * @returns {*} Returns the accumulated value.
     * @see _.reduceRight
     * @example
     *
     * _.reduce([1, 2], function(sum, n) {
     *   return sum + n;
     * }, 0);
     * // => 3
     *
     * _.reduce({ 'a': 1, 'b': 2, 'c': 1 }, function(result, value, key) {
     *   (result[value] || (result[value] = [])).push(key);
     *   return result;
     * }, {});
     * // => { '1': ['a', 'c'], '2': ['b'] } (iteration order is not guaranteed)
     */
    function reduce(collection, iteratee, accumulator) {
      var func = isArray(collection) ? arrayReduce : baseReduce,
          initAccum = arguments.length < 3;

      return func(collection, getIteratee(iteratee, 4), accumulator, initAccum, baseEach);
    }

    /**
     * Checks if `predicate` returns truthy for **any** element of `collection`.
     * Iteration is stopped once `predicate` returns truthy. The predicate is
     * invoked with three arguments: (value, index|key, collection).
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Collection
     * @param {Array|Object} collection The collection to iterate over.
     * @param {Function} [predicate=_.identity] The function invoked per iteration.
     * @param- {Object} [guard] Enables use as an iteratee for methods like `_.map`.
     * @returns {boolean} Returns `true` if any element passes the predicate check,
     *  else `false`.
     * @example
     *
     * _.some([null, 0, 'yes', false], Boolean);
     * // => true
     *
     * var users = [
     *   { 'user': 'barney', 'active': true },
     *   { 'user': 'fred',   'active': false }
     * ];
     *
     * // The `_.matches` iteratee shorthand.
     * _.some(users, { 'user': 'barney', 'active': false });
     * // => false
     *
     * // The `_.matchesProperty` iteratee shorthand.
     * _.some(users, ['active', false]);
     * // => true
     *
     * // The `_.property` iteratee shorthand.
     * _.some(users, 'active');
     * // => true
     */
    function some(collection, predicate, guard) {
      var func = isArray(collection) ? arraySome : baseSome;
      if (guard && isIterateeCall(collection, predicate, guard)) {
        predicate = undefined;
      }
      return func(collection, getIteratee(predicate, 3));
    }

    /*------------------------------------------------------------------------*/

    /**
     * Creates a function that memoizes the result of `func`. If `resolver` is
     * provided, it determines the cache key for storing the result based on the
     * arguments provided to the memoized function. By default, the first argument
     * provided to the memoized function is used as the map cache key. The `func`
     * is invoked with the `this` binding of the memoized function.
     *
     * **Note:** The cache is exposed as the `cache` property on the memoized
     * function. Its creation may be customized by replacing the `_.memoize.Cache`
     * constructor with one whose instances implement the
     * [`Map`](http://ecma-international.org/ecma-262/7.0/#sec-properties-of-the-map-prototype-object)
     * method interface of `clear`, `delete`, `get`, `has`, and `set`.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Function
     * @param {Function} func The function to have its output memoized.
     * @param {Function} [resolver] The function to resolve the cache key.
     * @returns {Function} Returns the new memoized function.
     * @example
     *
     * var object = { 'a': 1, 'b': 2 };
     * var other = { 'c': 3, 'd': 4 };
     *
     * var values = _.memoize(_.values);
     * values(object);
     * // => [1, 2]
     *
     * values(other);
     * // => [3, 4]
     *
     * object.a = 2;
     * values(object);
     * // => [1, 2]
     *
     * // Modify the result cache.
     * values.cache.set(object, ['a', 'b']);
     * values(object);
     * // => ['a', 'b']
     *
     * // Replace `_.memoize.Cache`.
     * _.memoize.Cache = WeakMap;
     */
    function memoize(func, resolver) {
      if (typeof func != 'function' || resolver != null && typeof resolver != 'function') {
        throw new TypeError(FUNC_ERROR_TEXT);
      }
      var memoized = function memoized() {
        var args = arguments,
            key = resolver ? resolver.apply(this, args) : args[0],
            cache = memoized.cache;

        if (cache.has(key)) {
          return cache.get(key);
        }
        var result = func.apply(this, args);
        memoized.cache = cache.set(key, result) || cache;
        return result;
      };
      memoized.cache = new (memoize.Cache || MapCache)();
      return memoized;
    }

    // Expose `MapCache`.
    memoize.Cache = MapCache;

    /**
     * Creates a function that negates the result of the predicate `func`. The
     * `func` predicate is invoked with the `this` binding and arguments of the
     * created function.
     *
     * @static
     * @memberOf _
     * @since 3.0.0
     * @category Function
     * @param {Function} predicate The predicate to negate.
     * @returns {Function} Returns the new negated function.
     * @example
     *
     * function isEven(n) {
     *   return n % 2 == 0;
     * }
     *
     * _.filter([1, 2, 3, 4, 5, 6], _.negate(isEven));
     * // => [1, 3, 5]
     */
    function negate(predicate) {
      if (typeof predicate != 'function') {
        throw new TypeError(FUNC_ERROR_TEXT);
      }
      return function () {
        var args = arguments;
        switch (args.length) {
          case 0:
            return !predicate.call(this);
          case 1:
            return !predicate.call(this, args[0]);
          case 2:
            return !predicate.call(this, args[0], args[1]);
          case 3:
            return !predicate.call(this, args[0], args[1], args[2]);
        }
        return !predicate.apply(this, args);
      };
    }

    /*------------------------------------------------------------------------*/

    /**
     * Performs a
     * [`SameValueZero`](http://ecma-international.org/ecma-262/7.0/#sec-samevaluezero)
     * comparison between two values to determine if they are equivalent.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to compare.
     * @param {*} other The other value to compare.
     * @returns {boolean} Returns `true` if the values are equivalent, else `false`.
     * @example
     *
     * var object = { 'a': 1 };
     * var other = { 'a': 1 };
     *
     * _.eq(object, object);
     * // => true
     *
     * _.eq(object, other);
     * // => false
     *
     * _.eq('a', 'a');
     * // => true
     *
     * _.eq('a', Object('a'));
     * // => false
     *
     * _.eq(NaN, NaN);
     * // => true
     */
    function eq(value, other) {
      return value === other || value !== value && other !== other;
    }

    /**
     * Checks if `value` is likely an `arguments` object.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is an `arguments` object,
     *  else `false`.
     * @example
     *
     * _.isArguments(function() { return arguments; }());
     * // => true
     *
     * _.isArguments([1, 2, 3]);
     * // => false
     */
    var isArguments = baseIsArguments(function () {
      return arguments;
    }()) ? baseIsArguments : function (value) {
      return isObjectLike(value) && hasOwnProperty.call(value, 'callee') && !propertyIsEnumerable.call(value, 'callee');
    };

    /**
     * Checks if `value` is classified as an `Array` object.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is an array, else `false`.
     * @example
     *
     * _.isArray([1, 2, 3]);
     * // => true
     *
     * _.isArray(document.body.children);
     * // => false
     *
     * _.isArray('abc');
     * // => false
     *
     * _.isArray(_.noop);
     * // => false
     */
    var isArray = Array.isArray;

    /**
     * Checks if `value` is array-like. A value is considered array-like if it's
     * not a function and has a `value.length` that's an integer greater than or
     * equal to `0` and less than or equal to `Number.MAX_SAFE_INTEGER`.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is array-like, else `false`.
     * @example
     *
     * _.isArrayLike([1, 2, 3]);
     * // => true
     *
     * _.isArrayLike(document.body.children);
     * // => true
     *
     * _.isArrayLike('abc');
     * // => true
     *
     * _.isArrayLike(_.noop);
     * // => false
     */
    function isArrayLike(value) {
      return value != null && isLength(value.length) && !isFunction(value);
    }

    /**
     * Checks if `value` is a buffer.
     *
     * @static
     * @memberOf _
     * @since 4.3.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a buffer, else `false`.
     * @example
     *
     * _.isBuffer(new Buffer(2));
     * // => true
     *
     * _.isBuffer(new Uint8Array(2));
     * // => false
     */
    var isBuffer = nativeIsBuffer || stubFalse;

    /**
     * Checks if `value` is classified as a `Function` object.
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a function, else `false`.
     * @example
     *
     * _.isFunction(_);
     * // => true
     *
     * _.isFunction(/abc/);
     * // => false
     */
    function isFunction(value) {
      if (!isObject(value)) {
        return false;
      }
      // The use of `Object#toString` avoids issues with the `typeof` operator
      // in Safari 9 which returns 'object' for typed arrays and other constructors.
      var tag = baseGetTag(value);
      return tag == funcTag || tag == genTag || tag == asyncTag || tag == proxyTag;
    }

    /**
     * Checks if `value` is a valid array-like length.
     *
     * **Note:** This method is loosely based on
     * [`ToLength`](http://ecma-international.org/ecma-262/7.0/#sec-tolength).
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a valid length, else `false`.
     * @example
     *
     * _.isLength(3);
     * // => true
     *
     * _.isLength(Number.MIN_VALUE);
     * // => false
     *
     * _.isLength(Infinity);
     * // => false
     *
     * _.isLength('3');
     * // => false
     */
    function isLength(value) {
      return typeof value == 'number' && value > -1 && value % 1 == 0 && value <= MAX_SAFE_INTEGER;
    }

    /**
     * Checks if `value` is the
     * [language type](http://www.ecma-international.org/ecma-262/7.0/#sec-ecmascript-language-types)
     * of `Object`. (e.g. arrays, functions, objects, regexes, `new Number(0)`, and `new String('')`)
     *
     * @static
     * @memberOf _
     * @since 0.1.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is an object, else `false`.
     * @example
     *
     * _.isObject({});
     * // => true
     *
     * _.isObject([1, 2, 3]);
     * // => true
     *
     * _.isObject(_.noop);
     * // => true
     *
     * _.isObject(null);
     * // => false
     */
    function isObject(value) {
      var type = typeof value === 'undefined' ? 'undefined' : _typeof(value);
      return value != null && (type == 'object' || type == 'function');
    }

    /**
     * Checks if `value` is object-like. A value is object-like if it's not `null`
     * and has a `typeof` result of "object".
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is object-like, else `false`.
     * @example
     *
     * _.isObjectLike({});
     * // => true
     *
     * _.isObjectLike([1, 2, 3]);
     * // => true
     *
     * _.isObjectLike(_.noop);
     * // => false
     *
     * _.isObjectLike(null);
     * // => false
     */
    function isObjectLike(value) {
      return value != null && (typeof value === 'undefined' ? 'undefined' : _typeof(value)) == 'object';
    }

    /**
     * Checks if `value` is classified as a `String` primitive or object.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a string, else `false`.
     * @example
     *
     * _.isString('abc');
     * // => true
     *
     * _.isString(1);
     * // => false
     */
    function isString(value) {
      return typeof value == 'string' || !isArray(value) && isObjectLike(value) && baseGetTag(value) == stringTag;
    }

    /**
     * Checks if `value` is classified as a `Symbol` primitive or object.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a symbol, else `false`.
     * @example
     *
     * _.isSymbol(Symbol.iterator);
     * // => true
     *
     * _.isSymbol('abc');
     * // => false
     */
    function isSymbol(value) {
      return (typeof value === 'undefined' ? 'undefined' : _typeof(value)) == 'symbol' || isObjectLike(value) && baseGetTag(value) == symbolTag;
    }

    /**
     * Checks if `value` is classified as a typed array.
     *
     * @static
     * @memberOf _
     * @since 3.0.0
     * @category Lang
     * @param {*} value The value to check.
     * @returns {boolean} Returns `true` if `value` is a typed array, else `false`.
     * @example
     *
     * _.isTypedArray(new Uint8Array);
     * // => true
     *
     * _.isTypedArray([]);
     * // => false
     */
    var isTypedArray = nodeIsTypedArray ? baseUnary(nodeIsTypedArray) : baseIsTypedArray;

    /**
     * Converts `value` to an array.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Lang
     * @param {*} value The value to convert.
     * @returns {Array} Returns the converted array.
     * @example
     *
     * _.toArray({ 'a': 1, 'b': 2 });
     * // => [1, 2]
     *
     * _.toArray('abc');
     * // => ['a', 'b', 'c']
     *
     * _.toArray(1);
     * // => []
     *
     * _.toArray(null);
     * // => []
     */
    function toArray(value) {
      if (!value) {
        return [];
      }
      if (isArrayLike(value)) {
        return isString(value) ? stringToArray(value) : copyArray(value);
      }
      if (symIterator && value[symIterator]) {
        return iteratorToArray(value[symIterator]());
      }
      var tag = getTag(value),
          func = tag == mapTag ? mapToArray : tag == setTag ? setToArray : values;

      return func(value);
    }

    /**
     * Converts `value` to a finite number.
     *
     * @static
     * @memberOf _
     * @since 4.12.0
     * @category Lang
     * @param {*} value The value to convert.
     * @returns {number} Returns the converted number.
     * @example
     *
     * _.toFinite(3.2);
     * // => 3.2
     *
     * _.toFinite(Number.MIN_VALUE);
     * // => 5e-324
     *
     * _.toFinite(Infinity);
     * // => 1.7976931348623157e+308
     *
     * _.toFinite('3.2');
     * // => 3.2
     */
    function toFinite(value) {
      if (!value) {
        return value === 0 ? value : 0;
      }
      value = toNumber(value);
      if (value === INFINITY || value === -INFINITY) {
        var sign = value < 0 ? -1 : 1;
        return sign * MAX_INTEGER;
      }
      return value === value ? value : 0;
    }

    /**
     * Converts `value` to an integer.
     *
     * **Note:** This method is loosely based on
     * [`ToInteger`](http://www.ecma-international.org/ecma-262/7.0/#sec-tointeger).
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to convert.
     * @returns {number} Returns the converted integer.
     * @example
     *
     * _.toInteger(3.2);
     * // => 3
     *
     * _.toInteger(Number.MIN_VALUE);
     * // => 0
     *
     * _.toInteger(Infinity);
     * // => 1.7976931348623157e+308
     *
     * _.toInteger('3.2');
     * // => 3
     */
    function toInteger(value) {
      var result = toFinite(value),
          remainder = result % 1;

      return result === result ? remainder ? result - remainder : result : 0;
    }

    /**
     * Converts `value` to a number.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to process.
     * @returns {number} Returns the number.
     * @example
     *
     * _.toNumber(3.2);
     * // => 3.2
     *
     * _.toNumber(Number.MIN_VALUE);
     * // => 5e-324
     *
     * _.toNumber(Infinity);
     * // => Infinity
     *
     * _.toNumber('3.2');
     * // => 3.2
     */
    function toNumber(value) {
      if (typeof value == 'number') {
        return value;
      }
      if (isSymbol(value)) {
        return NAN;
      }
      if (isObject(value)) {
        var other = typeof value.valueOf == 'function' ? value.valueOf() : value;
        value = isObject(other) ? other + '' : other;
      }
      if (typeof value != 'string') {
        return value === 0 ? value : +value;
      }
      value = value.replace(reTrim, '');
      var isBinary = reIsBinary.test(value);
      return isBinary || reIsOctal.test(value) ? freeParseInt(value.slice(2), isBinary ? 2 : 8) : reIsBadHex.test(value) ? NAN : +value;
    }

    /**
     * Converts `value` to a string. An empty string is returned for `null`
     * and `undefined` values. The sign of `-0` is preserved.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Lang
     * @param {*} value The value to convert.
     * @returns {string} Returns the converted string.
     * @example
     *
     * _.toString(null);
     * // => ''
     *
     * _.toString(-0);
     * // => '-0'
     *
     * _.toString([1, 2, 3]);
     * // => '1,2,3'
     */
    function toString(value) {
      return value == null ? '' : baseToString(value);
    }

    /*------------------------------------------------------------------------*/

    /**
     * Gets the value at `path` of `object`. If the resolved value is
     * `undefined`, the `defaultValue` is returned in its place.
     *
     * @static
     * @memberOf _
     * @since 3.7.0
     * @category Object
     * @param {Object} object The object to query.
     * @param {Array|string} path The path of the property to get.
     * @param {*} [defaultValue] The value returned for `undefined` resolved values.
     * @returns {*} Returns the resolved value.
     * @example
     *
     * var object = { 'a': [{ 'b': { 'c': 3 } }] };
     *
     * _.get(object, 'a[0].b.c');
     * // => 3
     *
     * _.get(object, ['a', '0', 'b', 'c']);
     * // => 3
     *
     * _.get(object, 'a.b.c', 'default');
     * // => 'default'
     */
    function get(object, path, defaultValue) {
      var result = object == null ? undefined : baseGet(object, path);
      return result === undefined ? defaultValue : result;
    }

    /**
     * Checks if `path` is a direct or inherited property of `object`.
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Object
     * @param {Object} object The object to query.
     * @param {Array|string} path The path to check.
     * @returns {boolean} Returns `true` if `path` exists, else `false`.
     * @example
     *
     * var object = _.create({ 'a': _.create({ 'b': 2 }) });
     *
     * _.hasIn(object, 'a');
     * // => true
     *
     * _.hasIn(object, 'a.b');
     * // => true
     *
     * _.hasIn(object, ['a', 'b']);
     * // => true
     *
     * _.hasIn(object, 'b');
     * // => false
     */
    function hasIn(object, path) {
      return object != null && hasPath(object, path, baseHasIn);
    }

    /**
     * Creates an array of the own enumerable property names of `object`.
     *
     * **Note:** Non-object values are coerced to objects. See the
     * [ES spec](http://ecma-international.org/ecma-262/7.0/#sec-object.keys)
     * for more details.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Object
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names.
     * @example
     *
     * function Foo() {
     *   this.a = 1;
     *   this.b = 2;
     * }
     *
     * Foo.prototype.c = 3;
     *
     * _.keys(new Foo);
     * // => ['a', 'b'] (iteration order is not guaranteed)
     *
     * _.keys('hi');
     * // => ['0', '1']
     */
    function keys(object) {
      return isArrayLike(object) ? arrayLikeKeys(object) : baseKeys(object);
    }

    /**
     * Creates an array of the own and inherited enumerable property names of `object`.
     *
     * **Note:** Non-object values are coerced to objects.
     *
     * @static
     * @memberOf _
     * @since 3.0.0
     * @category Object
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property names.
     * @example
     *
     * function Foo() {
     *   this.a = 1;
     *   this.b = 2;
     * }
     *
     * Foo.prototype.c = 3;
     *
     * _.keysIn(new Foo);
     * // => ['a', 'b', 'c'] (iteration order is not guaranteed)
     */
    function keysIn(object) {
      return isArrayLike(object) ? arrayLikeKeys(object, true) : baseKeysIn(object);
    }

    /**
     * The opposite of `_.mapValues`; this method creates an object with the
     * same values as `object` and keys generated by running each own enumerable
     * string keyed property of `object` thru `iteratee`. The iteratee is invoked
     * with three arguments: (value, key, object).
     *
     * @static
     * @memberOf _
     * @since 3.8.0
     * @category Object
     * @param {Object} object The object to iterate over.
     * @param {Function} [iteratee=_.identity] The function invoked per iteration.
     * @returns {Object} Returns the new mapped object.
     * @see _.mapValues
     * @example
     *
     * _.mapKeys({ 'a': 1, 'b': 2 }, function(value, key) {
     *   return key + value;
     * });
     * // => { 'a1': 1, 'b2': 2 }
     */
    function mapKeys(object, iteratee) {
      var result = {};
      iteratee = getIteratee(iteratee, 3);

      baseForOwn(object, function (value, key, object) {
        baseAssignValue(result, iteratee(value, key, object), value);
      });
      return result;
    }

    /**
     * Creates an object with the same keys as `object` and values generated
     * by running each own enumerable string keyed property of `object` thru
     * `iteratee`. The iteratee is invoked with three arguments:
     * (value, key, object).
     *
     * @static
     * @memberOf _
     * @since 2.4.0
     * @category Object
     * @param {Object} object The object to iterate over.
     * @param {Function} [iteratee=_.identity] The function invoked per iteration.
     * @returns {Object} Returns the new mapped object.
     * @see _.mapKeys
     * @example
     *
     * var users = {
     *   'fred':    { 'user': 'fred',    'age': 40 },
     *   'pebbles': { 'user': 'pebbles', 'age': 1 }
     * };
     *
     * _.mapValues(users, function(o) { return o.age; });
     * // => { 'fred': 40, 'pebbles': 1 } (iteration order is not guaranteed)
     *
     * // The `_.property` iteratee shorthand.
     * _.mapValues(users, 'age');
     * // => { 'fred': 40, 'pebbles': 1 } (iteration order is not guaranteed)
     */
    function mapValues(object, iteratee) {
      var result = {};
      iteratee = getIteratee(iteratee, 3);

      baseForOwn(object, function (value, key, object) {
        baseAssignValue(result, key, iteratee(value, key, object));
      });
      return result;
    }

    /**
     * Creates an object composed of the `object` properties `predicate` returns
     * truthy for. The predicate is invoked with two arguments: (value, key).
     *
     * @static
     * @memberOf _
     * @since 4.0.0
     * @category Object
     * @param {Object} object The source object.
     * @param {Function} [predicate=_.identity] The function invoked per property.
     * @returns {Object} Returns the new object.
     * @example
     *
     * var object = { 'a': 1, 'b': '2', 'c': 3 };
     *
     * _.pickBy(object, _.isNumber);
     * // => { 'a': 1, 'c': 3 }
     */
    function pickBy(object, predicate) {
      if (object == null) {
        return {};
      }
      var props = arrayMap(getAllKeysIn(object), function (prop) {
        return [prop];
      });
      predicate = getIteratee(predicate);
      return basePickBy(object, props, function (value, path) {
        return predicate(value, path[0]);
      });
    }

    /**
     * Creates an array of the own enumerable string keyed property values of `object`.
     *
     * **Note:** Non-object values are coerced to objects.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Object
     * @param {Object} object The object to query.
     * @returns {Array} Returns the array of property values.
     * @example
     *
     * function Foo() {
     *   this.a = 1;
     *   this.b = 2;
     * }
     *
     * Foo.prototype.c = 3;
     *
     * _.values(new Foo);
     * // => [1, 2] (iteration order is not guaranteed)
     *
     * _.values('hi');
     * // => ['h', 'i']
     */
    function values(object) {
      return object == null ? [] : baseValues(object, keys(object));
    }

    /*------------------------------------------------------------------------*/

    /**
     * Creates a function that returns `value`.
     *
     * @static
     * @memberOf _
     * @since 2.4.0
     * @category Util
     * @param {*} value The value to return from the new function.
     * @returns {Function} Returns the new constant function.
     * @example
     *
     * var objects = _.times(2, _.constant({ 'a': 1 }));
     *
     * console.log(objects);
     * // => [{ 'a': 1 }, { 'a': 1 }]
     *
     * console.log(objects[0] === objects[1]);
     * // => true
     */
    function constant(value) {
      return function () {
        return value;
      };
    }

    /**
     * This method returns the first argument it receives.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Util
     * @param {*} value Any value.
     * @returns {*} Returns `value`.
     * @example
     *
     * var object = { 'a': 1 };
     *
     * console.log(_.identity(object) === object);
     * // => true
     */
    function identity(value) {
      return value;
    }

    /**
     * Creates a function that invokes `func` with the arguments of the created
     * function. If `func` is a property name, the created function returns the
     * property value for a given element. If `func` is an array or object, the
     * created function returns `true` for elements that contain the equivalent
     * source properties, otherwise it returns `false`.
     *
     * @static
     * @since 4.0.0
     * @memberOf _
     * @category Util
     * @param {*} [func=_.identity] The value to convert to a callback.
     * @returns {Function} Returns the callback.
     * @example
     *
     * var users = [
     *   { 'user': 'barney', 'age': 36, 'active': true },
     *   { 'user': 'fred',   'age': 40, 'active': false }
     * ];
     *
     * // The `_.matches` iteratee shorthand.
     * _.filter(users, _.iteratee({ 'user': 'barney', 'active': true }));
     * // => [{ 'user': 'barney', 'age': 36, 'active': true }]
     *
     * // The `_.matchesProperty` iteratee shorthand.
     * _.filter(users, _.iteratee(['user', 'fred']));
     * // => [{ 'user': 'fred', 'age': 40 }]
     *
     * // The `_.property` iteratee shorthand.
     * _.map(users, _.iteratee('user'));
     * // => ['barney', 'fred']
     *
     * // Create custom iteratee shorthands.
     * _.iteratee = _.wrap(_.iteratee, function(iteratee, func) {
     *   return !_.isRegExp(func) ? iteratee(func) : function(string) {
     *     return func.test(string);
     *   };
     * });
     *
     * _.filter(['abc', 'def'], /ef/);
     * // => ['def']
     */
    function iteratee(func) {
      return baseIteratee(typeof func == 'function' ? func : baseClone(func, CLONE_DEEP_FLAG));
    }

    /**
     * Adds all own enumerable string keyed function properties of a source
     * object to the destination object. If `object` is a function, then methods
     * are added to its prototype as well.
     *
     * **Note:** Use `_.runInContext` to create a pristine `lodash` function to
     * avoid conflicts caused by modifying the original.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Util
     * @param {Function|Object} [object=lodash] The destination object.
     * @param {Object} source The object of functions to add.
     * @param {Object} [options={}] The options object.
     * @param {boolean} [options.chain=true] Specify whether mixins are chainable.
     * @returns {Function|Object} Returns `object`.
     * @example
     *
     * function vowels(string) {
     *   return _.filter(string, function(v) {
     *     return /[aeiou]/i.test(v);
     *   });
     * }
     *
     * _.mixin({ 'vowels': vowels });
     * _.vowels('fred');
     * // => ['e']
     *
     * _('fred').vowels().value();
     * // => ['e']
     *
     * _.mixin({ 'vowels': vowels }, { 'chain': false });
     * _('fred').vowels();
     * // => ['e']
     */
    function mixin(object, source, options) {
      var props = keys(source),
          methodNames = baseFunctions(source, props);

      if (options == null && !(isObject(source) && (methodNames.length || !props.length))) {
        options = source;
        source = object;
        object = this;
        methodNames = baseFunctions(source, keys(source));
      }
      var chain = !(isObject(options) && 'chain' in options) || !!options.chain,
          isFunc = isFunction(object);

      arrayEach(methodNames, function (methodName) {
        var func = source[methodName];
        object[methodName] = func;
        if (isFunc) {
          object.prototype[methodName] = function () {
            var chainAll = this.__chain__;
            if (chain || chainAll) {
              var result = object(this.__wrapped__),
                  actions = result.__actions__ = copyArray(this.__actions__);

              actions.push({ 'func': func, 'args': arguments, 'thisArg': object });
              result.__chain__ = chainAll;
              return result;
            }
            return func.apply(object, arrayPush([this.value()], arguments));
          };
        }
      });

      return object;
    }

    /**
     * Reverts the `_` variable to its previous value and returns a reference to
     * the `lodash` function.
     *
     * @static
     * @since 0.1.0
     * @memberOf _
     * @category Util
     * @returns {Function} Returns the `lodash` function.
     * @example
     *
     * var lodash = _.noConflict();
     */
    function noConflict() {
      if (root._ === this) {
        root._ = oldDash;
      }
      return this;
    }

    /**
     * This method returns `undefined`.
     *
     * @static
     * @memberOf _
     * @since 2.3.0
     * @category Util
     * @example
     *
     * _.times(2, _.noop);
     * // => [undefined, undefined]
     */
    function noop() {}
    // No operation performed.


    /**
     * Creates a function that returns the value at `path` of a given object.
     *
     * @static
     * @memberOf _
     * @since 2.4.0
     * @category Util
     * @param {Array|string} path The path of the property to get.
     * @returns {Function} Returns the new accessor function.
     * @example
     *
     * var objects = [
     *   { 'a': { 'b': 2 } },
     *   { 'a': { 'b': 1 } }
     * ];
     *
     * _.map(objects, _.property('a.b'));
     * // => [2, 1]
     *
     * _.map(_.sortBy(objects, _.property(['a', 'b'])), 'a.b');
     * // => [1, 2]
     */
    function property(path) {
      return isKey(path) ? baseProperty(toKey(path)) : basePropertyDeep(path);
    }

    /**
     * This method returns a new empty array.
     *
     * @static
     * @memberOf _
     * @since 4.13.0
     * @category Util
     * @returns {Array} Returns the new empty array.
     * @example
     *
     * var arrays = _.times(2, _.stubArray);
     *
     * console.log(arrays);
     * // => [[], []]
     *
     * console.log(arrays[0] === arrays[1]);
     * // => false
     */
    function stubArray() {
      return [];
    }

    /**
     * This method returns `false`.
     *
     * @static
     * @memberOf _
     * @since 4.13.0
     * @category Util
     * @returns {boolean} Returns `false`.
     * @example
     *
     * _.times(2, _.stubFalse);
     * // => [false, false]
     */
    function stubFalse() {
      return false;
    }

    /*------------------------------------------------------------------------*/

    // Add methods that return wrapped values in chain sequences.
    lodash.chain = chain;
    lodash.constant = constant;
    lodash.drop = drop;
    lodash.filter = filter;
    lodash.flatten = flatten;
    lodash.groupBy = groupBy;
    lodash.iteratee = iteratee;
    lodash.keys = keys;
    lodash.keysIn = keysIn;
    lodash.map = map;
    lodash.mapKeys = mapKeys;
    lodash.mapValues = mapValues;
    lodash.memoize = memoize;
    lodash.mixin = mixin;
    lodash.negate = negate;
    lodash.partition = partition;
    lodash.pickBy = pickBy;
    lodash.property = property;
    lodash.reverse = reverse;
    lodash.take = take;
    lodash.tap = tap;
    lodash.thru = thru;
    lodash.toArray = toArray;
    lodash.values = values;

    // Add methods to `lodash.prototype`.
    mixin(lodash, lodash);

    /*------------------------------------------------------------------------*/

    // Add methods that return unwrapped values in chain sequences.
    lodash.eq = eq;
    lodash.forEach = forEach;
    lodash.get = get;
    lodash.hasIn = hasIn;
    lodash.identity = identity;
    lodash.isArguments = isArguments;
    lodash.isArray = isArray;
    lodash.isArrayLike = isArrayLike;
    lodash.isBuffer = isBuffer;
    lodash.isFunction = isFunction;
    lodash.isLength = isLength;
    lodash.isObject = isObject;
    lodash.isObjectLike = isObjectLike;
    lodash.isString = isString;
    lodash.isSymbol = isSymbol;
    lodash.isTypedArray = isTypedArray;
    lodash.last = last;
    lodash.stubArray = stubArray;
    lodash.stubFalse = stubFalse;
    lodash.noConflict = noConflict;
    lodash.noop = noop;
    lodash.reduce = reduce;
    lodash.some = some;
    lodash.toFinite = toFinite;
    lodash.toInteger = toInteger;
    lodash.toNumber = toNumber;
    lodash.toString = toString;

    // Add aliases.
    lodash.each = forEach;

    mixin(lodash, function () {
      var source = {};
      baseForOwn(lodash, function (func, methodName) {
        if (!hasOwnProperty.call(lodash.prototype, methodName)) {
          source[methodName] = func;
        }
      });
      return source;
    }(), { 'chain': false });

    /*------------------------------------------------------------------------*/

    /**
     * The semantic version number.
     *
     * @static
     * @memberOf _
     * @type {string}
     */
    lodash.VERSION = VERSION;

    // Add `LazyWrapper` methods for `_.drop` and `_.take` variants.
    arrayEach(['drop', 'take'], function (methodName, index) {
      LazyWrapper.prototype[methodName] = function (n) {
        n = n === undefined ? 1 : nativeMax(toInteger(n), 0);

        var result = this.__filtered__ && !index ? new LazyWrapper(this) : this.clone();

        if (result.__filtered__) {
          result.__takeCount__ = nativeMin(n, result.__takeCount__);
        } else {
          result.__views__.push({
            'size': nativeMin(n, MAX_ARRAY_LENGTH),
            'type': methodName + (result.__dir__ < 0 ? 'Right' : '')
          });
        }
        return result;
      };

      LazyWrapper.prototype[methodName + 'Right'] = function (n) {
        return this.reverse()[methodName](n).reverse();
      };
    });

    // Add `LazyWrapper` methods that accept an `iteratee` value.
    arrayEach(['filter', 'map', 'takeWhile'], function (methodName, index) {
      var type = index + 1,
          isFilter = type == LAZY_FILTER_FLAG || type == LAZY_WHILE_FLAG;

      LazyWrapper.prototype[methodName] = function (iteratee) {
        var result = this.clone();
        result.__iteratees__.push({
          'iteratee': getIteratee(iteratee, 3),
          'type': type
        });
        result.__filtered__ = result.__filtered__ || isFilter;
        return result;
      };
    });

    // Add `LazyWrapper` methods for `_.head` and `_.last`.
    arrayEach(['head', 'last'], function (methodName, index) {
      var takeName = 'take' + (index ? 'Right' : '');

      LazyWrapper.prototype[methodName] = function () {
        return this[takeName](1).value()[0];
      };
    });

    // Add `LazyWrapper` methods for `_.initial` and `_.tail`.
    arrayEach(['initial', 'tail'], function (methodName, index) {
      var dropName = 'drop' + (index ? '' : 'Right');

      LazyWrapper.prototype[methodName] = function () {
        return this.__filtered__ ? new LazyWrapper(this) : this[dropName](1);
      };
    });

    LazyWrapper.prototype.compact = function () {
      return this.filter(identity);
    };

    LazyWrapper.prototype.find = function (predicate) {
      return this.filter(predicate).head();
    };

    LazyWrapper.prototype.findLast = function (predicate) {
      return this.reverse().find(predicate);
    };

    LazyWrapper.prototype.invokeMap = baseRest(function (path, args) {
      if (typeof path == 'function') {
        return new LazyWrapper(this);
      }
      return this.map(function (value) {
        return baseInvoke(value, path, args);
      });
    });

    LazyWrapper.prototype.reject = function (predicate) {
      return this.filter(negate(getIteratee(predicate)));
    };

    LazyWrapper.prototype.slice = function (start, end) {
      start = toInteger(start);

      var result = this;
      if (result.__filtered__ && (start > 0 || end < 0)) {
        return new LazyWrapper(result);
      }
      if (start < 0) {
        result = result.takeRight(-start);
      } else if (start) {
        result = result.drop(start);
      }
      if (end !== undefined) {
        end = toInteger(end);
        result = end < 0 ? result.dropRight(-end) : result.take(end - start);
      }
      return result;
    };

    LazyWrapper.prototype.takeRightWhile = function (predicate) {
      return this.reverse().takeWhile(predicate).reverse();
    };

    LazyWrapper.prototype.toArray = function () {
      return this.take(MAX_ARRAY_LENGTH);
    };

    // Add `LazyWrapper` methods to `lodash.prototype`.
    baseForOwn(LazyWrapper.prototype, function (func, methodName) {
      var checkIteratee = /^(?:filter|find|map|reject)|While$/.test(methodName),
          isTaker = /^(?:head|last)$/.test(methodName),
          lodashFunc = lodash[isTaker ? 'take' + (methodName == 'last' ? 'Right' : '') : methodName],
          retUnwrapped = isTaker || /^find/.test(methodName);

      if (!lodashFunc) {
        return;
      }
      lodash.prototype[methodName] = function () {
        var value = this.__wrapped__,
            args = isTaker ? [1] : arguments,
            isLazy = value instanceof LazyWrapper,
            iteratee = args[0],
            useLazy = isLazy || isArray(value);

        var interceptor = function interceptor(value) {
          var result = lodashFunc.apply(lodash, arrayPush([value], args));
          return isTaker && chainAll ? result[0] : result;
        };

        if (useLazy && checkIteratee && typeof iteratee == 'function' && iteratee.length != 1) {
          // Avoid lazy use if the iteratee has a "length" value other than `1`.
          isLazy = useLazy = false;
        }
        var chainAll = this.__chain__,
            isHybrid = !!this.__actions__.length,
            isUnwrapped = retUnwrapped && !chainAll,
            onlyLazy = isLazy && !isHybrid;

        if (!retUnwrapped && useLazy) {
          value = onlyLazy ? value : new LazyWrapper(this);
          var result = func.apply(value, args);
          result.__actions__.push({ 'func': thru, 'args': [interceptor], 'thisArg': undefined });
          return new LodashWrapper(result, chainAll);
        }
        if (isUnwrapped && onlyLazy) {
          return func.apply(this, args);
        }
        result = this.thru(interceptor);
        return isUnwrapped ? isTaker ? result.value()[0] : result.value() : result;
      };
    });

    // Add `Array` methods to `lodash.prototype`.
    arrayEach(['pop', 'push', 'shift', 'sort', 'splice', 'unshift'], function (methodName) {
      var func = arrayProto[methodName],
          chainName = /^(?:push|sort|unshift)$/.test(methodName) ? 'tap' : 'thru',
          retUnwrapped = /^(?:pop|shift)$/.test(methodName);

      lodash.prototype[methodName] = function () {
        var args = arguments;
        if (retUnwrapped && !this.__chain__) {
          var value = this.value();
          return func.apply(isArray(value) ? value : [], args);
        }
        return this[chainName](function (value) {
          return func.apply(isArray(value) ? value : [], args);
        });
      };
    });

    // Map minified method names to their real names.
    baseForOwn(LazyWrapper.prototype, function (func, methodName) {
      var lodashFunc = lodash[methodName];
      if (lodashFunc) {
        var key = lodashFunc.name + '',
            names = realNames[key] || (realNames[key] = []);

        names.push({ 'name': methodName, 'func': lodashFunc });
      }
    });

    realNames[createHybrid(undefined, WRAP_BIND_KEY_FLAG).name] = [{
      'name': 'wrapper',
      'func': undefined
    }];

    // Add methods to `LazyWrapper`.
    LazyWrapper.prototype.clone = lazyClone;
    LazyWrapper.prototype.reverse = lazyReverse;
    LazyWrapper.prototype.value = lazyValue;

    // Add chain sequence methods to the `lodash` wrapper.
    lodash.prototype.chain = wrapperChain;
    lodash.prototype.commit = wrapperCommit;
    lodash.prototype.next = wrapperNext;
    lodash.prototype.plant = wrapperPlant;
    lodash.prototype.reverse = wrapperReverse;
    lodash.prototype.toJSON = lodash.prototype.valueOf = lodash.prototype.value = wrapperValue;

    // Add lazy aliases.
    lodash.prototype.first = lodash.prototype.head;

    if (symIterator) {
      lodash.prototype[symIterator] = wrapperToIterator;
    }

    /*--------------------------------------------------------------------------*/

    // Some AMD build optimizers, like r.js, check for condition patterns like:
    if (typeof define == 'function' && _typeof(define.amd) == 'object' && define.amd) {
      // Expose Lodash on the global object to prevent errors when Lodash is
      // loaded by a script tag in the presence of an AMD loader.
      // See http://requirejs.org/docs/errors.html#mismatch for more details.
      // Use `_.noConflict` to remove Lodash from the global object.
      root._ = lodash;

      // Define as an anonymous module so, through path mapping, it can be
      // referenced as the "underscore" module.
      define(function () {
        return lodash;
      });
    }
    // Check for `exports` after `define` in case a build optimizer adds it.
    else if (freeModule) {
        // Export for Node.js.
        (freeModule.exports = lodash)._ = lodash;
        // Export for CommonJS support.
        freeExports._ = lodash;
      } else {
        // Export to the global object.
        root._ = lodash;
      }
  }).call(this);

  //---------------------------------------------------------------------
  // Source file: ../bundled/lodash_epilogue.js

  /* eslint-enable */
  var _ = window._.noConflict();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/utils.js

  function escapeHTML(str) {
    var escaped = {
      "&": "&amp;",
      "<": "&lt;",
      ">": "&gt;",
      '"': "&quot;",
      "'": "&#039;",
      "/": "&#x2F;"
    };

    return str.replace(/[&<>'"\/]/g, function (m) {
      return escaped[m];
    });
  }

  function randomId() {
    return Math.floor(0x100000000 + Math.random() * 0xF00000000).toString(16);
  }

  function strToBool(str) {
    if (!str || !str.toLowerCase) return undefined;

    switch (str.toLowerCase()) {
      case 'true':
        return true;
      case 'false':
        return false;
      default:
        return undefined;
    }
  }

  // A wrapper for getComputedStyle that is compatible with older browsers.
  // This is significantly faster than jQuery's .css() function.
  function getStyle(el, styleProp) {
    var x;
    if (el.currentStyle) x = el.currentStyle[styleProp];else if (window.getComputedStyle) {
      // getComputedStyle can return null when we're inside a hidden iframe on
      // Firefox; don't attempt to retrieve style props in this case.
      // https://bugzilla.mozilla.org/show_bug.cgi?id=548397
      var style = document.defaultView.getComputedStyle(el, null);
      if (style) x = style.getPropertyValue(styleProp);
    }
    return x;
  }

  // Convert a number to a string with leading zeros
  function padZeros(n, digits) {
    var str = n.toString();
    while (str.length < digits) {
      str = "0" + str;
    }return str;
  }

  // Round to a specified number of significant digits.
  function roundSignif(x) {
    var digits = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 1;

    if (digits < 1) throw "Significant digits must be at least 1.";

    // This converts to a string and back to a number, which is inelegant, but
    // is less prone to FP rounding error than an alternate method which used
    // Math.round().
    return parseFloat(x.toPrecision(digits));
  }

  // Take a string with format "YYYY-MM-DD" and return a Date object.
  // IE8 and QTWebKit don't support YYYY-MM-DD, but they support YYYY/MM/DD
  function parseDate(dateString) {
    var date = new Date(dateString);
    if (isNaN(date)) date = new Date(dateString.replace(/-/g, "/"));
    return date;
  }

  // Given a Date object, return a string in yyyy-mm-dd format, using the
  // UTC date. This may be a day off from the date in the local time zone.
  function formatDateUTC(date) {
    if (date instanceof Date) {
      return date.getUTCFullYear() + '-' + padZeros(date.getUTCMonth() + 1, 2) + '-' + padZeros(date.getUTCDate(), 2);
    } else {
      return null;
    }
  }

  // Given an element and a function(width, height), returns a function(). When
  // the output function is called, it calls the input function with the offset
  // width and height of the input element--but only if the size of the element
  // is non-zero and the size is different than the last time the output
  // function was called.
  //
  // Basically we are trying to filter out extraneous calls to func, so that
  // when the window size changes or whatever, we don't run resize logic for
  // elements that haven't actually changed size or aren't visible anyway.
  function makeResizeFilter(el, func) {
    var lastSize = {};
    return function () {
      var size = { w: el.offsetWidth, h: el.offsetHeight };
      if (size.w === 0 && size.h === 0) return;
      if (size.w === lastSize.w && size.h === lastSize.h) return;
      lastSize = size;
      func(size.w, size.h);
    };
  }

  var _BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder || window.MozBlobBuilder || window.MSBlobBuilder;

  function makeBlob(parts) {

    // Browser compatibility is a mess right now. The code as written works in
    // a variety of modern browsers, but sadly gives a deprecation warning
    // message on the console in current versions (as of this writing) of
    // Chrome.

    // Safari 6.0 (8536.25) on Mac OS X 10.8.1:
    // Has Blob constructor but it doesn't work with ArrayBufferView args

    // Google Chrome 21.0.1180.81 on Xubuntu 12.04:
    // Has Blob constructor, accepts ArrayBufferView args, accepts ArrayBuffer
    // but with a deprecation warning message

    // Firefox 15.0 on Xubuntu 12.04:
    // Has Blob constructor, accepts both ArrayBuffer and ArrayBufferView args

    // Chromium 18.0.1025.168 (Developer Build 134367 Linux) on Xubuntu 12.04:
    // No Blob constructor. Has WebKitBlobBuilder.

    try {
      return new Blob(parts);
    } catch (e) {
      var blobBuilder = new _BlobBuilder();
      $.each(parts, function (i, part) {
        blobBuilder.append(part);
      });
      return blobBuilder.getBlob();
    }
  }

  function pixelRatio() {
    if (window.devicePixelRatio) {
      return window.devicePixelRatio;
    } else {
      return 1;
    }
  }

  // Takes a string expression and returns a function that takes an argument.
  //
  // When the function is executed, it will evaluate that expression using
  // "with" on the argument value, and return the result.
  function scopeExprToFunc(expr) {
    /*jshint evil: true */
    var expr_escaped = expr.replace(/[\\"']/g, '\\$&').replace(/\u0000/g, '\\0');
    try {
      var func = new Function('with (this) {\n        try {\n          return (' + expr + ');\n        } catch (e) {\n          console.error(\'Error evaluating expression: ' + expr_escaped + '\');\n          throw e;\n        }\n      }');
    } catch (e) {
      console.error("Error parsing expression: " + expr);
      throw e;
    }

    return function (scope) {
      return func.call(scope);
    };
  }

  function asArray(value) {
    if (value === null || value === undefined) return [];
    if ($.isArray(value)) return value;
    return [value];
  }

  // We need a stable sorting algorithm for ordering
  // bindings by priority and insertion order.
  function mergeSort(list, sortfunc) {
    function merge(sortfunc, a, b) {
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
      while (ia < a.length) {
        sorted.push(a[ia++]);
      }while (ib < b.length) {
        sorted.push(b[ib++]);
      }return sorted;
    }

    // Don't mutate list argument
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

  // Escape jQuery selector metacharacters: !"#$%&'()*+,./:;<=>?@[\]^`{|}~
  var $escape = exports.$escape = function (val) {
    return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/browser.js

  var browser = function () {

    var isQt = false;
    // For easy handling of Qt quirks using CSS
    if (/\bQt\//.test(window.navigator.userAgent)) {
      $(document.documentElement).addClass('qt');
      isQt = true;
    }

    // Enable special treatment for Qt 5 quirks on Linux
    if (/\bQt\/5/.test(window.navigator.userAgent) && /Linux/.test(window.navigator.userAgent)) {
      $(document.documentElement).addClass('qt5');
    }

    // Detect IE information
    var isIE = navigator.appName === 'Microsoft Internet Explorer';

    function getIEVersion() {
      var rv = -1;
      if (isIE) {
        var ua = navigator.userAgent;
        var re = new RegExp("MSIE ([0-9]{1,}[\\.0-9]{0,})");
        if (re.exec(ua) !== null) rv = parseFloat(RegExp.$1);
      }
      return rv;
    }

    return {
      isQt: isQt,
      isIE: isIE,
      IEVersion: getIEVersion()
    };
  }();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_rate.js

  var Invoker = function Invoker(target, func) {
    this.target = target;
    this.func = func;
  };

  (function () {
    this.normalCall = this.immediateCall = function () {
      this.func.apply(this.target, arguments);
    };
  }).call(Invoker.prototype);

  var Debouncer = function Debouncer(target, func, delayMs) {
    this.target = target;
    this.func = func;
    this.delayMs = delayMs;

    this.timerId = null;
    this.args = null;
  };

  (function () {
    this.normalCall = function () {
      var self = this;

      this.$clearTimer();
      this.args = arguments;

      this.timerId = setTimeout(function () {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (self.timerId === null) return;
        self.$clearTimer();
        self.$invoke();
      }, this.delayMs);
    };
    this.immediateCall = function () {
      this.$clearTimer();
      this.args = arguments;
      this.$invoke();
    };
    this.isPending = function () {
      return this.timerId !== null;
    };
    this.$clearTimer = function () {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    };
    this.$invoke = function () {
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

  (function () {
    this.normalCall = function () {
      var self = this;

      this.args = arguments;
      if (this.timerId === null) {
        this.$invoke();
        this.timerId = setTimeout(function () {
          // IE8 doesn't reliably clear timeout, so this additional
          // check is needed
          if (self.timerId === null) return;
          self.$clearTimer();
          if (self.args) self.normalCall.apply(self, self.args);
        }, this.delayMs);
      }
    };
    this.immediateCall = function () {
      this.$clearTimer();
      this.args = arguments;
      this.$invoke();
    };
    this.isPending = function () {
      return this.timerId !== null;
    };
    this.$clearTimer = function () {
      if (this.timerId !== null) {
        clearTimeout(this.timerId);
        this.timerId = null;
      }
    };
    this.$invoke = function () {
      this.func.apply(this.target, this.args);
      this.args = null;
    };
  }).call(Throttler.prototype);

  // Returns a debounced version of the given function.
  // Debouncing means that when the function is invoked,
  // there is a delay of `threshold` milliseconds before
  // it is actually executed, and if the function is
  // invoked again before that threshold has elapsed then
  // the clock starts over.
  //
  // For example, if a function is debounced with a
  // threshold of 1000ms, then calling it 17 times at
  // 900ms intervals will result in a single execution
  // of the underlying function, 1000ms after the 17th
  // call.
  function debounce(threshold, func) {
    var timerId = null;
    var self, args;
    return function () {
      self = this;
      args = arguments;
      if (timerId !== null) {
        clearTimeout(timerId);
        timerId = null;
      }
      timerId = setTimeout(function () {
        // IE8 doesn't reliably clear timeout, so this additional
        // check is needed
        if (timerId === null) return;
        timerId = null;
        func.apply(self, args);
      }, threshold);
    };
  }

  // Returns a throttled version of the given function.
  // Throttling means that the underlying function will
  // be executed no more than once every `threshold`
  // milliseconds.
  //
  // For example, if a function is throttled with a
  // threshold of 1000ms, then calling it 17 times at
  // 900ms intervals will result in something like 15
  // or 16 executions of the underlying function.
  // eslint-disable-next-line no-unused-vars
  function throttle(threshold, func) {
    var executionPending = false;
    var timerId = null;
    var self, args;

    function throttled() {
      self = null;
      args = null;
      if (timerId === null) {
        // Haven't seen a call recently. Execute now and
        // start a timer to buffer any subsequent calls.
        timerId = setTimeout(function () {
          // When time expires, clear the timer; and if
          // there has been a call in the meantime, repeat.
          timerId = null;
          if (executionPending) {
            executionPending = false;
            throttled.apply(self, args);
          }
        }, threshold);
        func.apply(this, arguments);
      } else {
        // Something executed recently. Don't do anything
        // except set up target/arguments to be called later
        executionPending = true;
        self = this;
        args = arguments;
      }
    }
    return throttled;
  }

  // Schedules data to be sent to shinyapp at the next setTimeout(0).
  // Batches multiple input calls into one websocket message.
  var InputBatchSender = function InputBatchSender(shinyapp) {
    this.shinyapp = shinyapp;
    this.timerId = null;
    this.pendingData = {};
    this.reentrant = false;
    this.lastChanceCallback = [];
  };
  (function () {
    this.setInput = function (name, value) {
      var self = this;

      this.pendingData[name] = value;

      if (!this.timerId && !this.reentrant) {
        this.timerId = setTimeout(function () {
          self.reentrant = true;
          try {
            $.each(self.lastChanceCallback, function (i, callback) {
              callback();
            });
            self.timerId = null;
            var currentData = self.pendingData;
            self.pendingData = {};
            self.shinyapp.sendInput(currentData);
          } finally {
            self.reentrant = false;
          }
        }, 0);
      }
    };
  }).call(InputBatchSender.prototype);

  var InputNoResendDecorator = function InputNoResendDecorator(target, initialValues) {
    this.target = target;
    this.lastSentValues = this.reset(initialValues);
  };
  (function () {
    this.setInput = function (name, value) {
      // Note that opts is not passed to setInput at this stage of the input
      // decorator stack. If in the future this setInput keeps track of opts, it
      // would be best not to store the `el`, because that could prevent it from
      // being GC'd.
      var _splitInputNameType = splitInputNameType(name);

      var inputName = _splitInputNameType.name;
      var inputType = _splitInputNameType.inputType;

      var jsonValue = JSON.stringify(value);

      if (this.lastSentValues[inputName] && this.lastSentValues[inputName].jsonValue === jsonValue && this.lastSentValues[inputName].inputType === inputType) {
        return;
      }
      this.lastSentValues[inputName] = { jsonValue: jsonValue, inputType: inputType };
      this.target.setInput(name, value);
    };
    this.reset = function () {
      var values = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      // Given an object with flat name-value format:
      //   { x: "abc", "y.shiny.number": 123 }
      // Create an object in cache format and save it:
      //   { x: { jsonValue: '"abc"', inputType: "" },
      //     y: { jsonValue: "123", inputType: "shiny.number" } }
      var cacheValues = {};

      for (var inputName in values) {
        if (values.hasOwnProperty(inputName)) {
          var _splitInputNameType2 = splitInputNameType(inputName);

          var name = _splitInputNameType2.name;
          var inputType = _splitInputNameType2.inputType;

          cacheValues[name] = {
            jsonValue: JSON.stringify(values[inputName]),
            inputType: inputType
          };
        }
      }

      this.lastSentValues = cacheValues;
    };
  }).call(InputNoResendDecorator.prototype);

  var InputEventDecorator = function InputEventDecorator(target) {
    this.target = target;
  };
  (function () {
    this.setInput = function (name, value, opts) {
      var evt = jQuery.Event("shiny:inputchanged");

      var input = splitInputNameType(name);
      evt.name = input.name;
      evt.inputType = input.inputType;
      evt.value = value;
      evt.binding = opts.binding;
      evt.el = opts.el;

      $(document).trigger(evt);

      if (!evt.isDefaultPrevented()) {
        name = evt.name;
        if (evt.inputType !== '') name += ':' + evt.inputType;

        // opts aren't passed along to lower levels in the input decorator
        // stack.
        this.target.setInput(name, evt.value);
      }
    };
  }).call(InputEventDecorator.prototype);

  var InputRateDecorator = function InputRateDecorator(target) {
    this.target = target;
    this.inputRatePolicies = {};
  };
  (function () {
    this.setInput = function (name, value, opts) {
      this.$ensureInit(name);

      if (opts.immediate) this.inputRatePolicies[name].immediateCall(name, value, opts);else this.inputRatePolicies[name].normalCall(name, value, opts);
    };
    this.setRatePolicy = function (name, mode, millis) {
      if (mode === 'direct') {
        this.inputRatePolicies[name] = new Invoker(this, this.$doSetInput);
      } else if (mode === 'debounce') {
        this.inputRatePolicies[name] = new Debouncer(this, this.$doSetInput, millis);
      } else if (mode === 'throttle') {
        this.inputRatePolicies[name] = new Throttler(this, this.$doSetInput, millis);
      }
    };
    this.$ensureInit = function (name) {
      if (!(name in this.inputRatePolicies)) this.setRatePolicy(name, 'direct');
    };
    this.$doSetInput = function (name, value, opts) {
      this.target.setInput(name, value, opts);
    };
  }).call(InputRateDecorator.prototype);

  var InputDeferDecorator = function InputDeferDecorator(target) {
    this.target = target;
    this.pendingInput = {};
  };
  (function () {
    this.setInput = function (name, value, opts) {
      if (/^\./.test(name)) this.target.setInput(name, value, opts);else this.pendingInput[name] = { value: value, opts: opts };
    };
    this.submit = function () {
      for (var name in this.pendingInput) {
        if (this.pendingInput.hasOwnProperty(name)) {
          var input = this.pendingInput[name];
          this.target.setInput(name, input.value, input.opts);
        }
      }
    };
  }).call(InputDeferDecorator.prototype);

  var InputValidateDecorator = function InputValidateDecorator(target) {
    this.target = target;
  };
  (function () {
    this.setInput = function (name, value, opts) {
      if (!name) throw "Can't set input with empty name.";

      opts = addDefaultInputOpts(opts);

      this.target.setInput(name, value, opts);
    };
  }).call(InputValidateDecorator.prototype);

  // Merge opts with defaults, and return a new object.
  function addDefaultInputOpts(opts) {
    return $.extend({
      immediate: false,
      binding: null,
      el: null
    }, opts);
  }

  function splitInputNameType(name) {
    var name2 = name.split(':');
    return {
      name: name2[0],
      inputType: name2.length > 1 ? name2[1] : ''
    };
  }

  //---------------------------------------------------------------------
  // Source file: ../srcjs/shinyapp.js

  var ShinyApp = function ShinyApp() {
    this.$socket = null;

    // Cached input values
    this.$inputValues = {};

    // Input values at initialization (and reconnect)
    this.$initialInput = {};

    // Output bindings
    this.$bindings = {};

    // Cached values/errors
    this.$values = {};
    this.$errors = {};

    // Conditional bindings (show/hide element based on expression)
    this.$conditionals = {};

    this.$pendingMessages = [];
    this.$activeRequests = {};
    this.$nextRequestId = 0;

    this.$allowReconnect = false;
  };

  (function () {

    this.connect = function (initialInput) {
      if (this.$socket) throw "Connect was already called on this application object";

      this.$socket = this.createSocket();
      this.$initialInput = initialInput;
      $.extend(this.$inputValues, initialInput);

      this.$updateConditionals();
    };

    this.isConnected = function () {
      return !!this.$socket;
    };

    var scheduledReconnect = null;
    this.reconnect = function () {
      // This function can be invoked directly even if there's a scheduled
      // reconnect, so be sure to clear any such scheduled reconnects.
      clearTimeout(scheduledReconnect);

      if (this.isConnected()) throw "Attempted to reconnect, but already connected.";

      this.$socket = this.createSocket();
      this.$initialInput = $.extend({}, this.$inputValues);
      this.$updateConditionals();
    };

    this.createSocket = function () {
      var self = this;

      var createSocketFunc = exports.createSocket || function () {
        var protocol = 'ws:';
        if (window.location.protocol === 'https:') protocol = 'wss:';

        var defaultPath = window.location.pathname;
        // some older WebKit browsers return the pathname already decoded;
        // if we find invalid URL characters in the path, encode them
        if (!/^([$#!&-;=?-[\]_a-z~]|%[0-9a-fA-F]{2})+$/.test(defaultPath)) {
          defaultPath = encodeURI(defaultPath);
          // Bizarrely, QtWebKit requires us to encode these characters *twice*
          if (browser.isQt) {
            defaultPath = encodeURI(defaultPath);
          }
        }
        if (!/\/$/.test(defaultPath)) defaultPath += '/';
        defaultPath += 'websocket/';

        var ws = new WebSocket(protocol + '//' + window.location.host + defaultPath);
        ws.binaryType = 'arraybuffer';

        return ws;
      };

      var socket = createSocketFunc();
      var hasOpened = false;
      socket.onopen = function () {
        hasOpened = true;

        $(document).trigger({
          type: 'shiny:connected',
          socket: socket
        });

        self.onConnected();

        socket.send(JSON.stringify({
          method: 'init',
          data: self.$initialInput
        }));

        while (self.$pendingMessages.length) {
          var msg = self.$pendingMessages.shift();
          socket.send(msg);
        }
      };
      socket.onmessage = function (e) {
        self.dispatchMessage(e.data);
      };
      // Called when a successfully-opened websocket is closed, or when an
      // attempt to open a connection fails.
      socket.onclose = function () {
        // These things are needed only if we've successfully opened the
        // websocket.
        if (hasOpened) {
          $(document).trigger({
            type: 'shiny:disconnected',
            socket: socket
          });

          self.$notifyDisconnected();
        }

        self.onDisconnected(); // Must be run before self.$removeSocket()
        self.$removeSocket();
      };
      return socket;
    };

    this.sendInput = function (values) {
      var msg = JSON.stringify({
        method: 'update',
        data: values
      });

      this.$sendMsg(msg);

      $.extend(this.$inputValues, values);
      this.$updateConditionals();
    };

    this.$notifyDisconnected = function () {

      // function to normalize hostnames
      var normalize = function normalize(hostname) {
        if (hostname === "127.0.0.1") return "localhost";else return hostname;
      };

      // Send a 'disconnected' message to parent if we are on the same domin
      var parentUrl = parent !== window ? document.referrer : null;
      if (parentUrl) {
        // parse the parent href
        var a = document.createElement('a');
        a.href = parentUrl;

        // post the disconnected message if the hostnames are the same
        if (normalize(a.hostname) === normalize(window.location.hostname)) {
          var protocol = a.protocol.replace(':', ''); // browser compatability
          var origin = protocol + '://' + a.hostname;
          if (a.port) origin = origin + ':' + a.port;
          parent.postMessage('disconnected', origin);
        }
      }
    };

    this.$removeSocket = function () {
      this.$socket = null;
    };

    this.$scheduleReconnect = function (delay) {
      var self = this;
      scheduledReconnect = setTimeout(function () {
        self.reconnect();
      }, delay);
    };

    // How long should we wait before trying the next reconnection?
    // The delay will increase with subsequent attempts.
    // .next: Return the time to wait for next connection, and increment counter.
    // .reset: Reset the attempt counter.
    var reconnectDelay = function () {
      var attempts = 0;
      // Time to wait before each reconnection attempt. If we go through all of
      // these values, repeated use the last one. Add 500ms to each one so that
      // in the last 0.5s, it shows "..."
      var delays = [1500, 1500, 2500, 2500, 5500, 5500, 10500];

      return {
        next: function next() {
          var i = attempts;
          // Instead of going off the end, use the last one
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

    this.onDisconnected = function () {
      // Add gray-out overlay, if not already present
      var $overlay = $('#shiny-disconnected-overlay');
      if ($overlay.length === 0) {
        $(document.body).append('<div id="shiny-disconnected-overlay"></div>');
      }

      // To try a reconnect, both the app (this.$allowReconnect) and the
      // server (this.$socket.allowReconnect) must allow reconnections, or
      // session$allowReconnect("force") was called. The "force" option should
      // only be used for testing.
      if (this.$allowReconnect === true && this.$socket.allowReconnect === true || this.$allowReconnect === "force") {
        var delay = reconnectDelay.next();
        exports.showReconnectDialog(delay);
        this.$scheduleReconnect(delay);
      }
    };

    this.onConnected = function () {
      $('#shiny-disconnected-overlay').remove();
      exports.hideReconnectDialog();
      reconnectDelay.reset();
    };

    // NB: Including blobs will cause IE to break!
    // TODO: Make blobs work with Internet Explorer
    //
    // Websocket messages are normally one-way--i.e. the client passes a
    // message to the server but there is no way for the server to provide
    // a response to that specific message. makeRequest provides a way to
    // do asynchronous RPC over websocket. Each request has a method name
    // and arguments, plus optionally one or more binary blobs can be
    // included as well. The request is tagged with a unique number that
    // the server will use to label the corresponding response.
    //
    // @param method A string that tells the server what logic to run.
    // @param args An array of objects that should also be passed to the
    //   server in JSON-ified form.
    // @param onSuccess A function that will be called back if the server
    //   responds with success. If the server provides a value in the
    //   response, the function will be called with it as the only argument.
    // @param onError A function that will be called back if the server
    //   responds with error, or if the request fails for any other reason.
    //   The parameter to onError will be a string describing the error.
    // @param blobs Optionally, an array of Blob, ArrayBuffer, or string
    //   objects that will be made available to the server as part of the
    //   request. Strings will be encoded using UTF-8.
    this.makeRequest = function (method, args, onSuccess, onError, blobs) {
      var requestId = this.$nextRequestId;
      while (this.$activeRequests[requestId]) {
        requestId = (requestId + 1) % 1000000000;
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
        // We have binary data to transfer; form a different kind of packet.
        // Start with a 4-byte signature, then for each blob, emit 4 bytes for
        // the length followed by the blob. The json payload is UTF-8 encoded
        // and used as the first blob.

        var uint32_to_buf = function uint32_to_buf(val) {
          var buffer = new ArrayBuffer(4);
          var view = new DataView(buffer);
          view.setUint32(0, val, true); // little-endian
          return buffer;
        };

        var payload = [];
        payload.push(uint32_to_buf(0x01020202)); // signature

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

    this.$sendMsg = function (msg) {
      if (!this.$socket.readyState) {
        this.$pendingMessages.push(msg);
      } else {
        this.$socket.send(msg);
      }
    };

    this.receiveError = function (name, error) {
      if (this.$errors[name] === error) return;

      this.$errors[name] = error;
      delete this.$values[name];

      var binding = this.$bindings[name];
      var evt = jQuery.Event('shiny:error');
      evt.name = name;
      evt.error = error;
      evt.binding = binding;
      $(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding && binding.onValueError) {
        binding.onValueError(evt.error);
      }
    };

    this.receiveOutput = function (name, value) {
      if (this.$values[name] === value) return undefined;

      this.$values[name] = value;
      delete this.$errors[name];

      var binding = this.$bindings[name];
      var evt = jQuery.Event('shiny:value');
      evt.name = name;
      evt.value = value;
      evt.binding = binding;
      $(binding ? binding.el : document).trigger(evt);
      if (!evt.isDefaultPrevented() && binding) {
        binding.onValueChange(evt.value);
      }

      return value;
    };

    this.bindOutput = function (id, binding) {
      if (!id) throw "Can't bind an element with no ID";
      if (this.$bindings[id]) throw "Duplicate binding for ID " + id;
      this.$bindings[id] = binding;

      if (this.$values[id] !== undefined) binding.onValueChange(this.$values[id]);else if (this.$errors[id] !== undefined) binding.onValueError(this.$errors[id]);

      return binding;
    };

    this.unbindOutput = function (id, binding) {
      if (this.$bindings[id] === binding) {
        delete this.$bindings[id];
        return true;
      } else {
        return false;
      }
    };

    // Narrows a scopeComponent -- an input or output object -- to one constrained
    // by nsPrefix. Returns a new object with keys removed and renamed as
    // necessary.
    function narrowScope(scopeComponent, nsPrefix) {
      return _(scopeComponent).pickBy(function (_ref, k) {
        _objectDestructuringEmpty(_ref);

        return k.startsWith(nsPrefix);
      }).mapKeys(function (_ref2, k, _ref3) {
        _objectDestructuringEmpty(_ref3);

        _objectDestructuringEmpty(_ref2);

        return k.substring(nsPrefix.length);
      }).value();
    }

    this.$updateConditionals = function () {
      $(document).trigger({
        type: 'shiny:conditional'
      });

      var inputs = {};

      // Input keys use "name:type" format; we don't want the user to
      // have to know about the type suffix when referring to inputs.
      for (var name in this.$inputValues) {
        if (this.$inputValues.hasOwnProperty(name)) {
          var shortName = name.replace(/:.*/, '');
          inputs[shortName] = this.$inputValues[name];
        }
      }

      var scope = { input: inputs, output: this.$values };

      var conditionals = $(document).find('[data-display-if]');
      for (var i = 0; i < conditionals.length; i++) {
        var el = $(conditionals[i]);
        var condFunc = el.data('data-display-if-func');

        if (!condFunc) {
          var condExpr = el.attr('data-display-if');
          condFunc = scopeExprToFunc(condExpr);
          el.data('data-display-if-func', condFunc);
        }

        var nsPrefix = el.attr('data-ns-prefix');
        var nsScope = nsPrefix ? {
          input: narrowScope(scope.input, nsPrefix),
          output: narrowScope(scope.output, nsPrefix)
        } : scope;

        var show = condFunc(nsScope);
        var showing = el.css("display") !== "none";
        if (show !== showing) {
          if (show) {
            el.trigger('show');
            el.show();
            el.trigger('shown');
          } else {
            el.trigger('hide');
            el.hide();
            el.trigger('hidden');
          }
        }
      }
    };

    // Message handler management functions =================================

    // Records insertion order of handlers. Maps number to name. This is so
    // we can dispatch messages to handlers in the order that handlers were
    // added.
    var messageHandlerOrder = [];
    // Keep track of handlers by name. Maps name to handler function.
    var messageHandlers = {};

    // Two categories of message handlers: those that are from Shiny, and those
    // that are added by the user. The Shiny ones handle messages in
    // msgObj.values, msgObj.errors, and so on. The user ones handle messages
    // in msgObj.custom.foo and msgObj.custom.bar.
    var customMessageHandlerOrder = [];
    var customMessageHandlers = {};

    // Adds Shiny (internal) message handler
    function addMessageHandler(type, handler) {
      if (messageHandlers[type]) {
        throw 'handler for message of type "' + type + '" already added.';
      }
      if (typeof handler !== 'function') {
        throw 'handler must be a function.';
      }
      if (handler.length !== 1) {
        throw 'handler must be a function that takes one argument.';
      }
      messageHandlerOrder.push(type);
      messageHandlers[type] = handler;
    }

    // Adds custom message handler - this one is exposed to the user
    function addCustomMessageHandler(type, handler) {
      // Remove any previously defined handlers so that only the most recent one
      // will be called
      if (customMessageHandlers[type]) {
        var typeIdx = customMessageHandlerOrder.indexOf(type);
        if (typeIdx !== -1) {
          customMessageHandlerOrder.splice(typeIdx, 1);
          delete customMessageHandlers[type];
        }
      }
      if (typeof handler !== 'function') {
        throw 'handler must be a function.';
      }
      if (handler.length !== 1) {
        throw 'handler must be a function that takes one argument.';
      }

      customMessageHandlerOrder.push(type);
      customMessageHandlers[type] = handler;
    }

    exports.addCustomMessageHandler = addCustomMessageHandler;

    this.dispatchMessage = function (data) {
      var msgObj = {};
      if (typeof data === "string") {
        msgObj = JSON.parse(data);
      } else {
        // data is arraybuffer
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

      var evt = jQuery.Event('shiny:message');
      evt.message = msgObj;
      $(document).trigger(evt);
      if (evt.isDefaultPrevented()) return;

      // Send msgObj.foo and msgObj.bar to appropriate handlers
      this._sendMessagesToHandlers(evt.message, messageHandlers, messageHandlerOrder);

      this.$updateConditionals();
    };

    // A function for sending messages to the appropriate handlers.
    // - msgObj: the object containing messages, with format {msgObj.foo, msObj.bar
    this._sendMessagesToHandlers = function (msgObj, handlers, handlerOrder) {
      // Dispatch messages to handlers, if handler is present
      for (var i = 0; i < handlerOrder.length; i++) {
        var msgType = handlerOrder[i];
        if (msgObj.hasOwnProperty(msgType)) {
          // Execute each handler with 'this' referring to the present value of
          // 'this'
          handlers[msgType].call(this, msgObj[msgType]);
        }
      }
    };

    // Message handlers =====================================================

    addMessageHandler('values', function (message) {
      for (var name in this.$bindings) {
        if (this.$bindings.hasOwnProperty(name)) this.$bindings[name].showProgress(false);
      }

      for (var key in message) {
        if (message.hasOwnProperty(key)) this.receiveOutput(key, message[key]);
      }
    });

    addMessageHandler('errors', function (message) {
      for (var key in message) {
        if (message.hasOwnProperty(key)) this.receiveError(key, message[key]);
      }
    });

    addMessageHandler('inputMessages', function (message) {
      // inputMessages should be an array
      for (var i = 0; i < message.length; i++) {
        var $obj = $('.shiny-bound-input#' + $escape(message[i].id));
        var inputBinding = $obj.data('shiny-input-binding');

        // Dispatch the message to the appropriate input object
        if ($obj.length > 0) {
          var el = $obj[0];
          var evt = jQuery.Event('shiny:updateinput');
          evt.message = message[i].message;
          evt.binding = inputBinding;
          $(el).trigger(evt);
          if (!evt.isDefaultPrevented()) inputBinding.receiveMessage(el, evt.message);
        }
      }
    });

    addMessageHandler('javascript', function (message) {
      /*jshint evil: true */
      eval(message);
    });

    addMessageHandler('console', function (message) {
      for (var i = 0; i < message.length; i++) {
        if (console.log) console.log(message[i]);
      }
    });

    addMessageHandler('progress', function (message) {
      if (message.type && message.message) {
        var handler = progressHandlers[message.type];
        if (handler) handler.call(this, message.message);
      }
    });

    addMessageHandler('notification', function (message) {
      if (message.type === 'show') exports.notifications.show(message.message);else if (message.type === 'remove') exports.notifications.remove(message.message);else throw 'Unkown notification type: ' + message.type;
    });

    addMessageHandler('modal', function (message) {
      if (message.type === 'show') exports.modal.show(message.message);else if (message.type === 'remove') exports.modal.remove(); // For 'remove', message content isn't used
      else throw 'Unkown modal type: ' + message.type;
    });

    addMessageHandler('response', function (message) {
      var requestId = message.tag;
      var request = this.$activeRequests[requestId];
      if (request) {
        delete this.$activeRequests[requestId];
        if ('value' in message) request.onSuccess(message.value);else request.onError(message.error);
      }
    });

    addMessageHandler('allowReconnect', function (message) {
      if (message === true || message === false || message === "force") {
        this.$allowReconnect = message;
      } else {
        throw "Invalid value for allowReconnect: " + message;
      }
    });

    addMessageHandler('custom', function (message) {
      // For old-style custom messages - should deprecate and migrate to new
      // method
      if (exports.oncustommessage) {
        exports.oncustommessage(message);
      }

      // Send messages.foo and messages.bar to appropriate handlers
      this._sendMessagesToHandlers(message, customMessageHandlers, customMessageHandlerOrder);
    });

    addMessageHandler('config', function (message) {
      this.config = { workerId: message.workerId, sessionId: message.sessionId };
      if (message.user) exports.user = message.user;
      $(document).trigger('shiny:sessioninitialized');
    });

    addMessageHandler('busy', function (message) {
      if (message === 'busy') {
        $(document.documentElement).addClass('shiny-busy');
        $(document).trigger('shiny:busy');
      } else if (message === 'idle') {
        $(document.documentElement).removeClass('shiny-busy');
        $(document).trigger('shiny:idle');
      }
    });

    addMessageHandler('recalculating', function (message) {
      if (message.hasOwnProperty('name') && message.hasOwnProperty('status')) {
        var binding = this.$bindings[message.name];
        $(binding ? binding.el : null).trigger({
          type: 'shiny:' + message.status
        });
      }
    });

    addMessageHandler('reload', function (message) {
      window.location.reload();
    });

    addMessageHandler('shiny-insert-ui', function (message) {
      var targets = $(message.selector);
      if (targets.length === 0) {
        // render the HTML and deps to a null target, so
        // the side-effect of rendering the deps, singletons,
        // and <head> still occur
        console.warn('The selector you chose ("' + message.selector + '") could not be found in the DOM.');
        exports.renderHtml(message.content.html, $([]), message.content.deps);
      } else {
        targets.each(function (i, target) {
          exports.renderContent(target, message.content, message.where);
          return message.multiple;
        });
      }
    });

    addMessageHandler('shiny-remove-ui', function (message) {
      var els = $(message.selector);
      els.each(function (i, el) {
        exports.unbindAll(el, true);
        $(el).remove();
        // If `multiple` is false, returning false terminates the function
        // and no other elements are removed; if `multiple` is true,
        // returning true continues removing all remaining elements.
        return message.multiple;
      });
    });

    addMessageHandler('updateQueryString', function (message) {

      // leave the bookmarking code intact
      if (message.mode === "replace") {
        window.history.replaceState(null, null, message.queryString);
        return;
      }

      var what = null;
      if (message.queryString.charAt(0) === "#") what = "hash";else if (message.queryString.charAt(0) === "?") what = "query";else throw "The 'query' string must start with either '?' " + "(to update the query string) or with '#' (to " + "update the hash).";

      var path = window.location.pathname;
      var oldQS = window.location.search;
      var oldHash = window.location.hash;

      /* Barbara -- December 2016
      Note: we could check if the new QS and/or hash are different
      from the old one(s) and, if not, we could choose not to push
      a new state (whether or not we would replace it is moot/
      inconsequential). However, I think that it is better to
      interpret each call to `updateQueryString` as representing
      new state (even if the message.queryString is the same), so
      that check isn't even performed as of right now.
      */

      var relURL = path;
      if (what === "query") relURL += message.queryString;else relURL += oldQS + message.queryString; // leave old QS if it exists
      window.history.pushState(null, null, relURL);

      // for the case when message.queryString has both a query string
      // and a hash (`what = "hash"` allows us to trigger the
      // hashchange event)
      if (message.queryString.indexOf("#") !== -1) what = "hash";

      // for the case when there was a hash before, but there isn't
      // any hash now (e.g. for when only the query string is updated)
      if (window.location.hash !== oldHash) what = "hash";

      // This event needs to be triggered manually because pushState() never
      // causes a hashchange event to be fired,
      if (what === "hash") $(document).trigger("hashchange");
    });

    addMessageHandler("resetBrush", function (message) {
      exports.resetBrush(message.brushId);
    });

    // Progress reporting ====================================================

    var progressHandlers = {
      // Progress for a particular object
      binding: function binding(message) {
        var key = message.id;
        var binding = this.$bindings[key];
        if (binding && binding.showProgress) {
          binding.showProgress(true);
        }
      },

      // Open a page-level progress bar
      open: function open(message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.

          // Progress bar starts hidden; will be made visible if a value is provided
          // during updates.
          exports.notifications.show({
            html: '<div id="shiny-progress-' + message.id + '" class="shiny-progress-notification">' + '<div class="progress progress-striped active" style="display: none;"><div class="progress-bar"></div></div>' + '<div class="progress-text">' + '<span class="progress-message">message</span> ' + '<span class="progress-detail"></span>' + '</div>' + '</div>',
            id: message.id,
            duration: null
          });
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          // Add progress container (for all progress items) if not already present
          var $container = $('.shiny-progress-container');
          if ($container.length === 0) {
            $container = $('<div class="shiny-progress-container"></div>');
            $('body').append($container);
          }

          // Add div for just this progress ID
          var depth = $('.shiny-progress.open').length;
          // The 'bar' class is needed for backward compatibility with Bootstrap 2.
          var $progress = $('<div class="shiny-progress open">' + '<div class="progress progress-striped active"><div class="progress-bar bar"></div></div>' + '<div class="progress-text">' + '<span class="progress-message">message</span>' + '<span class="progress-detail"></span>' + '</div>' + '</div>');

          $progress.attr('id', message.id);
          $container.append($progress);

          // Stack bars
          var $progressBar = $progress.find('.progress');
          $progressBar.css('top', depth * $progressBar.height() + 'px');

          // Stack text objects
          var $progressText = $progress.find('.progress-text');
          $progressText.css('top', 3 * $progressBar.height() + depth * $progressText.outerHeight() + 'px');

          $progress.hide();
        }
      },

      // Update page-level progress bar
      update: function update(message) {
        if (message.style === "notification") {
          // For new-style (starting in Shiny 0.14) progress indicators that use
          // the notification API.
          var $progress = $('#shiny-progress-' + message.id);

          if ($progress.length === 0) return;

          if (typeof message.message !== 'undefined') {
            $progress.find('.progress-message').text(message.message);
          }
          if (typeof message.detail !== 'undefined') {
            $progress.find('.progress-detail').text(message.detail);
          }
          if (typeof message.value !== 'undefined' && message.value !== null) {
            $progress.find('.progress').show();
            $progress.find('.progress-bar').width(message.value * 100 + '%');
          }
        } else if (message.style === "old") {
          // For old-style (Shiny <=0.13.2) progress indicators.

          var $progress = $('#' + message.id + '.shiny-progress');
          if (typeof message.message !== 'undefined') {
            $progress.find('.progress-message').text(message.message);
          }
          if (typeof message.detail !== 'undefined') {
            $progress.find('.progress-detail').text(message.detail);
          }
          if (typeof message.value !== 'undefined' && message.value !== null) {
            $progress.find('.progress').show();
            $progress.find('.bar').width(message.value * 100 + '%');
          }

          $progress.fadeIn();
        }
      },

      // Close page-level progress bar
      close: function close(message) {
        if (message.style === "notification") {
          exports.notifications.remove(message.id);
        } else if (message.style === "old") {
          var $progress = $('#' + message.id + '.shiny-progress');
          $progress.removeClass('open');

          $progress.fadeOut({
            complete: function complete() {
              $progress.remove();

              // If this was the last shiny-progress, remove container
              if ($('.shiny-progress').length === 0) $('.shiny-progress-container').remove();
            }
          });
        }
      }
    };

    exports.progressHandlers = progressHandlers;

    // Returns a URL which can be queried to get values from inside the server
    // function. This is enabled with `options(shiny.testmode=TRUE)`.
    this.getTestSnapshotBaseUrl = function () {
      var _ref4 = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      var _ref4$fullUrl = _ref4.fullUrl;
      var fullUrl = _ref4$fullUrl === undefined ? true : _ref4$fullUrl;

      var loc = window.location;
      var url = "";

      if (fullUrl) {
        // Strip off everything after last slash in path, like dirname() in R
        url = loc.origin + loc.pathname.replace(/\/[^/]*$/, "");
      }
      url += "/session/" + encodeURIComponent(this.config.sessionId) + "/dataobj/shinytest?w=" + encodeURIComponent(this.config.workerId) + "&nonce=" + randomId();

      return url;
    };
  }).call(ShinyApp.prototype);

  exports.showReconnectDialog = function () {
    var reconnectTime = null;

    function updateTime() {
      var $time = $("#shiny-reconnect-time");
      // If the time has been removed, exit and don't reschedule this function.
      if ($time.length === 0) return;

      var seconds = Math.floor((reconnectTime - new Date().getTime()) / 1000);
      if (seconds > 0) {
        $time.text(" in " + seconds + "s");
      } else {
        $time.text("...");
      }

      // Reschedule this function after 1 second
      setTimeout(updateTime, 1000);
    }

    return function (delay) {
      reconnectTime = new Date().getTime() + delay;

      // If there's already a reconnect dialog, don't add another
      if ($('#shiny-reconnect-text').length > 0) return;

      var html = '<span id="shiny-reconnect-text">Attempting to reconnect</span>' + '<span id="shiny-reconnect-time"></span>';
      var action = '<a id="shiny-reconnect-now" href="#" onclick="Shiny.shinyapp.reconnect();">Try now</a>';

      exports.notifications.show({
        id: "reconnect",
        html: html,
        action: action,
        duration: null,
        closeButton: false,
        type: 'warning'
      });

      updateTime();
    };
  }();

  exports.hideReconnectDialog = function () {
    exports.notifications.remove("reconnect");
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/notifications.js

  exports.notifications = function () {

    // Milliseconds to fade in or out
    var fadeDuration = 250;

    function show() {
      var _ref5 = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      var _ref5$html = _ref5.html;
      var html = _ref5$html === undefined ? '' : _ref5$html;
      var _ref5$action = _ref5.action;
      var action = _ref5$action === undefined ? '' : _ref5$action;
      var _ref5$deps = _ref5.deps;
      var deps = _ref5$deps === undefined ? [] : _ref5$deps;
      var _ref5$duration = _ref5.duration;
      var duration = _ref5$duration === undefined ? 5000 : _ref5$duration;
      var _ref5$id = _ref5.id;
      var id = _ref5$id === undefined ? null : _ref5$id;
      var _ref5$closeButton = _ref5.closeButton;
      var closeButton = _ref5$closeButton === undefined ? true : _ref5$closeButton;
      var _ref5$type = _ref5.type;
      var type = _ref5$type === undefined ? null : _ref5$type;

      if (!id) id = randomId();

      // Create panel if necessary
      _createPanel();

      // Get existing DOM element for this ID, or create if needed.
      var $notification = _get(id);
      if ($notification.length === 0) $notification = _create(id);

      // Render html and dependencies
      var newHtml = '<div class="shiny-notification-content-text">' + html + '</div>' + ('<div class="shiny-notification-content-action">' + action + '</div>');
      var $content = $notification.find('.shiny-notification-content');
      exports.renderContent($content, { html: newHtml, deps: deps });

      // Remove any existing classes of the form 'shiny-notification-xxxx'.
      // The xxxx would be strings like 'warning'.
      var classes = $notification.attr('class').split(/\s+/).filter(function (cls) {
        return cls.match(/^shiny-notification-/);
      }).join(' ');
      $notification.removeClass(classes);

      // Add class. 'default' means no additional CSS class.
      if (type && type !== 'default') $notification.addClass('shiny-notification-' + type);

      // Make sure that the presence/absence of close button matches with value
      // of `closeButton`.
      var $close = $notification.find('.shiny-notification-close');
      if (closeButton && $close.length === 0) {
        $notification.append('<div class="shiny-notification-close">&times;</div>');
      } else if (!closeButton && $close.length !== 0) {
        $close.remove();
      }

      // If duration was provided, schedule removal. If not, clear existing
      // removal callback (this happens if a message was first added with
      // a duration, and then updated with no duration).
      if (duration) _addRemovalCallback(id, duration);else _clearRemovalCallback(id);

      return id;
    }

    function remove(id) {
      _get(id).fadeOut(fadeDuration, function () {

        exports.unbindAll(this);
        $(this).remove();

        // If no more notifications, remove the panel from the DOM.
        if (_ids().length === 0) {
          _getPanel().remove();
        }
      });
    }

    // Returns an individual notification DOM object (wrapped in jQuery).
    function _get(id) {
      if (!id) return null;
      return _getPanel().find('#shiny-notification-' + $escape(id));
    }

    // Return array of all notification IDs
    function _ids() {
      return _getPanel().find('.shiny-notification').map(function () {
        return this.id.replace(/shiny-notification-/, '');
      }).get();
    }

    // Returns the notification panel DOM object (wrapped in jQuery).
    function _getPanel() {
      return $('#shiny-notification-panel');
    }

    // Create notifications panel and return the jQuery object. If the DOM
    // element already exists, just return it.
    function _createPanel() {
      var $panel = _getPanel();

      if ($panel.length > 0) return $panel;

      $('body').append('<div id="shiny-notification-panel">');

      return $panel;
    }

    // Create a notification DOM element and return the jQuery object. If the
    // DOM element already exists for the ID, just return it without creating.
    function _create(id) {
      var $notification = _get(id);

      if ($notification.length === 0) {
        $notification = $('<div id="shiny-notification-' + id + '" class="shiny-notification">' + '<div class="shiny-notification-close">&times;</div>' + '<div class="shiny-notification-content"></div>' + '</div>');

        $notification.find('.shiny-notification-close').on('click', function (e) {
          e.preventDefault();
          e.stopPropagation();
          remove(id);
        });

        _getPanel().append($notification);
      }

      return $notification;
    }

    // Add a callback to remove a notification after a delay in ms.
    function _addRemovalCallback(id, delay) {
      // If there's an existing removalCallback, clear it before adding the new
      // one.
      _clearRemovalCallback(id);

      // Attach new removal callback
      var removalCallback = setTimeout(function () {
        remove(id);
      }, delay);
      _get(id).data('removalCallback', removalCallback);
    }

    // Clear a removal callback from a notification, if present.
    function _clearRemovalCallback(id) {
      var $notification = _get(id);
      var oldRemovalCallback = $notification.data('removalCallback');
      if (oldRemovalCallback) {
        clearTimeout(oldRemovalCallback);
      }
    }

    return {
      show: show,
      remove: remove
    };
  }();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/modal.js

  exports.modal = {

    // Show a modal dialog. This is meant to handle two types of cases: one is
    // that the content is a Bootstrap modal dialog, and the other is that the
    // content is non-Bootstrap. Bootstrap modals require some special handling,
    // which is coded in here.
    show: function show() {
      var _ref6 = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

      var _ref6$html = _ref6.html;
      var html = _ref6$html === undefined ? '' : _ref6$html;
      var _ref6$deps = _ref6.deps;
      var deps = _ref6$deps === undefined ? [] : _ref6$deps;


      // If there was an existing Bootstrap modal, then there will be a modal-
      // backdrop div that was added outside of the modal wrapper, and it must be
      // removed; otherwise there can be multiple of these divs.
      $('.modal-backdrop').remove();

      // Get existing wrapper DOM element, or create if needed.
      var $modal = $('#shiny-modal-wrapper');
      if ($modal.length === 0) {
        $modal = $('<div id="shiny-modal-wrapper"></div>');
        $('body').append($modal);

        // If the wrapper's content is a Bootstrap modal, then when the inner
        // modal is hidden, remove the entire thing, including wrapper.
        $modal.on('hidden.bs.modal', function (e) {
          if (e.target === $("#shiny-modal")[0]) {
            exports.unbindAll($modal);
            $modal.remove();
          }
        });
      }

      $modal.on('keydown.shinymodal', function (e) {
        // If we're listening for Esc, don't let the event propagate. See
        // https://github.com/rstudio/shiny/issues/1453. The value of
        // data("keyboard") needs to be checked inside the handler, because at
        // the time that $modal.on() is called, the $("#shiny-modal") div doesn't
        // yet exist.
        if ($("#shiny-modal").data("keyboard") === false) return;

        if (e.keyCode === 27) {
          e.stopPropagation();
          e.preventDefault();
        }
      });

      // Set/replace contents of wrapper with html.
      exports.renderContent($modal, { html: html, deps: deps });
    },

    remove: function remove() {
      var $modal = $('#shiny-modal-wrapper');

      $modal.off('keydown.shinymodal');

      // Look for a Bootstrap modal and if present, trigger hide event. This will
      // trigger the hidden.bs.modal callback that we set in show(), which unbinds
      // and removes the element.
      if ($modal.find('.modal').length > 0) {
        $modal.find('.modal').modal('hide');
      } else {
        // If not a Bootstrap modal dialog, simply unbind and remove it.
        exports.unbindAll($modal);
        $modal.remove();
      }
    }
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/file_processor.js

  // Generic driver class for doing chunk-wise asynchronous processing of a
  // FileList object. Subclass/clone it and override the `on*` functions to
  // make it do something useful.
  var FileProcessor = function FileProcessor(files) {
    this.files = files;
    this.fileIndex = -1;
    // Currently need to use small chunk size because R-Websockets can't
    // handle continuation frames
    this.aborted = false;
    this.completed = false;

    // TODO: Register error/abort callbacks

    this.$run();
  };
  (function () {
    // Begin callbacks. Subclassers/cloners may override any or all of these.
    this.onBegin = function (files, cont) {
      setTimeout(cont, 0);
    };
    this.onFile = function (file, cont) {
      setTimeout(cont, 0);
    };
    this.onComplete = function () {};
    this.onAbort = function () {};
    // End callbacks

    // Aborts processing, unless it's already completed
    this.abort = function () {
      if (this.completed || this.aborted) return;

      this.aborted = true;
      this.onAbort();
    };

    // Returns a bound function that will call this.$run one time.
    this.$getRun = function () {
      var self = this;
      var called = false;
      return function () {
        if (called) return;
        called = true;
        self.$run();
      };
    };

    // This function will be called multiple times to advance the process.
    // It relies on the state of the object's fields to know what to do next.
    this.$run = function () {

      if (this.aborted || this.completed) return;

      if (this.fileIndex < 0) {
        // Haven't started yet--begin
        this.fileIndex = 0;
        this.onBegin(this.files, this.$getRun());
        return;
      }

      if (this.fileIndex === this.files.length) {
        // Just ended
        this.completed = true;
        this.onComplete();
        return;
      }

      // If we got here, then we have a file to process, or we are
      // in the middle of processing a file, or have just finished
      // processing a file.

      var file = this.files[this.fileIndex++];
      this.onFile(file, this.$getRun());
    };
  }).call(FileProcessor.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/binding_registry.js

  var BindingRegistry = function BindingRegistry() {
    this.bindings = [];
    this.bindingNames = {};
  };
  (function () {
    this.register = function (binding, bindingName, priority) {
      var bindingObj = { binding: binding, priority: priority || 0 };
      this.bindings.unshift(bindingObj);
      if (bindingName) {
        this.bindingNames[bindingName] = bindingObj;
        binding.name = bindingName;
      }
    };
    this.setPriority = function (bindingName, priority) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj) throw "Tried to set priority on unknown binding " + bindingName;
      bindingObj.priority = priority || 0;
    };
    this.getPriority = function (bindingName) {
      var bindingObj = this.bindingNames[bindingName];
      if (!bindingObj) return false;
      return bindingObj.priority;
    };
    this.getBindings = function () {
      // Sort the bindings. The ones with higher priority are consulted
      // first; ties are broken by most-recently-registered.
      return mergeSort(this.bindings, function (a, b) {
        return b.priority - a.priority;
      });
    };
  }).call(BindingRegistry.prototype);

  var inputBindings = exports.inputBindings = new BindingRegistry();
  var outputBindings = exports.outputBindings = new BindingRegistry();

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding.js

  var OutputBinding = exports.OutputBinding = function () {};
  (function () {
    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el['data-input-id'] || el.id;
    };

    this.onValueChange = function (el, data) {
      this.clearError(el);
      this.renderValue(el, data);
    };
    this.onValueError = function (el, err) {
      this.renderError(el, err);
    };
    this.renderError = function (el, err) {
      this.clearError(el);
      if (err.message === '') {
        // not really error, but we just need to wait (e.g. action buttons)
        $(el).empty();
        return;
      }
      var errClass = 'shiny-output-error';
      if (err.type !== null) {
        // use the classes of the error condition as CSS class names
        errClass = errClass + ' ' + $.map(asArray(err.type), function (type) {
          return errClass + '-' + type;
        }).join(' ');
      }
      $(el).addClass(errClass).text(err.message);
    };
    this.clearError = function (el) {
      $(el).attr('class', function (i, c) {
        return c.replace(/(^|\s)shiny-output-error\S*/g, '');
      });
    };
    this.showProgress = function (el, show) {
      var RECALC_CLASS = 'recalculating';
      if (show) $(el).addClass(RECALC_CLASS);else $(el).removeClass(RECALC_CLASS);
    };
  }).call(OutputBinding.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_text.js

  var textOutputBinding = new OutputBinding();
  $.extend(textOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-text-output');
    },
    renderValue: function renderValue(el, data) {
      $(el).text(data);
    }
  });
  outputBindings.register(textOutputBinding, 'shiny.textOutput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_image.js

  var imageOutputBinding = new OutputBinding();
  $.extend(imageOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-image-output, .shiny-plot-output');
    },
    renderValue: function renderValue(el, data) {
      // The overall strategy:
      // * Clear out existing image and event handlers.
      // * Create new image.
      // * Create various event handlers.
      // * Bind those event handlers to events.
      // * Insert the new image.

      var outputId = this.getId(el);

      var $el = $(el);
      var img;

      // Remove event handlers that were added in previous renderValue()
      $el.off('.image_output');

      // Get existing img element if present.
      var $img = $el.find('img');

      if ($img.length === 0) {
        // If a img element is not already present, that means this is either
        // the first time renderValue() has been called, or this is after an
        // error.
        img = document.createElement('img');
        $el.append(img);
        $img = $(img);
      } else {
        // Trigger custom 'reset' event for any existing images in the div
        img = $img[0];
        $img.trigger('reset');
      }

      if (!data) {
        $el.empty();
        return;
      }

      // If value is undefined, return alternate. Sort of like ||, except it won't
      // return alternate for other falsy values (0, false, null).
      function OR(value, alternate) {
        if (value === undefined) return alternate;
        return value;
      }

      var opts = {
        clickId: $el.data('click-id'),
        clickClip: OR(strToBool($el.data('click-clip')), true),

        dblclickId: $el.data('dblclick-id'),
        dblclickClip: OR(strToBool($el.data('dblclick-clip')), true),
        dblclickDelay: OR($el.data('dblclick-delay'), 400),

        hoverId: $el.data('hover-id'),
        hoverClip: OR(strToBool($el.data('hover-clip')), true),
        hoverDelayType: OR($el.data('hover-delay-type'), 'debounce'),
        hoverDelay: OR($el.data('hover-delay'), 300),
        hoverNullOutside: OR(strToBool($el.data('hover-null-outside')), false),

        brushId: $el.data('brush-id'),
        brushClip: OR(strToBool($el.data('brush-clip')), true),
        brushDelayType: OR($el.data('brush-delay-type'), 'debounce'),
        brushDelay: OR($el.data('brush-delay'), 300),
        brushFill: OR($el.data('brush-fill'), '#666'),
        brushStroke: OR($el.data('brush-stroke'), '#000'),
        brushOpacity: OR($el.data('brush-opacity'), 0.3),
        brushDirection: OR($el.data('brush-direction'), 'xy'),
        brushResetOnNew: OR(strToBool($el.data('brush-reset-on-new')), false),

        coordmap: data.coordmap
      };

      // Copy items from data to img. Don't set the coordmap as an attribute.
      $.each(data, function (key, value) {
        if (value === null || key === 'coordmap') {
          return;
        }
        img.setAttribute(key, value);
      });

      // Unset any attributes in the current img that were not provided in the
      // new data.
      for (var i = 0; i < img.attributes.length; i++) {
        var attrib = img.attributes[i];
        // Need to check attrib.specified on IE because img.attributes contains
        // all possible attributes on IE.
        if (attrib.specified && !data.hasOwnProperty(attrib.name)) {
          img.removeAttribute(attrib.name);
        }
      }

      if (!opts.coordmap) opts.coordmap = [];

      imageutils.initCoordmap($el, opts.coordmap);

      // This object listens for mousedowns, and triggers mousedown2 and dblclick2
      // events as appropriate.
      var clickInfo = imageutils.createClickInfo($el, opts.dblclickId, opts.dblclickDelay);

      $el.on('mousedown.image_output', clickInfo.mousedown);

      if (browser.isIE && browser.IEVersion === 8) {
        $el.on('dblclick.image_output', clickInfo.dblclickIE8);
      }

      // ----------------------------------------------------------
      // Register the various event handlers
      // ----------------------------------------------------------
      if (opts.clickId) {
        var clickHandler = imageutils.createClickHandler(opts.clickId, opts.clickClip, opts.coordmap);
        $el.on('mousedown2.image_output', clickHandler.mousedown);

        // When img is reset, do housekeeping: clear $el's mouse listener and
        // call the handler's onResetImg callback.
        $img.on('reset', clickHandler.onResetImg);
      }

      if (opts.dblclickId) {
        // We'll use the clickHandler's mousedown function, but register it to
        // our custom 'dblclick2' event.
        var dblclickHandler = imageutils.createClickHandler(opts.dblclickId, opts.clickClip, opts.coordmap);
        $el.on('dblclick2.image_output', dblclickHandler.mousedown);

        $img.on('reset', dblclickHandler.onResetImg);
      }

      if (opts.hoverId) {
        var hoverHandler = imageutils.createHoverHandler(opts.hoverId, opts.hoverDelay, opts.hoverDelayType, opts.hoverClip, opts.hoverNullOutside, opts.coordmap);
        $el.on('mousemove.image_output', hoverHandler.mousemove);
        $el.on('mouseout.image_output', hoverHandler.mouseout);

        $img.on('reset', hoverHandler.onResetImg);
      }

      if (opts.brushId) {
        // Make image non-draggable (Chrome, Safari)
        $img.css('-webkit-user-drag', 'none');
        // Firefox, IE<=10
        $img.on('dragstart', function () {
          return false;
        });

        // Disable selection of image and text when dragging in IE<=10
        $el.on('selectstart.image_output', function () {
          return false;
        });

        var brushHandler = imageutils.createBrushHandler(opts.brushId, $el, opts, opts.coordmap, outputId);
        $el.on('mousedown.image_output', brushHandler.mousedown);
        $el.on('mousemove.image_output', brushHandler.mousemove);

        $img.on('reset', brushHandler.onResetImg);
      }

      if (opts.clickId || opts.dblclickId || opts.hoverId || opts.brushId) {
        $el.addClass('crosshair');
      }

      if (data.error) console.log('Error on server extracting coordmap: ' + data.error);
    },

    renderError: function renderError(el, err) {
      $(el).find('img').trigger('reset');
      OutputBinding.prototype.renderError.call(this, el, err);
    },

    clearError: function clearError(el) {
      // Remove all elements except img and the brush; this is usually just
      // error messages.
      $(el).contents().filter(function () {
        return this.tagName !== "IMG" && this.id !== el.id + '_brush';
      }).remove();

      OutputBinding.prototype.clearError.call(this, el);
    }
  });
  outputBindings.register(imageOutputBinding, 'shiny.imageOutput');

  var imageutils = {};

  // Modifies the panel objects in a coordmap, adding scale(), scaleInv(),
  // and clip() functions to each one.
  imageutils.initPanelScales = function (coordmap) {
    // Map a value x from a domain to a range. If clip is true, clip it to the
    // range.
    function mapLinear(x, domainMin, domainMax, rangeMin, rangeMax, clip) {
      // By default, clip to range
      clip = clip || true;

      var factor = (rangeMax - rangeMin) / (domainMax - domainMin);
      var val = x - domainMin;
      var newval = val * factor + rangeMin;

      if (clip) {
        var max = Math.max(rangeMax, rangeMin);
        var min = Math.min(rangeMax, rangeMin);
        if (newval > max) newval = max;else if (newval < min) newval = min;
      }
      return newval;
    }

    // Create scale and inverse-scale functions for a single direction (x or y).
    function scaler1D(domainMin, domainMax, rangeMin, rangeMax, logbase) {
      return {
        scale: function scale(val, clip) {
          if (logbase) val = Math.log(val) / Math.log(logbase);
          return mapLinear(val, domainMin, domainMax, rangeMin, rangeMax, clip);
        },

        scaleInv: function scaleInv(val, clip) {
          var res = mapLinear(val, rangeMin, rangeMax, domainMin, domainMax, clip);
          if (logbase) res = Math.pow(logbase, res);
          return res;
        }
      };
    }

    // Modify panel, adding scale and inverse-scale functions that take objects
    // like {x:1, y:3}, and also add clip function.
    function addScaleFuns(panel) {
      var d = panel.domain;
      var r = panel.range;
      var xlog = panel.log && panel.log.x ? panel.log.x : null;
      var ylog = panel.log && panel.log.y ? panel.log.y : null;
      var xscaler = scaler1D(d.left, d.right, r.left, r.right, xlog);
      var yscaler = scaler1D(d.bottom, d.top, r.bottom, r.top, ylog);

      panel.scale = function (val, clip) {
        return {
          x: xscaler.scale(val.x, clip),
          y: yscaler.scale(val.y, clip)
        };
      };

      panel.scaleInv = function (val, clip) {
        return {
          x: xscaler.scaleInv(val.x, clip),
          y: yscaler.scaleInv(val.y, clip)
        };
      };

      // Given a scaled offset (in pixels), clip it to the nearest panel region.
      panel.clip = function (offset) {
        var newOffset = {
          x: offset.x,
          y: offset.y
        };

        var bounds = panel.range;

        if (offset.x > bounds.right) newOffset.x = bounds.right;else if (offset.x < bounds.left) newOffset.x = bounds.left;

        if (offset.y > bounds.bottom) newOffset.y = bounds.bottom;else if (offset.y < bounds.top) newOffset.y = bounds.top;

        return newOffset;
      };
    }

    // Add the functions to each panel object.
    for (var i = 0; i < coordmap.length; i++) {
      var panel = coordmap[i];
      addScaleFuns(panel);
    }
  };

  // This adds functions to the coordmap object to handle various
  // coordinate-mapping tasks, and send information to the server.
  // The input coordmap is an array of objects, each of which represents a panel.
  // coordmap must be an array, even if empty, so that it can be modified in
  // place; when empty, we add a dummy panel to the array.
  // It also calls initPanelScales, which modifies each panel object to have
  // scale, scaleInv, and clip functions.
  imageutils.initCoordmap = function ($el, coordmap) {
    var el = $el[0];

    // If we didn't get any panels, create a dummy one where the domain and range
    // are simply the pixel dimensions.
    // that we modify.
    if (coordmap.length === 0) {
      var bounds = {
        top: 0,
        left: 0,
        right: el.clientWidth - 1,
        bottom: el.clientHeight - 1
      };

      coordmap[0] = {
        domain: bounds,
        range: bounds,
        mapping: {}
      };
    }

    // Add scaling functions to each panel
    imageutils.initPanelScales(coordmap);

    // Firefox doesn't have offsetX/Y, so we need to use an alternate
    // method of calculation for it. Even though other browsers do have
    // offsetX/Y, we need to calculate relative to $el, because sometimes the
    // mouse event can come with offset relative to other elements on the
    // page. This happens when the event listener is bound to, say, window.
    coordmap.mouseOffset = function (mouseEvent) {
      var offset = $el.offset();
      return {
        x: mouseEvent.pageX - offset.left,
        y: mouseEvent.pageY - offset.top
      };
    };

    // Given two sets of x/y coordinates, return an object representing the
    // min and max x and y values. (This could be generalized to any number
    // of points).
    coordmap.findBox = function (offset1, offset2) {
      return {
        xmin: Math.min(offset1.x, offset2.x),
        xmax: Math.max(offset1.x, offset2.x),
        ymin: Math.min(offset1.y, offset2.y),
        ymax: Math.max(offset1.y, offset2.y)
      };
    };

    // Shift an array of values so that they are within a min and max.
    // The vals will be shifted so that they maintain the same spacing
    // internally. If the range in vals is larger than the range of
    // min and max, the result might not make sense.
    coordmap.shiftToRange = function (vals, min, max) {
      if (!(vals instanceof Array)) vals = [vals];

      var maxval = Math.max.apply(null, vals);
      var minval = Math.min.apply(null, vals);
      var shiftAmount = 0;
      if (maxval > max) {
        shiftAmount = max - maxval;
      } else if (minval < min) {
        shiftAmount = min - minval;
      }

      var newvals = [];
      for (var i = 0; i < vals.length; i++) {
        newvals[i] = vals[i] + shiftAmount;
      }
      return newvals;
    };

    // Given an offset, return an object representing which panel it's in. The
    // `expand` argument tells it to expand the panel area by that many pixels.
    // It's possible for an offset to be within more than one panel, because
    // of the `expand` value. If that's the case, find the nearest panel.
    coordmap.getPanel = function (offset, expand) {
      expand = expand || 0;

      var x = offset.x;
      var y = offset.y;

      var matches = []; // Panels that match
      var dists = []; // Distance of offset to each matching panel
      var b;
      for (var i = 0; i < coordmap.length; i++) {
        b = coordmap[i].range;

        if (x <= b.right + expand && x >= b.left - expand && y <= b.bottom + expand && y >= b.top - expand) {
          matches.push(coordmap[i]);

          // Find distance from edges for x and y
          var xdist = 0;
          var ydist = 0;
          if (x > b.right && x <= b.right + expand) {
            xdist = x - b.right;
          } else if (x < b.left && x >= b.left - expand) {
            xdist = x - b.left;
          }
          if (y > b.bottom && y <= b.bottom + expand) {
            ydist = y - b.bottom;
          } else if (y < b.top && y >= b.top - expand) {
            ydist = y - b.top;
          }

          // Cartesian distance
          dists.push(Math.sqrt(Math.pow(xdist, 2) + Math.pow(ydist, 2)));
        }
      }

      if (matches.length) {
        // Find shortest distance
        var min_dist = Math.min.apply(null, dists);
        for (i = 0; i < matches.length; i++) {
          if (dists[i] === min_dist) {
            return matches[i];
          }
        }
      }

      return null;
    };

    // Is an offset in a panel? If supplied, `expand` tells us to expand the
    // panels by that many pixels in all directions.
    coordmap.isInPanel = function (offset, expand) {
      expand = expand || 0;

      if (coordmap.getPanel(offset, expand)) return true;

      return false;
    };

    // Returns a function that sends mouse coordinates, scaled to data space.
    // If that function is passed a null event, it will send null.
    coordmap.mouseCoordinateSender = function (inputId, clip, nullOutside) {
      if (clip === undefined) clip = true;
      if (nullOutside === undefined) nullOutside = false;

      return function (e) {
        if (e === null) {
          exports.onInputChange(inputId, null);
          return;
        }

        var offset = coordmap.mouseOffset(e);
        // If outside of plotting region
        if (!coordmap.isInPanel(offset)) {
          if (nullOutside) {
            exports.onInputChange(inputId, null);
            return;
          }
          if (clip) return;
        }
        if (clip && !coordmap.isInPanel(offset)) return;

        var panel = coordmap.getPanel(offset);
        var coords = panel.scaleInv(offset);

        // Add the panel (facet) variables, if present
        $.extend(coords, panel.panel_vars);

        // Add variable name mappings
        coords.mapping = panel.mapping;

        // Add scaling information
        coords.domain = panel.domain;
        coords.range = panel.range;
        coords.log = panel.log;

        coords[".nonce"] = Math.random();
        exports.onInputChange(inputId, coords);
      };
    };
  };

  // This object provides two public event listeners: mousedown, and
  // dblclickIE8.
  // We need to make sure that, when the image is listening for double-
  // clicks, that a double-click doesn't trigger two click events. We'll
  // trigger custom mousedown2 and dblclick2 events with this mousedown
  // listener.
  imageutils.createClickInfo = function ($el, dblclickId, dblclickDelay) {
    var clickTimer = null;
    var pending_e = null; // A pending mousedown2 event

    // Create a new event of type eventType (like 'mousedown2'), and trigger
    // it with the information stored in this.e.
    function triggerEvent(newEventType, e) {
      // Extract important info from e and construct a new event with type
      // eventType.
      var e2 = $.Event(newEventType, {
        which: e.which,
        pageX: e.pageX,
        pageY: e.pageY,
        offsetX: e.offsetX,
        offsetY: e.offsetY
      });

      $el.trigger(e2);
    }

    function triggerPendingMousedown2() {
      // It's possible that between the scheduling of a mousedown2 and the
      // time this callback is executed, someone else triggers a
      // mousedown2, so check for that.
      if (pending_e) {
        triggerEvent('mousedown2', pending_e);
        pending_e = null;
      }
    }

    // Set a timer to trigger a mousedown2 event, using information from the
    // last recorded mousdown event.
    function scheduleMousedown2(e) {
      pending_e = e;

      clickTimer = setTimeout(function () {
        triggerPendingMousedown2();
      }, dblclickDelay);
    }

    function mousedown(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      // If no dblclick listener, immediately trigger a mousedown2 event.
      if (!dblclickId) {
        triggerEvent('mousedown2', e);
        return;
      }

      // If there's a dblclick listener, make sure not to count this as a
      // click on the first mousedown; we need to wait for the dblclick
      // delay before we can be sure this click was a single-click.
      if (pending_e === null) {
        scheduleMousedown2(e);
      } else {
        clearTimeout(clickTimer);

        // If second click is too far away, it doesn't count as a double
        // click. Instead, immediately trigger a mousedown2 for the previous
        // click, and set this click as a new first click.
        if (pending_e && Math.abs(pending_e.offsetX - e.offsetX) > 2 || Math.abs(pending_e.offsetY - e.offsetY) > 2) {

          triggerPendingMousedown2();
          scheduleMousedown2(e);
        } else {
          // The second click was close to the first one. If it happened
          // within specified delay, trigger our custom 'dblclick2' event.
          pending_e = null;
          triggerEvent('dblclick2', e);
        }
      }
    }

    // IE8 needs a special hack because when you do a double-click it doesn't
    // trigger the click event twice - it directly triggers dblclick.
    function dblclickIE8(e) {
      e.which = 1; // In IE8, e.which is 0 instead of 1. ???
      triggerEvent('dblclick2', e);
    }

    return {
      mousedown: mousedown,
      dblclickIE8: dblclickIE8
    };
  };

  // ----------------------------------------------------------
  // Handler creators for click, hover, brush.
  // Each of these returns an object with a few public members. These public
  // members are callbacks that are meant to be bound to events on $el with
  // the same name (like 'mousedown').
  // ----------------------------------------------------------

  imageutils.createClickHandler = function (inputId, clip, coordmap) {
    var clickInfoSender = coordmap.mouseCoordinateSender(inputId, clip);

    return {
      mousedown: function mousedown(e) {
        // Listen for left mouse button only
        if (e.which !== 1) return;
        clickInfoSender(e);
      },
      onResetImg: function onResetImg() {
        clickInfoSender(null);
      }
    };
  };

  imageutils.createHoverHandler = function (inputId, delay, delayType, clip, nullOutside, coordmap) {
    var sendHoverInfo = coordmap.mouseCoordinateSender(inputId, clip, nullOutside);

    var hoverInfoSender;
    if (delayType === 'throttle') hoverInfoSender = new Throttler(null, sendHoverInfo, delay);else hoverInfoSender = new Debouncer(null, sendHoverInfo, delay);

    // What to do when mouse exits the image
    var mouseout;
    if (nullOutside) mouseout = function mouseout() {
      hoverInfoSender.normalCall(null);
    };else mouseout = function mouseout() {};

    return {
      mousemove: function mousemove(e) {
        hoverInfoSender.normalCall(e);
      },
      mouseout: mouseout,
      onResetImg: function onResetImg() {
        hoverInfoSender.immediateCall(null);
      }
    };
  };

  // Returns a brush handler object. This has three public functions:
  // mousedown, mousemove, and onResetImg.
  imageutils.createBrushHandler = function (inputId, $el, opts, coordmap, outputId) {
    // Parameter: expand the area in which a brush can be started, by this
    // many pixels in all directions. (This should probably be a brush option)
    var expandPixels = 20;

    // Represents the state of the brush
    var brush = imageutils.createBrush($el, opts, coordmap, expandPixels);

    // Brush IDs can span multiple image/plot outputs. When an output is brushed,
    // if a brush with the same ID is active on a different image/plot, it must
    // be dismissed (but without sending any data to the server). We implement
    // this by sending the shiny-internal:brushed event to all plots, and letting
    // each plot decide for itself what to do.
    //
    // The decision to have the event sent to each plot (as opposed to a single
    // event triggered on, say, the document) was made to make cleanup easier;
    // listening on an event on the document would prevent garbage collection
    // of plot outputs that are removed from the document.
    $el.on("shiny-internal:brushed.image_output", function (e, coords) {
      // If the new brush shares our ID but not our output element ID, we
      // need to clear our brush (if any).
      if (coords.brushId === inputId && coords.outputId !== outputId) {
        $el.data("mostRecentBrush", false);
        brush.reset();
      }
    });

    // Set cursor to one of 7 styles. We need to set the cursor on the whole
    // el instead of the brush div, because the brush div has
    // 'pointer-events:none' so that it won't intercept pointer events.
    // If `style` is null, don't add a cursor style.
    function setCursorStyle(style) {
      $el.removeClass('crosshair grabbable grabbing ns-resize ew-resize nesw-resize nwse-resize');

      if (style) $el.addClass(style);
    }

    function sendBrushInfo() {
      var coords = brush.boundsData();

      // We're in a new or reset state
      if (isNaN(coords.xmin)) {
        exports.onInputChange(inputId, null);
        // Must tell other brushes to clear.
        imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
          brushId: inputId, outputId: null
        });
        return;
      }

      var panel = brush.getPanel();

      // Add the panel (facet) variables, if present
      $.extend(coords, panel.panel_vars);

      // Add variable name mappings
      coords.mapping = panel.mapping;

      // Add scaling information
      coords.domain = panel.domain;
      coords.range = panel.range;
      coords.log = panel.log;

      coords.direction = opts.brushDirection;

      coords.brushId = inputId;
      coords.outputId = outputId;

      // Send data to server
      exports.onInputChange(inputId, coords);

      $el.data("mostRecentBrush", true);
      imageOutputBinding.find(document).trigger("shiny-internal:brushed", coords);
    }

    var brushInfoSender;
    if (opts.brushDelayType === 'throttle') {
      brushInfoSender = new Throttler(null, sendBrushInfo, opts.brushDelay);
    } else {
      brushInfoSender = new Debouncer(null, sendBrushInfo, opts.brushDelay);
    }

    function mousedown(e) {
      // This can happen when mousedown inside the graphic, then mouseup
      // outside, then mousedown inside. Just ignore the second
      // mousedown.
      if (brush.isBrushing() || brush.isDragging() || brush.isResizing()) return;

      // Listen for left mouse button only
      if (e.which !== 1) return;

      var offset = coordmap.mouseOffset(e);

      // Ignore mousedown events outside of plotting region, expanded by
      // a number of pixels specified in expandPixels.
      if (opts.brushClip && !coordmap.isInPanel(offset, expandPixels)) return;

      brush.up({ x: NaN, y: NaN });
      brush.down(offset);

      if (brush.isInResizeArea(offset)) {
        brush.startResizing(offset);

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveResizing).on('mouseup.image_brush', mouseupResizing);
      } else if (brush.isInsideBrush(offset)) {
        brush.startDragging(offset);
        setCursorStyle('grabbing');

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveDragging).on('mouseup.image_brush', mouseupDragging);
      } else {
        var panel = coordmap.getPanel(offset, expandPixels);
        brush.startBrushing(panel.clip(offset));

        // Attach the move and up handlers to the window so that they respond
        // even when the mouse is moved outside of the image.
        $(document).on('mousemove.image_brush', mousemoveBrushing).on('mouseup.image_brush', mouseupBrushing);
      }
    }

    // This sets the cursor style when it's in the el
    function mousemove(e) {
      var offset = coordmap.mouseOffset(e);

      if (!(brush.isBrushing() || brush.isDragging() || brush.isResizing())) {
        // Set the cursor depending on where it is
        if (brush.isInResizeArea(offset)) {
          var r = brush.whichResizeSides(offset);

          if (r.left && r.top || r.right && r.bottom) {
            setCursorStyle('nwse-resize');
          } else if (r.left && r.bottom || r.right && r.top) {
            setCursorStyle('nesw-resize');
          } else if (r.left || r.right) {
            setCursorStyle('ew-resize');
          } else if (r.top || r.bottom) {
            setCursorStyle('ns-resize');
          }
        } else if (brush.isInsideBrush(offset)) {
          setCursorStyle('grabbable');
        } else if (coordmap.isInPanel(offset, expandPixels)) {
          setCursorStyle('crosshair');
        } else {
          setCursorStyle(null);
        }
      }
    }

    // mousemove handlers while brushing or dragging
    function mousemoveBrushing(e) {
      brush.brushTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    function mousemoveDragging(e) {
      brush.dragTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    function mousemoveResizing(e) {
      brush.resizeTo(coordmap.mouseOffset(e));
      brushInfoSender.normalCall();
    }

    // mouseup handlers while brushing or dragging
    function mouseupBrushing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));

      brush.stopBrushing();
      setCursorStyle('crosshair');

      // If the brush didn't go anywhere, hide the brush, clear value,
      // and return.
      if (brush.down().x === brush.up().x && brush.down().y === brush.up().y) {
        brush.reset();
        brushInfoSender.immediateCall();
        return;
      }

      // Send info immediately on mouseup, but only if needed. If we don't
      // do the pending check, we might send the same data twice (with
      // with difference nonce).
      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    function mouseupDragging(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));

      brush.stopDragging();
      setCursorStyle('grabbable');

      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    function mouseupResizing(e) {
      // Listen for left mouse button only
      if (e.which !== 1) return;

      $(document).off('mousemove.image_brush').off('mouseup.image_brush');

      brush.up(coordmap.mouseOffset(e));
      brush.stopResizing();

      if (brushInfoSender.isPending()) brushInfoSender.immediateCall();
    }

    // Brush maintenance: When an image is re-rendered, the brush must either
    // be removed (if brushResetOnNew) or imported (if !brushResetOnNew). The
    // "mostRecentBrush" bit is to ensure that when multiple outputs share the
    // same brush ID, inactive brushes don't send null values up to the server.

    // This should be called when the img (not the el) is reset
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

    return {
      mousedown: mousedown,
      mousemove: mousemove,
      onResetImg: onResetImg
    };
  };

  // Returns an object that represents the state of the brush. This gets wrapped
  // in a brushHandler, which provides various event listeners.
  imageutils.createBrush = function ($el, opts, coordmap, expandPixels) {
    // Number of pixels outside of brush to allow start resizing
    var resizeExpand = 10;

    var el = $el[0];
    var $div = null; // The div representing the brush

    var state = {};
    reset();

    function reset() {
      // Current brushing/dragging/resizing state
      state.brushing = false;
      state.dragging = false;
      state.resizing = false;

      // Offset of last mouse down and up events
      state.down = { x: NaN, y: NaN };
      state.up = { x: NaN, y: NaN };

      // Which side(s) we're currently resizing
      state.resizeSides = {
        left: false,
        right: false,
        top: false,
        bottom: false
      };

      // Bounding rectangle of the brush, in pixel and data dimensions. We need to
      // record data dimensions along with pixel dimensions so that when a new
      // plot is sent, we can re-draw the brush div with the appropriate coords.
      state.boundsPx = {
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

      // Panel object that the brush is in
      state.panel = null;

      // The bounds at the start of a drag/resize
      state.changeStartBounds = {
        xmin: NaN,
        xmax: NaN,
        ymin: NaN,
        ymax: NaN
      };

      if ($div) $div.remove();
    }

    // If there's an existing brush div, use that div to set the new brush's
    // settings, provided that the x, y, and panel variables have the same names,
    // and there's a panel with matching panel variable values.
    function importOldBrush() {
      var oldDiv = $el.find('#' + el.id + '_brush');
      if (oldDiv.length === 0) return;

      var oldBoundsData = oldDiv.data('bounds-data');
      var oldPanel = oldDiv.data('panel');

      if (!oldBoundsData || !oldPanel) return;

      // Compare two objects. This checks that objects a and b have the same est
      // of keys, and that each key has the same value. This function isn't
      // perfect, but it's good enough for comparing variable mappings, below.
      function isEquivalent(a, b) {
        if (a === undefined) {
          if (b === undefined) return true;else return false;
        }
        if (a === null) {
          if (b === null) return true;else return false;
        }

        var aProps = Object.getOwnPropertyNames(a);
        var bProps = Object.getOwnPropertyNames(b);

        if (aProps.length !== bProps.length) return false;

        for (var i = 0; i < aProps.length; i++) {
          var propName = aProps[i];
          if (a[propName] !== b[propName]) {
            return false;
          }
        }
        return true;
      }

      // Find a panel that has matching vars; if none found, we can't restore.
      // The oldPanel and new panel must match on their mapping vars, and the
      // values.
      for (var i = 0; i < coordmap.length; i++) {
        var curPanel = coordmap[i];

        if (isEquivalent(oldPanel.mapping, curPanel.mapping) && isEquivalent(oldPanel.panel_vars, curPanel.panel_vars)) {
          // We've found a matching panel
          state.panel = coordmap[i];
          break;
        }
      }

      // If we didn't find a matching panel, remove the old div and return
      if (state.panel === null) {
        oldDiv.remove();
        return;
      }

      $div = oldDiv;

      boundsData(oldBoundsData);
      updateDiv();
    }

    // Return true if the offset is inside min/max coords
    function isInsideBrush(offset) {
      var bounds = state.boundsPx;
      return offset.x <= bounds.xmax && offset.x >= bounds.xmin && offset.y <= bounds.ymax && offset.y >= bounds.ymin;
    }

    // Return true if offset is inside a region to start a resize
    function isInResizeArea(offset) {
      var sides = whichResizeSides(offset);
      return sides.left || sides.right || sides.top || sides.bottom;
    }

    // Return an object representing which resize region(s) the cursor is in.
    function whichResizeSides(offset) {
      var b = state.boundsPx;
      // Bounds with expansion
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

      if ((opts.brushDirection === 'xy' || opts.brushDirection === 'x') && offset.y <= e.ymax && offset.y >= e.ymin) {
        if (offset.x < b.xmin && offset.x >= e.xmin) res.left = true;else if (offset.x > b.xmax && offset.x <= e.xmax) res.right = true;
      }

      if ((opts.brushDirection === 'xy' || opts.brushDirection === 'y') && offset.x <= e.xmax && offset.x >= e.xmin) {
        if (offset.y < b.ymin && offset.y >= e.ymin) res.top = true;else if (offset.y > b.ymax && offset.y <= e.ymax) res.bottom = true;
      }

      return res;
    }

    // Sets the bounds of the brush, given a box and optional panel. This
    // will fit the box bounds into the panel, so we don't brush outside of it.
    // This knows whether we're brushing in the x, y, or xy directions, and sets
    // bounds accordingly.
    // If no box is passed in, just return current bounds.
    function boundsPx(box) {
      if (box === undefined) return state.boundsPx;

      var min = { x: box.xmin, y: box.ymin };
      var max = { x: box.xmax, y: box.ymax };

      var panel = state.panel;
      var panelBounds = panel.range;

      if (opts.brushClip) {
        min = panel.clip(min);
        max = panel.clip(max);
      }

      if (opts.brushDirection === 'xy') {
        // No change

      } else if (opts.brushDirection === 'x') {
        // Extend top and bottom of plotting area
        min.y = panelBounds.top;
        max.y = panelBounds.bottom;
      } else if (opts.brushDirection === 'y') {
        min.x = panelBounds.left;
        max.x = panelBounds.right;
      }

      state.boundsPx = {
        xmin: min.x,
        xmax: max.x,
        ymin: min.y,
        ymax: max.y
      };

      // Positions in data space
      var minData = state.panel.scaleInv(min);
      var maxData = state.panel.scaleInv(max);
      // For reversed scales, the min and max can be reversed, so use findBox
      // to ensure correct order.
      state.boundsData = coordmap.findBox(minData, maxData);
      // Round to 14 significant digits to avoid spurious changes in FP values
      // (#1634).
      state.boundsData = _.mapValues(state.boundsData, function (val) {
        return roundSignif(val, 14);
      });

      // We also need to attach the data bounds and panel as data attributes, so
      // that if the image is re-sent, we can grab the data bounds to create a new
      // brush. This should be fast because it doesn't actually modify the DOM.
      $div.data('bounds-data', state.boundsData);
      $div.data('panel', state.panel);
      return undefined;
    }

    // Get or set the bounds of the brush using coordinates in the data space.
    function boundsData(box) {
      if (box === undefined) {
        return state.boundsData;
      }

      var min = { x: box.xmin, y: box.ymin };
      var max = { x: box.xmax, y: box.ymax };

      var minPx = state.panel.scale(min);
      var maxPx = state.panel.scale(max);

      // The scaling function can reverse the direction of the axes, so we need to
      // find the min and max again.
      boundsPx({
        xmin: Math.min(minPx.x, maxPx.x),
        xmax: Math.max(minPx.x, maxPx.x),
        ymin: Math.min(minPx.y, maxPx.y),
        ymax: Math.max(minPx.y, maxPx.y)
      });
      return undefined;
    }

    function getPanel() {
      return state.panel;
    }

    // Add a new div representing the brush.
    function addDiv() {
      if ($div) $div.remove();

      // Start hidden; we'll show it when movement occurs
      $div = $(document.createElement('div')).attr('id', el.id + '_brush').css({
        'background-color': opts.brushFill,
        'opacity': opts.brushOpacity,
        'pointer-events': 'none',
        'position': 'absolute'
      }).hide();

      var borderStyle = '1px solid ' + opts.brushStroke;
      if (opts.brushDirection === 'xy') {
        $div.css({
          'border': borderStyle
        });
      } else if (opts.brushDirection === 'x') {
        $div.css({
          'border-left': borderStyle,
          'border-right': borderStyle
        });
      } else if (opts.brushDirection === 'y') {
        $div.css({
          'border-top': borderStyle,
          'border-bottom': borderStyle
        });
      }

      $el.append($div);
      $div.offset({ x: 0, y: 0 }).width(0).outerHeight(0);
    }

    // Update the brush div to reflect the current brush bounds.
    function updateDiv() {
      // Need parent offset relative to page to calculate mouse offset
      // relative to page.
      var imgOffset = $el.offset();
      var b = state.boundsPx;
      $div.offset({
        top: imgOffset.top + b.ymin,
        left: imgOffset.left + b.xmin
      }).outerWidth(b.xmax - b.xmin + 1).outerHeight(b.ymax - b.ymin + 1);
    }

    function down(offset) {
      if (offset === undefined) return state.down;

      state.down = offset;
      return undefined;
    }

    function up(offset) {
      if (offset === undefined) return state.up;

      state.up = offset;
      return undefined;
    }

    function isBrushing() {
      return state.brushing;
    }

    function startBrushing() {
      state.brushing = true;
      addDiv();
      state.panel = coordmap.getPanel(state.down, expandPixels);

      boundsPx(coordmap.findBox(state.down, state.down));
      updateDiv();
    }

    function brushTo(offset) {
      boundsPx(coordmap.findBox(state.down, offset));
      $div.show();
      updateDiv();
    }

    function stopBrushing() {
      state.brushing = false;

      // Save the final bounding box of the brush
      boundsPx(coordmap.findBox(state.down, state.up));
    }

    function isDragging() {
      return state.dragging;
    }

    function startDragging() {
      state.dragging = true;
      state.changeStartBounds = $.extend({}, state.boundsPx);
    }

    function dragTo(offset) {
      // How far the brush was dragged
      var dx = offset.x - state.down.x;
      var dy = offset.y - state.down.y;

      // Calculate what new positions would be, before clipping.
      var start = state.changeStartBounds;
      var newBounds = {
        xmin: start.xmin + dx,
        xmax: start.xmax + dx,
        ymin: start.ymin + dy,
        ymax: start.ymax + dy
      };

      // Clip to the plotting area
      if (opts.brushClip) {
        var panelBounds = state.panel.range;

        // Convert to format for shiftToRange
        var xvals = [newBounds.xmin, newBounds.xmax];
        var yvals = [newBounds.ymin, newBounds.ymax];

        xvals = coordmap.shiftToRange(xvals, panelBounds.left, panelBounds.right);
        yvals = coordmap.shiftToRange(yvals, panelBounds.top, panelBounds.bottom);

        // Convert back to bounds format
        newBounds = {
          xmin: xvals[0],
          xmax: xvals[1],
          ymin: yvals[0],
          ymax: yvals[1]
        };
      }

      boundsPx(newBounds);
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
      state.changeStartBounds = $.extend({}, state.boundsPx);
      state.resizeSides = whichResizeSides(state.down);
    }

    function resizeTo(offset) {
      // How far the brush was dragged
      var dx = offset.x - state.down.x;
      var dy = offset.y - state.down.y;

      // Calculate what new positions would be, before clipping.
      var b = $.extend({}, state.changeStartBounds);
      var panelBounds = state.panel.range;

      if (state.resizeSides.left) {
        b.xmin = coordmap.shiftToRange([b.xmin + dx], panelBounds.left, b.xmax)[0];
      } else if (state.resizeSides.right) {
        b.xmax = coordmap.shiftToRange([b.xmax + dx], b.xmin, panelBounds.right)[0];
      }

      if (state.resizeSides.top) {
        b.ymin = coordmap.shiftToRange([b.ymin + dy], panelBounds.top, b.ymax)[0];
      } else if (state.resizeSides.bottom) {
        b.ymax = coordmap.shiftToRange([b.ymax + dy], b.ymin, panelBounds.bottom)[0];
      }

      boundsPx(b);
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

      boundsPx: boundsPx,
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

  exports.resetBrush = function (brushId) {
    exports.onInputChange(brushId, null);
    imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
      brushId: brushId, outputId: null
    });
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_html.js

  var htmlOutputBinding = new OutputBinding();
  $.extend(htmlOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-html-output');
    },
    onValueError: function onValueError(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function renderValue(el, data) {
      exports.renderContent(el, data);
    }
  });
  outputBindings.register(htmlOutputBinding, 'shiny.htmlOutput');

  var renderDependencies = exports.renderDependencies = function (dependencies) {
    if (dependencies) {
      $.each(dependencies, function (i, dep) {
        renderDependency(dep);
      });
    }
  };

  // Render HTML in a DOM element, add dependencies, and bind Shiny
  // inputs/outputs. `content` can be null, a string, or an object with
  // properties 'html' and 'deps'.
  exports.renderContent = function (el, content) {
    var where = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : "replace";

    if (where === "replace") {
      exports.unbindAll(el);
    }

    exports.unbindAll(el);

    var html;
    var dependencies = [];
    if (content === null) {
      html = '';
    } else if (typeof content === 'string') {
      html = content;
    } else if ((typeof content === 'undefined' ? 'undefined' : _typeof(content)) === 'object') {
      html = content.html;
      dependencies = content.deps || [];
    }

    exports.renderHtml(html, el, dependencies, where);

    var scope = el;
    if (where === "replace") {
      exports.initializeInputs(el);
      exports.bindAll(el);
    } else {
      var $parent = $(el).parent();
      if ($parent.length > 0) {
        scope = $parent;
        if (where === "beforeBegin" || where === "afterEnd") {
          var $grandparent = $parent.parent();
          if ($grandparent.length > 0) scope = $grandparent;
        }
      }
      exports.initializeInputs(scope);
      exports.bindAll(scope);
    }
  };

  // Render HTML in a DOM element, inserting singletons into head as needed
  exports.renderHtml = function (html, el, dependencies) {
    var where = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : 'replace';

    renderDependencies(dependencies);
    return singletons.renderHtml(html, el, where);
  };

  var htmlDependencies = {};
  function registerDependency(name, version) {
    htmlDependencies[name] = version;
  }

  // Client-side dependency resolution and rendering
  function renderDependency(dep) {
    if (htmlDependencies.hasOwnProperty(dep.name)) return false;

    registerDependency(dep.name, dep.version);

    var href = dep.src.href;

    var $head = $("head").first();

    if (dep.meta) {
      var metas = $.map(asArray(dep.meta), function (obj, idx) {
        // only one named pair is expected in obj as it's already been decomposed
        var name = Object.keys(obj)[0];
        return $("<meta>").attr("name", name).attr("content", obj[name]);
      });
      $head.append(metas);
    }

    if (dep.stylesheet) {
      var stylesheets = $.map(asArray(dep.stylesheet), function (stylesheet) {
        return $("<link rel='stylesheet' type='text/css'>").attr("href", href + "/" + encodeURI(stylesheet));
      });
      $head.append(stylesheets);
    }

    if (dep.script) {
      var scripts = $.map(asArray(dep.script), function (scriptName) {
        return $("<script>").attr("src", href + "/" + encodeURI(scriptName));
      });
      $head.append(scripts);
    }

    if (dep.attachment) {
      // dep.attachment might be a single string, an array, or an object.
      var attachments = dep.attachment;
      if (typeof attachments === "string") attachments = [attachments];
      if ($.isArray(attachments)) {
        // The contract for attachments is that arrays of attachments are
        // addressed using 1-based indexes. Convert this array to an object.
        var tmp = {};
        $.each(attachments, function (index, attachment) {
          tmp[index + 1 + ""] = attachment;
        });
        attachments = tmp;
      }

      var attach = $.map(attachments, function (attachment, key) {
        return $("<link rel='attachment'>").attr("id", dep.name + "-" + key + "-attachment").attr("href", href + "/" + encodeURI(attachment));
      });
      $head.append(attach);
    }

    if (dep.head) {
      var $newHead = $("<head></head>");
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
        $(el).html(processed.html);
      } else {
        el.insertAdjacentHTML(where, processed.html);
      }
      return processed;
    },
    // Take an object where keys are names of singletons, and merges it into
    // knownSingletons
    register: function register(s) {
      $.extend(this.knownSingletons, s);
    },
    // Takes a string or array of strings and adds them to knownSingletons
    registerNames: function registerNames(s) {
      if (typeof s === 'string') {
        this.knownSingletons[s] = true;
      } else if (s instanceof Array) {
        for (var i = 0; i < s.length; i++) {
          this.knownSingletons[s[i]] = true;
        }
      }
    },
    // Inserts new content into document head
    _addToHead: function _addToHead(head) {
      if (head.length > 0) {
        var tempDiv = $("<div>" + head + "</div>")[0];
        var $head = $('head');
        while (tempDiv.hasChildNodes()) {
          $head.append(tempDiv.firstChild);
        }
      }
    },
    // Reads HTML and returns an object with info about singletons
    _processHtml: function _processHtml(val) {
      var self = this;
      var newSingletons = {};
      var newVal;

      var findNewPayload = function findNewPayload(match, p1, sig, payload) {
        if (self.knownSingletons[sig] || newSingletons[sig]) return "";
        newSingletons[sig] = true;
        return payload;
      };
      while (true) {
        newVal = val.replace(self._reSingleton, findNewPayload);
        if (val.length === newVal.length) break;
        val = newVal;
      }

      var heads = [];
      var headAddPayload = function headAddPayload(match, payload) {
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
        singletons: newSingletons
      };
    },
    _reSingleton: /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/,
    _reHead: /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/
  };

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_downloadlink.js

  var downloadLinkOutputBinding = new OutputBinding();
  $.extend(downloadLinkOutputBinding, {
    find: function find(scope) {
      return $(scope).find('a.shiny-download-link');
    },
    renderValue: function renderValue(el, data) {
      $(el).attr('href', data);
    }
  });
  outputBindings.register(downloadLinkOutputBinding, 'shiny.downloadLink');

  // Trigger shiny:filedownload event whenever a downloadButton/Link is clicked
  $(document).on('click.shinyDownloadLink', 'a.shiny-download-link', function (e) {
    var evt = jQuery.Event('shiny:filedownload');
    evt.name = this.id;
    evt.href = this.href;
    $(document).trigger(evt);
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_datatable.js

  var datatableOutputBinding = new OutputBinding();
  $.extend(datatableOutputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-datatable-output');
    },
    onValueError: function onValueError(el, err) {
      exports.unbindAll(el);
      this.renderError(el, err);
    },
    renderValue: function renderValue(el, _data) {
      var $el = $(el).empty();
      if (!_data || !_data.colnames) return;

      var colnames = $.makeArray(_data.colnames);
      var header = $.map(colnames, function (x) {
        return '<th>' + x + '</th>';
      }).join('');
      header = '<thead><tr>' + header + '</tr></thead>';
      var footer = '';
      if (_data.options === null || _data.options.searching !== false) {
        footer = $.map(colnames, function (x) {
          // placeholder needs to be escaped (and HTML tags are stripped off)
          return '<th><input type="text" placeholder="' + escapeHTML(x.replace(/(<([^>]+)>)/ig, '')) + '" /></th>';
        }).join('');
        footer = '<tfoot>' + footer + '</tfoot>';
      }
      var content = '<table class="table table-striped table-hover">' + header + footer + '</table>';
      $el.append(content);

      // options that should be eval()ed
      if (_data.evalOptions) $.each(_data.evalOptions, function (i, x) {
        /*jshint evil: true */
        _data.options[x] = eval('(' + _data.options[x] + ')');
      });

      // caseInsensitive searching? default true
      var searchCI = _data.options === null || typeof _data.options.search === 'undefined' || _data.options.search.caseInsensitive !== false;
      var oTable = $(el).children("table").DataTable($.extend({
        "processing": true,
        "serverSide": true,
        "order": [],
        "orderClasses": false,
        "pageLength": 25,
        "ajax": {
          "url": _data.action,
          "type": "POST",
          "data": function data(d) {
            d.search.caseInsensitive = searchCI;
            d.escape = _data.escape;
          }
        }
      }, _data.options));
      // the table object may need post-processing
      if (typeof _data.callback === 'string') {
        /*jshint evil: true */
        var callback = eval('(' + _data.callback + ')');
        if (typeof callback === 'function') callback(oTable);
      }

      // use debouncing for searching boxes
      $el.find('label input').first().unbind('keyup').keyup(debounce(_data.searchDelay, function () {
        oTable.search(this.value).draw();
      }));
      var searchInputs = $el.find("tfoot input");
      if (searchInputs.length > 0) {
        // this is a little weird: aoColumns/bSearchable are still in DT 1.10
        // https://github.com/DataTables/DataTables/issues/388
        $.each(oTable.settings()[0].aoColumns, function (i, x) {
          // hide the text box if not searchable
          if (!x.bSearchable) searchInputs.eq(i).hide();
        });
        searchInputs.keyup(debounce(_data.searchDelay, function () {
          oTable.column(searchInputs.index(this)).search(this.value).draw();
        }));
      }
      // FIXME: ugly scrollbars in tab panels b/c Bootstrap uses 'visible: auto'
      $el.parents('.tab-content').css('overflow', 'visible');
    }
  });
  outputBindings.register(datatableOutputBinding, 'shiny.datatableOutput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/output_binding_adapter.js

  var OutputBindingAdapter = function OutputBindingAdapter(el, binding) {
    this.el = el;
    this.binding = binding;

    // If the binding actually has a resize method, override the prototype of
    // onResize with a version that does a makeResizeFilter on the element.
    if (binding.resize) {
      this.onResize = makeResizeFilter(el, function (width, height) {
        binding.resize(el, width, height);
      });
    }
  };
  (function () {
    this.getId = function () {
      return this.binding.getId(this.el);
    };
    this.onValueChange = function (data) {
      this.binding.onValueChange(this.el, data);
    };
    this.onValueError = function (err) {
      this.binding.onValueError(this.el, err);
    };
    this.showProgress = function (show) {
      this.binding.showProgress(this.el, show);
    };
    this.onResize = function () {
      // Intentionally left blank; see constructor
    };
  }).call(OutputBindingAdapter.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding.js

  var InputBinding = exports.InputBinding = function () {};

  (function () {

    // Returns a jQuery object or element array that contains the
    // descendants of scope that match this binding
    this.find = function (scope) {
      throw "Not implemented";
    };

    this.getId = function (el) {
      return el['data-input-id'] || el.id;
    };

    // Gives the input a type in case the server needs to know it
    // to deserialize the JSON correctly
    this.getType = function () {
      return false;
    };
    this.getValue = function (el) {
      throw "Not implemented";
    };

    // The callback method takes one argument, whose value is boolean. If true,
    // allow deferred (debounce or throttle) sending depending on the value of
    // getRatePolicy. If false, send value immediately.
    this.subscribe = function (el, callback) {};
    this.unsubscribe = function (el) {};

    // This is used for receiving messages that tell the input object to do
    // things, such as setting values (including min, max, and others).
    // 'data' should be an object with elements corresponding to value, min,
    // max, etc., as appropriate for the type of input object. It also should
    // trigger a change event.
    this.receiveMessage = function (el, data) {
      throw "Not implemented";
    };
    this.getState = function (el, data) {
      throw "Not implemented";
    };

    this.getRatePolicy = function () {
      return null;
    };

    // Some input objects need initialization before being bound. This is
    // called when the document is ready (for statically-added input objects),
    // and when new input objects are added to the document with
    // htmlOutputBinding.renderValue() (for dynamically-added input objects).
    // This is called before the input is bound.
    this.initialize = function (el) {};

    // This is called after unbinding the output.
    this.dispose = function (el) {};
  }).call(InputBinding.prototype);

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_text.js

  var textInputBinding = new InputBinding();
  $.extend(textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
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
      $(el).on('keyup.textInputBinding input.textInputBinding', function (event) {
        callback(true);
      });
      $(el).on('change.textInputBinding', function (event) {
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.textInputBinding');
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function getState(el) {
      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: el.value
      };
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    }
  });
  inputBindings.register(textInputBinding, 'shiny.textInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_textarea.js

  var textareaInputBinding = {};
  $.extend(textareaInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('textarea');
    }
  });
  inputBindings.register(textareaInputBinding, 'shiny.textareaInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_password.js

  var passwordInputBinding = {};
  $.extend(passwordInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="password"]');
    },
    getType: function getType(el) {
      return "shiny.password";
    }
  });
  inputBindings.register(passwordInputBinding, 'shiny.passwordInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_number.js

  var numberInputBinding = {};
  $.extend(numberInputBinding, textInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function getValue(el) {
      var numberVal = $(el).val();
      if (/^\s*$/.test(numberVal)) // Return null if all whitespace
        return null;else if (!isNaN(numberVal)) // If valid Javascript number string, coerce to number
        return +numberVal;else return numberVal; // If other string like "1e6", send it unchanged
    },
    setValue: function setValue(el, value) {
      el.value = value;
    },
    getType: function getType(el) {
      return "shiny.number";
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) el.value = data.value;
      if (data.hasOwnProperty('min')) el.min = data.min;
      if (data.hasOwnProperty('max')) el.max = data.max;
      if (data.hasOwnProperty('step')) el.step = data.step;

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function getState(el) {
      return { label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        min: Number(el.min),
        max: Number(el.max),
        step: Number(el.step) };
    }
  });
  inputBindings.register(numberInputBinding, 'shiny.numberInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_checkbox.js

  var checkboxInputBinding = new InputBinding();
  $.extend(checkboxInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="checkbox"]');
    },
    getValue: function getValue(el) {
      return el.checked;
    },
    setValue: function setValue(el, value) {
      el.checked = value;
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.checkboxInputBinding', function (event) {
        callback(true);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.checkboxInputBinding');
    },
    getState: function getState(el) {
      return {
        label: $(el).parent().find('span').text(),
        value: el.checked
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) el.checked = data.value;

      if (data.hasOwnProperty('label')) $(el).parent().find('span').text(data.label);

      $(el).trigger('change');
    }
  });
  inputBindings.register(checkboxInputBinding, 'shiny.checkboxInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_slider.js

  // Necessary to get hidden sliders to send their updated values
  function forceIonSliderUpdate(slider) {
    if (slider.$cache && slider.$cache.input) slider.$cache.input.trigger('change');else console.log("Couldn't force ion slider to update");
  }

  var sliderInputBinding = {};
  $.extend(sliderInputBinding, textInputBinding, {
    find: function find(scope) {
      // Check if ionRangeSlider plugin is loaded
      if (!$.fn.ionRangeSlider) return [];

      return $(scope).find('input.js-range-slider');
    },
    getType: function getType(el) {
      var dataType = $(el).data('data-type');
      if (dataType === 'date') return 'shiny.date';else if (dataType === 'datetime') return 'shiny.datetime';else return false;
    },
    getValue: function getValue(el) {
      var $el = $(el);
      var result = $(el).data('ionRangeSlider').result;

      // Function for converting numeric value from slider to appropriate type.
      var convert;
      var dataType = $el.data('data-type');
      if (dataType === 'date') {
        convert = function convert(val) {
          return formatDateUTC(new Date(+val));
        };
      } else if (dataType === 'datetime') {
        convert = function convert(val) {
          // Convert ms to s
          return +val / 1000;
        };
      } else {
        convert = function convert(val) {
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
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');

      $el.data('immediate', true);
      try {
        if (this._numValues(el) === 2 && value instanceof Array) {
          slider.update({ from: value[0], to: value[1] });
        } else {
          slider.update({ from: value });
        }

        forceIonSliderUpdate(slider);
      } finally {
        $el.data('immediate', false);
      }
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.sliderInputBinding', function (event) {
        callback(!$(el).data('immediate') && !$(el).data('animating'));
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.sliderInputBinding');
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);
      var slider = $el.data('ionRangeSlider');
      var msg = {};

      if (data.hasOwnProperty('value')) {
        if (this._numValues(el) === 2 && data.value instanceof Array) {
          msg.from = data.value[0];
          msg.to = data.value[1];
        } else {
          msg.from = data.value;
        }
      }
      if (data.hasOwnProperty('min')) msg.min = data.min;
      if (data.hasOwnProperty('max')) msg.max = data.max;
      if (data.hasOwnProperty('step')) msg.step = data.step;

      if (data.hasOwnProperty('label')) $el.parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $el.data('immediate', true);
      try {
        slider.update(msg);
        forceIonSliderUpdate(slider);
      } finally {
        $el.data('immediate', false);
      }
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    getState: function getState(el) {},
    initialize: function initialize(el) {
      var opts = {};
      var $el = $(el);
      var dataType = $el.data('data-type');
      var timeFormat = $el.data('time-format');
      var timeFormatter;

      // Set up formatting functions
      if (dataType === 'date') {
        timeFormatter = strftime.utc();
        opts.prettify = function (num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      } else if (dataType === 'datetime') {
        var timezone = $el.data('timezone');
        if (timezone) timeFormatter = strftime.timezone(timezone);else timeFormatter = strftime;

        opts.prettify = function (num) {
          return timeFormatter(timeFormat, new Date(num));
        };
      }

      $el.ionRangeSlider(opts);
    },

    // Number of values; 1 for single slider, 2 for range slider
    _numValues: function _numValues(el) {
      if ($(el).data('ionRangeSlider').options.type === 'double') return 2;else return 1;
    }
  });
  inputBindings.register(sliderInputBinding, 'shiny.sliderInput');

  $(document).on('click', '.slider-animate-button', function (evt) {
    evt.preventDefault();
    var self = $(this);
    var target = $('#' + $escape(self.attr('data-target-id')));
    var startLabel = 'Play';
    var stopLabel = 'Pause';
    var loop = self.attr('data-loop') !== undefined && !/^\s*false\s*$/i.test(self.attr('data-loop'));
    var animInterval = self.attr('data-interval');
    if (isNaN(animInterval)) animInterval = 1500;else animInterval = +animInterval;

    if (!target.data('animTimer')) {
      var slider;
      var timer;

      // Separate code paths:
      // Backward compatible code for old-style jsliders (Shiny <= 0.10.2.2),
      // and new-style ionsliders.
      if (target.hasClass('jslider')) {
        slider = target.slider();

        // If we're currently at the end, restart
        if (!slider.canStepNext()) slider.resetToStart();

        timer = setInterval(function () {
          if (loop && !slider.canStepNext()) {
            slider.resetToStart();
          } else {
            slider.stepNext();
            if (!loop && !slider.canStepNext()) {
              self.click(); // stop the animation
            }
          }
        }, animInterval);
      } else {
        slider = target.data('ionRangeSlider');
        // Single sliders have slider.options.type == "single", and only the
        // `from` value is used. Double sliders have type == "double", and also
        // use the `to` value for the right handle.
        var sliderCanStep = function sliderCanStep() {
          if (slider.options.type === "double") return slider.result.to < slider.result.max;else return slider.result.from < slider.result.max;
        };
        var sliderReset = function sliderReset() {
          var val = { from: slider.result.min };
          // Preserve the current spacing for double sliders
          if (slider.options.type === "double") val.to = val.from + (slider.result.to - slider.result.from);

          slider.update(val);
          forceIonSliderUpdate(slider);
        };
        var sliderStep = function sliderStep() {
          // Don't overshoot the end
          var val = {
            from: Math.min(slider.result.max, slider.result.from + slider.options.step)
          };
          if (slider.options.type === "double") val.to = Math.min(slider.result.max, slider.result.to + slider.options.step);

          slider.update(val);
          forceIonSliderUpdate(slider);
        };

        // If we're currently at the end, restart
        if (!sliderCanStep()) sliderReset();

        timer = setInterval(function () {
          if (loop && !sliderCanStep()) {
            sliderReset();
          } else {
            sliderStep();
            if (!loop && !sliderCanStep()) {
              self.click(); // stop the animation
            }
          }
        }, animInterval);
      }

      target.data('animTimer', timer);
      self.attr('title', stopLabel);
      self.addClass('playing');
      target.data('animating', true);
    } else {
      clearTimeout(target.data('animTimer'));
      target.removeData('animTimer');
      self.attr('title', startLabel);
      self.removeClass('playing');
      target.removeData('animating');
    }
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_date.js

  var dateInputBinding = new InputBinding();
  $.extend(dateInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-date-input');
    },
    getType: function getType(el) {
      return "shiny.date";
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function getValue(el) {
      var date = $(el).find('input').bsDatepicker('getUTCDate');
      return formatDateUTC(date);
    },
    // value must be an unambiguous string like '2001-01-01', or a Date object.
    setValue: function setValue(el, value) {
      // R's NA, which is null here will remove current value
      if (value === null) {
        $(el).find('input').val('').bsDatepicker('update');
        return;
      }

      var date = this._newDate(value);
      // If date is invalid, do nothing
      if (isNaN(date)) return;

      $(el).find('input').bsDatepicker('setUTCDate', date);
    },
    getState: function getState(el) {
      var $el = $(el);
      var $input = $el.find('input');

      var min = $input.data('datepicker').startDate;
      var max = $input.data('datepicker').endDate;

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      var startview = $input.data('datepicker').startViewMode;
      if (startview === 2) startview = 'decade';else if (startview === 1) startview = 'year';else if (startview === 0) startview = 'month';

      return {
        label: $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        valueString: $input.val(),
        min: min,
        max: max,
        language: $input.data('datepicker').language,
        weekstart: $input.data('datepicker').weekStart,
        format: this._formatToString($input.data('datepicker').format),
        startview: startview
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $input = $(el).find('input');

      if (data.hasOwnProperty('label')) $(el).find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) this._setMin($input[0], data.min);

      if (data.hasOwnProperty('max')) this._setMax($input[0], data.max);

      // Must set value only after min and max have been set. If new value is
      // outside the bounds of the previous min/max, then the result will be a
      // blank input.
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('keyup.dateInputBinding input.dateInputBinding', function (event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateInputBinding change.dateInputBinding', function (event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.dateInputBinding');
    },
    getRatePolicy: function getRatePolicy() {
      return {
        policy: 'debounce',
        delay: 250
      };
    },
    initialize: function initialize(el) {
      var $input = $(el).find('input');

      var date = $input.data('initial-date');
      // If initial_date is null, set to current date
      if (date === undefined || date === null) {
        // Get local date, but as UTC
        date = this._dateAsUTC(new Date());
      }

      this.setValue(el, date);

      // Set the start and end dates, from min-date and max-date. These always
      // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // support for date-startdate and data-enddate, which use the current
      // date format.
      if ($input.data('min-date') !== undefined) {
        this._setMin($input[0], $input.data('min-date'));
      }
      if ($input.data('max-date') !== undefined) {
        this._setMax($input[0], $input.data('max-date'));
      }
    },
    // Given a format object from a date picker, return a string
    _formatToString: function _formatToString(format) {
      // Format object has structure like:
      // { parts: ['mm', 'dd', 'yy'], separators: ['', '/', '/' ,''] }
      var str = '';
      for (var i = 0; i < format.parts.length; i++) {
        str += format.separators[i] + format.parts[i];
      }
      str += format.separators[i];
      return str;
    },
    // Given an unambiguous date string or a Date object, set the min (start) date.
    // null will unset. undefined will result in no change,
    _setMin: function _setMin(el, date) {
      if (date === undefined) return;
      if (date === null) {
        $(el).bsDatepicker('setStartDate', null);
      } else {
        date = this._newDate(date);
        date = this._UTCDateAsLocal(date);
        if (!isNaN(date)) {
          // Workaround for https://github.com/eternicode/bootstrap-datepicker/issues/2010
          // If the start date when there's a two-digit year format, it will set
          // the date value to null. So we'll save the value, set the start
          // date, and the restore the value.
          var curValue = $(el).bsDatepicker('getUTCDate');
          $(el).bsDatepicker('setStartDate', date);
          $(el).bsDatepicker('setUTCDate', curValue);
        }
      }
    },
    // Given an unambiguous date string or a Date object, set the max (end) date
    // null will unset.
    _setMax: function _setMax(el, date) {
      if (date === undefined) return;
      if (date === null) {
        $(el).bsDatepicker('setEndDate', null);
      } else {
        date = this._newDate(date);
        date = this._UTCDateAsLocal(date);
        if (!isNaN(date)) {
          // Workaround for same issue as in _setMin.
          var curValue = $(el).bsDatepicker('getUTCDate');
          $(el).bsDatepicker('setEndDate', date);
          $(el).bsDatepicker('setUTCDate', curValue);
        }
      }
    },
    // Given a date string of format yyyy-mm-dd, return a Date object with
    // that date at 12AM UTC.
    // If date is a Date object, return it unchanged.
    _newDate: function _newDate(date) {
      if (date instanceof Date) return date;
      if (!date) return null;

      // Get Date object - this will be at 12AM in UTC, but may print
      // differently at the Javascript console.
      var d = parseDate(date);

      // If invalid date, return null
      if (isNaN(d)) return null;

      return new Date(d.getTime());
    },
    // Given a Date object, return a Date object which has the same "clock time"
    // in UTC. For example, if input date is 2013-02-01 23:00:00 GMT-0600 (CST),
    // output will be 2013-02-01 23:00:00 UTC. Note that the JS console may
    // print this in local time, as "Sat Feb 02 2013 05:00:00 GMT-0600 (CST)".
    _dateAsUTC: function _dateAsUTC(date) {
      return new Date(date.getTime() - date.getTimezoneOffset() * 60000);
    },
    // The inverse of _dateAsUTC. This is needed to adjust time zones because
    // some bootstrap-datepicker methods only take local dates as input, and not
    // UTC.
    _UTCDateAsLocal: function _UTCDateAsLocal(date) {
      return new Date(date.getTime() + date.getTimezoneOffset() * 60000);
    }
  });
  inputBindings.register(dateInputBinding, 'shiny.dateInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_daterange.js

  var dateRangeInputBinding = {};
  $.extend(dateRangeInputBinding, dateInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-date-range-input');
    },
    // Return the date in an unambiguous format, yyyy-mm-dd (as opposed to a
    // format like mm/dd/yyyy)
    getValue: function getValue(el) {
      var $inputs = $(el).find('input');
      var start = $inputs.eq(0).bsDatepicker('getUTCDate');
      var end = $inputs.eq(1).bsDatepicker('getUTCDate');

      return [formatDateUTC(start), formatDateUTC(end)];
    },
    // value must be an object, with optional fields `start` and `end`. These
    // should be unambiguous strings like '2001-01-01', or Date objects.
    setValue: function setValue(el, value) {
      if (!(value instanceof Object)) {
        return;
      }

      // Get the start and end input objects
      var $inputs = $(el).find('input');

      // If value is undefined, don't try to set
      // null will remove the current value
      if (value.start !== undefined) {
        if (value.start === null) {
          $inputs.eq(0).val('').bsDatepicker('update');
        } else {
          var start = this._newDate(value.start);
          $inputs.eq(0).bsDatepicker('setUTCDate', start);
        }
      }
      if (value.end !== undefined) {
        if (value.end === null) {
          $inputs.eq(1).val('').bsDatepicker('update');
        } else {
          var end = this._newDate(value.end);
          $inputs.eq(1).bsDatepicker('setUTCDate', end);
        }
      }
    },
    getState: function getState(el) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      // For many of the properties, assume start and end have the same values
      var min = $startinput.bsDatepicker('getStartDate');
      var max = $startinput.bsDatepicker('getEndDate');

      // Stringify min and max. If min and max aren't set, they will be
      // -Infinity and Infinity; replace these with null.
      min = min === -Infinity ? null : formatDateUTC(min);
      max = max === Infinity ? null : formatDateUTC(max);

      // startViewMode is stored as a number; convert to string
      var startview = $startinput.data('datepicker').startView;
      if (startview === 2) startview = 'decade';else if (startview === 1) startview = 'year';else if (startview === 0) startview = 'month';

      return {
        label: $el.find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        valueString: [$startinput.val(), $endinput.val()],
        min: min,
        max: max,
        weekstart: $startinput.data('datepicker').weekStart,
        format: this._formatToString($startinput.data('datepicker').format),
        language: $startinput.data('datepicker').language,
        startview: startview
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      if (data.hasOwnProperty('label')) $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      if (data.hasOwnProperty('min')) {
        this._setMin($startinput[0], data.min);
        this._setMin($endinput[0], data.min);
      }

      if (data.hasOwnProperty('max')) {
        this._setMax($startinput[0], data.max);
        this._setMax($endinput[0], data.max);
      }

      // Must set value only after min and max have been set. If new value is
      // outside the bounds of the previous min/max, then the result will be a
      // blank input.
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      $el.trigger('change');
    },
    initialize: function initialize(el) {
      var $el = $(el);
      var $inputs = $el.find('input');
      var $startinput = $inputs.eq(0);
      var $endinput = $inputs.eq(1);

      var start = $startinput.data('initial-date');
      var end = $endinput.data('initial-date');

      // If empty/null, use local date, but as UTC
      if (start === undefined || start === null) start = this._dateAsUTC(new Date());

      if (end === undefined || end === null) end = this._dateAsUTC(new Date());

      this.setValue(el, { "start": start, "end": end });

      // // Set the start and end dates, from min-date and max-date. These always
      // // use yyyy-mm-dd format, instead of bootstrap-datepicker's built-in
      // // support for date-startdate and data-enddate, which use the current
      // // date format.
      this._setMin($startinput[0], $startinput.data('min-date'));
      this._setMin($endinput[0], $startinput.data('min-date'));
      this._setMax($startinput[0], $endinput.data('max-date'));
      this._setMax($endinput[0], $endinput.data('max-date'));
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('keyup.dateRangeInputBinding input.dateRangeInputBinding', function (event) {
        // Use normal debouncing policy when typing
        callback(true);
      });
      $(el).on('changeDate.dateRangeInputBinding change.dateRangeInputBinding', function (event) {
        // Send immediately when clicked
        callback(false);
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.dateRangeInputBinding');
    }
  });
  inputBindings.register(dateRangeInputBinding, 'shiny.dateRangeInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_select.js

  var selectInputBinding = new InputBinding();
  $.extend(selectInputBinding, {
    find: function find(scope) {
      return $(scope).find('select');
    },
    getId: function getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function getValue(el) {
      return $(el).val();
    },
    setValue: function setValue(el, value) {
      var selectize = this._selectize(el);
      if (typeof selectize !== 'undefined') {
        selectize.setValue(value);
      } else $(el).val(value);
    },
    getState: function getState(el) {
      // Store options in an array of objects, each with with value and label
      var options = new Array(el.length);
      for (var i = 0; i < el.length; i++) {
        options[i] = { value: el[i].value,
          label: el[i].label };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el),
          selectize;

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        selectize = this._selectize(el);
        // Must destroy selectize before appending new options, otherwise
        // selectize will restore the original select
        if (selectize) selectize.destroy();
        // Clear existing options and add each new one
        $el.empty().append(data.options);
        this._selectize(el);
      }

      // re-initialize selectize
      if (data.hasOwnProperty('config')) {
        $el.parent().find('script[data-for="' + $escape(el.id) + '"]').replaceWith(data.config);
        this._selectize(el, true);
      }

      // use server-side processing for selectize
      if (data.hasOwnProperty('url')) {
        selectize = this._selectize(el);
        selectize.clearOptions();
        var thiz = this,
            loaded = false;
        selectize.settings.load = function (query, callback) {
          var settings = selectize.settings;
          $.ajax({
            url: data.url,
            data: {
              query: query,
              field: JSON.stringify([settings.searchField]),
              value: settings.valueField,
              conju: settings.searchConjunction,
              maxop: settings.maxOptions
            },
            type: 'GET',
            error: function error() {
              callback();
            },
            success: function success(res) {
              callback(res);
              if (!loaded && data.hasOwnProperty('value')) thiz.setValue(el, data.value);
              loaded = true;
            }
          });
        };
        // perform an empty search after changing the `load` function
        selectize.load(function (callback) {
          selectize.settings.load.apply(selectize, ['', callback]);
        });
      } else if (data.hasOwnProperty('value')) {
        this.setValue(el, data.value);
      }

      if (data.hasOwnProperty('label')) $(el).parent().parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.selectInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.selectInputBinding');
    },
    initialize: function initialize(el) {
      this._selectize(el);
    },
    _selectize: function _selectize(el, update) {
      if (!$.fn.selectize) return undefined;
      var $el = $(el);
      var config = $el.parent().find('script[data-for="' + $escape(el.id) + '"]');
      if (config.length === 0) return undefined;
      var options = $.extend({
        labelField: 'label',
        valueField: 'value',
        searchField: ['label']
      }, JSON.parse(config.html()));
      // selectize created from selectInput()
      if (typeof config.data('nonempty') !== 'undefined') {
        options = $.extend(options, {
          onItemRemove: function onItemRemove(value) {
            if (this.getValue() === "") $("select#" + $escape(el.id)).empty().append($("<option/>", {
              "value": value,
              "selected": true
            })).trigger("change");
          },
          onDropdownClose: function onDropdownClose($dropdown) {
            if (this.getValue() === "") this.setValue($("select#" + $escape(el.id)).val());
          }
        });
      }
      // options that should be eval()ed
      if (config.data('eval') instanceof Array) $.each(config.data('eval'), function (i, x) {
        /*jshint evil: true*/
        options[x] = eval('(' + options[x] + ')');
      });
      var control = $el.selectize(options)[0].selectize;
      // .selectize() does not really update settings; must destroy and rebuild
      if (update) {
        var settings = $.extend(control.settings, options);
        control.destroy();
        control = $el.selectize(settings)[0].selectize;
      }
      return control;
    }
  });
  inputBindings.register(selectInputBinding, 'shiny.selectInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_radio.js

  var radioInputBinding = new InputBinding();
  $.extend(radioInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-input-radiogroup');
    },
    getValue: function getValue(el) {
      // Select the radio objects that have name equal to the grouping div's id
      return $('input:radio[name="' + $escape(el.id) + '"]:checked').val();
    },
    setValue: function setValue(el, value) {
      $('input:radio[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop('checked', true);
    },
    getState: function getState(el) {
      var $objs = $('input:radio[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
          label: this._getLabel($objs[i]) };
      }

      return {
        label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('div.shiny-options-group').remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find('label.radio').remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.radioInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.radioInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function _getLabel(obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function _setLabel(obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(radioInputBinding, 'shiny.radioInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_checkboxgroup.js

  var checkboxGroupInputBinding = new InputBinding();
  $.extend(checkboxGroupInputBinding, {
    find: function find(scope) {
      return $(scope).find('.shiny-input-checkboxgroup');
    },
    getValue: function getValue(el) {
      // Select the checkbox objects that have name equal to the grouping div's id
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]:checked');
      var values = new Array($objs.length);
      for (var i = 0; i < $objs.length; i++) {
        values[i] = $objs[i].value;
      }
      return values;
    },
    setValue: function setValue(el, value) {
      // Clear all checkboxes
      $('input:checkbox[name="' + $escape(el.id) + '"]').prop('checked', false);

      // Accept array
      if (value instanceof Array) {
        for (var i = 0; i < value.length; i++) {
          $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value[i]) + '"]').prop('checked', true);
        }
        // Else assume it's a single value
      } else {
        $('input:checkbox[name="' + $escape(el.id) + '"][value="' + $escape(value) + '"]').prop('checked', true);
      }
    },
    getState: function getState(el) {
      var $objs = $('input:checkbox[name="' + $escape(el.id) + '"]');

      // Store options in an array of objects, each with with value and label
      var options = new Array($objs.length);
      for (var i = 0; i < options.length; i++) {
        options[i] = { value: $objs[i].value,
          label: this._getLabel($objs[i]) };
      }

      return { label: $(el).find('label[for="' + $escape(el.id) + '"]').text(),
        value: this.getValue(el),
        options: options
      };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // This will replace all the options
      if (data.hasOwnProperty('options')) {
        // Clear existing options and add each new one
        $el.find('div.shiny-options-group').remove();
        // Backward compatibility: for HTML generated by shinybootstrap2 package
        $el.find('label.checkbox').remove();
        $el.append(data.options);
      }

      if (data.hasOwnProperty('value')) this.setValue(el, data.value);

      if (data.hasOwnProperty('label')) $el.find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.checkboxGroupInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.checkboxGroupInputBinding');
    },
    // Given an input DOM object, get the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _getLabel: function _getLabel(obj) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        return $.trim($(obj.parentNode).find('span').text());
      }

      return null;
    },
    // Given an input DOM object, set the associated label. Handles labels
    // that wrap the input as well as labels associated with 'for' attribute.
    _setLabel: function _setLabel(obj, value) {
      // If <label><input /><span>label text</span></label>
      if (obj.parentNode.tagName === "LABEL") {
        $(obj.parentNode).find('span').text(value);
      }

      return null;
    }

  });
  inputBindings.register(checkboxGroupInputBinding, 'shiny.checkboxGroupInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_actionbutton.js

  var actionButtonInputBinding = new InputBinding();
  $.extend(actionButtonInputBinding, {
    find: function find(scope) {
      return $(scope).find(".action-button");
    },
    getValue: function getValue(el) {
      return $(el).data('val') || 0;
    },
    setValue: function setValue(el, value) {
      $(el).data('val', value);
    },
    getType: function getType(el) {
      return 'shiny.action';
    },
    subscribe: function subscribe(el, callback) {
      $(el).on("click.actionButtonInputBinding", function (e) {
        var $el = $(this);
        var val = $el.data('val') || 0;
        $el.data('val', val + 1);

        callback();
      });
    },
    getState: function getState(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function receiveMessage(el, data) {
      var $el = $(el);

      // retrieve current label and icon
      var label = $el.text();
      var icon = '';

      // to check (and store) the previous icon, we look for a $el child
      // object that has an i tag, and some (any) class (this prevents
      // italicized text - which has an i tag but, usually, no class -
      // from being mistakenly selected)
      if ($el.find('i[class]').length > 0) {
        var icon_html = $el.find('i[class]')[0];
        if (icon_html === $el.children()[0]) {
          // another check for robustness
          icon = $(icon_html).prop('outerHTML');
        }
      }

      // update the requested properties
      if (data.hasOwnProperty('label')) label = data.label;
      if (data.hasOwnProperty('icon')) {
        icon = data.icon;
        // if the user entered icon=character(0), remove the icon
        if (icon.length === 0) icon = '';
      }

      // produce new html
      $el.html(icon + ' ' + label);
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off(".actionButtonInputBinding");
    }
  });
  inputBindings.register(actionButtonInputBinding, 'shiny.actionButtonInput');

  $(document).on('click', 'a.action-button', function (e) {
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_tabinput.js

  var bootstrapTabInputBinding = new InputBinding();
  $.extend(bootstrapTabInputBinding, {
    find: function find(scope) {
      return $(scope).find('ul.nav.shiny-tab-input');
    },
    getValue: function getValue(el) {
      var anchor = $(el).find('li:not(.dropdown).active').children('a');
      if (anchor.length === 1) return this._getTabName(anchor);

      return null;
    },
    setValue: function setValue(el, value) {
      var self = this;
      var anchors = $(el).find('li:not(.dropdown)').children('a');
      anchors.each(function () {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          return false; // Break out of each()
        }
        return true;
      });
    },
    getState: function getState(el) {
      return { value: this.getValue(el) };
    },
    receiveMessage: function receiveMessage(el, data) {
      if (data.hasOwnProperty('value')) this.setValue(el, data.value);
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function (event) {
        callback();
      });
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.bootstrapTabInputBinding');
    },
    _getTabName: function _getTabName(anchor) {
      return anchor.attr('data-value') || anchor.text();
    }
  });
  inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/input_binding_fileinput.js

  var IE8FileUploader = function IE8FileUploader(shinyapp, id, fileEl) {
    this.shinyapp = shinyapp;
    this.id = id;
    this.fileEl = fileEl;
    this.beginUpload();
  };
  (function () {
    this.beginUpload = function () {
      var self = this;
      // Create invisible frame
      var iframeId = 'shinyupload_iframe_' + this.id;
      this.iframe = document.createElement('iframe');
      this.iframe.id = iframeId;
      this.iframe.name = iframeId;
      this.iframe.setAttribute('style', 'position: fixed; top: 0; left: 0; width: 0; height: 0; border: none');
      $('body').append(this.iframe);
      var iframeDestroy = function iframeDestroy() {
        // Forces Shiny to flushReact, flush outputs, etc. Without this we get
        // invalidated reactives, but observers don't actually execute.
        self.shinyapp.makeRequest('uploadieFinish', [], function () {}, function () {});
        $(self.iframe).remove();
      };
      if (this.iframe.attachEvent) {
        this.iframe.attachEvent('onload', iframeDestroy);
      } else {
        this.iframe.onload = iframeDestroy;
      }

      this.form = document.createElement('form');
      this.form.method = 'POST';
      this.form.setAttribute('enctype', 'multipart/form-data');
      this.form.action = "session/" + encodeURI(this.shinyapp.config.sessionId) + "/uploadie/" + encodeURI(this.id);
      this.form.id = 'shinyupload_form_' + this.id;
      this.form.target = iframeId;
      $(this.form).insertAfter(this.fileEl).append(this.fileEl);
      this.form.submit();
    };
  }).call(IE8FileUploader.prototype);

  var FileUploader = function FileUploader(shinyapp, id, files, el) {
    this.shinyapp = shinyapp;
    this.id = id;
    this.el = el;
    FileProcessor.call(this, files);
  };
  $.extend(FileUploader.prototype, FileProcessor.prototype);
  (function () {
    this.makeRequest = function (method, args, onSuccess, onFailure, blobs) {
      this.shinyapp.makeRequest(method, args, onSuccess, onFailure, blobs);
    };
    this.onBegin = function (files, cont) {
      var self = this;

      // Reset progress bar
      this.$setError(null);
      this.$setActive(true);
      this.$setVisible(true);
      this.onProgress(null, 0);

      this.totalBytes = 0;
      this.progressBytes = 0;
      $.each(files, function (i, file) {
        self.totalBytes += file.size;
      });

      var fileInfo = $.map(files, function (file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
      });

      this.makeRequest('uploadInit', [fileInfo], function (response) {
        self.jobId = response.jobId;
        self.uploadUrl = response.uploadUrl;
        cont();
      }, function (error) {
        self.onError(error);
      });
    };
    this.onFile = function (file, cont) {
      var self = this;
      this.onProgress(file, 0);

      $.ajax(this.uploadUrl, {
        type: 'POST',
        cache: false,
        xhr: function xhr() {
          var xhrVal = $.ajaxSettings.xhr();
          if (xhrVal.upload) {
            xhrVal.upload.onprogress = function (e) {
              if (e.lengthComputable) {
                self.onProgress(file, (self.progressBytes + e.loaded) / self.totalBytes);
              }
            };
          }
          return xhrVal;
        },
        data: file,
        processData: false,
        success: function success() {
          self.progressBytes += file.size;
          cont();
        },
        error: function error(jqXHR, textStatus, errorThrown) {
          self.onError(jqXHR.responseText || textStatus);
        }
      });
    };
    this.onComplete = function () {
      var self = this;

      var fileInfo = $.map(this.files, function (file, i) {
        return {
          name: file.name,
          size: file.size,
          type: file.type
        };
      });

      // Trigger shiny:inputchanged. Unlike a normal shiny:inputchanged event,
      // it's not possible to modify the information before the values get
      // sent to the server.
      var evt = jQuery.Event("shiny:inputchanged");
      evt.name = this.id;
      evt.value = fileInfo;
      evt.binding = fileInputBinding;
      evt.el = this.el;
      evt.inputType = 'shiny.fileupload';
      $(document).trigger(evt);

      this.makeRequest('uploadEnd', [this.jobId, this.id], function (response) {
        self.$setActive(false);
        self.onProgress(null, 1);
        self.$bar().text('Upload complete');
      }, function (error) {
        self.onError(error);
      });
      this.$bar().text('Finishing upload');
    };
    this.onError = function (message) {
      this.$setError(message || '');
      this.$setActive(false);
    };
    this.onAbort = function () {
      this.$setVisible(false);
    };
    this.onProgress = function (file, completed) {
      this.$bar().width(Math.round(completed * 100) + '%');
      this.$bar().text(file ? file.name : '');
    };
    this.$container = function () {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress');
    };
    this.$bar = function () {
      return $('#' + $escape(this.id) + '_progress.shiny-file-input-progress .progress-bar');
    };
    this.$setVisible = function (visible) {
      this.$container().css('visibility', visible ? 'visible' : 'hidden');
    };
    this.$setError = function (error) {
      this.$bar().toggleClass('progress-bar-danger', error !== null);
      if (error !== null) {
        this.onProgress(null, 1);
        this.$bar().text(error);
      }
    };
    this.$setActive = function (active) {
      this.$container().toggleClass('active', !!active);
    };
  }).call(FileUploader.prototype);

  function uploadFiles(evt) {
    // If previously selected files are uploading, abort that.
    var $el = $(evt.target);
    var uploader = $el.data('currentUploader');
    if (uploader) uploader.abort();

    var files = evt.target.files;
    // IE8 here does not necessarily mean literally IE8; it indicates if the web
    // browser supports the FileList object (IE8/9 do not support it)
    var IE8 = typeof files === 'undefined';
    var id = fileInputBinding.getId(evt.target);

    if (!IE8 && files.length === 0) return;

    // Clear data-restore attribute if present.
    $el.removeAttr('data-restore');

    // Set the label in the text box
    var $fileText = $el.closest('div.input-group').find('input[type=text]');
    if (IE8) {
      // If we're using IE8/9, just use this placeholder
      $fileText.val("[Uploaded file]");
    } else if (files.length === 1) {
      $fileText.val(files[0].name);
    } else {
      $fileText.val(files.length + " files");
    }

    // Start the new upload and put the uploader in 'currentUploader'.
    if (IE8) {
      /*jshint nonew:false */
      new IE8FileUploader(exports.shinyapp, id, evt.target);
    } else {
      $el.data('currentUploader', new FileUploader(exports.shinyapp, id, files, evt.target));
    }
  }

  var fileInputBinding = new InputBinding();
  $.extend(fileInputBinding, {
    find: function find(scope) {
      return $(scope).find('input[type="file"]');
    },
    getId: function getId(el) {
      return InputBinding.prototype.getId.call(this, el) || el.name;
    },
    getValue: function getValue(el) {
      // This returns a non-undefined value only when there's a 'data-restore'
      // attribute, which is set only when restoring Shiny state. If a file is
      // uploaded through the browser, 'data-restore' gets cleared.
      var data = $(el).attr('data-restore');
      if (data) {
        data = JSON.parse(data);

        // Set the label in the text box
        var $fileText = $(el).closest('div.input-group').find('input[type=text]');
        if (data.name.length === 1) {
          $fileText.val(data.name[0]);
        } else {
          $fileText.val(data.name.length + " files");
        }

        // Manually set up progress bar. A bit inelegant because it duplicates
        // code from FileUploader, but duplication is less bad than alternatives.
        var $progress = $(el).closest('div.form-group').find('.progress');
        var $bar = $progress.find('.progress-bar');
        $progress.removeClass('active');
        $bar.width('100%');
        $bar.css('visibility', 'visible');

        return data;
      } else {
        return null;
      }
    },
    setValue: function setValue(el, value) {
      // Not implemented
    },
    getType: function getType(el) {
      // This will be used only when restoring a file from a saved state.
      return 'shiny.file';
    },
    subscribe: function subscribe(el, callback) {
      $(el).on('change.fileInputBinding', uploadFiles);
    },
    unsubscribe: function unsubscribe(el) {
      $(el).off('.fileInputBinding');
    }
  });
  inputBindings.register(fileInputBinding, 'shiny.fileInputBinding');

  //---------------------------------------------------------------------
  // Source file: ../srcjs/init_shiny.js

  function initShiny() {

    var shinyapp = exports.shinyapp = new ShinyApp();

    function bindOutputs() {
      var scope = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : document;

      scope = $(scope);

      var bindings = outputBindings.getBindings();

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy
          if (!id) continue;

          // In some uncommon cases, elements that are later in the
          // matches array can be removed from the document by earlier
          // iterations. See https://github.com/rstudio/shiny/issues/1399
          if (!$.contains(document, el)) continue;

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

    function unbindOutputs() {
      var scope = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : document;
      var includeSelf = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var outputs = $(scope).find('.shiny-bound-output');

      if (includeSelf && $(scope).hasClass('shiny-bound-output')) {
        outputs.push(scope);
      }

      for (var i = 0; i < outputs.length; i++) {
        var $el = $(outputs[i]);
        var bindingAdapter = $el.data('shiny-output-binding');
        if (!bindingAdapter) continue;
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

    exports.onInputChange = function (name, value, opts) {
      opts = addDefaultInputOpts(opts);
      inputs.setInput(name, value, opts);
    };

    var boundInputs = {};

    function valueChangeCallback(binding, el, allowDeferred) {
      var id = binding.getId(el);
      if (id) {
        var value = binding.getValue(el);
        var type = binding.getType(el);
        if (type) id = id + ":" + type;

        var opts = { immediate: !allowDeferred, binding: binding, el: el };
        inputs.setInput(id, value, opts);
      }
    }

    function bindInputs() {
      var scope = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : document;

      var bindings = inputBindings.getBindings();

      var inputItems = {};

      for (var i = 0; i < bindings.length; i++) {
        var binding = bindings[i].binding;
        var matches = binding.find(scope) || [];
        for (var j = 0; j < matches.length; j++) {
          var el = matches[j];
          var id = binding.getId(el);

          // Check if ID is falsy, or if already bound
          if (!id || boundInputs[id]) continue;

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
          var thisCallback = function () {
            var thisBinding = binding;
            var thisEl = el;
            return function (allowDeferred) {
              valueChangeCallback(thisBinding, thisEl, allowDeferred);
            };
          }();

          binding.subscribe(el, thisCallback);
          $(el).data('shiny-input-binding', binding);
          $(el).addClass('shiny-bound-input');
          var ratePolicy = binding.getRatePolicy(el);
          if (ratePolicy !== null) {
            inputsRate.setRatePolicy(effectiveId, ratePolicy.policy, ratePolicy.delay);
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

    function unbindInputs() {
      var scope = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : document;
      var includeSelf = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      var inputs = $(scope).find('.shiny-bound-input');

      if (includeSelf && $(scope).hasClass('shiny-bound-input')) {
        inputs.push(scope);
      }

      for (var i = 0; i < inputs.length; i++) {
        var el = inputs[i];
        var binding = $(el).data('shiny-input-binding');
        if (!binding) continue;
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
    function unbindAll(scope) {
      var includeSelf = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : false;

      unbindInputs(scope, includeSelf);
      unbindOutputs(scope, includeSelf);
    }
    exports.bindAll = function (scope) {
      // _bindAll returns input values; it doesn't send them to the server.
      // export.bindAll needs to send the values to the server.
      var currentInputItems = _bindAll(scope);
      $.each(currentInputItems, function (name, item) {
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
    function initializeInputs() {
      var scope = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : document;

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
      if (!bindingAdapter) return null;else return bindingAdapter.getId();
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
    var initialValues = _.mapValues(_bindAll(document), function (x) {
      return x.value;
    });

    // The server needs to know the size of each image and plot output element,
    // in case it is auto-sizing
    $('.shiny-image-output, .shiny-plot-output').each(function () {
      var id = getIdFromEl(this);
      if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
        initialValues['.clientdata_output_' + id + '_width'] = this.offsetWidth;
        initialValues['.clientdata_output_' + id + '_height'] = this.offsetHeight;
      }
    });
    function doSendImageSize() {
      $('.shiny-image-output, .shiny-plot-output').each(function () {
        var id = getIdFromEl(this);
        if (this.offsetWidth !== 0 || this.offsetHeight !== 0) {
          inputs.setInput('.clientdata_output_' + id + '_width', this.offsetWidth);
          inputs.setInput('.clientdata_output_' + id + '_height', this.offsetHeight);
        }
      });
      $('.shiny-bound-output').each(function () {
        var $this = $(this),
            binding = $this.data('shiny-output-binding');
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
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendImageSizeDebouncer.isPending()) sendImageSizeDebouncer.immediateCall();
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
        return isHidden(obj.parentNode);
      }
    }
    var lastKnownVisibleOutputs = {};
    // Set initial state of outputs to hidden, if needed
    $('.shiny-bound-output').each(function () {
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
      $('.shiny-bound-output').each(function () {
        var id = getIdFromEl(this);
        delete lastKnownVisibleOutputs[id];
        // Assume that the object is hidden when width and height are 0
        var hidden = isHidden(this),
            evt = {
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
        if (lastKnownVisibleOutputs.hasOwnProperty(name)) inputs.setInput('.clientdata_output_' + name + '_hidden', true);
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
    inputBatchSender.lastChanceCallback.push(function () {
      if (sendOutputHiddenStateDebouncer.isPending()) sendOutputHiddenStateDebouncer.immediateCall();
    });

    // Given a namespace and a handler function, return a function that invokes
    // the handler only when e's namespace matches. For example, if the
    // namespace is "bs", it would match when e.namespace is "bs" or "bs.tab".
    // If the namespace is "bs.tab", it would match for "bs.tab", but not "bs".
    function filterEventsByNamespace(namespace, handler) {
      namespace = namespace.split(".");

      return function (e) {
        var eventNamespace = e.namespace.split(".");

        // If any of the namespace strings aren't present in this event, quit.
        for (var i = 0; i < namespace.length; i++) {
          if (eventNamespace.indexOf(namespace[i]) === -1) return;
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
    $.each(bs3classes, function (idx, classname) {
      $('body').on('shown.bs.' + classname + '.sendImageSize', '*', filterEventsByNamespace('bs', sendImageSize));
      $('body').on('shown.bs.' + classname + '.sendOutputHiddenState ' + 'hidden.bs.' + classname + '.sendOutputHiddenState', '*', filterEventsByNamespace('bs', sendOutputHiddenState));
    });

    // This is needed for Bootstrap 2 compatibility and for non-Bootstrap
    // related shown/hidden events (like conditionalPanel)
    $('body').on('shown.sendImageSize', '*', sendImageSize);
    $('body').on('shown.sendOutputHiddenState hidden.sendOutputHiddenState', '*', sendOutputHiddenState);

    // Send initial pixel ratio, and update it if it changes
    initialValues['.clientdata_pixelratio'] = pixelRatio();
    $(window).resize(function () {
      inputs.setInput('.clientdata_pixelratio', pixelRatio());
    });

    // Send initial URL
    initialValues['.clientdata_url_protocol'] = window.location.protocol;
    initialValues['.clientdata_url_hostname'] = window.location.hostname;
    initialValues['.clientdata_url_port'] = window.location.port;
    initialValues['.clientdata_url_pathname'] = window.location.pathname;

    // Send initial URL search (query string) and update it if it changes
    initialValues['.clientdata_url_search'] = window.location.search;

    $(window).on('pushstate', function (e) {
      inputs.setInput('.clientdata_url_search', window.location.search);
    });

    $(window).on('popstate', function (e) {
      inputs.setInput('.clientdata_url_search', window.location.search);
    });

    // This is only the initial value of the hash. The hash can change, but
    // a reactive version of this isn't sent because watching for changes can
    // require polling on some browsers. The JQuery hashchange plugin can be
    // used if this capability is important.
    initialValues['.clientdata_url_hash_initial'] = window.location.hash;
    initialValues['.clientdata_url_hash'] = window.location.hash;

    $(window).on('hashchange', function (e) {
      inputs.setInput('.clientdata_url_hash', location.hash);
    });

    // The server needs to know what singletons were rendered as part of
    // the page loading
    var singletonText = initialValues['.clientdata_singletons'] = $('script[type="application/shiny-singletons"]').text();
    singletons.registerNames(singletonText.split(/,/));

    var dependencyText = $('script[type="application/html-dependencies"]').text();
    $.each(dependencyText.split(/;/), function (i, depStr) {
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
    $(document).one("shiny:connected", function () {
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

  $(function () {
    // Init Shiny a little later than document ready, so user code can
    // run first (i.e. to register bindings)
    setTimeout(initShiny, 1);
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/reactlog.js

  $(document).on('keydown', function (e) {
    if (e.which !== 114 || !e.ctrlKey && !e.metaKey || e.shiftKey || e.altKey) return;
    var url = 'reactlog?w=' + window.escape(exports.shinyapp.config.workerId) + "&s=" + window.escape(exports.shinyapp.config.sessionId);
    window.open(url);
    e.preventDefault();
  });

  //---------------------------------------------------------------------
  // Source file: ../srcjs/_end.js
})();

//# sourceMappingURL=shiny.js.map
//# sourceMappingURL=shiny.js.map
