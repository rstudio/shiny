function escapeHTML(str) {
  var escaped = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    '"': "&quot;",
    "'": "&#039;",
    "/": "&#x2F;"
  };

  return str.replace(/[&<>'"\/]/g, function(m) {
    return escaped[m];
  });
}

function randomId() {
  return Math.floor(0x100000000 + (Math.random() * 0xF00000000)).toString(16);
}

function strToBool(str) {
  if (!str || !str.toLowerCase)
    return undefined;

  switch(str.toLowerCase()) {
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
  if (el.currentStyle)
    x = el.currentStyle[styleProp];
  else if (window.getComputedStyle) {
    // getComputedStyle can return null when we're inside a hidden iframe on
    // Firefox; don't attempt to retrieve style props in this case.
    // https://bugzilla.mozilla.org/show_bug.cgi?id=548397
    var style = document.defaultView.getComputedStyle(el, null);
    if (style)
      x = style.getPropertyValue(styleProp);
  }
  return x;
}

// Convert a number to a string with leading zeros
function padZeros(n, digits) {
  var str = n.toString();
  while (str.length < digits)
    str = "0" + str;
  return str;
}

// Round to a specified number of significant digits.
function roundSignif(x, digits = 1) {
  if (digits < 1)
    throw "Significant digits must be at least 1.";

  // This converts to a string and back to a number, which is inelegant, but
  // is less prone to FP rounding error than an alternate method which used
  // Math.round().
  return parseFloat(x.toPrecision(digits));
}

// Take a string with format "YYYY-MM-DD" and return a Date object.
// IE8 and QTWebKit don't support YYYY-MM-DD, but they support YYYY/MM/DD
function parseDate(dateString) {
  var date = new Date(dateString);
  if (isNaN(date))
    date = new Date(dateString.replace(/-/g, "/"));
  return date;
}

// Given a Date object, return a string in yyyy-mm-dd format, using the
// UTC date. This may be a day off from the date in the local time zone.
function formatDateUTC(date) {
  if (date instanceof Date) {
    return date.getUTCFullYear() + '-' +
           padZeros(date.getUTCMonth()+1, 2) + '-' +
           padZeros(date.getUTCDate(), 2);

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
  return function() {
    var size = { w: el.offsetWidth, h: el.offsetHeight };
    if (size.w === 0 && size.h === 0)
      return;
    if (size.w === lastSize.w && size.h === lastSize.h)
      return;
    lastSize = size;
    func(size.w, size.h);
  };
}

var _BlobBuilder = window.BlobBuilder || window.WebKitBlobBuilder ||
    window.MozBlobBuilder || window.MSBlobBuilder;

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
  }
  catch (e) {
    var blobBuilder = new _BlobBuilder();
    $.each(parts, function(i, part) {
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
    var func = new Function(
      `with (this) {
        try {
          return (${expr});
        } catch (e) {
          console.error('Error evaluating expression: ${expr_escaped}');
          throw e;
        }
      }`
    );
  } catch (e) {
    console.error("Error parsing expression: " + expr);
    throw e;
  }


  return function(scope) {
    return func.call(scope);
  };
}

function asArray(value) {
  if (value === null || value === undefined)
    return [];
  if ($.isArray(value))
    return value;
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
      }
      else {
        sorted.push(b[ib++]);
      }
    }
    while (ia < a.length)
      sorted.push(a[ia++]);
    while (ib < b.length)
      sorted.push(b[ib++]);
    return sorted;
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
var $escape = exports.$escape = function(val) {
  return val.replace(/([!"#$%&'()*+,.\/:;<=>?@\[\\\]^`{|}~])/g, '\\$1');
};

// Maps a function over an object, preserving keys. Like the mapValues
// function from lodash.
function mapValues(obj, f) {
  const newObj = {};
  for (let key in obj) {
    if (obj.hasOwnProperty(key))
      newObj[key] = f(obj[key]);
  }
  return newObj;
}

// Binary equality function used by the equal function.
function _equal(x, y) {
  if ($.type(x) === "object" && $.type(y) === "object") {
    if (Object.keys(x).length !== Object.keys(y).length) return false;
    for (let prop in x)
      if (!y.hasOwnProperty(prop) || !_equal(x[prop], y[prop]))
        return false;
  } else if ($.type(x) === "array" && $.type(y) === "array") {
    if (x.length !== y.length) return false;
    for (let i = 0; i < x.length; i++)
      if (!_equal(x[i], y[i])) return false;
  } else {
    return (x === y);
  }
}

// Structural or "deep" equality predicate. Tests two or more arguments for
// equality, traversing arrays and objects (as determined by $.type) as
// necessary.
//
// Objects other than objects and arrays are tested for equality using ===.
function equal(...args) {
  if (args.length < 2) throw new Error("equal requires at least two arguments.");
  for (let i = 0; i < args.length-1; i++) {
    if (!_equal(args[i], args[i+1]))
      return false;
  }
  return true;
};

// Creates functions -- "multimethods" -- that are polymorphic on one or more of
// their arguments.
//
// Multimethods can take any number of arguments. Arguments are passed to an
// applicable function or "method", returning its result. By default, if no
// method was applicable, an exception is thrown.
//
// Methods are searched in the order that they were installed, and the first
// applicable method is the one used.
//
// A method is applicable when the "dispatch value" associated with it
// corresponds to the value returned by the dispatch function. The dispatch
// function defaults to the value of the first argument passed to the
// multimethod.
//
// The correspondence between the value returned by the dispatch function and
// any method's dispatch value is established by the test function, which is
// user-definable and defaults to "equals" or deep equality.
//
// # Chainable Functions
//
// The function returned by multimethod() exposes functions as properties. These
// functions generally return the multimethod, and so can be chained.
//
// - dispatch([function newDispatch]): Sets the dispatch function. The dispatch
//   function can take any number of arguments, but must return a dispatch
//   value. The default dispatch function returns the first argument passed to
//   the multimethod.
//
// - test([function newTest]): Sets the test function. The test function takes
//   two arguments: the dispatch value produced by the dispath function, and the
//   dispatch value associated with some method. It must return a boolean
//   indicating whether or not to select the method. The default test function
//   is deep equal.
//
// - when(object dispatchVal, function method): Adds a new dispatch value/method
//   combination.
//
// - whenAny(array<object> dispatchVals, function method): Like when, but
//   associates the method with every dispatch values contained in the
//   dispatchVals array.
//
// - else(function newDefaultMethod): Sets the default function. This function
//   is invoked when no methods apply. If not set, an exception is thrown.
//
// - clone(): Returns a new, functionally-equivalent multimethod. This is a way
//   to extend an existing multimethod in a local context -- such as inside a
//   function -- without modifying the original. NOTE: The array of methods is
//   copied, but the dispatch values themselves are not. It's not a good idea to
//   mutate dispatch values.
//
// # Self-calls
//
// The multimethod function can obtained inside its constituent method bodies,
// without referring to it by name.
//
// This makes it possible for one method to call another, or to pass the
// multimethod to other functions as a callback, within methods.
//
// The mechanism is: the multimethod itself is bound as "this" to methods when
// they are called. Since arrow functions cannot be bound to objects, **self-calls
// are only possible within methods created using the "function" keyword**.
//
// # Examples
//
// Handling events:
//
//   var handleEvent = multimethod()
//    .dispatch(e => [e.target.tagName.toLowerCase(), e.type])
//    .when(["h1", "click"], e => "you clicked on an h1")
//    .when(["p", "mouseover"], e => "you moused over a p"})
//    .else(e => {
//      let tag = e.target.tagName.toLowerCase();
//      return `you did ${e.type} to an ${tag}`;
//    });
//
//   $(document).on("click mouseover mouseup mousedown", e => console.log(handle(e)))
//
// Self-calls:
//
//   var demoSelfCall = multimethod()
//    .when(0, function(n) {
//      this(1);
//    })
//    .when(1, function(n) {
//      doSomething(this);
//    })
//    .when(2, _ => console.log("tada"));
//
// Using (abusing?) the test function:
//
//   var fizzBuzz = multimethod()
//    .test((x, divs) => divs.map(d => x % d === 0).every(Boolean))
//    .when([3, 5], x => "FizzBuzz")
//    .when([3], x => "Fizz")
//    .when([5], x => "Buzz")
//    .else(x => x);
//
//   for(let i = 0; i <= 100; i++) console.log(fizzBuzz(i));
//
// Getting carried away:
//
//   var factorial = multimethod()
//    .when(0, () => 1)
//    .when(1, (_, prod = 1) => prod)
//    .else(function(n, prod = 1) {
//       return this(n-1, n*prod);
//    });
function multimethod(dispatch = (firstArg) => firstArg,
                     test = _equal,
                     defaultMethod = null,
                     methods = []) {

  function RecurArgs (args) { this.args = args; }
  function recur(...args) { return new RecurArgs(args); }

  let invoke = (...args) => {
    var dispatchVal = dispatch.apply(null, args), ret;
    for (let i = 0; i < methods.length; i++) {
      let [methodVal, methodFn] = methods[i];
      if (test(dispatchVal, methodVal)) {
        ret = methodFn.apply(recur, args);
        while (ret instanceof RecurArgs)
          ret = invoke.apply(recur, ret.args);
        return ret;
      }
    }
    if (defaultMethod) {
      ret = defaultMethod.apply(recur, args);
      while (ret instanceof RecurArgs)
        ret = invoke.apply(recur, ret.args);
      return ret;
    } else {
      throw new Error(`No method for dispatch value ${dispatchVal}`);
    }
  };
  invoke.dispatch = (newDispatch = (firstArg) => firstArg) => {
    dispatch = newDispatch;
    return invoke;
  };
  invoke.test = (newTest = _equal) => {
    test = newTest;
    return invoke;
  };
  invoke.when = (dispatchVal, methodFn) => {
    methods = methods.concat([[dispatchVal, methodFn]]);
    return invoke;
  };
  invoke.whenAny = (dispatchVals, methodFn) => {
    methods = methods.concat((dispatchVals).map(v => [v, methodFn]));
    return invoke;
  };
  invoke.else = (newDefaultMethod = null) => {
    defaultMethod = newDefaultMethod;
    return invoke;
  };
  invoke.clone = () => {
    return multimethod(dispatch, test, defaultMethod, methods.slice());
  };
  return invoke;
}

