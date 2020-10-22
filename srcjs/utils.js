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
  var expr_escaped = expr
    .replace(/[\\"']/g, '\\$&')
    .replace(/\u0000/g, '\\0')
    .replace(/\n/g, '\\n')
    .replace(/\r/g, '\\r')
    // \b has a special meaning; need [\b] to match backspace char.
    .replace(/[\b]/g, '\\b');

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
      newObj[key] = f(obj[key], key, obj);
  }
  return newObj;
}

// This is does the same as Number.isNaN, but that function unfortunately does
// not exist in any version of IE.
function isnan(x) {
  return typeof(x) === 'number' && isNaN(x);
}

// Binary equality function used by the equal function.
function _equal(x, y) {
  if ($.type(x) === "object" && $.type(y) === "object") {
    if (Object.keys(x).length !== Object.keys(y).length) return false;
    for (let prop in x)
      if (!y.hasOwnProperty(prop) || !_equal(x[prop], y[prop]))
        return false;
    return true;
  } else if ($.type(x) === "array" && $.type(y) === "array") {
    if (x.length !== y.length) return false;
    for (let i = 0; i < x.length; i++)
      if (!_equal(x[i], y[i])) return false;
    return true;
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

// Compare version strings like "1.0.1", "1.4-2". `op` must be a string like
// "==" or "<".
exports.compareVersion = function(a, op, b) {
  function versionParts(ver) {
    return (ver + "")
      .replace(/-/, ".")
      .replace(/(\.0)+[^\.]*$/, "")
      .split(".");
  }

  function cmpVersion(a, b) {
    a = versionParts(a);
    b = versionParts(b);
    var len = Math.min(a.length, b.length);
    var cmp;

    for(var i=0; i<len; i++) {
      cmp = parseInt(a[i], 10) - parseInt(b[i], 10);
      if(cmp !== 0) {
        return cmp;
      }
    }
    return a.length - b.length;
  }

  var diff = cmpVersion(a, b);

  if (op === "==")      return (diff === 0);
  else if (op === ">=") return (diff >=  0);
  else if (op === ">")  return (diff >   0);
  else if (op === "<=") return (diff <=  0);
  else if (op === "<")  return (diff <   0);
  else                  throw `Unknown operator: ${op}`;
};


function updateLabel(labelTxt, labelNode) {
  // Only update if label was specified in the update method
  if (typeof labelTxt === "undefined") return;
  if (labelNode.length !== 1) {
    throw new Error("labelNode must be of length 1");
  }

  // Should the label be empty?
  var emptyLabel = $.isArray(labelTxt) && labelTxt.length === 0;

  if (emptyLabel) {
    labelNode.addClass("shiny-label-null");
  } else {
    labelNode.text(labelTxt);
    labelNode.removeClass("shiny-label-null");
  }

}
