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
  var func = new Function("with (this) {return (" + expr + ");}");
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
