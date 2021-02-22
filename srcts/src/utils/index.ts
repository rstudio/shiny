import { devicePixelRatio } from "../window/pixelRatio";
import { $ } from "../jquery";
import { makeBlob } from "./blob";

function escapeHTML(str: string): string {
  const escaped = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    // eslint-disable-next-line prettier/prettier
    "\"": "&quot;",
    "'": "&#039;",
    "/": "&#x2F;",
  };

  return str.replace(/[&<>'"/]/g, function (m) {
    return escaped[m];
  });
}

function randomId(): string {
  return Math.floor(0x100000000 + Math.random() * 0xf00000000).toString(16);
}

function strToBool(str: string): boolean | undefined {
  if (!str || !str.toLowerCase) return undefined;

  switch (str.toLowerCase()) {
    case "true":
      return true;
    case "false":
      return false;
    default:
      return undefined;
  }
}

// A wrapper for getComputedStyle that is compatible with older browsers.
// This is significantly faster than jQuery's .css() function.
function getStyle(el: any, styleProp: string): string | undefined {
  let x = undefined;

  if (el.currentStyle) x = el.currentStyle[styleProp];
  else if (window.getComputedStyle) {
    // getComputedStyle can return null when we're inside a hidden iframe on
    // Firefox; don't attempt to retrieve style props in this case.
    // https://bugzilla.mozilla.org/show_bug.cgi?id=548397
    const style = document.defaultView.getComputedStyle(el, null);

    if (style) x = style.getPropertyValue(styleProp);
  }
  return x;
}

// Convert a number to a string with leading zeros
function padZeros(n: number, digits: number): string {
  let str = n.toString();

  while (str.length < digits) str = "0" + str;
  return str;
}

// Round to a specified number of significant digits.
function roundSignif(x: number, digits = 1): number {
  if (digits < 1) throw "Significant digits must be at least 1.";

  // This converts to a string and back to a number, which is inelegant, but
  // is less prone to FP rounding error than an alternate method which used
  // Math.round().
  return parseFloat(x.toPrecision(digits));
}

// Take a string with format "YYYY-MM-DD" and return a Date object.
// IE8 and QTWebKit don't support YYYY-MM-DD, but they support YYYY/MM/DD
function parseDate(dateString: any): Date {
  let date = new Date(dateString);

  if (date.toString() === "Invalid Date") {
    date = new Date(dateString.replace(/-/g, "/"));
  }
  return date;
}

// Given a Date object, return a string in yyyy-mm-dd format, using the
// UTC date. This may be a day off from the date in the local time zone.
function formatDateUTC(date: any): null | string {
  if (date instanceof Date) {
    return (
      date.getUTCFullYear() +
      "-" +
      padZeros(date.getUTCMonth() + 1, 2) +
      "-" +
      padZeros(date.getUTCDate(), 2)
    );
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
interface lastSizeInterface {
  w?: number;
  h?: number;
}
function makeResizeFilter(
  el: HTMLElement,
  func: (width: any, height: any) => any
): () => any {
  let lastSize: lastSizeInterface = {};

  return function () {
    const size = { w: el.offsetWidth, h: el.offsetHeight };

    if (size.w === 0 && size.h === 0) return;
    if (size.w === lastSize.w && size.h === lastSize.h) return;
    lastSize = size;
    func(size.w, size.h);
  };
}

function pixelRatio(): number {
  if (devicePixelRatio()) {
    return Math.round(window.devicePixelRatio * 100) / 100;
  } else {
    return 1;
  }
}

// Takes a string expression and returns a function that takes an argument.
//
// When the function is executed, it will evaluate that expression using
// "with" on the argument value, and return the result.
scopeExprToFunc.call;
function scopeExprToFunc(expr: string): (scope: any) => any {
  /*jshint evil: true */
  const exprEscaped = expr
    .replace(/[\\"']/g, "\\$&")
    // eslint-disable-next-line no-control-regex
    .replace(/\u0000/g, "\\0")
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r")
    // \b has a special meaning; need [\b] to match backspace char.
    .replace(/[\b]/g, "\\b");

  let func: any;

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
  } catch (e) {
    console.error("Error parsing expression: " + expr);
    throw e;
  }

  return function (scope) {
    return func.call(scope);
  };
}

function asArray<T>(value: T | Array<T> | null | undefined): Array<T> {
  if (value === null || value === undefined) return [];
  if (Array.isArray(value)) return value;
  return [value];
}

// We need a stable sorting algorithm for ordering
// bindings by priority and insertion order.
function mergeSort<T>(
  list: Array<T>,
  sortfunc: (a: any, b: any) => boolean
): Array<T> {
  function merge(sortfunc, a, b) {
    let ia = 0;
    let ib = 0;
    const sorted = [];

    while (ia < a.length && ib < b.length) {
      if (sortfunc(a[ia], b[ib]) <= 0) {
        sorted.push(a[ia++]);
      } else {
        sorted.push(b[ib++]);
      }
    }
    while (ia < a.length) sorted.push(a[ia++]);
    while (ib < b.length) sorted.push(b[ib++]);
    return sorted;
  }

  // Don't mutate list argument
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

// Escape jQuery selector metacharacters: !"#$%&'()*+,./:;<=>?@[\]^`{|}~
const $escape = (exports.$escape = function (val) {
  return val.replace(/([!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~])/g, "\\$1");
});

// Maps a function over an object, preserving keys. Like the mapValues
// function from lodash.
function mapValues(
  obj: Record<string, unknown>,
  f: (value: unknown, key: string, obj: Record<string, unknown>) => unknown
): Record<string, unknown> {
  const newObj: Record<string, unknown> = {};

  for (const key in obj) {
    // eslint-disable-next-line no-prototype-builtins
    if (obj.hasOwnProperty(key)) newObj[key] = f(obj[key], key, obj);
  }
  return newObj;
}

// This is does the same as Number.isNaN, but that function unfortunately does
// not exist in any version of IE.
function isnan(x: unknown): boolean {
  return typeof x === "number" && isNaN(x);
}

// Binary equality function used by the equal function.
function _equal(x: any, y: any): boolean {
  if ($.type(x) === "object" && $.type(y) === "object") {
    if (Object.keys(x).length !== Object.keys(y).length) return false;
    for (const prop in x) {
      // eslint-disable-next-line no-prototype-builtins
      if (!y.hasOwnProperty(prop) || !_equal(x[prop], y[prop])) return false;
    }
    return true;
  } else if ($.type(x) === "array" && $.type(y) === "array") {
    if (x.length !== y.length) return false;
    for (let i = 0; i < x.length; i++) if (!_equal(x[i], y[i])) return false;
    return true;
  } else {
    return x === y;
  }
}

// Structural or "deep" equality predicate. Tests two or more arguments for
// equality, traversing arrays and objects (as determined by $.type) as
// necessary.
//
// Objects other than objects and arrays are tested for equality using ===.
function equal(...args): boolean {
  if (args.length < 2)
    throw new Error("equal requires at least two arguments.");
  for (let i = 0; i < args.length - 1; i++) {
    if (!_equal(args[i], args[i + 1])) return false;
  }
  return true;
}

// Compare version strings like "1.0.1", "1.4-2". `op` must be a string like
// "==" or "<".
const compareVersion = function (
  a: string,
  op: "==" | ">=" | ">" | "<=" | "<",
  b: string
): boolean {
  function versionParts(ver) {
    return (ver + "")
      .replace(/-/, ".")
      .replace(/(\.0)+[^.]*$/, "")
      .split(".");
  }

  function cmpVersion(a, b) {
    a = versionParts(a);
    b = versionParts(b);
    const len = Math.min(a.length, b.length);
    let cmp;

    for (let i = 0; i < len; i++) {
      cmp = parseInt(a[i], 10) - parseInt(b[i], 10);
      if (cmp !== 0) {
        return cmp;
      }
    }
    return a.length - b.length;
  }

  const diff = cmpVersion(a, b);

  if (op === "==") return diff === 0;
  else if (op === ">=") return diff >= 0;
  else if (op === ">") return diff > 0;
  else if (op === "<=") return diff <= 0;
  else if (op === "<") return diff < 0;
  else throw `Unknown operator: ${op}`;
};

function updateLabel(
  labelTxt: undefined | string,
  labelNode: JQuery<HTMLElement>
): void {
  // Only update if label was specified in the update method
  if (typeof labelTxt === "undefined") return;
  if (labelNode.length !== 1) {
    throw new Error("labelNode must be of length 1");
  }

  // Should the label be empty?
  const emptyLabel = Array.isArray(labelTxt) && labelTxt.length === 0;

  if (emptyLabel) {
    labelNode.addClass("shiny-label-null");
  } else {
    labelNode.text(labelTxt);
    labelNode.removeClass("shiny-label-null");
  }
}

// Compute the color property of an a tag, scoped within the element
function getComputedLinkColor(el: HTMLElement): string {
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

export {
  escapeHTML,
  randomId,
  strToBool,
  getStyle,
  padZeros,
  roundSignif,
  parseDate,
  formatDateUTC,
  makeResizeFilter,
  pixelRatio,
  scopeExprToFunc,
  asArray,
  mergeSort,
  $escape,
  mapValues,
  isnan,
  _equal,
  equal,
  compareVersion,
  updateLabel,
  getComputedLinkColor,
  makeBlob,
};
