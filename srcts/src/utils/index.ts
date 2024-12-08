import $ from "jquery";
import { windowDevicePixelRatio } from "../window/pixelRatio";
import type { MapValuesUnion, MapWithResult } from "./extraTypes";
import { hasDefinedProperty, hasOwnProperty } from "./object";

function escapeHTML(str: string): string {
  /* eslint-disable @typescript-eslint/naming-convention */
  const escaped: { [key: string]: string } = {
    "&": "&amp;",
    "<": "&lt;",
    ">": "&gt;",
    // eslint-disable-next-line prettier/prettier
    '"': "&quot;",
    "'": "&#039;",
    "/": "&#x2F;",
  };
  /* eslint-enable @typescript-eslint/naming-convention */

  return str.replace(/[&<>'"/]/g, function (m) {
    return escaped[m] as string;
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
function getStyle(el: Element, styleProp: string): string | undefined {
  let x = undefined;

  if ("currentStyle" in el) {
    // @ts-expect-error; Old, IE 5+ attribute only - https://developer.mozilla.org/en-US/docs/Web/API/Element/currentStyle
    x = el.currentStyle[styleProp];
  } else {
    // getComputedStyle can return null when we're inside a hidden iframe on
    // Firefox; don't attempt to retrieve style props in this case.
    // https://bugzilla.mozilla.org/show_bug.cgi?id=548397
    const style = document?.defaultView?.getComputedStyle(el, null);

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
function parseDate(dateString: string): Date {
  let date = new Date(dateString);

  if (date.toString() === "Invalid Date") {
    date = new Date(dateString.replace(/-/g, "/"));
  }
  return date;
}

// Given a Date object, return a string in yyyy-mm-dd format, using the
// UTC date. This may be a day off from the date in the local time zone.
function formatDateUTC(x: Date): string;
function formatDateUTC(date: Date | null): string | null {
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
type LastSizeInterface = {
  w?: number;
  h?: number;
};
function makeResizeFilter(
  el: HTMLElement,
  func: (
    width: HTMLElement["offsetWidth"],
    height: HTMLElement["offsetHeight"]
  ) => void
): () => void {
  let lastSize: LastSizeInterface = {};

  return function () {
    const rect = el.getBoundingClientRect();
    const size = { w: rect.width, h: rect.height };

    if (size.w === 0 && size.h === 0) return;
    if (size.w === lastSize.w && size.h === lastSize.h) return;
    lastSize = size;
    func(size.w, size.h);
  };
}

function pixelRatio(): number {
  if (windowDevicePixelRatio()) {
    return Math.round(windowDevicePixelRatio() * 100) / 100;
  } else {
    return 1;
  }
}

function getBoundingClientSizeBeforeZoom(el: HTMLElement): {
  width: number;
  height: number;
} {
  const rect = el.getBoundingClientRect();
  // Cast to any because currentCSSZoom isn't in the type def of HTMLElement
  // TODO: typescript >= 5.5.2 added this property to the type definition
  const zoom = (el as any).currentCSSZoom || 1;
  return {
    width: rect.width / zoom,
    height: rect.height / zoom,
  };
}

// Takes a string expression and returns a function that takes an argument.
//
// When the function is executed, it will evaluate that expression using
// "with" on the argument value, and return the result.
function scopeExprToFunc(expr: string): (scope: unknown) => unknown {
  /*jshint evil: true */
  const exprEscaped = expr
    .replace(/[\\"']/g, "\\$&")
    // eslint-disable-next-line no-control-regex
    .replace(/\u0000/g, "\\0")
    .replace(/\n/g, "\\n")
    .replace(/\r/g, "\\r")
    // \b has a special meaning; need [\b] to match backspace char.
    .replace(/[\b]/g, "\\b");

  let func: () => unknown;

  try {
    // @ts-expect-error; Do not know how to type this _dangerous_ situation
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

  return function (scope: unknown): unknown {
    return func.call(scope);
  };
}

function asArray<T>(value: T | T[] | null | undefined): T[] {
  if (value === null || value === undefined) return [];
  if (Array.isArray(value)) return value;
  return [value];
}

// We need a stable sorting algorithm for ordering
// bindings by priority and insertion order.
function mergeSort<Item>(
  list: Item[],
  sortfunc: (a: Item, b: Item) => boolean | number
): Item[] {
  function merge(a: Item[], b: Item[]) {
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
      const merged = merge(listA, listB);
      const args = [i, merged.length] as [number, number];

      Array.prototype.push.apply(args, merged);
      Array.prototype.splice.apply(list, args);
    }
  }

  return list;
}

// Escape jQuery selector metacharacters: !"#$%&'()*+,./:;<=>?@[\]^`{|}~
function $escape(val: undefined): undefined;
function $escape(val: string): string;
function $escape(val: string | undefined): string | undefined {
  if (typeof val === "undefined") return val;
  return val.replace(/([!"#$%&'()*+,./:;<=>?@[\\\]^`{|}~])/g, "\\$1");
}

// Maps a function over an object, preserving keys. Like the mapValues
// function from lodash.
function mapValues<T extends { [key: string]: any }, R>(
  obj: T,
  f: (value: MapValuesUnion<T>, key: string, object: typeof obj) => R
): MapWithResult<T, R> {
  const newObj = {} as MapWithResult<T, R>;

  Object.keys(obj).forEach((key: keyof typeof obj) => {
    newObj[key] = f(obj[key], key as string, obj);
  });
  return newObj;
}

// This is does the same as Number.isNaN, but that function unfortunately does
// not exist in any version of IE.
function isnan(x: unknown): boolean {
  return typeof x === "number" && isNaN(x);
}

// Binary equality function used by the equal function.
// (Name existed before TS conversion)
// eslint-disable-next-line @typescript-eslint/naming-convention
function _equal(x: unknown, y: unknown): boolean {
  if ($.type(x) === "object" && $.type(y) === "object") {
    const xo = x as { [key: string]: unknown };
    const yo = y as { [key: string]: unknown };

    if (Object.keys(xo).length !== Object.keys(yo).length) return false;
    for (const prop in xo) {
      if (!hasOwnProperty(yo, prop) || !_equal(xo[prop], yo[prop]))
        return false;
    }
    return true;
  } else if ($.type(x) === "array" && $.type(y) === "array") {
    const xa = x as unknown[];
    const ya = y as unknown[];

    if (xa.length !== ya.length) return false;
    for (let i = 0; i < xa.length; i++) if (!_equal(xa[i], ya[i])) return false;
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
function equal(...args: unknown[]): boolean {
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
  op: "<" | "<=" | "==" | ">" | ">=",
  b: string
): boolean {
  function versionParts(ver: string) {
    return (ver + "")
      .replace(/-/, ".")
      .replace(/(\.0)+[^.]*$/, "")
      .split(".");
  }

  function cmpVersion(a: string, b: string) {
    const aParts = versionParts(a);
    const bParts = versionParts(b);
    const len = Math.min(aParts.length, bParts.length);
    let cmp;

    for (let i = 0; i < len; i++) {
      cmp = parseInt(aParts[i], 10) - parseInt(bParts[i], 10);
      if (cmp !== 0) {
        return cmp;
      }
    }
    return aParts.length - bParts.length;
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
  labelTxt: string | undefined,
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

function isBS3(): boolean {
  // @ts-expect-error; Check if `window.bootstrap` exists
  return !window.bootstrap;
}

function toLowerCase<T extends string>(str: T): Lowercase<T> {
  return str.toLowerCase() as Lowercase<T>;
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
  getBoundingClientSizeBeforeZoom,
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
  hasOwnProperty,
  hasDefinedProperty,
  isBS3,
  toLowerCase,
};
