import $ from "jquery";
import { bindScope } from "./bind";

const _reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
const _reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;

const knownSingletons = {};

function renderHtml(
  html,
  el: bindScope,
  where
): {
  html: any;
  head: string;
  singletons: Record<string, true>;
} {
  const processed = _processHtml(html);

  _addToHead(processed.head);
  register(processed.singletons);
  if (where === "replace") {
    $(el).html(processed.html);
  } else {
    let elElements: Array<HTMLElement>;

    if (el instanceof HTMLElement) {
      elElements = [el];
    } else {
      elElements = el.toArray();
    }
    $.each(elElements, (i, el) => {
      el.insertAdjacentHTML(where, processed.html);
    });
  }
  return processed;
}
// Take an object where keys are names of singletons, and merges it into
// knownSingletons
function register(s) {
  $.extend(knownSingletons, s);
}
// Takes a string or array of strings and adds them to knownSingletons
function registerNames(s): void {
  if (typeof s === "string") {
    knownSingletons[s] = true;
  } else if (s instanceof Array) {
    for (let i = 0; i < s.length; i++) {
      knownSingletons[s[i]] = true;
    }
  }
}
// Inserts new content into document head
function _addToHead(head) {
  if (head.length > 0) {
    const tempDiv = $("<div>" + head + "</div>").get(0);
    const $head = $("head");

    while (tempDiv.hasChildNodes()) {
      // @ts-expect-error; TODO-barret; IDK how this function works. Seems like it would add the first child forever.
      $head.append(tempDiv.firstChild);
    }
  }
}
// Reads HTML and returns an object with info about singletons
function _processHtml(val) {
  const newSingletons = {};
  let newVal;

  const findNewPayload = function (match, p1, sig, payload) {
    if (knownSingletons[sig] || newSingletons[sig]) return "";
    newSingletons[sig] = true;
    return payload;
  };

  // eslint-disable-next-line no-constant-condition
  while (true) {
    newVal = val.replace(_reSingleton, findNewPayload);
    if (val.length === newVal.length) break;
    val = newVal;
  }

  const heads = [];
  const headAddPayload = function (match, payload) {
    heads.push(payload);
    return "";
  };

  // eslint-disable-next-line no-constant-condition
  while (true) {
    newVal = val.replace(_reHead, headAddPayload);
    if (val.length === newVal.length) break;
    val = newVal;
  }

  return {
    html: val,
    head: heads.join("\n"),
    singletons: newSingletons,
  };
}

export { renderHtml, registerNames };
