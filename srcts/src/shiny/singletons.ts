import $ from "jquery";

const _reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
const _reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;

const knownSingletons = {};

function renderHtml(
  html,
  el,
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
    el.insertAdjacentHTML(where, processed.html);
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
    const tempDiv = $("<div>" + head + "</div>")[0];
    const $head = $("head");

    while (tempDiv.hasChildNodes()) {
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
