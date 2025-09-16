import $ from "jquery";
import type { BindScope } from "./bind";

const reSingleton = /<!--(SHINY.SINGLETON\[([\w]+)\])-->([\s\S]*?)<!--\/\1-->/;
const reHead = /<head(?:\s[^>]*)?>([\s\S]*?)<\/head>/;

const knownSingletons: { [key: string]: boolean } = {};

type WherePosition =
  | "afterBegin"
  | "afterEnd"
  | "beforeBegin"
  | "beforeEnd"
  | "replace";

function renderHtml(
  html: string,
  el: BindScope,
  where: WherePosition,
): ReturnType<typeof processHtml> {
  const processed = processHtml(html);

  addToHead(processed.head);
  register(processed.singletons);

  // N.B. even though the DOM insertion below _could_ be done with vanilla JS,
  // we intentionally use jQuery so that <script> tags execute.
  // https://github.com/rstudio/shiny/pull/3630
  switch (where.toLowerCase()) {
    case "replace":
      $(el).html(processed.html);
      break;
    case "beforebegin":
      $(el).before(processed.html);
      break;
    case "afterbegin":
      $(el).prepend(processed.html);
      break;
    case "beforeend":
      $(el).append(processed.html);
      break;
    case "afterend":
      $(el).after(processed.html);
      break;
    default:
      throw new Error("Unknown where position: " + where);
  }

  return processed;
}
// Take an object where keys are names of singletons, and merges it into
// knownSingletons
function register(s: typeof knownSingletons) {
  $.extend(knownSingletons, s);
}
// Takes a string or array of strings and adds them to knownSingletons
function registerNames(s: string[] | string): void {
  if (typeof s === "string") {
    knownSingletons[s] = true;
  } else if (s instanceof Array) {
    for (let i = 0; i < s.length; i++) {
      knownSingletons[s[i]] = true;
    }
  }
}
// Inserts new content into document head
function addToHead(head: string) {
  if (head.length > 0) {
    const tempDiv = $("<div>" + head + "</div>").get(0) as HTMLDivElement;
    const $head = $("head");

    while (tempDiv.hasChildNodes()) {
      // @ts-expect-error; TODO-barret; IDK how this function works. Seems like it would add the first child forever.
      $head.append(tempDiv.firstChild);
    }
  }
}
// Reads HTML and returns an object with info about singletons
function processHtml(val: string): {
  html: string;
  head: string;
  singletons: typeof knownSingletons;
} {
  const newSingletons: typeof knownSingletons = {};
  let newVal: string;

  const findNewPayload = function (
    match: string,
    p1: string,
    sig: string,
    payload: string,
  ) {
    if (knownSingletons[sig] || newSingletons[sig]) return "";
    newSingletons[sig] = true;
    return payload;
  };

  while (true) {
    newVal = val.replace(reSingleton, findNewPayload);
    if (val.length === newVal.length) break;
    val = newVal;
  }

  const heads: string[] = [];
  const headAddPayload = function (match: string, payload: string) {
    heads.push(payload);
    return "";
  };

  while (true) {
    newVal = val.replace(reHead, headAddPayload);
    if (val.length === newVal.length) break;
    val = newVal;
  }

  return {
    html: val,
    head: heads.join("\n"),
    singletons: newSingletons,
  };
}

export { registerNames, renderHtml };
export type { WherePosition };
