import $ from "jquery";
import { toLowerCase } from "../utils";
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
  where: WherePosition
): ReturnType<typeof processHtml> {
  const processed = processHtml(html);

  addToHead(processed.head);
  register(processed.singletons);
  if (where === "replace") {
    $(el).html(processed.html);
  } else {
    let elElements: HTMLElement[];

    if (el instanceof HTMLElement) {
      elElements = [el];
    } else {
      elElements = el.toArray();
    }
    $.each(elElements, (i, el) => {
      // type InsertPosition = "beforebegin" | "afterbegin" | "beforeend" | "afterend"
      el.insertAdjacentHTML(toLowerCase(where), processed.html);
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
    const tempDiv = $("<div>" + head + "</div>").get(0);
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

  const findNewPayload = function (match, p1, sig, payload) {
    if (knownSingletons[sig] || newSingletons[sig]) return "";
    newSingletons[sig] = true;
    return payload;
  };

  // eslint-disable-next-line no-constant-condition
  while (true) {
    newVal = val.replace(reSingleton, findNewPayload);
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

export { renderHtml, registerNames };
export type { WherePosition };
