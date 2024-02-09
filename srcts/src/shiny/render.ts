import $ from "jquery";
import { asArray, hasDefinedProperty } from "../utils";
import { isIE } from "../utils/browser";
import type { BindScope } from "./bind";
import {
  shinyBindAll,
  shinyInitializeInputs,
  shinyUnbindAll,
} from "./initedMethods";
import { sendImageSizeFns } from "./sendImageSize";

import { renderHtml as singletonsRenderHtml } from "./singletons";
import type { WherePosition } from "./singletons";

// There are synchronous and asynchronous versions of the exported functions
// renderContent(), renderHtml(), and renderDependencies(). This is because they
// the original versions of these functions were synchronous, but we added
// support for asynchronous rendering, to avoid the deprecated XMLHttpRequest
// function (https://github.com/rstudio/shiny/pull/3666).
//
// At the bottom, there is the appendScriptTags(), which calls $.append(), which
// in turn calls (synchronous) XMLHttpRequest(); and its counterpart
// appendScriptTagsAsync(), which uses a different (asynchronous) method. The
// sync and async versions of this function necessitate the sync and async
// versions of the other functions.
//
// The async versions of these functions are used internally and should be used
// for new external code when possible, but for backward compatibility for
// external code that calls these functions, we'll keep the synchronous versions
// around as well.

// =============================================================================
// renderContent
// =============================================================================
// Render HTML in a DOM element, add dependencies, and bind Shiny
// inputs/outputs. `content` can be null, a string, or an object with
// properties 'html' and 'deps'.
async function renderContentAsync(
  el: BindScope,
  content: string | { html: string; deps?: HtmlDep[] } | null,
  where: WherePosition = "replace"
): Promise<void> {
  if (where === "replace") {
    shinyUnbindAll(el);
  }

  let html = "";
  let dependencies: HtmlDep[] = [];

  if (content === null) {
    html = "";
  } else if (typeof content === "string") {
    html = content;
  } else if (typeof content === "object") {
    html = content.html;
    dependencies = content.deps || [];
  }

  await renderHtmlAsync(html, el, dependencies, where);

  let scope: BindScope = el;

  if (where === "replace") {
    shinyInitializeInputs(el);
    await shinyBindAll(el);
  } else {
    const $parent = $(el).parent();

    if ($parent.length > 0) {
      scope = $parent;
      if (where === "beforeBegin" || where === "afterEnd") {
        const $grandparent = $parent.parent();

        if ($grandparent.length > 0) scope = $grandparent;
      }
    }
    shinyInitializeInputs(scope);
    await shinyBindAll(scope);
  }
}

function renderContent(
  el: BindScope,
  content: string | { html: string; deps?: HtmlDep[] } | null,
  where: WherePosition = "replace"
): Promise<void> {
  if (where === "replace") {
    shinyUnbindAll(el);
  }

  let html = "";
  let dependencies: HtmlDep[] = [];

  if (content === null) {
    html = "";
  } else if (typeof content === "string") {
    html = content;
  } else if (typeof content === "object") {
    html = content.html;
    dependencies = content.deps || [];
  }

  renderHtml(html, el, dependencies, where);

  let scope: BindScope = el;

  if (where === "replace") {
    shinyInitializeInputs(el);
    return shinyBindAll(el);
  } else {
    const $parent = $(el).parent();

    if ($parent.length > 0) {
      scope = $parent;
      if (where === "beforeBegin" || where === "afterEnd") {
        const $grandparent = $parent.parent();

        if ($grandparent.length > 0) scope = $grandparent;
      }
    }
    shinyInitializeInputs(scope);
    return shinyBindAll(scope);
  }
}

// =============================================================================
// renderHtml
// =============================================================================
// Render HTML in a DOM element, inserting singletons into head as needed
async function renderHtmlAsync(
  html: string,
  el: BindScope,
  dependencies: HtmlDep[],
  where: WherePosition = "replace"
): Promise<ReturnType<typeof singletonsRenderHtml>> {
  await renderDependenciesAsync(dependencies);
  return singletonsRenderHtml(html, el, where);
}

// Render HTML in a DOM element, inserting singletons into head as needed
function renderHtml(
  html: string,
  el: BindScope,
  dependencies: HtmlDep[],
  where: WherePosition = "replace"
): ReturnType<typeof singletonsRenderHtml> {
  renderDependencies(dependencies);
  return singletonsRenderHtml(html, el, where);
}

// =============================================================================
// renderDependencies
// =============================================================================
async function renderDependenciesAsync(
  dependencies: HtmlDep[] | null
): Promise<void> {
  if (dependencies) {
    for (const dep of dependencies) {
      await renderDependencyAsync(dep);
    }
  }
}

function renderDependencies(dependencies: HtmlDep[] | null): void {
  if (dependencies) {
    for (const dep of dependencies) {
      renderDependency(dep);
    }
  }
}

// =============================================================================
// HTML dependency types
// =============================================================================
type HtmlDepVersion = string;

type MetaItem = {
  name: string;
  content: string;
  [x: string]: string;
};

type StylesheetItem = {
  href: string;
  rel?: string;
  type?: string;
};

type ScriptItem = {
  src: string;
  [x: string]: string;
};

type AttachmentItem = {
  key: string;
  href: string;
  [x: string]: string;
};

// This supports the older R htmltools HtmlDependency structure, and it also
// encompasses the newer, consistent HTMLDependency structure.
type HtmlDep = {
  name: string;
  version: HtmlDepVersion;
  restyle?: boolean;
  src?: { href: string };
  meta?: MetaItem[] | { [x: string]: string };
  stylesheet?: string[] | StylesheetItem | StylesheetItem[] | string;
  script?: ScriptItem | ScriptItem[] | string[] | string;
  attachment?: AttachmentItem[] | string[] | string | { [key: string]: string };
  head?: string;
};

// This is the newer, consistent HTMLDependency structure.
type HtmlDepNormalized = {
  name: string;
  version: HtmlDepVersion;
  restyle?: boolean;
  meta: MetaItem[];
  stylesheet: StylesheetItem[];
  script: ScriptItem[];
  attachment: AttachmentItem[];
  head?: string;
};

// =============================================================================
// renderDependency helper functions
// =============================================================================
const htmlDependencies: { [key: string]: HtmlDepVersion } = {};

function registerDependency(name: string, version: HtmlDepVersion): void {
  htmlDependencies[name] = version;
}

// Re-render stylesheet(s) if the dependency has specificially requested it
// and it matches an existing dependency (name and version)
function needsRestyle(dep: HtmlDepNormalized) {
  if (!dep.restyle) {
    return false;
  }
  const names = Object.keys(htmlDependencies);
  const idx = names.indexOf(dep.name);

  if (idx === -1) {
    return false;
  }
  return htmlDependencies[names[idx]] === dep.version;
}

function addStylesheetsAndRestyle(links: HTMLLinkElement[]): void {
  const $head = $("head").first();

  // This inline <style> based approach works for IE11
  const refreshStyle = function (href: string, oldSheet: CSSStyleSheet | null) {
    const xhr = new XMLHttpRequest();

    xhr.open("GET", href);
    xhr.onload = function () {
      const id =
        "shiny_restyle_" + href.split("?restyle")[0].replace(/\W/g, "_");
      const oldStyle = $head.find("style#" + id);
      const newStyle = $("<style>").attr("id", id).html(xhr.responseText);

      $head.append(newStyle);

      // We can remove the old styles immediately because the new styles
      // should have been applied synchronously.
      oldStyle.remove();
      removeSheet(oldSheet);
      sendImageSizeFns.transitioned();
    };
    xhr.send();
  };

  const findSheet = function (href: string | undefined): CSSStyleSheet | null {
    if (!href) return null;

    for (let i = 0; i < document.styleSheets.length; i++) {
      const sheet = document.styleSheets[i];
      // The sheet's href is a full URL

      if (typeof sheet.href === "string" && sheet.href.indexOf(href) > -1) {
        return sheet;
      }
    }
    return null;
  };

  // Removes the stylesheet from document.styleSheets, and also removes
  // the owning <link> element, if present.
  const removeSheet = function (sheet: CSSStyleSheet | null) {
    if (!sheet) return;
    sheet.disabled = true;
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore; .cssText doesn't normally exist, but it does on IE?
    if (isIE()) sheet.cssText = "";

    if (sheet.ownerNode instanceof Element) {
      $(sheet.ownerNode).remove();
    }
  };

  links.map((link) => {
    const $link = $(link);
    // Find any document.styleSheets that match this link's href
    // so we can remove it after bringing in the new stylesheet
    const oldSheet = findSheet($link.attr("href"));

    // Add a timestamp to the href to prevent caching
    const href = $link.attr("href") + "?restyle=" + new Date().getTime();
    // Use inline <style> approach for IE, otherwise use the more elegant
    // <link> -based approach

    if (isIE()) {
      refreshStyle(href, oldSheet);
    } else {
      $link.attr("href", href);

      // This part is a bit tricky. The link's onload callback will be
      // invoked after the file is loaded, but it can be _before_ the
      // styles are actually applied. The amount of time it takes for the
      // style to be applied is not predictable. We need to make sure the
      // styles are applied before we send updated size/style information
      // to the server.
      //
      // We do this by adding _another_ link, with CSS content
      // base64-encoded and inlined into the href. We also add a dummy DOM
      // element that the CSS applies to. The dummy CSS includes a
      // transition, and when the `transitionend` event happens, we call
      // sendImageSizeFns.transitioned() and remove the old sheet. We also remove the
      // dummy DOM element and dummy CSS content.
      //
      // The reason this works is because (we assume) that if multiple
      // <link> tags are added, they will be applied in the same order
      // that they are loaded. This seems to be true in the browsers we
      // have tested.
      //
      // Because it is common for multiple stylesheets to arrive close
      // together, but not on exactly the same tick, we call
      // sendImageSizeFns.transitioned(), which is debounced. Otherwise, it can result in
      // the same plot being redrawn multiple times with different
      // styling.
      $link.attr("onload", () => {
        const $dummyEl = $("<div>")
          .css("transition", "0.1s all")
          .css("position", "absolute")
          .css("top", "-1000px")
          .css("left", "0");

        $dummyEl.one("transitionend", () => {
          $dummyEl.remove();
          removeSheet(oldSheet);
          sendImageSizeFns.transitioned();
        });
        $(document.body).append($dummyEl);

        // To ensure a transition actually happens, change the inline style _after_
        // the DOM element has been added, and also use a new random color each time
        // to prevent any potential caching done by the browser
        const color = "#" + Math.floor(Math.random() * 16777215).toString(16);

        setTimeout(() => $dummyEl.css("color", color), 10);
      });

      $head.append(link);
    }
  });
}

function getStylesheetLinkTags(dep: HtmlDepNormalized): HTMLLinkElement[] {
  // Convert stylesheet objs to links early, because if `restyle` is true, we'll
  // pass them through to `addStylesheetsAndRestyle` below.
  return dep.stylesheet.map((x) => {
    // Add "rel" and "type" fields if not already present.
    if (!hasDefinedProperty(x, "rel")) x.rel = "stylesheet";
    if (!hasDefinedProperty(x, "type")) x.type = "text/css";

    const link = document.createElement("link");

    Object.entries(x).forEach(function ([attr, val]: [
      string,
      string | undefined
    ]) {
      if (attr === "href") {
        val = encodeURI(val as string);
      }
      // If val isn't truthy (e.g., null), consider it a boolean attribute
      link.setAttribute(attr, val ? val : "");
    });

    return link;
  });
}

function appendStylesheetLinkTags(
  dep: HtmlDepNormalized,
  $head: JQuery<HTMLElement>
): void {
  const stylesheetLinks = getStylesheetLinkTags(dep);

  if (stylesheetLinks.length !== 0) {
    $head.append(stylesheetLinks);
  }
}

function appendScriptTags(dep: HtmlDepNormalized, $head: JQuery<HTMLElement>) {
  dep.script.forEach((x) => {
    const script = document.createElement("script");

    Object.entries(x).forEach(function ([attr, val]) {
      if (attr === "src") {
        val = encodeURI(val);
      }
      // If val isn't truthy (e.g., null), consider it a boolean attribute
      script.setAttribute(attr, val ? val : "");
    });

    $head.append(script);
  });
}

async function appendScriptTagsAsync(dep: HtmlDepNormalized): Promise<void> {
  const scriptPromises: Array<Promise<any>> = [];

  dep.script.forEach((x) => {
    const script = document.createElement("script");

    if (!hasDefinedProperty(x, "async")) {
      // Set async to false by default, so that if there are multiple script
      // tags, they are guaranteed to run in order. For dynamically added
      // <script> tags, browsers set async to true by default, which differs
      // from static <script> tags in the html, which default to false.
      //
      // Refs:
      // https://stackoverflow.com/a/8996894/412655
      // https://jason-ge.medium.com/dynamically-load-javascript-files-in-order-5318ac6bcc61
      //
      // Note that one odd thing about these dynamically-created <script> tags
      // is that even though the JS object's `x.script` property is true, it
      // does NOT show up as a property on the <script> element.
      script.async = false;
    }

    Object.entries(x).forEach(function ([attr, val]) {
      if (attr === "src") {
        val = encodeURI(val);
      }
      // If val isn't truthy (e.g., null), consider it a boolean attribute
      script.setAttribute(attr, val ? val : "");
    });

    const p = new Promise((resolve, reject) => {
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      script.onload = (e: Event) => {
        resolve(null);
      };
      script.onerror = (e: Event | string) => {
        reject(e);
      };
    });

    scriptPromises.push(p);
    document.head.append(script);
  });

  await Promise.allSettled(scriptPromises);
}

function appendMetaTags(
  dep: HtmlDepNormalized,
  $head: JQuery<HTMLElement>
): void {
  dep.meta.forEach((x) => {
    const meta = document.createElement("meta");

    for (const [attr, val] of Object.entries(x)) {
      meta.setAttribute(attr, val);
    }
    $head.append(meta);
  });
}

function appendAttachmentLinkTags(
  dep: HtmlDepNormalized,
  $head: JQuery<HTMLElement>
): void {
  dep.attachment.forEach((x) => {
    const link = $("<link rel='attachment'>")
      .attr("id", dep.name + "-" + x.key + "-attachment")
      .attr("href", encodeURI(x.href));

    $head.append(link);
  });
}

function appendExtraHeadContent(
  dep: HtmlDepNormalized,
  $head: JQuery<HTMLElement>
): void {
  if (dep.head) {
    const $newHead = $("<head></head>");

    $newHead.html(dep.head);
    $head.append($newHead.children());
  }
}

// =============================================================================
// renderDependency
// =============================================================================
// Client-side dependency resolution and rendering
async function renderDependencyAsync(dep_: HtmlDep): Promise<boolean> {
  const dep = normalizeHtmlDependency(dep_);

  // If a restyle is needed, do that stuff and return. Note that other items
  // (like scripts) aren't added, because they would have been added in a
  // previous run.
  if (needsRestyle(dep)) {
    addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
    return true;
  }

  if (hasDefinedProperty(htmlDependencies, dep.name)) return false;

  registerDependency(dep.name, dep.version);

  const $head = $("head").first();

  // Add each type of element to the DOM.
  appendMetaTags(dep, $head);
  appendStylesheetLinkTags(dep, $head);
  await appendScriptTagsAsync(dep);
  appendAttachmentLinkTags(dep, $head);
  appendExtraHeadContent(dep, $head);

  return true;
}

// Old-school synchronous version of renderDependencyAsync. This function is
// here to preserve compatibility with outside packages that use it. The
// implementation is the same except that it calls appendScriptTags() instead of
// appendScriptTagsAsync().
function renderDependency(dep_: HtmlDep): boolean {
  const dep = normalizeHtmlDependency(dep_);

  // If a restyle is needed, do that stuff and return. Note that other items
  // (like scripts) aren't added, because they would have been added in a
  // previous run.
  if (needsRestyle(dep)) {
    addStylesheetsAndRestyle(getStylesheetLinkTags(dep));
    return true;
  }

  if (hasDefinedProperty(htmlDependencies, dep.name)) return false;

  registerDependency(dep.name, dep.version);

  const $head = $("head").first();

  // Add each type of element to the DOM.
  appendMetaTags(dep, $head);
  appendStylesheetLinkTags(dep, $head);
  appendScriptTags(dep, $head);
  appendAttachmentLinkTags(dep, $head);
  appendExtraHeadContent(dep, $head);

  return true;
}

// Convert legacy HtmlDependency to new HTMLDependency format. This is
// idempotent; new HTMLDependency objects are returned unchanged.
function normalizeHtmlDependency(dep: HtmlDep): HtmlDepNormalized {
  const hrefPrefix: string | undefined = dep.src?.href;

  const result: HtmlDepNormalized = {
    name: dep.name,
    version: dep.version,
    restyle: dep.restyle,
    meta: [],
    stylesheet: [],
    script: [],
    attachment: [],
    head: dep.head,
  };

  if (dep.meta) {
    if (Array.isArray(dep.meta)) {
      // Assume we already have the canonical format:
      //   [{name: "myname", content: "mycontent"}, ...]
      result.meta = dep.meta;
    } else {
      // If here, then we have the legacy format, which we have to convert.
      //   {myname: "mycontent", ...}
      result.meta = Object.entries(dep.meta).map(function ([attr, val]) {
        return { name: attr, content: val };
      });
    }
  }

  result.stylesheet = asArray(dep.stylesheet).map((s) => {
    if (typeof s === "string") {
      s = { href: s };
    }
    if (hrefPrefix) {
      s.href = hrefPrefix + "/" + s.href;
    }
    return s;
  });

  result.script = asArray(dep.script).map((s) => {
    if (typeof s === "string") {
      s = { src: s };
    }
    if (hrefPrefix) {
      s.src = hrefPrefix + "/" + s.src;
    }
    return s;
  });

  // dep.attachment might be one of the following types, which we will convert
  // as shown:
  // 0. undefined => []
  // 1. A single string:
  //    "foo.txt"
  //    => [{key: "1", href: "foo.txt"}]
  // 2. An array of strings:
  //    ["foo.txt" ,"bar.dat"]
  //    => [{key: "1", href: "foo.txt"}, {key: "2", href: "bar.dat"}]
  // 3. An object:
  //    {foo: "foo.txt", bar: "bar.dat"}
  //    => [{key: "foo", href: "foo.txt"}, {key: "bar", href: "bar.dat"}]
  // 4. An array of objects:
  //    [{key: "foo", href: "foo.txt"}, {key: "bar", href: "bar.dat"}]
  //    => (Returned unchanged)
  //
  // Note that the first three formats are from legacy code, and the last format
  // is from new code.
  let attachments = dep.attachment;

  // Convert format 0 (undefined) to format 2 or 4.
  if (!attachments) attachments = [];

  // Convert format 1 to format 2.
  if (typeof attachments === "string") attachments = [attachments];

  if (Array.isArray(attachments)) {
    // If we've gotten here, the format is either 2 or 4. Even though they are
    // quite different, we can handle them both in the same loop.

    // Need to give TypeScript a bit of help so that it's happy with .map()
    // below. Instead of a union of two array types, tell it it's an array of
    // a union of two types.
    const tmp = <Array<AttachmentItem | string>>attachments;

    // The contract for attachments is that arrays of attachments are
    // addressed using 1-based indexes. Convert this array to an object.
    attachments = tmp.map((attachment, index) => {
      if (typeof attachment === "string") {
        return {
          key: (index + 1).toString(),
          href: attachment,
        };
      } else {
        return attachment;
      }
    });
  } else {
    // If we got here, it's format 3.
    attachments = Object.entries(attachments).map(function ([attr, val]) {
      return { key: attr, href: val };
    });
  }

  // At this point, we've normalized the format to #4. Now we can iterate over
  // it and prepend `hrefPrefix`.
  result.attachment = attachments.map((s) => {
    if (hrefPrefix) {
      s.href = hrefPrefix + "/" + s.href;
    }
    return s;
  });

  return result;
}

export {
  renderContentAsync,
  renderContent,
  renderHtmlAsync,
  renderHtml,
  renderDependenciesAsync,
  renderDependencies,
  registerDependency,
};
export type { HtmlDep };
