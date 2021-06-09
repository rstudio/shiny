import $ from "jquery";
import { asArray, hasOwnProperty } from "../utils";
import { isIE } from "../utils/browser";
import { bindScope } from "./bind";
import {
  shinyBindAll,
  shinyInitializeInputs,
  shinyUnbindAll,
} from "./initedMethods";
import { sendImageSizeFns } from "./sendImageSize";

import { renderHtml as singletonsRenderHtml } from "./singletons";
import type { WherePosition } from "./singletons";

function renderDependencies(dependencies: null | Array<HtmlDep>): void {
  if (dependencies) {
    $.each(dependencies, function (i, dep) {
      renderDependency(dep);
    });
  }
}

// Render HTML in a DOM element, add dependencies, and bind Shiny
// inputs/outputs. `content` can be null, a string, or an object with
// properties 'html' and 'deps'.
function renderContent(
  el: bindScope,
  content: null | string | { html: any; deps?: any },
  where: WherePosition = "replace"
): void {
  if (where === "replace") {
    shinyUnbindAll(el);
  }

  let html;
  let dependencies = [];

  if (content === null) {
    html = "";
  } else if (typeof content === "string") {
    html = content;
  } else if (typeof content === "object") {
    html = content.html;
    dependencies = content.deps || [];
  }

  renderHtml(html, el, dependencies, where);

  let scope: bindScope = el;

  if (where === "replace") {
    shinyInitializeInputs(el);
    shinyBindAll(el);
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
    shinyBindAll(scope);
  }
}

// Render HTML in a DOM element, inserting singletons into head as needed
function renderHtml(
  html: string,
  el: bindScope,
  dependencies: Array<HtmlDep>,
  where: WherePosition = "replace"
): ReturnType<typeof singletonsRenderHtml> {
  renderDependencies(dependencies);
  return singletonsRenderHtml(html, el, where);
}

type HtmlDepName = string;
type HtmlDepVersion = string;
type HtmlDep = {
  name: HtmlDepName;
  version: HtmlDepVersion;
  restyle?: boolean;
  src?: { href: string };
  meta?: string | Array<string>;
  stylesheet?: string | Array<string>;
  script?:
    | string
    | Array<string>
    | Record<string, string>
    | Array<Record<string, string>>;
  attachment?: string | Array<string> | Record<string, string>;
  head?: string;
};
const htmlDependencies: Record<HtmlDepName, HtmlDepVersion> = {};

function registerDependency(name: HtmlDepName, version: HtmlDepVersion): void {
  htmlDependencies[name] = version;
}

// Re-render stylesheet(s) if the dependency has specificially requested it
// and it matches an existing dependency (name and version)
function needsRestyle(dep: HtmlDep) {
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

// Client-side dependency resolution and rendering
function renderDependency(dep: HtmlDep) {
  const restyle = needsRestyle(dep);

  if (hasOwnProperty(htmlDependencies, dep.name) && !restyle) return false;

  registerDependency(dep.name, dep.version);

  const href = dep.src.href;

  const $head = $("head").first();

  if (dep.meta && !restyle) {
    const metas = $.map(asArray(dep.meta), function (obj, idx) {
      // only one named pair is expected in obj as it's already been decomposed
      const name = Object.keys(obj)[0];

      return $("<meta>").attr("name", name).attr("content", obj[name]);
      idx;
    });

    $head.append(metas);
  }

  if (dep.stylesheet) {
    const links = $.map(asArray(dep.stylesheet), function (stylesheet) {
      return $("<link rel='stylesheet' type='text/css'>").attr(
        "href",
        href + "/" + encodeURI(stylesheet)
      );
    });

    if (!restyle) {
      $head.append(links);
    } else {
      // This inline <style> based approach works for IE11
      const refreshStyle = function (href, oldSheet) {
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

      const findSheet = function (href) {
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
      const removeSheet = function (sheet) {
        if (!sheet) return;
        sheet.disabled = true;
        if (isIE()) sheet.cssText = "";
        $(sheet.ownerNode).remove();
      };

      $.map(links, function (link) {
        // Find any document.styleSheets that match this link's href
        // so we can remove it after bringing in the new stylesheet
        const oldSheet = findSheet(link.attr("href"));

        // Add a timestamp to the href to prevent caching
        const href = link.attr("href") + "?restyle=" + new Date().getTime();
        // Use inline <style> approach for IE, otherwise use the more elegant
        // <link> -based approach

        if (isIE()) {
          refreshStyle(href, oldSheet);
        } else {
          link.attr("href", href);

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
          link.attr("onload", () => {
            const dummyId = "dummy-" + Math.floor(Math.random() * 999999999);
            const cssString =
              "#" +
              dummyId +
              " { " +
              "color: #a7c920 !important; " + // An arbitrary color for the transition
              "transition: 0.1s all !important; " +
              "visibility: hidden !important; " +
              "position: absolute !important; " +
              "top: -1000px !important; " +
              "left: 0 !important; }";
            const base64CssString = "data:text/css;base64," + btoa(cssString);

            const $dummyLink = $("<link rel='stylesheet' type='text/css' />");

            $dummyLink.attr("href", base64CssString);

            const $dummyEl = $("<div id='" + dummyId + "'></div>");

            $dummyEl.one("transitionend", () => {
              $dummyEl.remove();
              removeSheet(findSheet($dummyLink.attr("href")));
              removeSheet(oldSheet);
              sendImageSizeFns.transitioned();
            });
            $(document.body).append($dummyEl);

            // Need to add the CSS with a setTimeout 0, to ensure that it
            // takes effect _after_ the DOM element has been added. This is
            // necessary to ensure that the transition actually occurs.
            setTimeout(() => $head.append($dummyLink), 0);
          });

          $head.append(link);
        }
      });
    }
  }

  if (dep.script && !restyle) {
    const scriptsAttrs = asArray(dep.script);
    const scripts = $.map(scriptsAttrs, function (x) {
      const script = document.createElement("script");

      // htmlDependency()'s script arg can be a character vector or a list()
      if (typeof x === "string") {
        x = { src: x };
      }

      // Can not destructure Object.entries into both a `const` and a `let` variable.
      // eslint-disable-next-line prefer-const
      for (let [attr, val] of Object.entries(x)) {
        if (attr === "src") {
          val = href + "/" + encodeURI(val);
        }
        // If val isn't truthy (e.g., null), consider it a boolean attribute
        script.setAttribute(attr, val ? val : "");
      }

      return script;
    });

    $head.append(scripts);
  }

  if (dep.attachment && !restyle) {
    // dep.attachment might be a single string, an array, or an object.
    let attachments = dep.attachment;

    if (typeof attachments === "string") attachments = [attachments];
    if (Array.isArray(attachments)) {
      // The contract for attachments is that arrays of attachments are
      // addressed using 1-based indexes. Convert this array to an object.
      const tmp = {};

      $.each(attachments, function (index, attachment) {
        const key = index + 1 + "";

        tmp[key] = attachment;
      });
      attachments = tmp;
    }

    const attach = $.map(attachments, function (attachment, key) {
      return $("<link rel='attachment'>")
        .attr("id", dep.name + "-" + key + "-attachment")
        .attr("href", href + "/" + encodeURI(attachment));
    });

    $head.append(attach);
  }

  if (dep.head && !restyle) {
    const $newHead = $("<head></head>");

    $newHead.html(dep.head);
    $head.append($newHead.children());
  }
  return true;
}

export { renderDependencies, renderContent, renderHtml, registerDependency };
