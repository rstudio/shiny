/* eslint-disable unicorn/filename-case */
import "./globalShiny";

type ShowcaseSrcMessage = {
  srcref: number[];
  srcfile: string;
};

const animateMs = 400;

// Given a DOM node and a column (count of characters), walk recursively
// through the node's siblings counting characters until the given number
// of characters have been found.
//
// If the given count is bigger than the number of characters contained by
// the node and its siblings, returns a null node and the number of
// characters found.
function findTextColPoint(
  node: Node,
  col: number,
): { element: Node | null; offset: number } {
  let cols = 0;

  if (node.nodeType === 3) {
    const nchar = node.nodeValue?.replace(/\n/g, "").length ?? 0;

    if (nchar >= col) {
      return { element: node, offset: col };
    } else {
      cols += nchar;
    }
  } else if (node.nodeType === 1 && node.firstChild) {
    const ret = findTextColPoint(node.firstChild, col);

    if (ret.element !== null) {
      return ret;
    } else {
      cols += ret.offset;
    }
  }
  if (node.nextSibling) return findTextColPoint(node.nextSibling, col - cols);
  else return { element: null, offset: cols };
}

// Returns an object indicating the element containing the given line and
// column of text, and the offset into that element where the text was found.
//
// If the given line and column are not found, returns a null element and
// the number of lines found.
function findTextPoint(
  el: Node,
  line: number,
  col: number,
): { element: Node | null; offset: number } {
  let newlines = 0;

  for (let childId = 0; childId < el.childNodes.length; childId++) {
    const child = el.childNodes[childId];
    // If this is a text node, count the number of newlines it contains.

    if (child.nodeType === 3) {
      // TEXT_NODE
      const newlinere = /\n/g;
      let match: ReturnType<RegExp["exec"]>;

      while ((match = newlinere.exec(child.nodeValue!)) !== null) {
        newlines++;
        // Found the desired line, now find the column.
        if (newlines === line) {
          return findTextColPoint(child, match.index + col + 1);
        }
      }
    }
    // If this is not a text node, descend recursively to see how many
    // lines it contains.
    else if (child.nodeType === 1) {
      // ELEMENT_NODE
      const ret = findTextPoint(child, line - newlines, col);

      if (ret.element !== null) return ret;
      else newlines += ret.offset;
    }
  }
  return { element: null, offset: newlines };
}

// Draw a highlight effect for the given source ref. srcref is assumed to be
// an integer array of length 6, following the standard R format for source
// refs.
function highlightSrcref(
  srcref: ShowcaseSrcMessage["srcref"],
  srcfile: ShowcaseSrcMessage["srcfile"],
) {
  // Check to see if the browser supports text ranges (IE8 doesn't)
  if (!document.createRange) return;

  // Check to see if we already have a marker for this source ref
  let el = document.getElementById("srcref_" + srcref);

  if (!el) {
    // We don't have a marker, create one
    el = document.createElement("span");
    el.id = "srcref_" + srcref;
    const ref = srcref;
    const code = document.getElementById(srcfile.replace(/\./g, "_") + "_code");
    // if there is no code file (might be a shiny file), quit early

    if (!code) return;
    const start = findTextPoint(code, ref[0], ref[4]);
    const end = findTextPoint(code, ref[2], ref[5]);

    // If the insertion point can't be found, bail out now
    if (start.element === null || end.element === null) return;

    const range = document.createRange();
    // If the text points are inside different <SPAN>s, we may not be able to
    // surround them without breaking apart the elements to keep the DOM tree
    // intact. Just move the selection points to encompass the contents of
    // the SPANs.

    if (
      start.element.parentNode?.nodeName === "SPAN" &&
      start.element !== end.element
    ) {
      range.setStartBefore(start.element.parentNode);
    } else {
      range.setStart(start.element, start.offset);
    }
    if (
      end.element.parentNode?.nodeName === "SPAN" &&
      start.element !== end.element
    ) {
      range.setEndAfter(end.element.parentNode);
    } else {
      range.setEnd(end.element, end.offset);
    }
    range.surroundContents(el);
  }
  // End any previous highlight before starting this one
  $(el).stop(true, true).effect("highlight", null, 1600);
}

// If this is the main Shiny window, wire up our custom message handler.
// TODO-barret, this should work

if (window.Shiny) {
  window.Shiny.addCustomMessageHandler(
    "showcase-src",
    function (message: ShowcaseSrcMessage) {
      if (message.srcref && message.srcfile) {
        highlightSrcref(message.srcref, message.srcfile);
      }
    },
  );
}

let isCodeAbove = false;
const setCodePosition = function (above: boolean, animate: boolean) {
  const animateCodeMs = animate ? animateMs : 1;

  // set the source and targets for the tab move
  const newHostElement = above
    ? document.getElementById("showcase-sxs-code")
    : document.getElementById("showcase-code-inline");
  const currentHostElement = above
    ? document.getElementById("showcase-code-inline")
    : document.getElementById("showcase-sxs-code");

  const metadataElement = document.getElementById("showcase-app-metadata");

  if (metadataElement === null) {
    // if there's no app metadata, show and hide the entire well container
    // when the code changes position
    const wellElement = $("#showcase-well");

    if (above) {
      wellElement.fadeOut(animateCodeMs);
    } else {
      wellElement.fadeIn(animateCodeMs);
    }
  }

  // hide the new element before doing anything to it
  if (newHostElement === null || currentHostElement === null) {
    console.warn(
      "Could not find the host elements for the code tabs. " +
        "This is likely a bug in the showcase app.",
    );
    return;
  }
  $(newHostElement).hide();
  $(currentHostElement).fadeOut(animateCodeMs, function () {
    const tabs = document.getElementById("showcase-code-tabs");

    if (tabs === null) {
      console.warn(
        "Could not find the code tabs element. This is likely a bug in the showcase app.",
      );
      return;
    }

    currentHostElement.removeChild(tabs);
    newHostElement.appendChild(tabs);

    // remove or set the height of the code
    if (above) {
      setCodeHeightFromDocHeight();
    } else {
      document
        .getElementById("showcase-code-content")
        ?.removeAttribute("style");
    }

    $(newHostElement).fadeIn(animateCodeMs);
    if (!above) {
      // remove the applied width and zoom on the app container, and
      // scroll smoothly down to the code's new home
      document
        .getElementById("showcase-app-container")
        ?.removeAttribute("style");
      if (animate) {
        const top = $(newHostElement).offset()?.top;
        if (top !== undefined) {
          $(document.body).animate({ scrollTop: top });
        }
      }
    }
    // if there's a readme, move it either alongside the code or beneath
    // the app
    const readme = document.getElementById("readme-md");

    if (readme !== null) {
      readme.parentElement?.removeChild(readme);
      if (above) {
        currentHostElement.appendChild(readme);
        $(currentHostElement).fadeIn(animateCodeMs);
      } else
        document.getElementById("showcase-app-metadata")?.appendChild(readme);
    }

    // change the text on the toggle button to reflect the new state
    document.getElementById("showcase-code-position-toggle")!.innerHTML = above
      ? '<i class="fa fa-level-down"></i> show below'
      : '<i class="fa fa-level-up"></i> show with app';
  });
  if (above) {
    $(document.body).animate({ scrollTop: 0 }, animateCodeMs);
  }
  isCodeAbove = above;
  setAppCodeSxsWidths(above && animate);
  $(window).trigger("resize");
};

function setAppCodeSxsWidths(animate: boolean) {
  const appTargetWidth = 960;
  let appWidth = appTargetWidth;
  let zoom = 1.0;
  const totalWidth = document.getElementById("showcase-app-code")!.offsetWidth;

  if (totalWidth / 2 > appTargetWidth) {
    // If the app can use only half the available space and still meet its
    // target, take half the available space.
    appWidth = totalWidth / 2;
  } else if (totalWidth * 0.66 > appTargetWidth) {
    // If the app can meet its target by taking up more space (up to 66%
    // of its container), take up more space.
    appWidth = 960;
  } else {
    // The space is too narrow for the app and code to live side-by-side
    // in a friendly way. Keep the app at 2/3 of the space but scale it.
    appWidth = totalWidth * 0.66;
    zoom = appWidth / appTargetWidth;
  }
  $("#showcase-app-container").animate(
    {
      width: appWidth + "px",
      zoom: zoom * 100 + "%",
    },
    animate ? animateMs : 0,
  );
}

const toggleCodePosition = function () {
  setCodePosition(!isCodeAbove, true);
};

// if the browser is sized to wider than 1350px, show the code next to the
// app by default
const setInitialCodePosition = function () {
  if (document.body.offsetWidth > 1350) {
    setCodePosition(true, false);
  }
};

// make the code scrollable to about the height of the browser, less space
// for the tabs
function setCodeHeightFromDocHeight() {
  document.getElementById("showcase-code-content")!.style.height =
    $(window).height() + "px";
}

// if there's a block of markdown content, render it to HTML
function renderMarkdown() {
  const mdContent = document.getElementById("showcase-markdown-content");

  if (mdContent !== null) {
    // IE8 puts the content of <script> tags into innerHTML but
    // not innerText
    const content = mdContent.innerText || mdContent.innerHTML;

    const showdownConverter = (window as any).Showdown
      .converter as showdown.ConverterStatic;

    document.getElementById("readme-md")!.innerHTML =
      new showdownConverter().makeHtml(content);
  }
}

$(window).resize(function () {
  if (isCodeAbove) {
    setAppCodeSxsWidths(false);
    setCodeHeightFromDocHeight();
  }
});

declare global {
  interface Window {
    toggleCodePosition: () => void;
  }
}
window.toggleCodePosition = toggleCodePosition;

$(window).on("load", setInitialCodePosition);
$(window).on("load", renderMarkdown);

if (window.hljs) window.hljs.initHighlightingOnLoad();

export {};
