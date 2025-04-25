import $ from "jquery";

import { TextInputBinding } from "./text";

export class TextareaInputBinding extends TextInputBinding {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    // Inputs now also have the .shiny-input-textarea class
    return $(scope).find("textarea");
  }
}

/*************************************************************
 * Code below this point is for textAreaInput(autoresize=TRUE)
 ************************************************************/

interface DOMEvent<T extends EventTarget> extends Event {
  readonly target: T;
}

function onDelegatedEvent(
  eventName: string,
  selector: string,
  callback: (target: HTMLTextAreaElement) => void
) {
  document.addEventListener(eventName, (e) => {
    const e2 = e as DOMEvent<HTMLTextAreaElement>;
    if (e2.target.matches(selector)) {
      callback(e2.target);
    }
  });
}

// Use a single intersectionObserver as they are slow to create / use.
let textAreaIntersectionObserver: IntersectionObserver | null = null;

function callUpdateHeightWhenTargetIsVisible(target: HTMLTextAreaElement) {
  if (textAreaIntersectionObserver === null) {
    // Create a single observer to watch for the textarea becoming visible
    textAreaIntersectionObserver = new IntersectionObserver((entries) => {
      entries.forEach((entry) => {
        // Quit if the entry is not visible
        if (!entry.isIntersecting) {
          return;
        }
        // If the entry is visible (even if it's just a single pixel)
        // Stop observing the target
        textAreaIntersectionObserver?.unobserve(entry.target);

        // Update the height of the textarea
        updateHeight(entry.target as HTMLTextAreaElement);
      });
    });
  }

  textAreaIntersectionObserver.observe(target);
}

function updateHeight(target: HTMLTextAreaElement) {
  if (target.scrollHeight > 0) {
    // Automatically resize the textarea to fit its content.
    target.style.height = "auto";
    target.style.height = target.scrollHeight + "px";
  } else {
    // The textarea is not visible on the page, therefore it has a 0 scroll height.

    // If we should autoresize the text area height, then we can wait for the textarea to
    // become visible and call `updateHeight` again. Hopefully the scroll height is no
    // longer 0
    callUpdateHeightWhenTargetIsVisible(target);
  }
}

// Update on change
onDelegatedEvent(
  "input",
  "textarea.textarea-autoresize",
  (target: HTMLTextAreaElement) => {
    updateHeight(target);
  }
);

// Update on load
function updateOnLoad() {
  if (document.readyState === "loading") {
    // Document still loading, wait 10ms to check again.
    setTimeout(updateOnLoad, 10);
    return;
  }

  // document.readyState in ["interactive", "complete"];\
  const textAreas = document.querySelectorAll(
    "textarea.textarea-autoresize"
  ) as NodeListOf<HTMLTextAreaElement>;
  textAreas.forEach(updateHeight);
}

updateOnLoad();
