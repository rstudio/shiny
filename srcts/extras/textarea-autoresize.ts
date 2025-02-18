// By importing the css file, it will be copied to the output directory.
import "./textarea-autoresize.css";

export interface DOMEvent<T extends EventTarget> extends Event {
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
let textAreaIntersectionObserver: null | IntersectionObserver = null;

function callUpdateHeightWhenTargetIsVisible(target: HTMLTextAreaElement) {
  if (textAreaIntersectionObserver === null) {
    // Create a single observer to watch for the textarea becoming visible
    textAreaIntersectionObserver = new IntersectionObserver(
      (entries, observer) => {
        entries.forEach((entry) => {
          // Quit if the entry is not visible
          if (!entry.isIntersecting) {
            return;
          }
          // If the entry is visible (even if it's just a single pixel)
          // Stop observing the target
          textAreaIntersectionObserver!.unobserve(entry.target);

          // Update the height of the textarea
          update_height(entry.target as HTMLTextAreaElement);
        });
      }
    );
  }

  textAreaIntersectionObserver.observe(target);
}

function update_height(target: HTMLTextAreaElement) {
  if (target.scrollHeight > 0) {
    // Automatically resize the textarea to fit its content.
    target.style.height = "auto";
    target.style.height = target.scrollHeight + "px";
  } else {
    // The textarea is not visible on the page, therefore it has a 0 scroll height.

    // If we should autoresize the text area height, then we can wait for the textarea to
    // become visible and call `update_height` again. Hopefully the scroll height is no
    // longer 0
    callUpdateHeightWhenTargetIsVisible(target);
  }
}

// Update on change
onDelegatedEvent(
  "input",
  "textarea.textarea-autoresize",
  (target: HTMLTextAreaElement) => {
    update_height(target);
  }
);

// Update on load
function update_on_load() {
  if (document.readyState === "loading") {
    // Document still loading, wait 10ms to check again.
    setTimeout(update_on_load, 10);
    return;
  }

  // document.readyState in ["interactive", "complete"];\
  const textAreas = document.querySelectorAll(
    "textarea.textarea-autoresize"
  ) as NodeListOf<HTMLTextAreaElement>;
  textAreas.forEach(update_height);
}
update_on_load();
