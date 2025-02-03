import $ from "jquery";

// This object provides two public event listeners: mousedown, and
// dblclickIE8.
// We need to make sure that, when the image is listening for double-
// clicks, that a double-click doesn't trigger two click events. We'll
// trigger custom mousedown2 and dblclick2 events with this mousedown
// listener.
function createClickInfo(
  $el: JQuery<HTMLElement>,
  dblclickId: string,
  dblclickDelay: number,
): {
  mousedown: (e: JQuery.MouseDownEvent) => void;
  dblclickIE8: (e: JQuery.DoubleClickEvent) => void;
} {
  let clickTimer: number | undefined = undefined;
  let pendingE: JQuery.MouseDownEvent | null = null; // A pending mousedown2 event

  // Create a new event of type eventType (like 'mousedown2'), and trigger
  // it with the information stored in this.e.
  function triggerEvent(
    newEventType: string,
    e: JQuery.DoubleClickEvent | JQuery.MouseDownEvent,
  ) {
    // Extract important info from e and construct a new event with type
    // eventType.
    const e2 = $.Event(newEventType, {
      which: e.which,
      pageX: e.pageX,
      pageY: e.pageY,
    });

    $el.trigger(e2);
  }

  function triggerPendingMousedown2() {
    // It's possible that between the scheduling of a mousedown2 and the
    // time this callback is executed, someone else triggers a
    // mousedown2, so check for that.
    if (pendingE) {
      triggerEvent("mousedown2", pendingE);
      pendingE = null;
    }
  }

  // Set a timer to trigger a mousedown2 event, using information from the
  // last recorded mousdown event.
  function scheduleMousedown2(e: JQuery.MouseDownEvent) {
    pendingE = e;

    clickTimer = window.setTimeout(function () {
      triggerPendingMousedown2();
    }, dblclickDelay);
  }

  function mousedown(e: JQuery.MouseDownEvent) {
    // Listen for left mouse button only
    if (e.which !== 1) return;

    // If no dblclick listener, immediately trigger a mousedown2 event.
    if (!dblclickId) {
      triggerEvent("mousedown2", e);
      return;
    }

    // If there's a dblclick listener, make sure not to count this as a
    // click on the first mousedown; we need to wait for the dblclick
    // delay before we can be sure this click was a single-click.
    if (pendingE === null) {
      scheduleMousedown2(e);
    } else {
      clearTimeout(clickTimer);

      // If second click is too far away, it doesn't count as a double
      // click. Instead, immediately trigger a mousedown2 for the previous
      // click, and set this click as a new first click.
      if (
        (pendingE && Math.abs(pendingE.pageX - e.pageX) > 2) ||
        Math.abs(pendingE.pageY - e.pageY) > 2
      ) {
        triggerPendingMousedown2();
        scheduleMousedown2(e);
      } else {
        // The second click was close to the first one. If it happened
        // within specified delay, trigger our custom 'dblclick2' event.
        pendingE = null;
        triggerEvent("dblclick2", e);
      }
    }
  }

  // IE8 needs a special hack because when you do a double-click it doesn't
  // trigger the click event twice - it directly triggers dblclick.
  function dblclickIE8(e: JQuery.DoubleClickEvent) {
    e.which = 1; // In IE8, e.which is 0 instead of 1. ???
    triggerEvent("dblclick2", e);
  }

  return {
    mousedown: mousedown,
    dblclickIE8: dblclickIE8,
  };
}

export { createClickInfo };
