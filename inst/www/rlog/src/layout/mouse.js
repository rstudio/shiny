import updateGraph from "../updateGraph/updateGraph";

import rlog from "../rlog";

let update_from_timeline = function(e) {
  // Make sure left mouse button is down.
  // Firefox is stupid; e.which is always 1 on mousemove events,
  // even when button is not down!! So read e.originalEvent.buttons.
  if (typeof e.originalEvent.buttons !== "undefined") {
    if (e.originalEvent.buttons !== 1) return;
  } else if (e.which !== 1) {
    return;
  }

  let timeline = e.currentTarget;
  let pos = e.pageX || e.originalEvent.pageX; // pageX in pixels
  let width = timeline.offsetWidth; // width in pixels
  let targetStep = Math.max(Math.round(pos / width * rlog.log.length), 1);
  if (targetStep !== rlog.curTick) {
    updateGraph(targetStep);
  }
  return;
};

let add_mousedown = function(jqueryContainer) {
  jqueryContainer.on("mousedown mousemove", update_from_timeline);
};

export { add_mousedown, update_from_timeline };
export default update_from_timeline;
