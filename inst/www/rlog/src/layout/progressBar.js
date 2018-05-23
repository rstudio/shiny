import rlog from "../rlog";
import updateGraph from "../updateGraph/updateGraph";

let fillContainer = null;
let updateProgressBar = function() {
  fillContainer.width(rlog.curTick / rlog.log.length * 100 + "%");
};

let setContainers = function(fullContainer_, fillContainer_) {
  fillContainer = fillContainer_;
  fullContainer_.on("mousedown mousemove", updateFromProgressBar);
};

let updateFromProgressBar = function(e) {
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

let addTimelineTicks = function(
  jqueryContainer,
  className,
  enterExits,
  logLength
) {
  enterExits.map(function(i) {
    jqueryContainer.append(
      `<div class="${className}" style="left: ${100 * i / logLength}%;"></div>`
    );
  });
};

export { updateProgressBar as update, addTimelineTicks, setContainers };
