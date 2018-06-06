// @flow

import _ from "lodash";
import $ from "jquery";

import { rlog } from "../rlog";
import { updateGraph } from "../updateGraph";

let fillContainer: JQuery;
let updateProgressBar = function(): void {
  fillContainer.width(rlog.curTick / rlog.log.length * 100 + "%");
};

let setContainers = function(
  fullContainer_: JQuery,
  fillContainer_: JQuery
): void {
  fillContainer = fillContainer_;
  fullContainer_.on("mousedown mousemove", updateFromProgressBar);
};

let updateFromProgressBar = function(e: BaseJQueryEventObject): void {
  // Make sure left mouse button is down.
  // Firefox is stupid; e.which is always 1 on mousemove events,
  // even when button is not down!! So read e.originalEvent.buttons.
  if (!_.has(e.originalEvent, "buttons")) {
    // $FlowExpectError
    if (e.originalEvent.buttons !== 1) return;
  }
  // return if not left click
  if (e.which !== 1) {
    return;
  }
  let timeline = $(e.currentTarget)[0];
  let pos = e.pageX; // pageX in pixels  // || e.originalEvent.pageX;

  let width = timeline.offsetWidth; // width in pixels
  let targetStep = Math.max(Math.round(pos / width * rlog.log.length), 1);
  if (targetStep !== rlog.curTick) {
    updateGraph(targetStep);
  }
  return;
};

let addTimelineTicks = function(
  jqueryContainer: JQuery,
  backgroundColor: string,
  enterExits: Array<number>,
  logLength: number,
  className: string = ""
): void {
  enterExits.map(function(i) {
    // add an extra step to show that it is completed
    // i = i + 1;
    let left = 100 * i / logLength;
    let width = 100 * 1 / logLength * 0.75;
    jqueryContainer.append(
      `<div class="timeline-tick ${className}" style="background-color: ${backgroundColor}; left: ${left}%; width: ${width}%; margin-left: -${width}%;"></div>`
    );
  });
};

export { updateProgressBar as update, addTimelineTicks, setContainers };
