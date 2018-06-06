// @flow

import $ from "jquery";
import _ from "lodash";

import { rlog } from "./rlog";

import { GraphAtStep } from "./graph/GraphAtStep";

import colors from "./style/colors";

import * as cytoscapeInit from "./cyto/cytoscapeInit";

import console from "./utils/console";

import * as layoutKeydown from "./layout/keydown";

import * as updateGraph from "./updateGraph";

import * as logEntry from "./layout/logEntry";
import * as progressBar from "./layout/progressBar";

import type { LogType } from "./log/logStates";

import "./log/initStep";

// https://ponyfoo.com/articles/es6-modules-in-depth
// https://github.com/DrkSephy/es6-cheatsheet

// TODO-barret
// √ add buttons for moving around
// √ clean up how active states are done
// √ pulse on active enter change
// √ pulse on valueChange
// √ highlight tree on hover
// √ keep highlight sticky on click
// X add edge styles
// X  distinguish active vs running edges
// X set up cloning of graph after every 250 steps
// √ filtering
// X update legend
// - Re do how subsetting is done by "selecting" and item and always be interested in that item
// enter/exit status and invalidateStart/End need to be controled by a single array status

// // Questions
// should layout be done with full graph and only "turn on" / "turn off" the nodes/edges?
// should filtering be done with the full layout?
//

$(function() {
  console.log(rlog);
  window.barret = rlog;

  rlog.log = (window.log: LogType);
  rlog.cyto = cytoscapeInit.withContainer($("#cyto"));

  rlog.getGraph = new GraphAtStep(rlog.log);
  rlog.graph = rlog.getGraph.atStep(rlog.getGraph.maxStep);
  console.log(rlog.graph);

  $("#startStepButton").click(updateGraph.firstEnterExitEmpty);
  $("#endStepButton").click(updateGraph.lastEnterExitEmpty);
  $("#prevCycleButton").click(updateGraph.prevEnterExitEmpty);
  $("#nextCycleButton").click(updateGraph.nextEnterExitEmpty);
  $("#prevStepButton").click(updateGraph.prevStep);
  $("#nextStepButton").click(updateGraph.nextStep);

  progressBar.setContainers($("#timeline"), $("#timeline-fill"));
  let timelineBackground = $("#timeline-bg");
  progressBar.addTimelineTicks(
    timelineBackground,
    colors.regular.green1,
    rlog.getGraph.enterExitEmpties,
    rlog.log.length
  );
  progressBar.addTimelineTicks(
    timelineBackground,
    colors.regular.red,
    rlog.getGraph.queueEmpties,
    rlog.log.length
  );
  if (rlog.getGraph.marks.length > 0) {
    progressBar.addTimelineTicks(
      timelineBackground,
      colors.regular.purpleLite,
      rlog.getGraph.marks,
      rlog.log.length
    );
  }
  logEntry.setContainer($("#instructions"));

  $("#search").on("input", function(e) {
    updateGraph.withSearchString($(e.target).val());
  });

  {
    let docBody = document.body;
    if (docBody) {
      layoutKeydown.addKeydown($(docBody));
    }
  }

  if (rlog.getGraph.marks.length > 0) {
    let lastMark = _.last(rlog.getGraph.marks);
    // start the graph at the first enter/exit or first empty queue
    updateGraph.atTick(lastMark);
  } else {
    // start the graph at the first enter/exit or first empty queue
    // TODO-barret should start at nextEnterExitEmpty,
    // updateGraph.nextEnterExitEmpty()
    updateGraph.nextQueueEmpty();
  }
});
