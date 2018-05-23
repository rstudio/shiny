/* global log */

import $ from "jquery";

import rlog from "./rlog";

import GraphAtStep from "./graph/GraphAtStep";

import * as cytoscapeInit from "./cyto/cytoscapeInit";

import console from "./utils/console";

import * as layoutMouse from "./layout/mouse";
import * as layoutKeydown from "./layout/keydown";

import * as updateGraph from "./updateGraph/updateGraph";

import * as logEntry from "./layout/logEntry";
import * as progressBar from "./layout/progressBar";

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
// var time;
// try {
//   log = __DATA__;
//   // time = String(__TIME__).toLowerCase() === "true";
// } catch (e) {}

$(function() {
  console.log(rlog);
  window.barret = rlog;
  window.barret.updateGraph = updateGraph;
  rlog.log = log;
  rlog.cyto = cytoscapeInit.with_container($("#cyto"));

  rlog.getGraph = new GraphAtStep(log);
  rlog.graph = rlog.getGraph.atStep(rlog.getGraph.maxStep);
  console.log(rlog.graph);

  layoutMouse.add_mousedown($("#timeline"));

  rlog.getGraph.enterExitEmpties.map(function(i) {
    $("#timeline-bg").append(
      `<div class="timeline-enterexit" style="left: ${100 *
        i /
        rlog.log.length}%;"></div>`
    );
  });
  rlog.getGraph.queueEmpties.map(function(i) {
    $("#timeline-bg").append(
      `<div class="timeline-cycle" style="left: ${100 *
        i /
        rlog.log.length}%;"></div>`
    );
  });

  $("#startStepButton").click(updateGraph.firstEnterExitEmpty);
  $("#endStepButton").click(updateGraph.lastEnterExitEmpty);
  $("#prevCycleButton").click(updateGraph.prevEnterExitEmpty);
  $("#nextCycleButton").click(updateGraph.nextEnterExitEmpty);
  $("#prevStepButton").click(updateGraph.prevStep);
  $("#nextStepButton").click(updateGraph.nextStep);

  logEntry.setContainer($("#instructions"));
  progressBar.setContainer($("#timeline-fill"));

  $("#search").on("input", function(e) {
    updateGraph.withSearchString(e.target.value);
  });

  $(document.body).on("keydown", layoutKeydown);

  // start the graph at the first enter/exit or first empty queue
  // TODO-barret should start at nextEnterExitEmpty,
  // updateGraph.nextEnterExitEmpty()
  updateGraph.nextQueueEmpty();
});
