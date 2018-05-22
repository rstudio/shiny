/* global $, _, log */

import test from "./test";

import colors from "./style/colors";

import graphStyles from "./cyto/cytoStyle";
import layoutOptions from "./cyto/layoutOptions";

import GraphAtStep from "./graph/GraphAtStep";

import cytoscapeInit from "./cyto/cytoscapeInit";

import console from "./utils/console";


// https://ponyfoo.com/articles/es6-modules-in-depth
// https://github.com/DrkSephy/es6-cheatsheet

console.log(test.hello); // eslint-disable-line no-console

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
  window.cyto = cytoscapeInit.with_container($("#cyto"));

  // cytoFamilySuccPred = function(ele, addExtraLayer = true) {
  //   var familyEles = cyto.collection();
  //   if (ele.isEdge()) {
  //     var edge = ele;
  //     if (false) {
  //       // TODO-barret attempt at getting all nodes and edges from final graph
  //       // abandon, as the edge should be stored and constantly retrieved somewhere else
  //       console.log(`#${ele.source().id()} -> #${ele.target().id()}`)
  //       ele = getGraph.finalCyto.$(`#${ele.source().id()} -> #${ele.target().id()}`)
  //     }
  //     familyEles = familyEles
  //       .add(edge)
  //       .add(ele.target())
  //       .add(ele.target().successors())
  //       .add(ele.source())
  //       .add(ele.source().predecessors());
  //   } else {
  //     // is node
  //     var node = ele;
  //     if (false) {
  //       ele = getGraph.finalCyto.$id(ele.id())
  //     }
  //     familyEles = familyEles
  //       .add(node)
  //       .add(ele.successors())
  //       .add(ele.predecessors());
  //   }
  //   if (addExtraLayer) {
  //     var familyNodes = familyEles.nodes();
  //     familyEles = familyEles
  //       .add(familyNodes.incomers())
  //       .add(familyNodes.outgoers());
  //   }
  //   return familyEles;
  // }
  window.cyto.on("mouseover", function(evt) {
    var target = evt.target;
    if (target === window.cyto) return;

    // highlight all outgoer's outgoers and all incomer's incomers and self
    // var familyEles = cytoFamilySuccPred(target, false);

    // var elesData = function(eles) {
    //   return eles.map(function(ele) {
    //     return ele.data();
    //   })
    // }

    var hasCalled = false;
    var debounced = _.debounce(function() {
      hasCalled = true;
      updateGraph.hoverData(target.data());
    }, 200);
    debounced();
    // if a mouseout occurs before the function is executed, cancel it
    // works as mouseout is always called before mouseover
    target.once("mouseout", function(evtOut) {
      debounced.cancel();
      if (hasCalled) {
        // only remove hover if hover added
        updateGraph.hoverDataReset();
      }
    });
  });

  var cytoClickedBefore, cytoClickedTimeout;
  window.cyto.on("click", function(evt) {
    // remove focus on search
    $("#search").blur();

    // var elesData = function(eles) {
    //   return eles.map(function(ele) {
    //     return ele.data();
    //   })
    // }

    var target = evt.target;

    // check for double click
    // https://stackoverflow.com/a/44160927
    if (cytoClickedTimeout && cytoClickedBefore) {
      clearTimeout(cytoClickedTimeout);
    }
    if (cytoClickedBefore === target) {
      // is actually a double click... return!
      target.trigger("dblclick", evt);
      cytoClickedBefore = null;
      return;
    } else {
      cytoClickedTimeout = setTimeout(function() {
        cytoClickedBefore = null;
      }, 400);
      cytoClickedBefore = target;
      // continue like regular click
      console.log("click!!", evt);
    }

    if (target === window.cyto) {
      // remove sticky focus class
      updateGraph.stickyDatasReset();
      return;
    }

    // var familyEles = cytoFamilySuccPred(target, false);
    updateGraph.stickyDatas([target.data()]);
    //   elesData(familyEles),
    //   elesData(cyto.$().not(familyEles))
    // )
    return;
  });

  window.cyto.on("dblclick", function(evt, originalEvt) {
    // var elesData = function(eles) {
    //   return eles.map(function(ele) {
    //     return ele.data();
    //   })
    // }

    console.log("dbl click!!");
    // console.log("dbl click!!", evt, originalEvt);
    var target = evt.target;

    if (target === window.cyto) {
      // go back to full graph
      updateGraph.resetHoverStickyFilterData();
      return;
    }

    // var holdingShiftKey = originalEvt.originalEvent.shiftKey;
    // if (holdingShiftKey) {
    //   console.log("extra layers!")
    //   var familyEles = cytoFamilySuccPred(target, true);
    //   var familyDatas = elesData(familyEles)
    //
    //   var directFamilyEles = cytoFamilySuccPred(target, false);
    //   getGraph.updateHoverInfo(
    //     elesData(directFamilyEles),
    //     elesData(cyto.$().not(directFamilyEles))
    //   )
    //
    //   updateGraph.withDatas(familyDatas)
    //
    // } else {
    // var familyEles = cytoFamilySuccPred(target, false);
    // var familyDatas = elesData(familyEles)

    updateGraph.filterDatas([target.data()]);
    // updateGraph.withDatas(familyDatas)

    // }
  });

  window.getGraph = new GraphAtStep(log);
  var getGraph = window.getGraph;
  window.graph = getGraph.atStep(getGraph.maxStep);
  var graph = window.graph;
  console.log(graph);

  getGraph.enterExitEmpties.map(function(i) {
    $("#timeline-bg").append(
      `<div class="timeline-enterexit" style="left: ${100 *
        i /
        this.log.length}%;"></div>`
    );
  });
  getGraph.queueEmpties.map(function(i) {
    $("#timeline-bg").append(
      `<div class="timeline-cycle" style="left: ${100 *
        i /
        this.log.length}%;"></div>`
    );
  });

  function updateProgressBar() {
    $("#timeline-fill").width(window.curTick / window.log.length * 100 + "%");
  }
  function updateLogItem() {
    $("#instructions").text(
      JSON.stringify(window.log[window.curTick], null, "  ")
    );
  }
  $("#timeline").on("mousedown mousemove", function(e) {
    // Make sure left mouse button is down.
    // Firefox is stupid; e.which is always 1 on mousemove events,
    // even when button is not down!! So read e.originalEvent.buttons.
    if (typeof e.originalEvent.buttons !== "undefined") {
      if (e.originalEvent.buttons !== 1) return;
    } else if (e.which !== 1) {
      return;
    }

    var timeline = e.currentTarget;
    var pos = e.pageX || e.originalEvent.pageX; // pageX in pixels
    var width = timeline.offsetWidth; // width in pixels
    var targetStep = Math.max(Math.round(pos / width * window.log.length), 1);
    if (targetStep !== window.curTick) {
      window.curTick = targetStep;
      updateGraph();
    }
    return;
  });

  window.updateGraph = function() {
    getGraph.displayAtStep(window.curTick, window.cyto);
    updateProgressBar();
    updateLogItem();
  };
  var updateGraph = window.updateGraph;

  // when str length < 3 do not search
  // when str length = 0, reset filter
  // when str length >= 3, set filter to all elements that match
  updateGraph.withSearchString = function(str) {
    // if less than three chars...
    if (str.length < 3) {
      if (str.length === 0) {
        // TODO-barret show warning of resetting
        console.log("resetting log!");
        updateGraph.searchRegexReset();
      } else {
        // TODO-barret show warning of not enough characters
        console.log("do nothing");
      }
      return;
    }
    // escape the string
    // https://stackoverflow.com/a/17606289
    var escapeRegExp = function(str) {
      return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
    };
    var searchRegex = new RegExp(escapeRegExp(str));
    updateGraph.searchRegex(searchRegex);
    return;
  };

  // updateGraph.resetWithLog = function() {
  //   updateGraph.withLog(getGraph.originalLog)
  // }
  // updateGraph.withLog = function(log) {
  //   getGraph.withLog(log);
  //   getGraph.updateStickyDataReset()
  //
  //   var nextTick = getGraph.nextStep(curTick);
  //
  //   if (getGraph.prevStep(nextTick) != curTick) {
  //     // updateGraph.prevStep();
  //     updateGraph()
  //   } else {
  //     updateGraph()
  //   }
  // }

  // updateGraph.withDatas = function(datas) {
  //   var newLog = getGraph.filterLogOnDatas(datas);
  //   updateGraph.withLog(newLog);
  // }

  updateGraph.hoverData = function(data) {
    getGraph.updateHoverData(data);
    updateGraph();
  };
  updateGraph.hoverDataReset = function() {
    getGraph.updateHoverDataReset();
    updateGraph();
  };
  updateGraph.stickyDatas = function(data) {
    getGraph.updateStickyDatas(data);
    updateGraph();
  };
  updateGraph.stickyDatasReset = function() {
    getGraph.updateStickyDatasReset();
    updateGraph();
  };
  updateGraph.filterDatas = function(data) {
    getGraph.updateFilterDatas(data);
    updateGraph();
  };
  updateGraph.filterDatasReset = function() {
    getGraph.updateFilterDatasReset();
    updateGraph();
  };
  updateGraph.searchRegex = function(searchRegex) {
    getGraph.updateSearchRegex(searchRegex);
    updateGraph();
  };
  updateGraph.searchRegexReset = function(searchRegex) {
    getGraph.updateSearchRegexReset();
    updateGraph();
  };
  updateGraph.resetHoverStickyFilterData = function() {
    getGraph.updateHoverDataReset();
    getGraph.updateStickyDatasReset();
    getGraph.updateFilterDatasReset();
    getGraph.updateSearchRegexReset();
    updateGraph();
  };

  updateGraph.nextTick = function() {
    window.curTick += 1;
    updateGraph();
  };
  updateGraph.prevTick = function() {
    window.curTick -= 1;
    updateGraph();
  };
  updateGraph.nextStep = function() {
    // Move one step ahead (skipping unneccessary steps)
    window.curTick = getGraph.nextStep(window.curTick);
    updateGraph();
  };
  updateGraph.prevStep = function() {
    // Move one step back
    window.curTick = getGraph.prevStep(window.curTick);
    updateGraph();
  };

  updateGraph.nextEnterExitEmpty = function() {
    var nextTick;
    if (_.sortedIndexOf(getGraph.enterExitEmpties, window.curTick) !== -1) {
      // not at a cycle point
      if (getGraph.hasFilterDatas) {
        // if filtered, will go to previous step, then next step location
        nextTick = getGraph.nextStep(getGraph.prevStep(window.curTick));
      } else {
        // if not filtered
        nextTick = window.curTick;
      }
    } else {
      // at cycle point
      // first move one step forward... then find next enter/exit empty
      nextTick = getGraph.nextStep(window.curTick);
    }
    var val, i;
    // move to queue empty
    for (i = 0; i < getGraph.enterExitEmpties.length; i++) {
      val = getGraph.enterExitEmpties[i] - 1;
      if (nextTick <= val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  };
  updateGraph.prevEnterExitEmpty = function() {
    var prevTick;
    if (_.sortedIndexOf(getGraph.enterExitEmpties, window.curTick) !== -1) {
      // not at a cycle point
      if (getGraph.hasFilterDatas) {
        // if filtered, will go to next step, then prev step location
        prevTick = getGraph.prevStep(getGraph.nextStep(window.curTick));
      } else {
        // if not filtered
        prevTick = window.curTick;
      }
    } else {
      // at cycle point
      // first move one step forward... then find next enter/exit empty
      prevTick = getGraph.prevStep(window.curTick);
      console.log("at cycle point", window.curTick, prevTick);
    }
    var val, i;
    // move to queue empty
    for (i = getGraph.enterExitEmpties.length - 1; i >= 0; i--) {
      val = getGraph.enterExitEmpties[i];
      if (prevTick > val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  };
  updateGraph.lastEnterExitEmpty = function() {
    window.curTick =
      getGraph.enterExitEmpties[getGraph.enterExitEmpties.length - 1] || 0;
    updateGraph();
  };
  updateGraph.firstEnterExitEmpty = function() {
    window.curTick = getGraph.enterExitEmpties[0] || 0;
    updateGraph();
  };

  updateGraph.nextQueueEmpty = function() {
    var i, val;
    // move to queue empty
    for (i = 0; i < getGraph.enterExitEmpties.length; i++) {
      val = getGraph.queueEmpties[i];
      if (window.curTick < val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  };
  updateGraph.prevQueueEmpty = function() {
    var i, val;
    // move to queue empty
    for (i = getGraph.queueEmpties.length - 1; i >= 0; i--) {
      val = getGraph.queueEmpties[i];
      if (window.curTick > val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  };
  updateGraph.lastQueueEmpty = function() {
    window.curTick =
      getGraph.queueEmpties[getGraph.queueEmpties.length - 1] || 0;
    updateGraph();
  };
  updateGraph.firstQueueEmpty = function() {
    window.curTick = getGraph.queueEmpties[0] || 0;
    updateGraph();
  };

  window.curTick = 1;
  // TODO-barret should start at nextEnterExitEmpty,
  // updateGraph.nextEnterExitEmpty()
  updateGraph.nextQueueEmpty();

  $("#startStepButton").click(updateGraph.firstEnterExitEmpty);
  $("#endStepButton").click(updateGraph.lastEnterExitEmpty);
  $("#prevCycleButton").click(updateGraph.prevEnterExitEmpty);
  $("#nextCycleButton").click(updateGraph.nextEnterExitEmpty);
  $("#prevStepButton").click(updateGraph.prevStep);
  $("#nextStepButton").click(updateGraph.nextStep);

  $("#search").on("input", function(e) {
    updateGraph.withSearchString(e.target.value);
  });
  $(document.body).on("keydown", function(e) {
    console.log("e: ", e);
    if (e.target.id && e.target.id === "search") {
      // is in search text box
      if (e.which === 27) {
        $(e.target).blur();
      } else {
        // if (e.which == 13) { // enter
        // }
      }
      return;
    }
    if (e.which === 39 || e.which === 32) {
      // space, right
      if (e.altKey) {
        if (e.shiftKey) {
          // option + shift + right
          if (updateGraph.nextQueueEmpty()) {
            return;
          }
          // if it can't go right, try a cycle
        }
        // option + right
        // return false if there is no more enter/exit empty marks
        if (updateGraph.nextEnterExitEmpty()) {
          return;
        }
        // if it cant go right, try a step
      } else if (e.shiftKey) {
        // shift + right
        updateGraph.nextTick();
        return;
      }
      if (window.curTick < getGraph.maxStep) {
        // right
        updateGraph.nextStep();
        return;
      }
    }
    if (e.which === 37) {
      // left
      if (e.altKey) {
        if (e.shiftKey) {
          // option + shift + left
          if (updateGraph.prevQueueEmpty()) {
            return;
          }
          // if can't go left, try cycle
        }
        // option + left
        if (updateGraph.prevEnterExitEmpty()) {
          return;
        }
        // if can't go left, try step
      } else if (e.shiftKey) {
        // shift + left
        updateGraph.prevTick();
        return;
      }
      if (window.curTick > 1) {
        // left
        updateGraph.prevStep();
        return;
      }
    }
    if (e.which === 35) {
      // end
      // Seek to end
      updateGraph.lastEnterExitEmpty();
      return;
    }
    if (e.which === 36) {
      // home
      // Seek to beginning
      updateGraph.firstEnterExitEmpty();
      return;
    }

    if (e.which === 27) {
      // esc

      // remove hover
      // remove sticky
      // remove filter
      if (getGraph.hasHoverData) {
        console.log("reset hover");
        updateGraph.hoverDataReset();
      } else if (getGraph.hasStickyDatas) {
        console.log("reset sticky");
        updateGraph.stickyDatasReset();
      } else if (getGraph.hasFilterDatas) {
        console.log("reset filter");
        // must be in filter... so exit filter
        $("#search").val("");
        updateGraph.searchRegexReset();
      }
      return;
    }
    if (e.which === 38) {
      // arrow up
      if (getGraph.hasFilterDatas) {
        console.log("add layer!");
      }
      return;
    }
    if (e.which === 40) {
      // arrow down
      if (getGraph.hasFilterDatas) {
        console.log("remove layer!");
      }
      return;
    }
    if (e.which === 83) {
      // s
      _.defer(function() {
        $("#search").focus();
      });
      e.stopPropagation();
      return;
    }
  });
});
