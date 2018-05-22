import _ from "underscore";

import HoverStatus from "./HoverStatus";
import Graph from "./Graph";

import layoutOptions from "../cyto/layoutOptions";

// // TODO-barret use log states
// import logStates from "../log/logStates"

// TODO-barret make filterDatas and hoverDatas sub modules of subsetDatas or something


class GraphAtStep {
  constructor(log) {
    this.originalLog = log;

    // hoverInfo[key] = `HoverStatus`
    this.searchRegex = null;
    this.filterDatas = null;
    this.hoverData = null;
    this.stickyDatas = null;
    // this.hoverDefault = "focused"
    // this.hoverInfo = {} // use `hoverKey`

    this.filterMap = {};

    this.log = log;
    this.updateSteps(log);

    this.finalGraph = this.atStep(log.length);
    this.finalCyto = this.finalGraph.cytoGraph;
  }

  get hasSearchRegex() {
    return this.searchRegex ? true : false;
  }
  get hasFilterDatas() {
    return this.filterDatas ? this.filterDatas.length > 0 : false;
  }
  get hasStickyDatas() {
    return this.stickyDatas ? this.stickyDatas.length > 0 : false;
  }
  get hasHoverData() {
    return this.hoverData ? true : false;
  }

  updateSteps(log) {
    this.steps = [];
    this.asyncStarts = [];
    this.asyncStops = [];
    this.queueEmpties = [];
    this.enterExitEmpties = [];
    this.minStep = log[0].step;
    this.maxStep = log[log.length - 1].step;

    var data, i;
    var enterExitQueue = [];
    for (i = 0; i < log.length; i++) {
      data = log[i];
      switch (data.action) {
        case "enter":
          enterExitQueue.push(i);
          break;
        case "exit":
          enterExitQueue.pop();
          if (enterExitQueue.length === 0) {
            this.enterExitEmpties.push(data.step + 1);
          }
          break;
        case "asyncStart":
          this.asyncStarts.push(data.step);
          break;
        case "asyncStop":
          this.asyncStops.push(data.step);
          break;
        case "queueEmpty":
          this.queueEmpties.push(data.step);
          break;
      }

      switch (data.action) {
        case "invalidateStart":
          if (data.ctxId === "other") {
            break;
          }
          // TODO-barret check if reactId is a reactive values. If so, skip, otherwise add
          this.steps.push(data.step);
          break;
        case "define":
        // TODO-barret only for reactive values keys
        case "invalidateEnd":
        case "isolateInvalidateStart":
        case "isolateInvalidateEnd":
        // case "isolateEnter":
        // case "isolateExit":
        case "asyncStart":
        case "asyncStop":
        case "queueEmpty":
          break;
        default:
          this.steps.push(data.step);
          break;
      }
    }

    // this.graphCache = {};
    // this.cacheStep = 250;
    // var tmpGraph = new Graph(log);
    // for (i = 0; i < log.length; i++) {
    //   tmpGraph.addEntry(log[i])
    //   if ((i % this.cacheStep) == 0) {
    //     this.graphCache[i] = _.cloneDeep(tmpGraph)
    //   }
    // }
  }

  nextStep(k) {
    // if no filtering... get next step from step array
    if (!this.hasFilterDatas) {
      var nextStepPos = Math.min(
        this.steps.length - 1,
        _.sortedIndex(this.steps, k) + 1
      );
      return this.steps[nextStepPos];
    }

    var graph = this.atStep(k);
    var decendents = undefined,
      ancestors = undefined;

    var logEntry, i, ret;

    for (i = k + 1; i < this.log.length - 1; i++) {
      logEntry = this.log[i];

      // skip if if it's not a valid step anyways...
      if (_.sortedIndexOf(this.steps, logEntry.step) === -1) {
        continue;
      }
      ret = logEntry.step;
      switch (logEntry.action) {
        case "dependsOn":
          // lazy eval decendents and ancestors
          if (_.isNil(decendents) || _.isNil(ancestors)) {
            var filterReactIds = this.filterDatas.map(function(node) {
              return node.reactId;
            });
            decendents = _.union(
              filterReactIds,
              graph.decendentNodeIdsForDatas(this.filterDatas)
            );
            ancestors = _.union(
              filterReactIds,
              graph.ancestorNodeIdsForDatas(this.filterDatas)
            );
          }
          // reactId is target (ends at ancestors)
          if (_.indexOf(ancestors, logEntry.reactId) !== -1) {
            return ret;
          }
          // depOnReactId is source (starts from children)
          if (_.indexOf(decendents, logEntry.depOnReactId) !== -1) {
            return ret;
          }
          break;
        case "dependsOnRemove":
          // check for both to and from (since it must exist beforehand)
          if (
            _.has(graph.nodes, logEntry.reactId) &&
            _.has(graph.nodes, logEntry.depOnReactId)
          ) {
            return ret;
          }
          break;

        case "define":
        case "updateNodeLabel":
          if (this.hasSearchRegex) {
            if (this.searchRegex.test(logEntry.label)) {
              // if there is a search regex and the value is defined
              return ret;
            }
          }
          break;
        case "valueChange":
        case "enter":
        case "exit":
        case "invalidateStart":
        case "invalidateEnd":
        case "isolateEnter":
        case "isolateExit":
        case "isolateInvalidateStart":
        case "isolateInvalidateEnd":
          if (_.has(graph.nodes, logEntry.reactId)) {
            return ret;
          }
          break;

        case "asyncStart":
        case "asyncStop":
        case "queueEmpty":
          break;
        default:
          console.error(logEntry);
          throw "unknown logEntry action next";
      }
    }

    // return the max step possible
    return this.steps[this.steps.length - 1];
  }
  prevStep(k) {
    // if no filtering... get next step from step array
    if (!this.hasFilterDatas) {
      var prevStepPos = Math.max(_.sortedIndex(this.steps, k) - 1, 1);
      return this.steps[prevStepPos];
    }

    var graph = this.atStep(k);
    var logEntry, i, ret;

    for (i = k - 1; i >= 0; i--) {
      logEntry = this.log[i];

      // skip if if it's not a valid step anyways...
      if (_.sortedIndexOf(this.steps, logEntry.step) === -1) {
        continue;
      }
      ret = logEntry.step;
      switch (logEntry.action) {
        case "dependsOn":
        case "dependsOnRemove":
          // check for both to and from (since it must exist beforehand)
          if (
            _.has(graph.nodes, logEntry.reactId) &&
            _.has(graph.nodes, logEntry.depOnReactId)
          ) {
            return ret;
          }
          break;

        case "updateNodeLabel":
        case "valueChange":
        case "enter":
        case "exit":
        case "invalidateStart":
        case "invalidateEnd":
        case "isolateEnter":
        case "isolateExit":
        case "isolateInvalidateStart":
        case "isolateInvalidateEnd":
          if (_.has(graph.nodes, logEntry.reactId)) {
            return ret;
          }
          break;

        case "define":
          if (
            _.some(this.filterDatas, function(filterData) {
              return filterData.reactId === logEntry.reactId;
            })
          ) {
            // some filterdata is defined... so it must be a next step
            return ret;
          }
          break;
        case "asyncStart":
        case "asyncStop":
        case "queueEmpty":
          break;
        default:
          console.error(logEntry);
          throw "unknown logEntry action prev";
      }
    }

    return this.steps[0];
  }

  atStep(k) {
    var kVal = Math.max(1, Math.min(k, this.log.length));
    var i, graph;
    // if (kVal >= this.cacheStep) {
    //   iStart = Math.floor((kVal - 1) / this.cacheStep) * this.cacheStep;
    //   graph = _.cloneDeep(this.graphCache[iStart])
    // }
    graph = new Graph(this.log);
    for (i = 0; i < this.log.length && this.log[i].step <= kVal; i++) {
      graph.addEntry(this.log[i]);
    }

    // if any hover...
    if (this.hasHoverData) {
      if (graph.hasSomeData(this.hoverData)) {
        graph.hoverStatusOnNodeIds(
          graph.familyTreeNodeIds(this.hoverData),
          "state",
          HoverStatus.focused,
          HoverStatus.notFocused
        );
        graph.highlightSelected(this.hoverData);
      }
    }
    // if any sticky...
    if (this.hasStickyDatas) {
      if (
        _.some(
          this.stickyDatas.map(function(data) {
            return graph.hasSomeData(data);
          })
        )
      ) {
        // at least some sticky data is visible
        var stickyTree = graph.familyTreeNodeIdsForDatas(this.stickyDatas);
        graph.hoverStatusOnNodeIds(
          stickyTree,
          "sticky",
          HoverStatus.sticky,
          HoverStatus.notSticky
        );
        this.stickyDatas.map(function(data) {
          graph.highlightSelected(data);
        });
        if (!this.hoverData) {
          // if sticky data no hover data... make the sticky data hover!
          graph.hoverStatusOnNodeIds(
            stickyTree,
            "state",
            HoverStatus.focused,
            HoverStatus.notFocused
          );
        }
      }
    }

    // if any searching
    if (this.hasSearchRegex) {
      var searchRegex = this.searchRegex;
      var matchedNodes = _.filter(_.values(graph.nodes), function(node) {
        return searchRegex.test(node.label);
      });

      if (matchedNodes.length === 0) {
        // TODO-barret warn of no matches
        console.log("no matches!");
        window.getGraph.updateFilterDatasReset();
      } else {
        window.getGraph.updateFilterDatas(matchedNodes);
        // filter on regex
        graph.filterGraphOnNodeIds(
          graph.familyTreeNodeIdsForDatas(this.filterDatas)
        );
      }
    } else {
      // if any filtering...
      if (this.hasFilterDatas) {
        graph.filterGraphOnNodeIds(
          graph.familyTreeNodeIdsForDatas(this.filterDatas)
        );
      }
    }

    return graph;
  }

  // if some sticky items, set those to focused and everything else to not focused
  // else set all to focused
  resetHoverData() {
    this.hoverData = null;
    // if (this.stickyData) {
    //   // some sticky values... bring them to focus
    //   _.mapValues(this.hoverInfo, function(hoverStatus, key){
    //     if (hoverStatus.isSticky) {
    //       hoverStatus.toFocused();
    //     } else {
    //       hoverStatus.toNotFocused();
    //     }
    //   })
    //   this.hoverDefault = HoverStatus.notFocused;
    // } else {
    //   // no sticky values, bring everything to normal
    //   _.mapValues(this.hoverInfo, function(hoverStatus, key){
    //     hoverStatus.toFocused();
    //   })
    //   this.hoverDefault = "focused";
    // }
    return true;
  }

  updateHoverData(data) {
    this.hoverData = data;
  }
  updateHoverDataReset() {
    this.hoverData = null;
  }
  updateStickyDatas(dataArr) {
    this.stickyDatas = dataArr;
  }
  updateStickyDatasReset() {
    this.stickyDatas = null;
  }
  updateFilterDatas(dataArr) {
    this.filterDatas = dataArr;
  }
  updateFilterDatasReset() {
    this.filterDatas = null;
  }
  updateSearchRegex(regex) {
    this.searchRegex = regex;
  }
  updateSearchRegexReset() {
    this.updateFilterDatasReset();
    this.searchRegex = null;
  }
  // // set the value outright
  // updateHoverData(hoverData) {
  //   this.hoverData = hoverData;
  //   // var hoverInfo = this.hoverInfo;
  //   // focusedDatas.map(function(data) {
  //   //   hoverInfo[data.hoverKey].toFocused()
  //   // })
  //   // notFocusedDatas.map(function(data) {
  //   //   hoverInfo[data.hoverKey].toNotFocused()
  //   // })
  // }
  //
  // resetStickyInfo() {
  //   this.stickyData = null;
  //   // var anySticky = _.some(this.hoverInfo, ["sticky", true])
  //   // if (anySticky) {
  //   //   _.mapValues(this.hoverInfo, function(hoverStatus, key) {
  //   //     hoverStatus.toNotSticky()
  //   //     hoverStatus.toFocused()
  //   //   })
  //   // }
  //   // this.hoverDefault = "focused";
  //   return true;
  // }
  // updateStickyInfo(stickyData) {
  //   this.stickyData = stickyData;
  //   // var hoverInfo = this.hoverInfo;
  //   // stickyDatas.map(function(data) {
  //   //   hoverInfo[data.hoverKey].toSticky()
  //   // })
  //   // notStickyDatas.map(function(data) {
  //   //   hoverInfo[data.hoverKey].toNotSticky()
  //   // })
  // }

  filterLogOnDatas(datas) {
    var nodeMap = {};
    datas.map(function(data) {
      if (data instanceof Node) {
        nodeMap[data.reactId] = data;
      }
    });
    var newLog = _.filter(this.originalLog, function(logEntry) {
      switch (logEntry.action) {
        case "dependsOn":
        case "dependsOnRemove":
          // check for both to and from
          return (
            _.has(nodeMap, logEntry.reactId) &&
            _.has(nodeMap, logEntry.depOnReactId)
          );
        case "define":
        case "updateNodeLabel":
        case "valueChange":
        case "invalidateStart":
        case "enter":
        case "isolateInvalidateStart":
        case "isolateEnter":
        case "invalidateEnd":
        case "exit":
        case "isolateExit":
        case "isolateInvalidateEnd":
          // check for reactId
          return _.has(nodeMap, logEntry.reactId);
        case "queueEmpty":
        case "asyncStart":
        case "asyncStop":
          // always add
          return _.has(nodeMap, logEntry.reactId);
        default:
          console.error("logEntry.action: ", logEntry.action, logEntry);
          throw logEntry;
      }
    });
    console.log("new Log: ", newLog);
    return newLog;
  }

  // filterDatasLog() {
  //   var nodeMap = {};
  //   datas.map(function(data) {
  //     if (data instanceof Node) {
  //       nodeMap[data.reactId] = data;
  //     }
  //   });
  //   var newLog = _.filter(this.originalLog, function(logEntry) {
  //     switch (logEntry.action) {
  //       case "dependsOn":
  //       case "dependsOnRemove":
  //         // check for both to and from
  //         return (
  //           _.has(nodeMap, logEntry.reactId) &&
  //           _.has(nodeMap, logEntry.depOnReactId)
  //         );
  //         break;
  //       case "define":
  //       case "updateNodeLabel":
  //       case "valueChange":
  //       case "invalidateStart":
  //       case "enter":
  //       case "isolateInvalidateStart":
  //       case "isolateEnter":
  //       case "invalidateEnd":
  //       case "exit":
  //       case "isolateExit":
  //       case "isolateInvalidateEnd":
  //         // check for reactId
  //         return _.has(nodeMap, logEntry.reactId);
  //         break;
  //       case "queueEmpty":
  //       case "asyncStart":
  //       case "asyncStop":
  //         // always add
  //         return _.has(nodeMap, logEntry.reactId);
  //       default:
  //         console.error("logEntry.action: ", logEntry.action, data);
  //         throw data;
  //     }
  //   });
  //   console.log("new Log: ", newLog);
  //   return newLog;
  // }

  displayAtStep(k, cy) {
    var graph = this.atStep(k);

    cy.startBatch();

    var cytoDur = 400;
    var cyNodes = cy.nodes();
    var graphCyto = graph.cytoGraph;
    var graphNodes = graphCyto.nodes();
    var nodesLRB = cyNodes.diff(graphNodes);
    // .removeStyle()

    var onLayoutReady = [];

    // enter
    nodesLRB.right.map(function(graphNode) {
      var graphNodeData = graphNode.data();
      cy
        .add(graphNode)
        .classes(graphNodeData.cytoClasses)
        .style(graphNodeData.cytoStyle);
      // .animate({
      //   // style: ,
      //   duration: cytoDur
      // });
      window.barret = cy.$id(graphNode.id());
    });
    // update
    nodesLRB.both.map(function(cytoNode) {
      var cyNode = cy.$id(cytoNode.id());

      var graphNode = graphNodes.$id(cytoNode.id());
      var graphNodeData = graphNode.data();
      var graphClasses = graphNodeData.cytoClasses;

      cyNode
        // update to latest data
        .data(graphNodeData)
        .classes(graphClasses)
        .removeStyle()
        .style(graphNodeData.cytoStyle);
      // .animate({
      //   // style: graphNodeData.cytoStyle,
      //   duration: cytoDur
      // });

      // pulse value change
      if (graphNodeData.valueChangedStatus.isActiveAtStep(k)) {
        onLayoutReady.push(function() {
          cyNode.flashClass("nodeStartBig", 125);
        });
      }
      // pulse value enter or invalidate change
      if (
        graphNodeData.invalidateStatus.isActiveAtStep(k) ||
        graphNodeData.enterStatus.isActiveAtStep(k)
      ) {
        onLayoutReady.push(function() {
          switch (graphNodeData.type) {
            case "observable":
              cyNode.flashClass("nodeMiddleBig", 125);
              break;
            case "observer":
              cyNode.flashClass("nodeEndBig", 125);
              break;
          }
        });
      }
    });
    // exit
    nodesLRB.left.map(function(cytoNode) {
      cy.remove(cytoNode);
      // .animate({duration: cytoDur});
    });

    var cyEdges = cy.edges();
    var graphEdges = graphCyto.edges();
    var edgesLRB = cyEdges.diff(graphEdges);
    // enter
    edgesLRB.right.map(function(graphEdge) {
      var graphEdgeData = graphEdge.data();
      cy
        .add(graphEdge)
        .classes(graphEdgeData.cytoClasses)
        .removeStyle()
        .style(graphEdgeData.cytoStyle);
      // .animate({
      //   style: graphEdgeData.cytoStyle,
      //   duration: cytoDur
      // });
    });
    // update
    edgesLRB.both.map(function(cytoEdge) {
      var graphEdgeData = graphEdges.$id(cytoEdge.id()).data();
      cy
        .$id(cytoEdge.id())
        // .classes()
        .classes(graphEdgeData.cytoClasses)
        .data(graphEdgeData)
        .removeStyle()
        .style(graphEdgeData.cytoStyle);
      // .animate({
      //   style: graphEdgeData.cytoStyle,
      //   duration: cytoDur
      // });
    });
    // exit
    edgesLRB.left.map(function(cytoEdge) {
      // var graphEdge = cytoEdge.data();
      // remove the original edge
      cy.remove(cytoEdge).animate({ duration: cytoDur });
    });

    cy.endBatch();

    // send in sorted elements according to the key.
    // If provided in a consistent order, layouts are consistent.
    // `eles` default to `options.eles != null ? options.eles : cy.$();`
    var sortedElements = cy.$().sort(function(a, b) {
      return a.data().key > b.data().key ? 1 : -1;
    });

    // if no new edges appeared or disappeared
    // or no nodes entered or exited
    if (
      edgesLRB.right.length === edgesLRB.left.length &&
      nodesLRB.right.length === 0 &&
      nodesLRB.left.length === 0
    ) {
      // do not re-render layout... just call onLayoutReady
      onLayoutReady.map(function(fn) {
        fn();
      });
    } else {

      // TODO-barret move this method to layout
      // calculate a new layout
      // time expensive!!!
      cy
        .layout(
          _.assign(
            {
              // provide elements in sorted order to make determanistic layouts
              eles: sortedElements,
              // run on layout ready
              ready: function() {
                onLayoutReady.map(function(fn) {
                  fn();
                });
              },
            },
            layoutOptions
            // ,
            // TODO-barret Make animation a setting... it's expensive!
            // {animate: true}
          )
        )
        .run();
    }
  }
}



export default GraphAtStep;
