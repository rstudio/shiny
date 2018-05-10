
// TODO-barret
// √ add buttons for moving around
// √ clean up how active states are done
// √ pulse on active enter change
// √ pulse on valueChange
// √ highlight tree on hover
// √ keep highlight sticky on click
// add edge styles
//   distinguish active vs running edges
// set up cloning of graph after every 250 steps
// filtering
// update legend

// // Questions
// should layout be done with full graph and only "turn on" / "turn off" the nodes/edges?
// should filtering be done with the full layout?
//

colors = {
  // regular colors
  regular: {
    white: "#ffffff",
    black: "#000000",

    // http://colorbrewer2.org/#type=sequential&scheme=YlGn&n=4
    // #2-4
    green1: "#f7fcb9", // ready
    green2: "#78c679", // enter
    green3: "#238443", // active enter

    greenLite: "#b2df8a", // green from http://colorbrewer2.org/#type=qualitative&scheme=Paired&n=8

    // http://colorbrewer2.org/#type=qualitative&scheme=Set1&n=9
    red: "#e41a1c", // valueChange
    blue: "#377eb8", // frozen
    green: "#4daf4a", // enter
    purple: "#984ea3", //
    orange: "#ff7f00", //
    yellow: "#ffff33", //
    brown: "#a65628", //
    pink: "#f781bf", //
    grey: "#999999", // invalidate

    // http://colorbrewer2.org/#type=sequential&scheme=Greys&n=9
    grey1: "#d9d9d9", // invalidate
    grey2: "#969696", // active invalidate
    grey3: "#737373" // active invalidate
  },
  edges: {
    running: "#676767",
    isolate: "#818181",
    active: "#818181",
    inactive: "#ececec"
  },
  // filtered colors
  lite: {
    white: "#ffffff",
    black: "#b2b2b2", // personal attempt

    // http://colorbrewer2.org/#type=sequential&scheme=YlGn&n=9
    // #1-3
    green1: "#ffffe5",
    green2: "#f7fcb9",
    green3: "#d9f0a3",

    greenLite: "#d6eec0", // personal attempt
    // http://colorbrewer2.org/#type=qualitative&scheme=Pastel1&n=9
    red: "#fbb4ae",
    blue: "#b3cde3",
    green: "#ccebc5",
    purple: "#decbe4",
    orange: "#fed9a6",
    yellow: "#ffffcc",
    brown: "#e5d8bd",
    pink: "#fddaec",
    grey: "#f2f2f2"
  }
}


class HoverStatus {
  constructor(state) {
    this.sticky = false; // true / false
    this.state = state || "focused"; // "off", "focused", "notFocused"
  }
  get isSticky() { return this.sticky == true }
  toNotSticky() { this.sticky = false; }
  toSticky() { this.sticky = true; }

  get isFocused() { return this.state == "focused" }
  toFocused() { this.state = "focused"; }
  toNotFocused() { this.state = "notFocused"; }
}

// pulse on being active at step k; isAtStep(k)
// display engaged; isOn
// display active engaged; isOn and isActive
// display finished; isFinished
// display none; isOff
class ActiveStateStatus {
  constructor() {
    this.state = "off"; // "on", "finished", "off"
    this.activeStep = -1;
  }
  setState(state) { this.state = state; }
  setActiveAtStep(step) { this.toOn(); this.activeStep = step; }
  reset() {
    this.toOff();
    this.resetActive();
  }
  resetActive() {
    this.activeStep = -1;
  }
  get isOn() { return this.state == "on" }
  get isOff() { return this.state == "off" }
  get isFinished() { return this.state == "finished" }
  get isActive() { return this.isOn && this.activeStep > 0 }
  isActiveAtStep(k) { return this.isActive && this.activeStep == k}

  toOn() {
    this.state = "on";
  }
  toFinished() {
    this.state = "finished";
  }
  toOff() {
    this.state = "off";
  }
}
class StatusArr {
  constructor() {
    this.statusArr = [];
  }
  add(obj) {
    return this.statusArr.push(obj);
  }
  remove() {
    return this.statusArr.pop()
  }
  last() {
    return _.last(this.statusArr)
  }
  containsStatus(status) {
    var arr = this.statusArr, n = arr.length;
    for (var i = 0; i < n; i++) {
      if (arr[i].action == status) {
        return true;
      }
    }
    return false;
  }

  static expect_prev_status(curStatus, prevStatus, expectedAction) {
    var on_error = function(msg) {
      console.error("curStatus: ", curStatus)
      console.error("prevStatus: ", prevStatus)
      throw msg
    }
    if (prevStatus.action != expectedAction) {
      on_error(`prior node status does not have "${expectedAction}" status`)
    }
    if (prevStatus.ctxId != curStatus.ctxId) {
      on_error(`prior node "ctxId" status does not have the same "ctxId" status`)
    }

  }
}
class Node {
  constructor(data) {
    if (typeof data.reactId === "undefined") throw "data.reactId not provided in new Node";
    if (typeof data.label === "undefined") throw "data.label not provided in new Node";
    if (typeof data.type === "undefined") throw "data.type not provided in new Node";
    if (typeof data.session === "undefined") throw "data.session not provided in new Node";
    if (typeof data.time === "undefined") throw "data.time not provided in new Node";
    this.reactId = data.reactId;
    this.label = data.label;
    this.type = data.type;
    this.session = data.session;
    this.time = data.time;
    this.statusArr = new StatusArr(data.statusArr || []);
    this.value = data.value || null;
    this.hoverStatus = data.hoverStatus || new HoverStatus();

    this.valueChangedStatus = data.valueChangedStatus || new ActiveStateStatus();

    // this.inInvalidate = data.inInvalidate || false;
    // this.activeInvalidate = data.activeInvalidate || false;

    this.enterStatus = data.enterStatus || new ActiveStateStatus();

    this.invalidateStatus = data.invalidateStatus || new ActiveStateStatus();
  }
  get id() {return this.reactId.replace(/\$/g, "_")}
  get key() {return this.reactId}
  get hoverKey() {return this.key}
  statusAdd(obj) {
    this.statusArr.add(obj);
    return this.statusArr;
  }
  statusRemove() {return this.statusArr.remove();}
  statusLast() {return this.statusArr.last();}
  get inEnter() {return this.statusArr.containsStatus("enter");}
  get inIsolate() {return this.statusArr.containsStatus("isolateEnter");}
  // get inInvalidate() {return this.statusArr.containsStatus("invalidateStart");}
  get inIsolateInvalidate() {return this.statusArr.containsStatus("isolateInvalidateStart");}
  get cytoStyle() {
    return {}
  }
  get cytoLabel() {
    // if (this.label.length > 30) {
    //   return (this.label.substring(0, 27) + "aasdf")
    // }
    return this.label
  }
  get cytoClasses() {
    var classes = []
    switch(this.type) {
      case "observer": classes.push("nodeEnd"); break;
      case "observable": classes.push("nodeMiddle"); break;
      default: classes.push("nodeStart")
    }

    if (this.inEnter) classes.push("nodeEnter");
    if (this.enterStatus.isActive) classes.push("nodeEnterActive");

    if (this.type == "observer" || this.type == "observable") {
      if (this.invalidateStatus.isActive) classes.push("nodeInvalidateActive")
      else if (this.invalidateStatus.isOn) classes.push("nodeInvalidate")
      else if (this.invalidateStatus.isFinished) classes.push("nodeInvalidateDone")
    }
    // if (this.inInvalidate) classes.push("nodeInvalidate");
    if (this.inIsolate) classes.push("nodeIsolate");
    // if (this.inIsolateInvalidate) classes.push("nodeIsolateInvalidate");
    if (this.valueChangedStatus.isOn) classes.push("nodeValueChanged");

    switch (this.hoverStatus.state) {
      case "focused": break;
      case "notFocused":
        if (this.hoverStatus.sticky) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }

    return classes.join(" ")
  }
  get cytoData() {
    var retData = this;
    return {
      group: "nodes",
      data: retData
    }
  }
}
class Edge {
  constructor(data) {
    if (typeof data.reactId === "undefined") throw "data.reactId not provided to new Edge()";
    if (typeof data.depOnReactId === "undefined") throw "data.depOnReactId not provided to new Edge()";
    if (typeof data.ctxId === "undefined") throw "data.ctxId not provided to new Edge()";
    if (typeof data.time === "undefined") throw "data.time not provided to new Edge()";
    this.reactId = data.reactId;
    this.depOnReactId = data.depOnReactId;
    this.ctxId = data.ctxId;
    this.session = data.session || "Global";
    this.time = data.time;
    this.status = "normal";
    this.isGhost = false;
    this.hoverStatus = data.hoverStatus || new HoverStatus();
  }
  get id() {
    return `${ this.reactId }_${ this.depOnReactId }_${this.ctxId}`.replace(/\$/g, "_");
  }
  get source() {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target() {
    return this.reactId.replace(/\$/g, "_");
  }
  get key() {
    return `${ this.reactId } depends on ${ this.depOnReactId } in ${this.ctxId}`;
  }
  get ghostKey() {
    return `${ this.reactId } depends on ${ this.depOnReactId }`;
  }
  get hoverKey() {return this.ghostKey}
  get inIsolate() {
    return this.status == "isolate";
  }
  get cytoClasses() {
    var classes = [];
    if (this.inIsolate) classes.push("edgeIsolate");
    switch (this.hoverStatus.state) {
      case "off": break;
      case "focused": break;
      case "notFocused":
        if (this.hoverStatus.sticky) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }
    return classes.join(" ")
  }
  get cytoData() {
    var retData = this;
    return {
      group: "edges",
      data: retData
    }
  }
}
class GhostEdge {
  constructor(data) {
    if (typeof data.reactId === "undefined") throw "data.reactId not provided to new GhostEdge()";
    if (typeof data.depOnReactId === "undefined") throw "data.depOnReactId not provided to new GhostEdge()";
    if (typeof data.time === "undefined") throw "data.time not provided to new GhostEdge()";
    this.reactId = data.reactId;
    this.depOnReactId = data.depOnReactId;
    this.session = data.session || "Global";
    this.time = data.time;
    this.isGhost = true;
    this.hoverStatus = data.hoverStatus || new HoverStatus();
  }
  get id() {
    return `${ this.reactId }_${ this.depOnReactId }`.replace(/\$/g, "_");
  }
  get source() {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target() {
    return this.reactId.replace(/\$/g, "_");
  }
  get key() {
    return `${ this.reactId } depends on ${ this.depOnReactId }`;
  }
  get hoverKey() {return this.key}
  get cytoStyle() {
    return {};
    // return graphStyles.ghostEdge.default
  }
  get cytoClasses() {
    return "edgeGhost"
  }
  get cytoData() {
    var retData = this;
    return {
      group: "edges",
      data: retData
    }
  }
}

class Graph {
  constructor(log) {
    this.log = log;
    this.nodes = {};
    this.edges = {};
    this.edgesUnique = {};
    this.asyncStart = -1;
    this.asyncStop = -1;
    this.queueEmpty = -1;
    this.activeNodeEnter = [];
    this.activeInvalidateEnter = [];
  }

  get cytoGraph() {
    var cyto = cytoscape();
    var nodes = _.values(this.nodes).map(function(node) {
      return node.cytoData;
    });
    cyto.add(nodes);
    var ghostEdgeMap = _.assign({}, this.edgesUnique);
    var edges = _.values(this.edges).map(function(edge) {
      // remove matching unique/ghost edges
      if (_.has(ghostEdgeMap, edge.ghostKey)) {
        delete ghostEdgeMap[edge.ghostKey];
      }
      return edge.cytoData;
    });
    cyto.add(edges);
    var ghostEdges = _.values(ghostEdgeMap).map(function(edge) {
      return edge.cytoData;
    });
    cyto.add(ghostEdges);

    return cyto;
  }

  addEntry(data) {

    if (data.reactId == "rNoCtx") {
      return;
    }

    switch (data.action) {
      // {"action": "define", "reactId": "r3", "label": "plotObj", "type": "observable", "session": "fa3c747a6121aec5baa682cc3970b811", "time": 1524581676.5841},
      case "define":
        this.nodes[data.reactId] = new Node(data);
        break;

      // {"action": "updateNodeLabel", "nodeId": "1", "label": "input", "session": null, "time": 1522955046.5537},
      case "updateNodeLabel":
        this.nodes[data.reactId].label = data.label;
        break;

      case "valueChange":
        var node = this.nodes[data.reactId];
        node.value = data.value;
        node.valueChangedStatus.setActiveAtStep(data.step);
        break;

      case "invalidateStart":
        var node = this.nodes[data.reactId];
        var lastNodeId = _.last(this.activeInvalidateEnter)
        if (lastNodeId) {
          this.nodes[lastNodeId].invalidateStatus.resetActive()
        }
        this.activeInvalidateEnter.push(data.reactId)
        switch (node.type) {
          case "observable":
          case "observer":
            node.invalidateStatus.setActiveAtStep(data.step);
            break;
        }
        node.statusAdd(data)
        break;
      case "enter":
        var lastNodeId = _.last(this.activeNodeEnter);
        if (lastNodeId) {
          this.nodes[lastNodeId].enterStatus.resetActive()
        }
        this.activeNodeEnter.push(data.reactId);
        var node = this.nodes[data.reactId]
        node.enterStatus.setActiveAtStep(data.step);
        switch (node.type) {
          case "observer":
          case "observable":
            node.invalidateStatus.reset()
        }
        node.statusAdd(data)
        break;

      case "isolateInvalidateStart":
      case "isolateEnter":
        this.nodes[data.reactId].statusAdd(data)
        break;

      case "invalidateEnd":
      case "exit":
      case "isolateExit":
      case "isolateInvalidateEnd":
        var node = this.nodes[data.reactId];
        switch (data.action) {
          case "exit":
            this.nodes[_.last(this.activeNodeEnter)].enterStatus.reset();
            this.activeNodeEnter.pop();
            var lastNodeId = _.last(this.activeNodeEnter);
            if (lastNodeId) {
              this.nodes[lastNodeId].enterStatus.setActiveAtStep(data.step);
            }
            break;
          case "invalidateEnd":
            // turn off the previously active node
            this.nodes[_.last(this.activeInvalidateEnter)].invalidateStatus.resetActive();
            this.activeInvalidateEnter.pop();
            // if another invalidateStart node exists...
            //   set the previous invalidateStart node to active
            var lastNodeId = _.last(this.activeInvalidateEnter)
            if (lastNodeId) {
              this.nodes[lastNodeId].invalidateStatus.setActiveAtStep(data.step);
            }
            node.invalidateStatus.toFinished()
            if (node.valueChangedStatus.isOn) {
              node.valueChangedStatus.reset();
            }
            break;
          case "isolateInvalidateEnd":
            if (node.valueChangedStatus.isOn) {
              node.valueChangedStatus.reset();
            }
            break;
        }
        var prevData = node.statusLast()
        var expectedAction = {
          "exit": "enter",
          "isolateExit": "isolateEnter",
          "invalidateEnd": "invalidateStart",
          "isolateInvalidateEnd": "isolateInvalidateStart"
        }[data.action]
        StatusArr.expect_prev_status(data, prevData, expectedAction)
        node.statusRemove()
        break;

      case "dependsOn":
        var edge = new Edge(data);
        var edgeKey = edge.key;

        // store unique edges to always display a transparent dependency
        if (!_.has(this.edgesUnique, edge.ghostKey)) {
          this.edgesUnique[edge.ghostKey] = new GhostEdge(data);
        }

        if (!_.has(this.edges, edgeKey)) {
          this.edges[edgeKey] = edge;
        } else {
          edge = this.edges[edgeKey];
        }

        if (this.nodes[edge.reactId].statusLast().action == "isolateEnter") {
          edge.status = "isolate";
        } else {
          edge.status = "normal"
        }
        break;

      case "dependsOnRemove":
        var edge = new Edge(data);
        // remove the edge
        delete this.edges[edge.key];
        break;

      case "queueEmpty":
      case "asyncStart":
      case "asyncStop":
        this[data.action] = data.step
        break;

      default:
        console.error("data.action: ", data.action, data)
        throw data;
    }
  }
}


// initialize all log entries to have a step value
(function(){
  for (var i = 0; i < window.log.length; i++) {
    window.log[i].step = i;
  }
})()

class GraphAtStep {
  constructor(log) {
    this.originalLog = log;

    // hoverInfo[key] = `HoverStatus`
    this.hoverDefault = "focused"
    this.hoverInfo = {} // use `hoverKey`

    this.withLog(log);

    this.finalGraph = this.atStep(log.length);
    this.finalCyto = this.finalGraph.cytoGraph;

  }

  get hasFiltered() {
    return this.originalLog.length != this.log.length;
  }

  withLog(log) {
    this.log = log;
    this.steps = [];
    this.asyncStarts = [];
    this.asyncStops = [];
    this.queueEmpties = [];
    this.enterExitEmpties = [];
    this.minStep = log[0].step;
    this.maxStep = log[log.length - 1].step;

    var data, i;
    var enterExitQueue = [];
    for (i = 0; i < this.log.length; i++) {
      data = this.log[i];
      switch (data.action) {
        case "enter": enterExitQueue.push(i); break;
        case "exit":
          enterExitQueue.pop();
          if (enterExitQueue.length == 0) {
            this.enterExitEmpties.push(data.step + 1);
          }
          break;
        case "asyncStart": this.asyncStarts.push(data.step); break;
        case "asyncStop": this.asyncStops.push(data.step); break;
        case "queueEmpty": this.queueEmpties.push(data.step); break;
      }

      switch(data.action) {
        case "invalidateStart":
          if (this.log[i].ctxId == "other") {
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
    var nextStepPos = Math.min(this.steps.length - 1, _.sortedIndex(this.steps, k) + 1);
    return this.steps[nextStepPos]
  }
  prevStep(k) {
    var prevStepPos = Math.max(_.sortedIndex(this.steps, k) - 1, 1);
    return this.steps[prevStepPos]
  }

  atStep(k) {
    var kVal = Math.max(1, Math.min(k, this.log.length))
    var i, graph;
    // if (kVal >= this.cacheStep) {
    //   iStart = Math.floor((kVal - 1) / this.cacheStep) * this.cacheStep;
    //   graph = _.cloneDeep(this.graphCache[iStart])
    // }
    graph = new Graph(this.log);
    for (i = 0; i < this.log.length && this.log[i].step <= k; i++) {
      graph.addEntry(this.log[i]);
    }

    // set hover status of items
    var hoverInfo = this.hoverInfo;
    var hoverDefault = this.hoverDefault;
    var defaultHoverStatus = function(key) {
      var ret = new HoverStatus()
      ret.state = hoverDefault
      hoverInfo[key] = ret
      return ret;
    }
    // for nodes, edges, and ghost edges...
    //   set the hover status or define a new one and return it
    _.mapValues(
      _.assign({}, graph.nodes, graph.edges, graph.edgesUnique),
      function(data) {
        // set the hover status or define a new one and return it
        data.hoverStatus = hoverInfo[data.hoverKey] || defaultHoverStatus(data.hoverKey)
      }
    )
    return graph;
  }

  // if some sticky items, set those to focused and everything else to not focused
  // else set all to focused
  resetHoverInfo() {
    var anySticky = _.some(this.hoverInfo, ["sticky", true])
    if (anySticky) {
      // some sticky values... bring them to focus
      _.mapValues(this.hoverInfo, function(hoverStatus, key){
        if (hoverStatus.isSticky) {
          hoverStatus.toFocused();
        } else {
          hoverStatus.toNotFocused();
        }
      })
      this.hoverDefault = "notFocused";
    } else {
      // no sticky values, bring everything to normal
      _.mapValues(this.hoverInfo, function(hoverStatus, key){
        hoverStatus.toFocused();
      })
      this.hoverDefault = "focused";
    }
    return true;
  }
  // set the value outright
  updateHoverInfo(focusedDatas, notFocusedDatas) {
    var hoverInfo = this.hoverInfo;
    focusedDatas.map(function(data) {
      hoverInfo[data.hoverKey].toFocused()
    })
    notFocusedDatas.map(function(data) {
      hoverInfo[data.hoverKey].toNotFocused()
    })
  }

  resetStickyInfo() {
    var anySticky = _.some(this.hoverInfo, ["sticky", true])
    if (anySticky) {
      _.mapValues(this.hoverInfo, function(hoverStatus, key) {
        hoverStatus.toNotSticky()
        hoverStatus.toFocused()
      })
    }
    this.hoverDefault = "focused";
    return anySticky;
  }
  updateStickyInfo(stickyDatas, notStickyDatas) {
    var hoverInfo = this.hoverInfo;
    stickyDatas.map(function(data) {
      hoverInfo[data.hoverKey].toSticky()
    })
    notStickyDatas.map(function(data) {
      hoverInfo[data.hoverKey].toNotSticky()
    })
  }

  filterLogOnDatas(datas) {
    var keyMap = {};
    var nodeMap = {};
    datas.map(function(data) {
      keyMap[data.key] = data
      if (data instanceof Node) {
        nodeMap[data.reactId] = data
      }
    });
    var newLog = _.filter(this.originalLog, function(logEntry) {
      switch(logEntry.action) {
        case "dependsOn":
        case "dependsOnRemove":
          // check for both to and from
          return _.has(nodeMap, logEntry.reactId) && _.has(nodeMap, logEntry.depOnReactId);
          break;
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
        case "queueEmpty":
          // check for reactId
          return _.has(nodeMap, logEntry.reactId);
          break;
        case "asyncStart":
        case "asyncStop":
          // always add
          return _.has(nodeMap, logEntry.reactId);
        default:
          console.error("logEntry.action: ", logEntry.action, data)
          throw data;
      }
    })
    console.log("new Log: ", newLog);
    return newLog;
  }

  displayAtStep(k, cy) {
    var graph = this.atStep(k);

    cy.startBatch();

    var i;
    var cytoDur = 400;
    var cyNodes = cy.nodes();
    var graphCyto = graph.cytoGraph;
    var graphNodes = graphCyto.nodes();
    var nodesLRB = cyNodes.diff(graphNodes);
    // .removeStyle()

    var onLayoutReady = [];

    // enter
    nodesLRB.right.map(function(graphNode) {
      var graphNodeData = graphNode.data()
      cy
        .add(graphNode)
        .classes(graphNodeData.cytoClasses)
        .style(graphNodeData.cytoStyle)
        // .animate({
        //   // style: ,
        //   duration: cytoDur
        // });
      window.barret = cy.$id(graphNode.id());
    });
    // update
    nodesLRB.both.map(function(cytoNode) {
      var cyNode = cy.$id(cytoNode.id())

      var graphNode = graphNodes.$id(cytoNode.id());
      var graphNodeData = graphNode.data();
      var graphClasses = graphNodeData.cytoClasses;

      cyNode
        // update to latest data
        .data(graphNodeData)
        .classes(graphClasses)
        .removeStyle()
        .style(graphNodeData.cytoStyle)
        // .animate({
        //   // style: graphNodeData.cytoStyle,
        //   duration: cytoDur
        // });

      // pulse value change
      if (graphNodeData.valueChangedStatus.isActiveAtStep(k)) {
        onLayoutReady.push(function() {
          cyNode
            .flashClass("nodeStartBig", 125)
        })
      }
      // pulse value enter or invalidate change
      if (
        graphNodeData.invalidateStatus.isActiveAtStep(k) ||
        graphNodeData.enterStatus.isActiveAtStep(k)
      ) {
        onLayoutReady.push(function() {
          switch(graphNodeData.type) {
            case "observable": cyNode.flashClass("nodeMiddleBig", 125); break;
            case "observer": cyNode.flashClass("nodeEndBig", 125); break;
          }
        })
      }
    });
    // exit
    nodesLRB.left.map(function(cytoNode) {
      cy
        .remove(cytoNode)
        // .animate({duration: cytoDur});
    });

    var cyEdges = cy.edges()
    var graphEdges = graphCyto.edges();
    var edgesLRB = cyEdges.diff(graphEdges);
    // enter
    edgesLRB.right.map(function(graphEdge) {
      var graphEdgeData = graphEdge.data()
      cy
        .add(graphEdge)
        .classes(graphEdgeData.cytoClasses)
        .removeStyle()
        .style(graphEdgeData.cytoStyle)
        // .animate({
        //   style: graphEdgeData.cytoStyle,
        //   duration: cytoDur
        // });
    });
    // update
    edgesLRB.both.map(function(cytoEdge) {
      var graphEdgeData = graphEdges.$id(cytoEdge.id()).data()
      cy
        .$id(cytoEdge.id())
        // .classes()
        .classes(graphEdgeData.cytoClasses)
        .data(graphEdgeData)
        .removeStyle()
        .style(graphEdgeData.cytoStyle)
        // .animate({
        //   style: graphEdgeData.cytoStyle,
        //   duration: cytoDur
        // });
    });
    // exit
    edgesLRB.left.map(function(cytoEdge) {
      var graphEdge = cytoEdge.data()
      // remove the original edge
      cy
        .remove(cytoEdge)
        .animate({duration: cytoDur});
    })

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
      edgesLRB.right.length == edgesLRB.left.length &&
      nodesLRB.right.length == 0 &&
      nodesLRB.left.length == 0
    ) {
      // do not re-render layout... just call onLayoutReady
      onLayoutReady.map(function(fn) {
        fn();
      })
    } else {
      // calculate a new layout
      // time expensive!!!
      cy
        .layout(_.assign(
          {
            // provide elements in sorted order to make determanistic layouts
            eles: sortedElements,
            // run on layout ready
            ready: function() {
              onLayoutReady.map(function(fn) {
                fn();
              })
            }
          },
          layoutOptions
          // ,
          // TODO-barret Make animation a setting... it's expensive!
          // {animate: true}
        ))
        .run();
    }

  }
}

var layoutOptions = {
  name: "dagre",
  rankDir: "LR", // 'TB' for top to bottom flow, 'LR' for left to right,
  rankSep: 150, // the separation between node columns
  nodeSep: 10, // the separation within a node column
  edgeSep: 50, // the separation between adjacent edges in the same rank
  ranker: "longest-path", // Type of algorithm to assign a rank to each node in the input graph. Possible values: "network-simplex", "tight-tree" or "longest-path"
  nodeDimensionsIncludeLabels: true, // whether labels should be included in determining the space used by a node
  animate: true, // whether to transition the node positions
  animateFilter: function( node, i ){ return true; }, // whether to animate specific nodes when animation is on; non-animated nodes immediately go to their final positions
  animationDuration: 1000, // duration of animation in ms if enabled
  animationEasing: "ease-in-out-quad", // easing of animation if enabled

};

var nodeShapes = {
  start: "-1 1 0.33333333333 1 1 0 0.33333333333 -1 -1 -1",
  middle: "-1 1 0.5 1 1 0 0.5 -1 -1 -1 -0.5 0",
  end: "-1 1 1 1 1 -1 -1 -1 -0.33333333333 0"
}
var pulseScale = 1 + 1/16
var graphStyles = {
  node: {
    default: {
      "label": "data(cytoLabel)",
      "text-opacity": 0.5,
      "text-valign": "bottom",
      "text-margin-x": "-5",
      "text-halign": "right",
      "border-color": colors.regular.black,
      "border-style": "solid",
      "border-width": 1,
      "background-color": colors.regular.green1,
      "text-wrap": "ellipsis",
      "text-max-width": "200px"
    },
    start: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.start,
      width: 50 * 0.75,
      height: 30
    },
    startBig: {
      "border-width": 2,
      width: 50 * 0.75 * pulseScale,
      height: 30 * pulseScale
    },
    middle: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.middle,
      width: 50,
      height: 30
    },
    middleBig: {
      "border-width": 2,
      width: 50 * pulseScale,
      height: 30 * pulseScale
    },
    end: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.end,
      width: 50 * 0.75,
      height: 30
    },
    endBig: {
      "border-width": 2,
      width: 50 * 0.75 * pulseScale,
      height: 30 * pulseScale
    },
    enter: {
      // "border-width": 2,
      "background-color": colors.regular.green2
    },
    enterActive: {
      "background-color": colors.regular.green3
    },
    invalidate: {
      // "border-width": 2,
      "background-color": colors.regular.grey2
    },
    invalidateActive: {
      "background-color": colors.regular.grey3
    },
    invalidateDone: {
      "background-color": colors.regular.grey1
    },
    isolate: {
      "border-style": "dashed",
      // "border-width": 3,
      // "border-opacity"
    },
    isolateInvalidate: {
      "border-style": "dashed",
      "border-color": "darkgrey",
      "border-width": 3,
      // "border-opacity"
    },
    valueChanged: {
      "background-color": colors.regular.red // blood red
      // "border-style": "dashed",
      // "border-color": "darkgrey",
      // "border-width": 3,
      // "border-opacity"
    }
  },
  edge: {
    default: {
      "curve-style": "bezier",
      "width": 4,
      "target-arrow-shape": "triangle",
      "mid-target-arrow-shape": "triangle",
      "line-color": colors.edges.running, //"#9dbaea",
      "mid-target-arrow-color": colors.edges.running,
      "target-arrow-color": colors.edges.running
    },
    isolate: {
      "width": 4,
      "line-color": colors.edges.isolate,
      "mid-target-arrow-color": colors.edges.isolate,
      "target-arrow-color": colors.edges.isolate,
      "line-style": "dashed"
    }
  },
  ghostEdge: {
    default: {
      "width": 1,
      "mid-target-arrow-shape": "triangle",
      "mid-target-arrow-color": "#d0cfbc",
      "arrow-scale": 0.25,
      "curve-style": "haystack",
      "line-color": "#d0cfbc",
      "line-style": "dotted"
    }
  },
  focus: {
    hoverNotFocused: {
      "background-blacken": -0.75,
      "border-color": colors.regular.grey1,
      "line-color": colors.regular.grey1,
      "mid-target-arrow-color": colors.regular.grey1,
      "target-arrow-color": colors.regular.grey1
    },
    hoverNotFocusedButSticky: {
      "background-blacken": -0.35,
      "border-color": colors.regular.grey2,
      "line-color": colors.regular.grey2,
      "mid-target-arrow-color": colors.regular.grey2,
      "target-arrow-color": colors.regular.grey2
    },
    stickyNotFocused: {
      "background-blacken": -0.75,
      "border-color": colors.regular.grey1,
      "line-color": colors.regular.grey1,
      "mid-target-arrow-color": colors.regular.grey1,
      "target-arrow-color": colors.regular.grey1
    }
  }
}

var styleHelper = function(selector, style) {
  return {
    selector: selector,
    style: style
  }
}

$(function() {

  window.cyto = cytoscape({
    container: $("#cyto"),
    boxSelectionEnabled: false,
    autounselectify: true,
    layout: layoutOptions,
    style: [
      // order of the style definitions are how styles are applied
      styleHelper("node", graphStyles.node.default),
      styleHelper("edge", graphStyles.edge.default),
      styleHelper(".edgeGhost", graphStyles.ghostEdge.default),
      styleHelper(".edgeIsolate", graphStyles.edge.isolate),
      styleHelper(".nodeStart", graphStyles.node.start),
      styleHelper(".nodeMiddle", graphStyles.node.middle),
      styleHelper(".nodeEnd", graphStyles.node.end),
      styleHelper(".nodeStartBig", graphStyles.node.startBig),
      styleHelper(".nodeMiddleBig", graphStyles.node.middleBig),
      styleHelper(".nodeEndBig", graphStyles.node.endBig),
      styleHelper(".nodeEnter", graphStyles.node.enter),
      styleHelper(".nodeEnterActive", graphStyles.node.enterActive),
      styleHelper(".nodeInvalidate", graphStyles.node.invalidate),
      styleHelper(".nodeInvalidateActive", graphStyles.node.invalidateActive),
      styleHelper(".nodeInvalidateDone", graphStyles.node.invalidateDone),
      styleHelper(".nodeIsolate", graphStyles.node.isolate),
      styleHelper(".nodeIsolateInvalidate", graphStyles.node.isolateInvalidate),
      styleHelper(".nodeValueChanged", graphStyles.node.valueChanged),
      styleHelper(".hoverNotFocused", graphStyles.focus.hoverNotFocused),
      styleHelper(".hoverNotFocusedButSticky", graphStyles.focus.hoverNotFocusedButSticky),
      styleHelper(".stickyNotFocused", graphStyles.focus.stickyNotFocused)
    ]
  });

  cytoFamilySuccPred = function(ele, addExtraLayer = true) {
    var familyEles = cyto.collection();
    if (ele.isEdge()) {
      var edge = ele;
      if (false) {
        // TODO-barret attempt at getting all nodes and edges from final graph
        // abandon, as the edge should be stored and constantly retrieved somewhere else
        console.log(`#${ele.source().id()} -> #${ele.target().id()}`)
        ele = getGraph.finalCyto.$(`#${ele.source().id()} -> #${ele.target().id()}`)
      }
      familyEles = familyEles
        .add(edge)
        .add(ele.target())
        .add(ele.target().successors())
        .add(ele.source())
        .add(ele.source().predecessors());
    } else {
      // is node
      var node = ele;
      if (false) {
        ele = getGraph.finalCyto.$id(ele.id())
      }
      familyEles = familyEles
        .add(node)
        .add(ele.successors())
        .add(ele.predecessors());
    }
    if (addExtraLayer) {
      var familyNodes = familyEles.nodes();
      familyEles = familyEles
        .add(familyNodes.incomers())
        .add(familyNodes.outgoers());
    }
    return familyEles;
  }
  cyto.on("mouseover", function(evt) {
    var target = evt.target;
    if (target == cyto) return;

    // highlight all outgoer's outgoers and all incomer's incomers and self
    var familyEles = cytoFamilySuccPred(target, false);

    var elesData = function(eles) {
      return eles.map(function(ele) {
        return ele.data();
      })
    }

    var hasCalled = false;
    var debounced = _.debounce(function() {
      hasCalled = true;
      updateGraph.hoverInfo(
        elesData(familyEles),
        elesData(cyto.$().not(familyEles))
      )
    }, 200)
    debounced()
    // if a mouseout occurs before the function is executed, cancel it
    // works as mouseout is always called before mouseover
    familyEles.once("mouseout", function(evtOut) {
      debounced.cancel()
      if (hasCalled) {
        // only remove hover if hover added
        updateGraph.resetHoverInfo()
      }
    })
  });

  var cytoClickedBefore, cytoClickedTimeout;
  cyto.on("click", function(evt) {
    // remove focus on search
    $("#search").blur()

    var elesData = function(eles) {
      return eles.map(function(ele) {
        return ele.data();
      })
    }

    var target = evt.target

    // check for double click
    // https://stackoverflow.com/a/44160927
    if (cytoClickedTimeout && cytoClickedBefore) {
      clearTimeout(cytoClickedTimeout)
    }
    if (cytoClickedBefore == target) {
      // is actually a double click... return!
      target.trigger("dblclick", evt)
      cytoClickedBefore = null;
      return;
    } else {
      cytoClickedTimeout = setTimeout(function(){ cytoClickedBefore = null; }, 400);
      cytoClickedBefore = target;
      // continue like regular click
      console.log("click!!", evt)
    }


    if (target == cyto) {
      // remove sticky focus class
      updateGraph.resetStickyInfo()
      return;
    }

    var familyEles = cytoFamilySuccPred(target, false);
    updateGraph.stickyInfo(
      elesData(familyEles),
      elesData(cyto.$().not(familyEles))
    )
    return;
  })

  cyto.on("dblclick", function(evt, originalEvt) {
    var elesData = function(eles) {
      return eles.map(function(ele) {
        return ele.data();
      })
    }

    console.log("dbl click!!");
    // console.log("dbl click!!", evt, originalEvt);
    var target = evt.target;

    if (target == cyto) {
      // go back to full graph
      updateGraph.resetWithLog()
      return;
    }

    var holdingShiftKey = originalEvt.originalEvent.shiftKey;
    if (holdingShiftKey) {
      console.log("extra layers!")
      var familyEles = cytoFamilySuccPred(target, true);
      var familyDatas = elesData(familyEles)

      var directFamilyEles = cytoFamilySuccPred(target, false);
      getGraph.updateHoverInfo(
        elesData(directFamilyEles),
        elesData(cyto.$().not(directFamilyEles))
      )

      updateGraph.withDatas(familyDatas)

    } else {
      var familyEles = cytoFamilySuccPred(target, false);
      var familyDatas = elesData(familyEles)

      updateGraph.withDatas(familyDatas)

    }

  })




  window.getGraph = new GraphAtStep(window.log);
  window.graph = getGraph.atStep(getGraph.maxStep);
  console.log(graph);

  getGraph.enterExitEmpties.map(function(i) {
    $("#timeline-bg").append(`<div class=\"timeline-enterexit\" style=\"left: ${100 * i / this.log.length}%;\"></div>`)
  })
  getGraph.queueEmpties.map(function(i) {
    $("#timeline-bg").append(`<div class=\"timeline-cycle\" style=\"left: ${100 * i / this.log.length}%;\"></div>`)
  })

  function updateProgressBar() {
    $("#timeline-fill").width((curTick / window.log.length * 100) + "%");
  }
  function updateLogItem() {
    $("#instructions").text(JSON.stringify(window.log[curTick], null, "  "));
  }
  $('#timeline').on('mousedown mousemove', function(e) {
    // Make sure left mouse button is down.
    // Firefox is stupid; e.which is always 1 on mousemove events,
    // even when button is not down!! So read e.originalEvent.buttons.
    if (typeof(e.originalEvent.buttons) !== 'undefined') {
      if (e.originalEvent.buttons !== 1)
        return;
    } else if (e.which !== 1) {
      return;
    }

    var timeline = e.currentTarget;
    var pos = e.pageX || e.originalEvent.pageX; // pageX in pixels
    var width = timeline.offsetWidth; // width in pixels
    var targetStep = Math.max(Math.round((pos/width) * window.log.length), 1);
    if (targetStep != curTick) {
      window.curTick = targetStep;
      updateGraph()
    }
    return;
  });




  updateGraph = function() {
    getGraph.displayAtStep(window.curTick, window.cyto);
    updateProgressBar()
    updateLogItem()
  }

  updateGraph.withRegexString = function(regexStr) {

    var searchRegex;
    if (regexStr instanceof RegExp) {
      searchRegex = regexStr;
    } else {
      // is string, not regex

      // escape the string
      // https://stackoverflow.com/a/17606289
      var escapeRegExp = function (str) {
        return str.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"); // $& means the whole matched string
      }
      searchRegex = new RegExp(escapeRegExp(regexStr))
    }

    // if less than three chars...
    if (searchRegex.source.length < 2) {
      if (searchRegex.source.length == 0) {
        // TODO-barret show warning of resetting
        console.log("resetting log!")
        updateGraph.resetWithLog()
      } else {
        // do nothing
      }
      return;
    }

    getGraph.withLog(getGraph.originalLog);
    var cy = getGraph.atStep(window.curTick).cytoGraph;

    var matchedNodes = cy.nodes().filter(function(node) {
      return searchRegex.test(node.data().label)
    })

    if (matchedNodes.length == 0) {
      // TODO-barret show warning of no matches
      console.log("no matches!")
      return;
    }

    var familyEles = cy.collection()
      .add(matchedNodes)
      .add(matchedNodes.successors())
      .add(matchedNodes.predecessors());


    var elesData = function(eles) {
      return eles.map(function(ele) {
        return ele.data();
      })
    }

    updateGraph.withDatas(elesData(familyEles));
    return;
  }

  updateGraph.resetWithLog = function() {
    updateGraph.withLog(getGraph.originalLog)
  }
  updateGraph.withLog = function(log) {
    getGraph.withLog(log);
    getGraph.resetStickyInfo()

    var nextTick = getGraph.nextStep(curTick);

    if (getGraph.prevStep(nextTick) != curTick) {
      // updateGraph.prevStep();
      updateGraph()
    } else {
      updateGraph()
    }
  }
  updateGraph.withDatas = function(datas) {
    var newLog = getGraph.filterLogOnDatas(datas);
    updateGraph.withLog(newLog);
  }

  updateGraph.hoverInfo = function(focusedDatas, notFocusedDatas) {
    getGraph.updateHoverInfo(focusedDatas, notFocusedDatas)
    updateGraph()
  }
  updateGraph.resetHoverInfo = function() {
    getGraph.resetHoverInfo()
    updateGraph()
  }
  updateGraph.stickyInfo = function(stickyDatas, notStickyDatas) {
    getGraph.updateStickyInfo(stickyDatas, notStickyDatas)
    updateGraph()
  }
  updateGraph.resetStickyInfo = function() {
    getGraph.resetStickyInfo()
    updateGraph()
  }

  updateGraph.nextTick = function() {
    window.curTick += 1
    updateGraph()
  }
  updateGraph.prevTick = function() {
    window.curTick -= 1
    updateGraph()
  }
  updateGraph.nextStep = function() {
    // Move one step ahead (skipping unneccessary steps)
    window.curTick = getGraph.nextStep(curTick)
    updateGraph()
  }
  updateGraph.prevStep = function() {
    // Move one step back
    window.curTick = getGraph.prevStep(curTick)
    updateGraph()
  }

  updateGraph.nextEnterExitEmpty = function() {
    // move to queue empty
    for (var i = 0; i < getGraph.enterExitEmpties.length; i++) {
      val = getGraph.enterExitEmpties[i] - 1;
      if (curTick < val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  }
  updateGraph.prevEnterExitEmpty = function() {
    // move to queue empty
    for (var i = getGraph.enterExitEmpties.length - 1; i >= 0; i--) {
      val = getGraph.enterExitEmpties[i];
      if (curTick > val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  }
  updateGraph.lastEnterExitEmpty = function() {
    window.curTick = getGraph.enterExitEmpties[getGraph.enterExitEmpties.length - 1] || 0;
    updateGraph()
  }
  updateGraph.firstEnterExitEmpty = function() {
    window.curTick = getGraph.enterExitEmpties[0] || 0;
    updateGraph()
  }



  updateGraph.nextQueueEmpty = function() {
    // move to queue empty
    for (var i = 0; i < getGraph.enterExitEmpties.length; i++) {
      val = getGraph.queueEmpties[i];
      if (curTick < val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  }
  updateGraph.prevQueueEmpty = function() {
    // move to queue empty
    for (var i = getGraph.queueEmpties.length - 1; i >= 0; i--) {
      val = getGraph.queueEmpties[i];
      if (curTick > val) {
        window.curTick = val;
        updateGraph();
        return true;
      }
    }
    return false;
  }
  updateGraph.lastQueueEmpty = function() {
    window.curTick = getGraph.queueEmpties[getGraph.queueEmpties.length - 1] || 0;
    updateGraph()
  }
  updateGraph.firstQueueEmpty = function() {
    window.curTick = getGraph.queueEmpties[0] || 0;
    updateGraph()
  }

  window.curTick = 1
  updateGraph.nextEnterExitEmpty()

  $("#startStepButton").click(updateGraph.firstEnterExitEmpty)
  $("#endStepButton").click(updateGraph.lastEnterExitEmpty)
  $("#prevCycleButton").click(updateGraph.prevEnterExitEmpty)
  $("#nextCycleButton").click(updateGraph.nextEnterExitEmpty)
  $("#prevStepButton").click(updateGraph.prevStep)
  $("#nextStepButton").click(updateGraph.nextStep)

  $("#search").on("input", function(e) {
    updateGraph.withRegexString(e.target.value)
  })
  $(document.body).on("keydown", function(e) {
    console.log("e: ", e)
    if (e.target.id && e.target.id == "search") {
      // is in search text box
      if (e.which == 27) {
        $(e.target).blur()
      } else {
        // if (e.which == 13) { // enter
        // }

      }
      return;
    }
    if (e.which === 39 || e.which === 32) { // space, right
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
      if (curTick < getGraph.maxStep) {
        // right
        updateGraph.nextStep();
        return;
      }
    }
    if (e.which === 37) { // left
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
        updateGraph.prevTick()
        return;
      }
      if (curTick > 1) {
        // left
        updateGraph.prevStep()
        return;
      }
    }
    if (e.which === 35) { // end
      // Seek to end
      updateGraph.lastEnterExitEmpty()
      return;
    }
    if (e.which === 36) { // home
      // Seek to beginning
      updateGraph.firstEnterExitEmpty();
      return;
    }

    if (e.which == 27) { // esc
      if (getGraph.hasFiltered) {
        // must be in filter... so exit filter
        $("#search").val("");
        getGraph.resetHoverInfo()
        updateGraph.resetWithLog()
      }
      return;
    }
    if (e.which == 38) { // arrow up
      if (getGraph.hasFiltered) {

        // must be in filter... so exit filter
        getGraph.resetHoverInfo()
        updateGraph.resetWithLog()
      }
      return;
    }
    if (e.which == 40) { // arrow down
      if (getGraph.hasFiltered) {
        // must be in filter... so exit filter
        getGraph.resetHoverInfo()
        updateGraph.resetWithLog()
      }
      return;
    }
    if (e.which == 83) { // s
      _.defer(function() {
        $("#search").focus()
      })
      e.stopPropagation();
      return;
    }

  });


});
