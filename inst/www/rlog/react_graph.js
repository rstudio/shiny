// sort edges by key to keep graph stable
// add state colors for nodes
// add state colors for edges


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
    var statusObj;
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
  }
  get id() {
    return this.reactId.replace(/\$/g, "_")
  }
  get key() {
    return this.reactId
  }
  statusAdd(obj) {
    this.statusArr.add(obj);
    return this.statusArr;
  }
  statusPop() {
    return this.statusArr.remove();
  }
  statusLast() {
    return this.statusArr.last();
  }
  get inEnter() {
    return this.statusArr.containsStatus("enter");
  }
  get inIsolate() {
    return this.statusArr.containsStatus("isolateEnter");
  }
  get inInvalidate() {
    return this.statusArr.containsStatus("invalidateStart");
  }
  get inIsolateInvalidate() {
    return this.statusArr.containsStatus("isolateInvalidateStart");
  }
  get cytoStyle() {
    return {}
  }
  get cytoData() {
    var retData = this;
    var classes = ["nodeValid"];
    if (this.inEnter) classes.push("nodeEnter");
    if (this.inInvalidate) classes.push("nodeInvalidate");
    if (this.inIsolate) classes.push("nodeIsolate");
    if (this.inIsolateInvalidate) classes.push("nodeIsolateInvalidate");
    return {
      group: "nodes",
      data: retData,
      classes: classes.join(" ")
      // style: this.cytoStyle
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
  get depKey() {
    return `${ this.reactId } depends on ${ this.depOnReactId }`;
  }
  get cytoData() {
    var retData = this;
    var classes = [];

    return {
      group: "edges",
      data: retData,
      // ,
      // style: this.cytoStyle
      classes: classes.join(" ")
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
  get cytoStyle() {
    return {};
    return graphStyles.ghostEdge.default
  }
  get cytoData() {
    var retData = this;
    return {
      group: "edges",
      data: retData,
      classes: [
        "ghostEdge"
      ].join(' ')
      // style: this.cytoStyle
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
  }

  // get nodeIds() {
  //   return _.values(this.nodes).map(function(node) {
  //     return "#" + node.id
  //   }).join(", ")
  // }
  get cytoGraph() {
    var cyto = cytoscape();
    var nodes = _.values(this.nodes).map(function(node) {
      return node.cytoData;
    });
    cyto.add(nodes);
    var ghostEdgeMap = _.assign({}, this.edgesUnique);
    var edges = _.values(this.edges).map(function(edge) {
      // remove matching unique/ghost edges
      if (_.has(ghostEdgeMap, edge.depKey)) {
        delete ghostEdgeMap[edge.depKey];
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
        this.nodes[data.reactId].value = data.value;
        break;

      case "enter":
      case "isolateEnter":
      case "invalidateStart":
      case "isolateInvalidateStart":
        this.nodes[data.reactId].statusAdd(data)
        break;
      case "exit":
      case "isolateExit":
      case "invalidateEnd":
      case "isolateInvalidateEnd":
        var prevData = this.nodes[data.reactId].statusLast()
        var expectedAction = {
          "exit": "enter",
          "isolateExit": "isolateEnter",
          "invalidateEnd": "invalidateStart",
          "isolateInvalidateEnd": "isolateInvalidateStart"
        }[data.action]
        StatusArr.expect_prev_status(data, prevData, expectedAction)
        this.nodes[data.reactId].statusPop()
        break;

      case "dependsOn":
        var edge = new Edge(data);
        var edgeKey = edge.key;

        // store unique edges to always display a transparent dependency
        if (!_.has(this.edgesUnique, edge.depKey)) {
          this.edgesUnique[edge.depKey] = new GhostEdge(data);
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

class GraphAtStep {
  constructor(log) {
    this.log = log;
    this.asyncStarts = [];
    this.asyncStops = [];
    this.queueEmpties = [];
    this.minStep = 0;
    this.maxStep = log.length;
    var data;
    for (var i = 0; i < log.length; i++) {
      data = log[i];
      data.step = i;
      switch (data.action) {
        case "asyncStart": this.asyncStarts.push(i); break;
        case "asyncStop": this.asyncStops.push(i); break;
        case "queueEmpty": this.queueEmpties.push(i); break;
      }
    }
  }

  atStep(k) {
    var graph = new Graph(log);
    var i;
    var kVal = Math.max(1, Math.min(k, this.log.length))
    for (i = 0; i < kVal; i++) {
      graph.addEntry(log[i]);
    }
    return graph;
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

    // enter
    nodesLRB.right.map(function(graphNode) {
      cy
        .add(graphNode)
        .animate({
          style: graphNode.data().cytoStyle,
          duration: cytoDur
        });
    });
    // update
    nodesLRB.both.map(function(cytoNode) {
      var graphNodeData = graphNodes.$id(cytoNode.id()).data();
      cy
        .$id(cytoNode.id())
        // update to latest data
        .data(graphNodeData)
        .animate({
          style: graphNodeData.cytoStyle,
          duration: cytoDur
        });
    });
    // exit
    nodesLRB.left.map(function(cytoNode) {
      cy
        .remove(cytoNode)
        .animate({duration: cytoDur});
    });

    var cyEdges = cy.edges()
    var graphEdges = graphCyto.edges();
    var edgesLRB = cyEdges.diff(graphEdges);
    // enter
    edgesLRB.right.map(function(graphEdge) {
      cy
        .add(graphEdge)
        .animate({
          style: graphEdge.data().cytoStyle,
          duration: cytoDur
        });
    });
    // update
    edgesLRB.both.map(function(cytoEdge) {
      var graphEdgeData = graphEdges.$id(cytoEdge.id()).data()
      cy
        .$id(cytoEdge.id())
        .data(graphEdgeData)
        .animate({
          style: graphEdgeData.cytoStyle,
          duration: cytoDur
        });
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
    cy
      .layout(_.assign({eles: sortedElements}, layoutOptions))
      .run();
  }
}

var layoutOptions = {
  name: "dagre",
  rankDir: "LR",
  nodeSep: 5,
  edgeSep: 50,
  ranker: "longest-path", // Type of algorithm to assign a rank to each node in the input graph. Possible values: "network-simplex", "tight-tree" or "longest-path"
  nodeDimensionsIncludeLabels: true, // whether labels should be included in determining the space used by a node
  animate: true, // whether to transition the node positions
  animateFilter: function( node, i ){ return true; }, // whether to animate specific nodes when animation is on; non-animated nodes immediately go to their final positions
  animationDuration: 500, // duration of animation in ms if enabled
  animationEasing: undefined, // easing of animation if enabled

};

var nodeShapes = {
  start: "-1 1 0.33333333333 1 1 0 0.33333333333 -1 -1 -1",
  middle: "-1 1 0.5 1 1 0 0.5 -1 -1 -1 -0.5 0",
  end: "-1 1 1 1 1 -1 -1 -1 -0.33333333333 0"
}
var graphStyles = {
  node: {
    default: {
      "label": "data(label)",
      "text-opacity": 0.5,
      "text-valign": "bottom",
      "text-margin-x": "-5",
      "text-halign": "right",
      "background-color": "#11479e"
    },
    start: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.start,
      width: 50 * 0.75,
      height: 30,
      "background-color": "yellow"
    },
    middle: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.middle,
      width: 50,
      height: 30,
      "background-color": "orange"
    },
    end: {
      "shape": "polygon",
      "shape-polygon-points": nodeShapes.end,
      width: 50 * 0.75,
      height: 30,
      "background-color": "red"
    }
  },
  edge: {
    default: {
      "curve-style": "bezier",
      "width": 4,
      "target-arrow-shape": "triangle",
      "line-color": "#9dbaea",
      "target-arrow-color": "#9dbaea"
    }
  },
  ghostEdge: {
    default: {
      "width": 25,
      "mid-target-arrow-shape": "triangle",
      "curve-style": "haystack",
      "line-color": "#d5ce24",
      "line-style": "dotted"
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
      styleHelper(".startNode", graphStyles.node.start),
      styleHelper(".middleNode", graphStyles.node.middle),
      styleHelper(".endNode", graphStyles.node.end),
      styleHelper(".ghostEdge", graphStyles.ghostEdge.default)
    ]
  });
  window.getGraph = new GraphAtStep(log);
  window.graph = getGraph.atStep(getGraph.maxStep);
  console.log(graph);

  window.curTick = 90;
  function updateTimeline() {
    $("#timeline-fill").width((curTick / log.length * 100) + "%");
  }
  updateGraph =  function() {
    getGraph.displayAtStep(curTick, cyto);
    updateTimeline()
  }
  updateGraph()
  // getGraph.displayAtStep(getGraph.minStep, cyto);
  // setTimeout(function() { getGraph.displayAtStep(100, cyto); console.log("1")}, 1000)
  // setTimeout(function() { getGraph.displayAtStep(200, cyto); console.log("2") }, 2000)
  // setTimeout(function() { getGraph.displayAtStep(300, cyto); console.log("3") }, 3000)
  // setTimeout(function() { getGraph.displayAtStep(400, cyto); console.log("4") }, 4000)
  // setTimeout(function() { getGraph.displayAtStep(getGraph.maxStep, cyto); console.log("end") }, 5000)


  $(document.body).on("keydown", function(e) {
    if (e.which === 39 || e.which === 32) { // space, right
      // Move one step ahead
      window.curTick += 1
      updateGraph()
    }
    if (e.which === 37) { // left
      // Move one step back
      window.curTick -= 1
      updateGraph()
    }
    // if (e.which === 35) { // end
    //   // Seek to end
    //   while (log.length) {
    //     doNext();
    //   }
    // }
    // if (e.which === 36) { // home
    //   // Seek to beginning
    //   undoAll();
    // }
  });


});
