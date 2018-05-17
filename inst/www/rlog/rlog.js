/* global log, __DATA__, __TIME__ */

try {
  log = __DATA__;
  time = String(__TIME__).toLowerCase() === "true";
} catch (e) {}

// center the svg
var centerSvg = function(svg, g, zoom) {
  var e = document.documentElement,
    body = document.getElementsByTagName("body")[0],
    x = window.innerWidth || e.clientWidth || body.clientWidth,
    y = window.innerHeight || e.clientHeight || body.clientHeight,
    gGraph = g.graph(),
    graphWidth = gGraph.width,
    graphHeight = gGraph.height,
    widthScale = x / graphWidth,
    heightScale = y / graphHeight;

  if (heightScale > widthScale) {
    // tall and skinny
    svg.call(
      zoom.transform,
      d3.zoomIdentity
        .translate(
          // 20 px from left
          20,
          // middle
          (y - graphHeight) / 2
        )
        // scale to 20 px from left and right
        .scale((x - 20 - 20) / graphWidth)
    );
  } else {
    // wide and short
    svg.call(
      zoom.transform,
      d3.zoomIdentity
        .translate(
          // middle
          (x - g.graph().width * heightScale) / 2,
          // 20 px from top
          20
        )
        // scale to 20 px from top and bottom
        .scale((y - 20 - 20) / g.graph().height)
    );
  }
};

// create an array of dot text... one for each "stopping" state.
// * first pass. calculate all dots before rendering.
//   * second pass. Calculate dots when moving forward, remembering the past

class Node {
  constructor(data) {
    if (typeof data.nodeId === "undefined")
      throw "data.nodeId not provided in new Node";
    if (typeof data.label === "undefined")
      throw "data.label not provided in new Node";
    if (typeof data.type === "undefined")
      throw "data.type not provided in new Node";
    if (typeof data.session === "undefined")
      throw "data.session not provided in new Node";
    if (typeof data.time === "undefined")
      throw "data.time not provided in new Node";
    this.nodeId = data.nodeId;
    this.label = data.label;
    this.type = data.type;
    this.session = data.session;
    this.time = data.time;
    this.inIsolate = data.inIsolate || false;
    this.state = data.state || "normal";
  }
  get dotTxt() {
    // var borderTxt = this.inIsolate ? ""
    var fillTxt = "";
    switch (this.status) {
      case "removed":
        fillTxt = "color=transparent";
        break;
      case "computationStart":
        if (this.inIsolate) {
          fillTxt = `style="filled,dashed" fillcolor=green penwidth=3`;
        } else {
          fillTxt = "style=filled fillcolor=green";
        }
        break;
      // case "invalidateStart":
      //   fillTxt = "style=filled fillcolor=lightgrey"
    }
    return `  "${this.nodeId}" [id="${this.nodeId}" label="${this.nodeId}-${
      this.label
    }" ${fillTxt}]\n`;
  }
  static nodeDotTxt(name, options) {
    // if (options.length)
    // var optionTxt =
    return `  ${name} []`;
  }
}
class NodeReactiveValues {
  constructor(data) {
    if (typeof data.nodeId === "undefined")
      throw "data.nodeId not provided in new Node";
    if (typeof data.label === "undefined")
      throw "data.label not provided in new Node";
    if (typeof data.type === "undefined")
      throw "data.type not provided in new Node";
    if (typeof data.session === "undefined")
      throw "data.session not provided in new Node";
    if (typeof data.time === "undefined")
      throw "data.time not provided in new Node";
    this.nodeId = data.nodeId;
    this.label = data.label;
    this.type = data.type;
    this.session = data.session;
    this.time = data.time;
    this.data = {};
    this.dataValues = "";
    this.dataNames = [];
    this.asList = "";
    this.state = "normal";
    this.dependsNames = false;
    this.dependsList = false;
    this.dependsOnKey = {};
  }
  nodeIdKey(key) {
    return NodeReactiveValues.idKey(this.nodeId, key);
  }
  get nodeIdNames() {
    return NodeReactiveValues.idNames(this.nodeId);
  }
  get nodeIdList() {
    return NodeReactiveValues.idList(this.nodeId);
  }
  get dotTxt() {
    var nameTxt = "",
      listTxt = "";
    if (this.dependsNames) {
      nameTxt = `"${this.nodeIdNames}" [label = "names()"];`;
    }
    if (this.dependsList) {
      listTxt = `"${this.nodeIdList}" [label = "as.list()"];`;
    }

    var nodeTxt = "";
    var key, keyId, color;
    for (key in this.data) {
      color = this.dependsOnKey[key] ? "black" : "grey";
      keyId = this.nodeIdKey(key);
      nodeTxt += `      "${keyId}" [id="${keyId}" label="${key}" color="${color}" fontcolor="${color}"];\n`;
    }

    return `
    subgraph cluster_${this.nodeId} {
      style = dotted;
      label = "${this.nodeId}-${this.label}";
      // labeljust = "right";
      // graph [color = "#4daf4a"]
${nodeTxt}
      ${nameTxt}
      ${listTxt}
    } // end cluster_${this.nodeId}
    `;
  }

  static idKey(nodeId, key) {
    return nodeId + "_key_" + key;
  }
  static idList(nodeId) {
    return nodeId + "_as.list";
  }
  static idNames(nodeId) {
    return nodeId + "_names";
  }
}
class Edge {
  constructor(data) {
    if (typeof data.nodeId === "undefined")
      throw "data.nodeId not provided to new Edge()";
    if (typeof data.depOnNodeId === "undefined")
      throw "data.depOnNodeId not provided to new Edge()";
    if (typeof data.session === "undefined")
      throw "data.session not provided to new Edge()";
    if (typeof data.time === "undefined")
      throw "data.time not provided to new Edge()";
    this.nodeId = data.nodeId;
    this.depOnNodeId = data.depOnNodeId;
    this.session = data.session;
    this.time = data.time;
    this.status = "normal";
  }
  get key() {
    return `${this.nodeId} - ${this.depOnNodeId}`;
  }
  dotTxtWithColor(colorTxt) {
    return `  "${this.nodeId}" -> "${this.depOnNodeId}" [${colorTxt}, id="${
      this.nodeId
    }_${this.depOnNodeId}"]\n`;
  }
  get dotTxt() {
    switch (this.status) {
      case "isolate":
        return this.dotTxtWithColor("style=dashed color=grey");
        break;
      case "removed":
        return this.dotTxtWithColor("color=transparent");
      default:
        return this.dotTxtWithColor("color=black");
    }
  }
}
class RLog {
  constructor() {
    this.logPos = 0;
    this.nodes = {};
    this.edges = {};
    this.sessions = {};
    this.isolateNodeId = {};
    this.isolateCtxId = {};
  }

  removeIsolateCtx(data) {
    rLog.isolateNodeId[data.nodeId] = false;
    rLog.nodes[data.nodeId].inIsolate = false;

    var edges = this.isolateCtxId[data.ctxId].edges;
    for (var i = 0; i < edges.length; i++) {
      var edge = edges[i];
      callbacks.depOnRemove(edge);
    }
  }

  addNode(node) {
    node.session = node.session || "Global";
    this.sessions[node.session] = true;
    this.nodes[node.nodeId] = node;
  }
  addEdge(edge) {
    var session = edge.session || "Global";
    this.edges[edge.key] = edge;
  }

  get outputNodesIdsDotGraph() {
    var ret = "";
    var found = false;
    for (var key in this.nodes) {
      var node = this.nodes[key];
      if (/^output\$/.test(node.label)) {
        found = true;
        ret += "      " + node.nodeId + ";\n";
      }
    }
    if (!found) return "";
    return `
    subgraph cluster_outputNodes {
      style = dotted;
      label="";
      ${ret}
    }
    `;
  }
  get perSessionNodeIdsDotGraph() {
    var sessionTxt = RLog.joinDotGraph(this.nodes);
    var nodeMap = {};
    var node;
    var key, i;
    for (key in this.sessions) {
      nodeMap[key] = [];
    }
    for (var key in this.nodes) {
      node = this.nodes[key];
      nodeMap[node.session].push(node);
    }
    var session_i = 0;
    for (key in nodeMap) {
      if (key == "Global") continue;
      var nodeTxt = "";
      for (i = 0; i < nodeMap[key].length; i++) {
        node = nodeMap[key][i];
        nodeTxt += `"${node.nodeId}";`;
      }
      sessionTxt += `
      // subgraph cluster_session${session_i} {
      //   style = dotted;
      //   label = "Session: ${key}"
        ${nodeTxt}
      // }
      `;
      session_i += 1;
    }
    return sessionTxt;
  }
  static joinDotGraph(x) {
    var ret = "";
    for (var key in x) {
      ret += x[key].dotTxt;
    }
    return ret;
  }

  get fullDotGraph() {
    return `
  digraph ReactLog {
    compound = true;
    /*overlap = false;*/
    splines = true;
    rankdir = RL;
    labeljust = "r";


    node [shape = "ellipse"];
    nodesep = 0;
    subgraph cluster_global {
      label = "Global";
      style = dotted;

      // nodes
      ${this.perSessionNodeIdsDotGraph}

      // group_output nodes
      ${this.outputNodesIdsDotGraph}

      // edges
      ${RLog.joinDotGraph(this.edges)}
    }
  }
`;
  }
}

var callbacks = {
  // {"action": "nodeDef", "nodeId": "1", "label": "reactiveValues7781", "type": "reactiveValues", "session": null, "time": 1522955046.55142},
  nodeDef: function(data) {
    rLog.nodes[data.nodeId].status = "normal";
    // switch(data.type) {
    //   case "reactiveValues":
    //     rLog.addNode(new NodeReactiveValues(data));
    //   break;
    //   default:
    //     rLog.addNode(new Node(data));
    // }
  },

  // {"action": "updateNodeLabel", "nodeId": "1", "label": "input", "session": null, "time": 1522955046.5537},
  updateNodeLabel: function(data) {
    rLog.nodes[data.nodeId].label = data.label;
  },

  // {"action": "valueChangeReactValueNames", "nodeId": "1", "value": " chr(0) ", "session": "12b170ffbdc9227f62e06e9884fb3c61", "time": 1522955046.56054},
  valueChangeReactValueNames: function(data) {
    rLog.nodes[data.nodeId].value = data.label;
  },

  valueChangeReactValueNames: function(data) {
    rLog.nodes[data.nodeId].dataNames = data.value;
  },
  valueChangeReactValueValues: function(data) {
    rLog.nodes[data.nodeId].dataValues = data.value;
  },
  valueChangeReactValueKey: function(data) {
    rLog.nodes[data.nodeId].data[data.key] = data.value;
  },

  valueChangeStart: function(data) {
    rLog.nodes[data.nodeId].status = "valueChangeStart";
  },
  valueChangeEnd: function(data) {
    rLog.nodes[data.nodeId].status = "valueChangeEnd";
  },

  invalidateStart: function(data) {
    if (data.type == "isolate") {
      // ignore?
    } else {
      rLog.nodes[data.nodeId].status = "invalidateStart";
    }
  },
  invalidateEnd: function(data) {
    if (data.type == "isolate") {
      rLog.removeIsolateCtx(data);
    } else {
      // rLog.nodes[data.nodeId].status = "invalidateEnd";
    }
  },

  enter: function(data) {
    if (data.type == "isolate") {
      rLog.isolateNodeId[data.nodeId] = data.ctxId;
      rLog.isolateCtxId[data.ctxId] = { edges: [] };
      rLog.nodes[data.nodeId].inIsolate = true;
    } else {
      rLog.nodes[data.nodeId].status = "computationStart";
    }
  },
  exit: function(data) {
    if (data.type == "isolate") {
      // nothing?
    } else {
      rLog.nodes[data.nodeId].status = "computationEnd";
    }
  },

  dep: function(data) {
    var edge = new Edge(data);
    if (rLog.isolateNodeId[data.nodeId]) {
      var ctxId = rLog.isolateNodeId[data.nodeId];
      rLog.isolateCtxId[ctxId].edges.push(edge);
      edge.status = "isolate";
    }
    var curEdge = rLog.edges[edge.key];
    curEdge.status = edge.status;
    // rLog.addEdge(edge);
  },
  depReactiveValueKey: function(data) {
    rLog.nodes[data.depOnNodeId].dependsOnKey[data.key] = true;
    data.depOnNodeId = NodeReactiveValues.idKey(data.depOnNodeId, data.key);
    var edge = new Edge(data);
    if (rLog.isolateNodeId[data.nodeId]) {
      var ctxId = rLog.isolateNodeId[data.nodeId];
      rLog.isolateCtxId[ctxId].edges.push(edge);
      edge.status = "isolate";
    }
    var curEdge = rLog.edges[edge.key];
    curEdge.status = edge.status;
    // rLog.addEdge(edge);
  },

  depOnRemove: function(data) {
    var edge = new Edge(data);
    rLog.edges[edge.key].status = "removed";
  },
};

$(function() {
  // window.Viz = Viz;
  // window.barret = Viz;

  window.rLog = new RLog();
  lastPos = -1;
  endPos = log.length;

  var layoutOptions = {
    // https://github.com/cytoscape/cytoscape.js-dagre#api
    name: "dagre",
    rankDir: "LR",
    nodeSep: 5,
    edgeSep: 50,
    animate: false,
    ranker: "longest-path", // Type of algorithm to assign a rank to each node in the input graph. Possible values: 'network-simplex', 'tight-tree' or 'longest-path'
  };

  var cyStyles = (function() {
    var polygonSize = [50, 25];
    return {
      startShape: {
        // flat, arrow
        shape: "polygon",
        "shape-polygon-points": "-0.5 1 0.5 1 1 0 0.5 -1 -0.5 -1",
        width: polygonSize[0],
        height: polygonSize[1],
      },
      middleShape: {
        shape: "polygon",
        "shape-polygon-points": "-1 1 0.5 1 1 0 0.5 -1 -1 -1 -0.5 0",
        width: polygonSize[0],
        height: polygonSize[1],
      },
      endShape: {
        shape: "polygon",
        "shape-polygon-points": "-1 1 0.5 1 0.5 -1 -1 -1 -0.5 0",
        width: polygonSize[0],
        height: polygonSize[1],
      },
    };
  })();

  var cy = (window.cy = cytoscape({
    container: document.getElementById("cy"),

    boxSelectionEnabled: false,
    autounselectify: true,

    layout: layoutOptions,

    style: [
      {
        selector: "node",
        style: {
          content: "data(id)",
          "text-opacity": 0.5,
          "text-valign": "bottom",
          "text-margin-x": "-5",
          "text-halign": "right",
          "background-color": "#11479e",
        },
      },
      {
        selector: ".startNode",
        style: startStyle,
      },
      {
        selector: ".middleNode",
        style: middleStyle,
      },
      {
        selector: ".middleNodeShape",
        style: middleShape,
      },
      {
        selector: ".endNode",
        style: endStyle,
      },

      {
        selector: "edge",
        style: {
          "curve-style": "bezier",
          width: 4,
          "target-arrow-shape": "triangle",
          "line-color": "#9dbaea",
          "target-arrow-color": "#9dbaea",
        },
      },
    ],

    elements: {
      nodes: [
        { data: { id: "n0" } },
        { data: { id: "n1" } },
        { data: { id: "n2" } },
        { data: { id: "n3" } },
        { data: { id: "n4" } },
        { data: { id: "n5" } },
        { data: { id: "n6" } },
        { data: { id: "n7" } },
        { data: { id: "n8" } },
        { data: { id: "n9" } },
        { data: { id: "n10" } },
        { data: { id: "n11" } },
        { data: { id: "n12" } },
        { data: { id: "n13" } },
        { data: { id: "n14" } },
        { data: { id: "n15" } },
        { data: { id: "n16" } },
        { data: { id: "n17" } },
        { data: { id: "n18" } },
      ],
      edges: [
        { data: { source: "n0", target: "n1" } },
        { data: { source: "n0", target: "n5" } },
        { data: { source: "n0", target: "n7" } },
        { data: { source: "n1", target: "n2" } },
        { data: { source: "n1", target: "n3" } },
        { data: { source: "n4", target: "n5" } },
        { data: { source: "n4", target: "n6" } },
        { data: { source: "n6", target: "n7" } },
        { data: { source: "n6", target: "n8" } },
        { data: { source: "n8", target: "n9" } },
        { data: { source: "n8", target: "n10" } },
        { data: { id: "n11n12", source: "n11", target: "n12" } },
        { data: { source: "n12", target: "n13" } },
        { data: { source: "n13", target: "n14" } },
        { data: { source: "n13", target: "n15" } },
        { data: { source: "n16", target: "n17" } },
        { data: { source: "n9", target: "n4" } },
      ],
    },
  }));

  layoutOptions.animate = true;
  setTimeout(function() {
    cy.remove("#n17");
    cy.$("#n11, #n4, #n18").addClass("startNode");
    cy
      .$("#n0, #n6, #n12, #n1, #n5, #n7, #n8, #n13")
      .addClass("middleNodeShape")
      .animate({ style: middleStyle, duration: 1000 });
    cy.$("#n3, #n2, #n10, #n9, #n14, #n15").addClass("endNode");
    cy.$("#n11n12").animate({ style: { opacity: "0" } });
    cy.layout(layoutOptions).run();
  }, 2000);
  setTimeout(function() {
    cy.remove("#n16");
    cy.layout(layoutOptions).run();
  }, 3000);

  // TODO put back to using a worker
  // var graphviz = d3.select("#graphviz").graphviz(true).growEnteringEdges(true);
  var graphviz = d3
    .select("#graphviz")
    .graphviz(false)
    .growEnteringEdges(true);
  function transitionFactory() {
    return d3
      .transition()
      .delay(300)
      .duration(1000);
    // return d3.transition("main")
    //     .ease(d3.easeLinear)
    //     .delay(40)
    //     .duration(300 * dotIndex);
  }

  try {
    var result;
    var lastPos = 90;
    var data;
    var i;

    var edge, node;
    for (i = 0; i < log.length; i++) {
      data = log[i];
      switch (data.action) {
        case "nodeDef":
          node =
            data.type == "reactiveValues"
              ? new NodeReactiveValues(data)
              : new Node(data);
          node.status = "removed";
          rLog.addNode(node);
          break;

        case "depReactiveValueKey":
          rLog.nodes[data.depOnNodeId].dependsOnKey[data.key] = true;
          data = Object.assign({}, data);
          data.depOnNodeId = NodeReactiveValues.idKey(
            data.depOnNodeId,
            data.key
          );
        case "dep":
          edge = new Edge(data);
          edge.status = "removed";
          rLog.addEdge(edge);
          break;
      }
    }

    for (i = 0; i < lastPos - 1; i++) {
      data = log[rLog.logPos];
      callbacks[data.action].call(callbacks, data);
      rLog.logPos++;
      // $("#instructions").html("" + (rLog.logPos + 1) + " / " + Math.min(endPos, log.length));
    }

    var do_next_tick = function() {
      var data = log[rLog.logPos];
      if (lastPos == rLog.logPos) {
        console.error("rLog.logPos: ", rLog.logPos, "data: ", data);
        return;
      }
      lastPos = rLog.logPos;
      var txt =
        "" +
        (rLog.logPos + 1) +
        " / " +
        Math.min(endPos, log.length) +
        " " +
        data.action +
        " - " +
        rLog.nodes[data.nodeId].label +
        "\n" +
        JSON.stringify(data, null, "  ");
      $("#instructions").text(txt);

      if (!callbacks.hasOwnProperty(data.action)) {
        console.error("data: ", data);
        throw new Error("Unknown action " + data.action);
      }
      var result = callbacks[data.action].call(callbacks, data);

      rLog.logPos++;
      if (rLog.logPos > 90) {
        setTimeout(print_full_graph, 1);
      }
    };
    $(document.body).on("keydown", function(e) {
      if (e.which === 39 || e.which === 32) {
        // space, right
        // Move one step ahead
        do_next_tick();
      }
      // if (e.which === 37) { // left
      //   // Move one step back
      //   undo();
      // }
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
  } catch (e) {
    console.error("data: ", data);
    throw e;
  }

  var latestGraph = "";
  print_full_graph = function() {
    var nextGraph = rLog.fullDotGraph;
    if (latestGraph != nextGraph) {
      console.log(nextGraph);
      graphviz.transition(transitionFactory).renderDot(rLog.fullDotGraph);
      latestGraph = nextGraph;
    }
  };
});

var nodes = {};
var nodeList = [];
var nodeSelection = null;
var links = [];
var linkSelection = null;

var node, link; // d3 selections

var MAX_LINES = 6;

// var force = d3.layout.force()
//     .charge(-100)
//     .nodes(nodeList)
//     .links(links);
// force.on('tick', onTick);

function pathDataForNode(node) {
  switch (node.type) {
    case "observer":
      return "M -25,-50  c -75,0 -75,100 0,100  l 100,0  l 0,-100 Z";
    case "observable":
      return "M -25,-50  c -75,0 -75,100 0,100  l 60,0  l 50,-50  l -50,-50  Z";
    case "value":
      return "M -50,-50 l 0,100 l 100,0  l 50,-50  l -50,-50  Z";
  }
}

function getSourceCoords(node) {
  switch (node.type) {
    case "observer":
    case "observable":
      return { x: node.x - 5, y: node.y };
    default:
      return { x: node.x, y: node.y };
  }
}

function getTargetCoords(node) {
  switch (node.type) {
    case "observable":
      return { x: node.x + 7, y: node.y };
    case "value":
      return { x: node.x + 8, y: node.y };
    default:
      return { x: node.x, y: node.y };
  }
}

function compare(a, b) {
  if (a.id < b.id) return -1;
  else if (a.id > b.id) return 1;
  else return 0;
}

var timeDifferences = (function() {
  var enters = log.filter(function(obj) {
    return obj.action === "enter";
  });
  enters.sort(compare);
  var exits = log.filter(function(obj) {
    return obj.action === "exit";
  });
  exits.sort(compare);
  var diff = [];
  for (var i = 0; i < exits.length; i++)
    diff.push(exits[i].time - enters[i].time);
  return diff.map(function(x) {
    return x * 1000;
  });
})();

// colors taken from colorbrewer's 9-class Reds: http://colorbrewer2.org/?type=sequential&scheme=Reds&n=9
// var colorScale = d3.scale.quantize()
//                          .domain([d3.min(timeDifferences), d3.max(timeDifferences)])
//                          .range(['#fcbba1','#fc9272','#fb6a4a','#ef3b2c','#cb181d','#a50f15']);

function update() {
  force.size([
    document.documentElement.clientWidth / 4,
    document.documentElement.clientHeight / 4,
  ]);

  var layoutDirty = false;

  node = d3
    .select("#nodes")
    .selectAll(".node")
    .data(nodeList);
  layoutDirty = layoutDirty || !node.enter().empty() || !node.exit().empty();
  var newG = node
    .enter()
    .append("g")
    .attr("class", function(n) {
      return "node " + n.type;
    })
    .attr("r", 5)
    // don't show until next tick
    .style("display", "none")
    .on("mousedown", function() {
      d3.event.stopPropagation();
    })
    .on("mouseover", function(n) {
      $("#description").text(n.label);
    })
    .on("mouseout", function(d, i) {
      $("#description").text("");
    })
    .on("dblclick", function(d) {
      d3.event.stopPropagation();
      d3.select(this).classed("fixed", (d.fixed = false));
    })
    .call(
      force.drag().on("dragstart", function(d) {
        d3.select(this).classed("fixed", (d.fixed = true));
      })
    );
  newG
    .append("path")
    .attr("transform", "scale(0.08)")
    .attr("stroke", "black")
    .attr("stroke-width", 4)
    .attr("fill", "white")
    .attr("d", pathDataForNode);
  newG
    .append("text")
    .attr("x", 3)
    .attr("y", 1.5)
    .attr("font-size", 3.25)
    .attr("transform", function(n) {
      if (n.type !== "observer") return "translate(1.5, 0)";
      else return null;
    });
  node.exit().remove();
  node
    .classed("invalidated", function(n) {
      return n.invalidated;
    })
    .classed("running", function(n) {
      return n.running;
    })
    .classed("changed", function(n) {
      return n.changed;
    })
    .attr("fill", function(n) {
      if (n.invalidated) return "url(#diagonalHatch)";
      else return null;
    });
  var tspanLabel = node
    .selectAll("text")
    .filter(function(n) {
      // This filter is used to disregard all nodes whose labels have
      // not changed since the last time we updated them.
      var changed = n.label !== this.label;
      this.label = n.label;
      return changed;
    })
    .selectAll(".tspanLabel")
    .data(function(n) {
      var lines = n.label.replace(/ /g, "\xA0").split("\n");
      if (lines.length > MAX_LINES) {
        lines.splice(MAX_LINES);
      }
      return lines;
    });
  tspanLabel.enter().append("tspan");
  tspanLabel.exit().remove();
  tspanLabel
    .attr("x", 8)
    .attr("dy", function(line, i) {
      return i > 0 ? "1em" : 0;
    })
    .attr("opacity", function(line, i) {
      return Math.min(1, (MAX_LINES - i) * 0.25 - 0.15);
    })
    .classed("tspanLabel", true)
    .text(function(line) {
      return line;
    });

  var tspanTime = node
    .selectAll("text")
    .filter(function(n) {
      var changed = n.timeElapsed !== this.timeElapsed;
      this.timeElapsed = n.timeElapsed;
      return changed;
    })
    .selectAll(".tspanTime")
    .data(function(n) {
      return [n.timeElapsed];
    });
  tspanTime.enter().append("tspan");
  tspanTime.exit().remove();
  tspanTime
    .attr("x", 8)
    .attr("y", -2)
    .attr("fill", function(time) {
      return colorScale(time);
    })
    .classed("tspanTime", true)
    .text(function(time) {
      if (time === null) return "";
      else return "time elapsed: " + Math.round(time * 1000) / 1000 + " ms";
    });

  link = d3
    .select("#links")
    .selectAll(".link")
    .data(links);
  layoutDirty = layoutDirty || !link.enter().empty() || !link.exit().empty();
  link
    .enter()
    .append("path")
    .attr("class", "link")
    .attr("marker-mid", "url(#triangle)");
  link.exit().remove();

  if (layoutDirty) {
    force
      .nodes(
        nodeList.filter(function(n) {
          return !n.hide;
        })
      )
      .start();
    layoutDirty = false;
  }
}

function onTick() {
  node.style("display", null).attr("transform", function(n) {
    return "translate(" + n.x + " " + n.y + ")";
  });
  link.attr("d", function(link) {
    var source = getSourceCoords(link.source);
    var target = getTargetCoords(link.target);
    var mid = {
      x: (source.x + target.x) / 2,
      y: (source.y + target.y) / 2,
    };
    return (
      "M" +
      source.x +
      "," +
      source.y +
      " L" +
      mid.x +
      "," +
      mid.y +
      " L" +
      target.x +
      "," +
      target.y
    );
  });
}

function createNodeWithUndo(data) {
  var node;
  if (!data.prevId) {
    node = {
      label: data.label,
      type: data.type,
      hide: data.hide,
    };
    nodes[data.id] = node;
    pushUndo(function() {
      delete nodes[data.id];
    });
    if (!node.hide) {
      nodeList.push(node);
      pushUndo(function() {
        nodeList.pop();
      });
    }
  } else {
    node = nodes[data.prevId];
    var oldLabel = node.label;
    var oldInvalidated = node.invalidated;
    delete nodes[data.prevId];
    nodes[data.id] = node;
    node.label = data.label;
    node.invalidated = false;
    pushUndo(function() {
      node.label = oldLabel;
      node.invalidated = oldInvalidated;
      delete nodes[data.id];
      nodes[data.prevId] = node;
    });
  }
}

Array.prototype.pushWithUndo = function(value) {
  var self = this;
  this.push(value);
  pushUndo(function() {
    self.pop();
  });
};

Array.prototype.shiftWithUndo = function(value) {
  var self = this;
  var value = this.shift();
  pushUndo(function() {
    self.unshift(value);
  });
  return value;
};

var undoStack = [];
var currentUndos = null;
function startUndoScope() {
  if (currentUndos !== null) throw new Error("Illegal state");
  currentUndos = [];
}
function pushUndo(func) {
  currentUndos.push(func);
}
function endUndoScope() {
  var localUndos = currentUndos;
  undoStack.push(function() {
    while (localUndos.length) {
      localUndos.pop()();
    }
  });
  currentUndos = null;
}
function undo() {
  if (undoStack.length) {
    undoStack.pop()();
    update();
    return true;
  }
  return false;
}
function undoAll() {
  while (undo()) {}
}

// Here we monkeypatch Math.random to take part in the undo mechanism.
// This allows "random" d3 force-layout decisions to be reproducible.
// If we don't do this, then doing/undoing/redoing a node creation step
// looks very confusing, as the node comes flying in from a different
// direction each time.
var trueRandom = Math.random;
Math.random = (function() {
  var randomStack = [];
  return function() {
    if (!currentUndos) return trueRandom();

    var value;
    if (randomStack.length > 0) {
      value = randomStack.pop();
    } else {
      value = trueRandom();
    }
    pushUndo(function() {
      randomStack.push(value);
    });
    return value;
  };
})();

// var callbacks = {
//   ctx: function(data) {
//     createNodeWithUndo(data);
//     return true;},
//   dep: function(data) {
//     var dependsOn = nodes[data.dependsOn];
//     if (!dependsOn) {
//       createNodeWithUndo({
//         id: data.dependsOn,
//         label: data.dependsOn,
//         type: 'value'
//       });
//       dependsOn = nodes[data.dependsOn];
//     }
//     if (dependsOn.hide) {
//       dependsOn.hide = false;
//       nodeList.push(dependsOn);
//       pushUndo(function() {
//         dependsOn.hide = true;
//         nodeList.pop();
//       });
//     }
//     links.push({
//       source: nodes[data.id],
//       target: nodes[data.dependsOn]
//     });
//     pushUndo(function() {
//       links.pop();
//     });},
//   depId: function(data) {
//     links.push({
//       source: nodes[data.id],
//       target: nodes[data.dependsOn]
//     });
//     pushUndo(function() {
//       links.pop();
//     });},
//   invalidate: function(data) {
//     var node = nodes[data.id];
//     if (node.invalidated)
//       throw new Error('Illegal sequence');
//
//     node.invalidated = true;
//     pushUndo(function() {
//       node.invalidated = false;
//     });
//
//     var origLinks = links;
//     links = links.filter(function(link) {
//       return link.source !== node;
//     });
//     pushUndo(function() {
//       links = origLinks;
//     });},
//   valueChange: function(data) {
//     var existed = !!nodes[data.id];
//     createNodeWithUndo({
//       id: data.id,
//       label: data.id + ' = ' + data.value,
//       type: 'value',
//       prevId: nodes[data.id] ? data.id : null,
//       hide: existed ? nodes[data.id].hide : true
//     });
//     if (!existed || nodes[data.id].hide)
//       return true;
//     nodes[data.id].changed = true;
//     pushUndo(function() {
//       nodes[data.id].changed = false;
//     });
//     executeBeforeNextCommand.pushWithUndo(function() {
//       nodes[data.id].changed = false;
//       pushUndo(function() {
//         nodes[data.id].changed = true;
//       });
//     });},
//   enter: function(data) {
//     var node = nodes[data.id];
//     node.running = true;
//     node.start = data.time;
//     var oldTimeElapsed = node.timeElapsed;
//     node.timeElapsed = null;
//     pushUndo(function() {
//       node.running = false;
//       node.start = null;
//       node.timeElapsed = oldTimeElapsed;
//     });},
//   exit: function(data) {
//     var node = nodes[data.id];
//     node.running = false;
//     var oldTimeElapsed = node.timeElapsed;
//     node.timeElapsed = time ? (parseFloat(data.time) - parseFloat(node.start)) * 1000 : null;
//     pushUndo(function() {
//       node.running = true;
//       node.timeElapsed = oldTimeElapsed;
//     });
//   }
// };

update_node_value = function(data) {
  node = nodes[data.nodeId];
  var oldLabel = node.label;
  var oldInvalidated = node.invalidated;
  delete nodes[data.prevId];
  nodes[data.id] = node;
  node.label = data.label;
  node.invalidated = false;
  pushUndo(function() {
    node.label = oldLabel;
    node.invalidated = oldInvalidated;
    delete nodes[data.id];
    nodes[data.prevId] = node;
  });
};
// var callbacks = {
//   nodeDef: function(data) {
//     var node = {
//       label: data.label,
//       type: data.type,
//       hide: data.hide
//     };
//     nodes[data.nodeId] = node;
//     pushUndo(function() {
//       delete nodes[data.id];
//     });
//     if (!node.hide) {
//       nodeList.push(node);
//       pushUndo(function() {
//         nodeList.pop();
//       });
//     }
//     true
//   },
//
//   updateNodeLabel: function(data) {
//     var prevLabel = nodes[data.nodeId].label;
//     nodes[data.nodeId].label = data.label;
//     if (nodes[data.nodeId + "_names"])
//       nodes[data.nodeId+"_names"].label = "names(" + data.label + ")"
//     if (nodes[data.nodeId + "_asList"])
//       nodes[data.nodeId+"_asList"].label = "as.list(" + data.label + ")"
//     pushUndo(function() {
//       nodes[data.nodeId].label = prevLabel;
//       if (nodes[data.nodeId + "_names"])
//         nodes[data.nodeId+"_names"].label = "names(" + prevLabel + ")"
//       if (nodes[data.nodeId + "_asList"])
//         nodes[data.nodeId+"_asList"].label = "as.list(" + prevLabel + ")"
//     })
//   },
//
//   valueChangeReactValueNames: function(data) {
//     console.error("valueNames: ", data)
//     if (nodes[data.nodeId + "_names"]) {
//       update_node_value({
//         id: data.nodeId + "_names",
//         label: "names(" + nodes[data.nodeId].label + ")",
//         type: data.type,
//         hide: data.hide
//       })
//     } else {
//       callbacks.nodeDef({
//         id: data.nodeId + "_names",
//         label: "names(" + nodes[data.nodeId].label + ")",
//         type: data.type,
//         hide: data.hide
//       })
//     }
//   },
//
//   valueChangeReactValueValues: function(data) {
//     // console.error("valueValues updated", data)
//   },
//
//   valueChangeReactValueKey: function(data) {
//     console.error("valueKey updated", data)
//
//   }
//
//
// }

function processMessage(data, suppressUpdate) {
  if (!callbacks.hasOwnProperty(data.action))
    throw new Error("Unknown action " + data.action);
  var result = callbacks[data.action].call(callbacks, data);
  if (!suppressUpdate) update();
  return result;
}

var executeBeforeNextCommand = [];
function doNext(suppressUpdate) {
  if (!log.length) return;

  startUndoScope();
  while (executeBeforeNextCommand.length) {
    executeBeforeNextCommand.shiftWithUndo()();
  }
  while (log.length) {
    var result = (function() {
      var message = log.shift();
      pushUndo(function() {
        log.unshift(message);
      });
      return processMessage(message, suppressUpdate);
    })();
    if (!result) break;
  }
  if (!log.length) {
    $("#ended").fadeIn(1500);
    pushUndo(function() {
      $("#ended").hide();
    });
  }
  step++;
  updateTimeline();
  pushUndo(function() {
    step--;
    updateTimeline();
  });
  endUndoScope();
}

function countSteps() {
  if (undoStack.length !== 0) {
    throw new Error(
      "Illegal state; must call countSteps before execution begins"
    );
  }
  var steps = 0;
  while (log.length) {
    doNext();
    steps++;
  }
  while (undoStack.length) undoStack.pop()();
  return steps;
}

function updateTimeline() {
  $("#timeline-fill").width(step / totalSteps * 100 + "%");
}

function zoom() {
  var scale = d3.event.scale;
  var x = d3.event.translate[0];
  var y = d3.event.translate[1];
  d3
    .select("#viz")
    .attr(
      "transform",
      "scale(" + scale + ") translate(" + x / scale + " " + y / scale + ")"
    );
}

// // The total number of steps, as far as the user is concerned, in the log.
// // This may/will be different than the number of log entries, since each
// // step may include more than one log entry.
// var totalSteps;
// // The current step we're on.
// var step;
// $(function() {
//   d3.select('svg').call(d3.behavior.zoom().scale(4).on('zoom', zoom));
//   $(document.body).on('keydown', function(e) {
//     if (e.which === 39 || e.which === 32) { // space, right
//       // Move one step ahead
//       doNext();
//     }
//     if (e.which === 37) { // left
//       // Move one step back
//       undo();
//     }
//     if (e.which === 35) { // end
//       // Seek to end
//       while (log.length) {
//         doNext();
//       }
//     }
//     if (e.which === 36) { // home
//       // Seek to beginning
//       undoAll();
//     }
//   });
//
//   // Timeline click and scrub
//   $('#timeline').on('click mousemove', function(e) {
//     // Make sure left mouse button is down.
//     // Firefox is stupid; e.which is always 1 on mousemove events,
//     // even when button is not down!! So read e.originalEvent.buttons.
//     if (typeof(e.originalEvent.buttons) !== 'undefined') {
//       if (e.originalEvent.buttons !== 1)
//         return;
//     } else if (e.which !== 1) {
//       return;
//     }
//
//     var timeline = e.currentTarget;
//     var pos = e.offsetX || e.originalEvent.layerX;
//     var width = timeline.offsetWidth;
//     var targetStep = Math.round((pos/width) * totalSteps);
//     while (step < targetStep) {
//       doNext();
//     }
//     while (step > targetStep && step != 1) {
//       undo();
//     }
//   });
//
//   totalSteps = countSteps();
//   step = 0;
//
//   doNext();
//
//   // don't allow undoing past initial state
//   while (undoStack.length)
//     undoStack.pop();
//   executeBeforeNextCommand.push(function() {
//     $('#instructions').fadeOut(1000);
//     // It's weird for the instructions to fade back in, so no pushUndo here
//   });
// });
