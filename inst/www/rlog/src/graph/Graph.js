// @flow

import _ from "lodash";
import cytoscape from "cytoscape";

import { Node } from "./Node";
import { Edge } from "./Edge";
import { GhostEdge } from "./GhostEdge";
import { HoverStatus } from "./HoverStatus";
import { StatusArr } from "./StatusArr";

import console from "../utils/console";

import type {
  LogType,
  LogEntryAnyType,
  ReactIdType,
  LogEntryDefineType,
  LogEntryInvalidateStartType,
  LogEntryIsolateInvalidateStartType,
  LogEntryIsolateEnterType,
  LogEntryEnterType,
  LogEntryDependsOnType,
  LogEntryDependsOnRemoveType,
  LogEntryExitType,
  LogEntryIsolateExitType,
  LogEntryInvalidateEndType,
  LogEntryIsolateInvalidateEndType,
} from "../log/logStates";
import type { NodeIdType } from "./Node";
import type { EdgeIdType } from "./Edge";

type SomeGraphData = Node | Edge | GhostEdge;

class Graph {
  log: LogType;
  nodes: Map<NodeIdType, Node>;
  edges: Map<EdgeIdType, Edge>;
  edgesUnique: Map<EdgeIdType, GhostEdge>;
  // asyncStart: number;
  // queueEmpty: number;
  activeNodeEnter: Array<ReactIdType>;
  activeInvalidateEnter: Array<ReactIdType>;

  constructor(log: LogType) {
    this.log = log;
    this.nodes = new Map();
    this.edges = new Map();
    this.edgesUnique = new Map();
    // this.asyncStart = -1;
    // this.asyncStop = -1;
    // this.queueEmpty = -1;
    this.activeNodeEnter = [];
    this.activeInvalidateEnter = [];
  }

  get cytoGraph() {
    let getCytoDatas = function(x) {
      return _.values(x).map(item => item.cytoData);
    };
    let nodes = getCytoDatas(this.nodes);

    let ghostEdgeMap = new Map(this.edgesUnique.entries());
    let edges = _.values(this.edges).map(edge => {
      // remove matching unique/ghost edges
      if (ghostEdgeMap.has(edge.ghostKey)) {
        ghostEdgeMap.delete(edge.ghostKey);
      }
      return edge.cytoData;
    });
    let ghostEdges = getCytoDatas(ghostEdgeMap);

    let cyto = cytoscape();
    cyto.add(nodes);
    cyto.add(edges);
    cyto.add(ghostEdges);
    return cyto;
  }

  hasSomeData(data: SomeGraphData): boolean {
    if (data instanceof Node) {
      return this.nodes.has(data.key);
    } else if (isEdgeLike(data)) {
      if (data instanceof Edge) {
        if (this.edgesUnique.has(data.ghostKey)) return true;
      } else {
        if (this.edgesUnique.has(data.key)) return true;
      }

      let reactId = data.reactId;
      let depOnReactId = data.depOnReactId;
      return _.some([this.edges, this.edgesUnique], function(edgeGroup) {
        return _.some(_.values(edgeGroup), function(edge) {
          if (edge.reactId === reactId && edge.depOnReactId === depOnReactId) {
            return true;
          }
          return false;
        });
      });
    } else {
      console.error(data);
      throw "unsupported data type";
    }
  }

  highlightSelected(data: ?SomeGraphData) {
    if (!data) return;
    if (data instanceof Node) {
      let node = this.nodes.get(data.key);
      if (typeof node !== "undefined") {
        node.hoverStatus.selected = HoverStatus.valSelected;
        return;
      }
    } else if (isEdgeLike(data)) {
      if (data instanceof Edge) {
        let edge = this.edges.get(data.key);
        if (typeof edge !== "undefined") {
          edge.hoverStatus.selected = HoverStatus.valSelected;
          return;
        }
      }

      // highlight all the edges (and ghost edges) that have the same source and target
      let reactId = data.reactId;
      let depOnReactId = data.depOnReactId;
      let selectMatchingEdges = function(edge) {
        if (edge.reactId === reactId && edge.depOnReactId === depOnReactId) {
          edge.hoverStatus.selected = HoverStatus.valSelected;
        }
      };
      _.values(this.edgesUnique).map(selectMatchingEdges);
      _.values(this.edges).map(selectMatchingEdges);
      return;
    }
    return;
  }

  reactIdFromData(
    data: SomeGraphData | ?ReactIdType,
    getParentFromEdge: boolean = true
  ): ?ReactIdType {
    if (data === null) {
      throw "Graph.prototype.reactIdFromData(data) must submit non null data";
    }
    if (typeof data === "string") {
      return data;
    } else if (data instanceof Node) {
      return data.reactId;
    } else if (isEdgeLike(data)) {
      let node = getParentFromEdge
        ? this.nodes.get(data.depOnReactId)
        : this.nodes.get(data.reactId);
      if (node) {
        return node.reactId;
      } else {
        return null;
      }
    } else {
      console.error(data);
      throw "unsupported data type. Can only 'reactId's of 'Node's, 'GhostEdge's, or 'Edge's or from a reactId";
    }
  }

  // return array of node 'reactId's
  parentNodeIds(data: SomeGraphData | ?ReactIdType): Array<ReactIdType> {
    if (data === null) {
      throw "Graph.prototype.parentNodeIds(data) must submit non null data";
    }
    if (isEdgeLike(data)) {
      // return edge source
      return [data.reactId];
    } else {
      let reactId;
      reactId = this.reactIdFromData(data, true);
      if (!reactId) return [];
      return _.filter(_.values(this.edgesUnique), function(edge) {
        // if the target is the reactId
        return edge.reactId === reactId;
      }).map(function(edge) {
        // return the source
        return edge.depOnReactId;
      });
    }
  }
  childrenNodeIds(data: SomeGraphData | ?ReactIdType): Array<ReactIdType> {
    if (data === null) {
      throw "Graph.prototype.childrenNodeIds(data) must submit non null data";
    }
    if (isEdgeLike(data)) {
      // return edge target
      return [data.depOnReactId];
    } else {
      let reactId = this.reactIdFromData(data, false);
      if (!reactId) return [];
      return _.filter(_.values(this.edgesUnique), function(edge) {
        // if the source is the reactId
        return edge.depOnReactId === reactId;
      }).map(function(edge) {
        // return the target
        return edge.reactId;
      });
    }
  }

  ancestorNodeIds(data: SomeGraphData | ReactIdType): Array<ReactIdType> {
    let reactId = this.reactIdFromData(data, true);
    if (!reactId) {
      return [];
    } else {
      let originalReactId = reactId;
      let seenMap = new Set();
      let reactIdArr = [reactId];
      while (reactIdArr.length > 0) {
        reactId = reactIdArr.pop();
        if (!seenMap.has(reactId)) {
          this.parentNodeIds(reactId).forEach(function(parentReactId) {
            if (parentReactId) {
              reactIdArr.push(parentReactId);
            }
          });
          seenMap.add(reactId);
        }
      }
      seenMap.delete(originalReactId);
      return Array.from(seenMap).sort();
    }
  }
  decendentNodeIds(data: SomeGraphData | ReactIdType): Array<ReactIdType> {
    let reactId = this.reactIdFromData(data, false);
    if (!reactId) return [];
    let originalReactId = reactId;
    let seenMap = {};
    let reactIdArr = [reactId];
    while (reactIdArr.length > 0) {
      reactId = reactIdArr.pop();
      if (!_.has(seenMap, reactId)) {
        reactIdArr = reactIdArr.concat(this.childrenNodeIds(reactId));
        seenMap[reactId] = true;
      }
    }
    delete seenMap[originalReactId];
    return _.keys(seenMap).sort();
  }

  // all filtering can be done with only node reactIds
  familyTreeNodeIds(data: SomeGraphData | ReactIdType): Array<ReactIdType> {
    let ret = [];
    let reactId;
    if (isEdgeLike(data)) {
      reactId = this.reactIdFromData(data, true);
      if (reactId) ret.push(reactId);
      reactId = this.reactIdFromData(data, false);
      if (reactId) ret.push(reactId);
    } else {
      reactId = this.reactIdFromData(data);
      if (reactId) ret.push(reactId);
    }
    return _.union(
      ret,
      this.ancestorNodeIds(data),
      this.decendentNodeIds(data)
    );
  }

  familyTreeNodeIdsForDatas(datas: Array<SomeGraphData>): Array<ReactIdType> {
    let self = this;
    return _.union(
      // has an error as there is a double definition of _.flatMap.
      // One for an Array (defined first)
      // One for an Object (defined second, which stomps the first)
      // $FlowExpectError
      _.flatMap(datas, function(data: SomeGraphData) {
        return self.familyTreeNodeIds(data);
      })
    );
  }
  decendentNodeIdsForDatas(datas: Array<SomeGraphData>): Array<ReactIdType> {
    let self = this;
    return _.union(
      // has an error as there is a double definition of _.flatMap.
      // One for an Array (defined first)
      // One for an Object (defined second, which stomps the first)
      // $FlowExpectError
      _.flatMap(datas, function(data: SomeGraphData) {
        return self.decendentNodeIds(data);
      })
    );
  }
  ancestorNodeIdsForDatas(datas: Array<SomeGraphData>) {
    let self = this;
    return _.union(
      // has an error as there is a double definition of _.flatMap.
      // One for an Array (defined first)
      // One for an Object (defined second, which stomps the first)
      // $FlowExpectError
      _.flatMap(datas, function(data) {
        return self.ancestorNodeIds(data);
      })
    );
  }

  hoverStatusOnNodeIds(
    nodeIds: Array<ReactIdType>,
    hoverKey: "state" | "sticky",
    onStatus: typeof HoverStatus.valSticky | typeof HoverStatus.valFocused,
    offStatus:
      | typeof HoverStatus.valNotSticky
      | typeof HoverStatus.valNotFocused
  ) {
    let nodeMap = {};
    nodeIds.map(function(nodeId) {
      nodeMap[nodeId] = true;
    });

    // highlight nodes
    _.map(this.nodes, function(node) {
      if (_.has(nodeMap, node.reactId)) {
        node.hoverStatus[hoverKey] = onStatus;
      } else {
        node.hoverStatus[hoverKey] = offStatus;
      }
    });
    // highlight edges
    _.map(this.edges, function(edge) {
      if (_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId)) {
        edge.hoverStatus[hoverKey] = onStatus;
      } else {
        edge.hoverStatus[hoverKey] = offStatus;
      }
    });
    // highlight unique edges
    _.map(this.edgesUnique, function(edge) {
      if (_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId)) {
        edge.hoverStatus[hoverKey] = onStatus;
      } else {
        edge.hoverStatus[hoverKey] = offStatus;
      }
    });

    return this;
  }

  filterGraphOnNodeIds(nodeIds: Array<ReactIdType>) {
    let nodeMap = {};
    nodeIds.map(function(nodeId) {
      nodeMap[nodeId] = true;
    });

    let self = this;
    // prune nodes
    _.map(this.nodes, function(node, key) {
      if (!_.has(nodeMap, node.reactId)) {
        self.nodes.delete(key);
      }
    });
    // prune edges
    _.map(this.edges, function(edge, key) {
      if (
        !(_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId))
      ) {
        self.edges.delete(key);
      }
    });
    // prune unique edges
    _.map(this.edgesUnique, function(edge, key) {
      if (
        !(_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId))
      ) {
        self.edgesUnique.delete(key);
      }
    });

    return this;
  }

  addEntry(data: LogEntryAnyType) {
    if (data.reactId) {
      if (data.reactId === "rNoCtx") {
        return;
      }
    }

    let node, lastNodeId, edge;

    switch (data.action) {
      // {"action": "define", "reactId": "r3", "label": "plotObj", "type": "observable", "session": "fa3c747a6121aec5baa682cc3970b811", "time": 1524581676.5841},
      case "define": {
        let logEntry = ((data: LogEntryDefineType): Object);
        this.nodes.set(data.reactId, new Node(logEntry));
        break;
      }

      // {"action": "updateNodeLabel", "nodeId": "1", "label": "input", "session": null, "time": 1522955046.5537},
      case "updateNodeLabel":
        node = this.nodes.get(data.reactId);
        if (node) {
          node.label = data.label;
        }
        break;

      case "valueChange":
        node = this.nodes.get(data.reactId);
        if (node) {
          node.value = data.value;
          node.valueChangedStatus.setActiveAtStep(data.step);
        }
        break;

      case "invalidateStart": {
        let logEntry = ((data: LogEntryInvalidateStartType): Object);
        node = this.nodes.get(logEntry.reactId);
        lastNodeId = _.last(this.activeInvalidateEnter);
        if (lastNodeId) {
          let lastInvalidateNode = this.nodes.get(lastNodeId);
          if (lastInvalidateNode) {
            lastInvalidateNode.invalidateStatus.resetActive();
          }
        }
        this.activeInvalidateEnter.push(logEntry.reactId);
        if (node) {
          switch (node.type) {
            case "observable":
            case "observer":
              node.invalidateStatus.setActiveAtStep(logEntry.step);
              break;
          }
          node.statusAdd(logEntry);
        }
        break;
      }
      case "enter": {
        let logEntry = ((data: LogEntryEnterType): Object);
        lastNodeId = _.last(this.activeNodeEnter);
        if (lastNodeId) {
          let lastNode = this.nodes.get(lastNodeId);
          if (lastNode) {
            lastNode.enterStatus.resetActive();
          }
        }
        this.activeNodeEnter.push(logEntry.reactId);
        node = this.nodes.get(logEntry.reactId);
        if (node) {
          node.enterStatus.setActiveAtStep(logEntry.step);
          switch (node.type) {
            case "observer":
            case "observable":
              node.invalidateStatus.reset();
          }
          node.statusAdd(logEntry);
        }
        break;
      }

      case "isolateInvalidateStart":
      case "isolateEnter": {
        let logEntry = ((data:
          | LogEntryIsolateInvalidateStartType
          | LogEntryIsolateEnterType): Object);
        node = this.nodes.get(logEntry.reactId);
        if (node) {
          node.statusAdd(logEntry);
        }
        break;
      }

      case "invalidateEnd":
      case "exit":
      case "isolateExit":
      case "isolateInvalidateEnd": {
        node = this.nodes.get(data.reactId);
        switch (data.action) {
          case "exit": {
            let activeEnterNode = this.nodes.get(_.last(this.activeNodeEnter));
            if (activeEnterNode) {
              activeEnterNode.enterStatus.reset();
            }
            this.activeNodeEnter.pop();
            lastNodeId = _.last(this.activeNodeEnter);
            if (lastNodeId) {
              let curActiveNode = this.nodes.get(lastNodeId);
              if (curActiveNode) {
                curActiveNode.enterStatus.setActiveAtStep(data.step);
              }
            }
            break;
          }
          case "invalidateEnd": {
            // turn off the previously active node
            let curActiveNode = this.nodes.get(
              _.last(this.activeInvalidateEnter)
            );
            if (curActiveNode) {
              curActiveNode.invalidateStatus.resetActive();
            }
            this.activeInvalidateEnter.pop();
            // if another invalidateStart node exists...
            //   set the previous invalidateStart node to active
            lastNodeId = _.last(this.activeInvalidateEnter);
            if (lastNodeId) {
              let lastNode = this.nodes.get(lastNodeId);
              if (lastNode) {
                lastNode.invalidateStatus.setActiveAtStep(data.step);
              }
            }
            if (node) {
              node.invalidateStatus.toFinished();
              if (node.valueChangedStatus.isOn) {
                node.valueChangedStatus.reset();
              }
            }
            break;
          }
          case "isolateInvalidateEnd":
            if (node && node.valueChangedStatus.isOn) {
              node.valueChangedStatus.reset();
            }
            break;
        }
        if (node) {
          let prevData = node.statusLast();
          let expectedAction = {
            exit: "enter",
            isolateExit: "isolateEnter",
            invalidateEnd: "invalidateStart",
            isolateInvalidateEnd: "isolateInvalidateStart",
          }[data.action];
          let logEntry = ((data:
            | LogEntryExitType
            | LogEntryIsolateExitType
            | LogEntryInvalidateEndType
            | LogEntryIsolateInvalidateEndType): Object);
          StatusArr.expect_prev_status(logEntry, prevData, expectedAction);
          node.statusRemove();
        }
        break;
      }

      case "dependsOn": {
        let logEntry = ((data: LogEntryDependsOnType): Object);
        edge = new Edge(logEntry);
        let edgeKey = edge.key;

        // store unique edges to always display a transparent dependency
        if (!_.has(this.edgesUnique, edge.ghostKey)) {
          this.edgesUnique.set(edge.ghostKey, new GhostEdge(logEntry));
        }

        if (this.edges.has(edgeKey)) {
          edge = this.edges.get(edgeKey);
        } else {
          this.edges.set(edgeKey, edge);
        }

        if (edge) {
          node = this.nodes.get(edge.reactId);
          if (node && node.statusLast().action === "isolateEnter") {
            edge.status = "isolate";
          } else {
            edge.status = "normal";
          }
        }
        break;
      }

      case "dependsOnRemove": {
        let logEntry = ((data: LogEntryDependsOnRemoveType): Object);
        edge = new Edge(logEntry);
        // remove the edge
        this.edges.delete(edge.key);
        break;
      }

      case "queueEmpty":
      case "asyncStart":
      case "asyncStop":
        // do nothing
        // this[data.action] = data.step;
        break;

      default:
        console.error("data.action: ", data.action, data);
        throw data;
    }
  }
}

function isEdgeLike(data: any): boolean %checks {
  return data instanceof Edge || data instanceof GhostEdge;
}

export { Graph };
export type { SomeGraphData };
