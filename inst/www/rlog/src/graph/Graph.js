import _ from "lodash";
import cytoscape from "cytoscape";

import Node from "./Node";
import Edge from "./Edge";
import GhostEdge from "./GhostEdge";
import HoverStatus from "./HoverStatus";
import StatusArr from "./StatusArr";

import console from "../utils/console";

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
    let cyto = cytoscape();
    let nodes = _.values(this.nodes).map(function(node) {
      return node.cytoData;
    });
    cyto.add(nodes);
    let ghostEdgeMap = _.assign({}, this.edgesUnique);
    let edges = _.values(this.edges).map(function(edge) {
      // remove matching unique/ghost edges
      if (_.has(ghostEdgeMap, edge.ghostKey)) {
        delete ghostEdgeMap[edge.ghostKey];
      }
      return edge.cytoData;
    });
    cyto.add(edges);
    let ghostEdges = _.values(ghostEdgeMap).map(function(edge) {
      return edge.cytoData;
    });
    cyto.add(ghostEdges);

    return cyto;
  }

  hasSomeData(data) {
    if (data instanceof Node) {
      return _.has(this.nodes, data.key);
    } else if (data instanceof Edge || data instanceof GhostEdge) {
      if (data instanceof Edge) {
        if (_.has(this.edgesUnique, data.ghostKey)) return true;
      } else {
        if (_.has(this.edgesUnique, data.key)) return true;
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

  highlightSelected(data) {
    if (data instanceof Node) {
      if (_.has(this.nodes, data.key)) {
        this.nodes[data.key].hoverStatus.selected = HoverStatus.isSelected;
        return;
      }
    } else if (data instanceof Edge || data instanceof GhostEdge) {
      if (data instanceof Edge) {
        if (_.has(this.edges, data.key)) {
          this.edges[data.key].hoverStatus.selected = HoverStatus.isSelected;
          return;
        }
      }

      // highlight all the edges (and ghost edges) that have the same source and target
      let reactId = data.reactId;
      let depOnReactId = data.depOnReactId;
      let selectMatchingEdges = function(edge) {
        if (edge.reactId === reactId && edge.depOnReactId === depOnReactId) {
          edge.hoverStatus.selected = HoverStatus.isSelected;
        }
      };
      _.values(this.edgesUnique).map(selectMatchingEdges);
      _.values(this.edges).map(selectMatchingEdges);
      return;
    }
    return;
  }

  reactIdFromData(data, getParentFromEdge = true) {
    if (data instanceof Node) {
      return data.reactId;
    } else if (Graph.isEdgeLike(data)) {
      let node = getParentFromEdge
        ? this.nodes[data.depOnReactId]
        : this.nodes[data.reactId];
      if (node) {
        return node.reactId;
      } else {
        return null;
      }
    } else if (typeof data === "string") {
      return data;
    } else {
      console.error(data);
      throw "unsupported data type. Can only 'reactId's of 'Node's, 'GhostEdge's, or 'Edge's or from a reactId";
    }
  }

  static isEdgeLike(data) {
    return data instanceof Edge || data instanceof GhostEdge;
  }

  // return array of node 'reactId's
  parentNodeIds(data) {
    if (Graph.isEdgeLike(data)) {
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
  childrenNodeIds(data) {
    if (Graph.isEdgeLike(data)) {
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

  ancestorNodeIds(data) {
    let reactId = this.reactIdFromData(data, true);
    if (!reactId) return [];
    let originalReactId = reactId;
    let seenMap = {};
    let reactIdArr = [reactId];
    while (reactIdArr.length > 0) {
      reactId = reactIdArr.pop();
      if (!_.has(seenMap, reactId)) {
        reactIdArr = reactIdArr.concat(this.parentNodeIds(reactId));
        seenMap[reactId] = true;
      }
    }
    delete seenMap[originalReactId];
    return _.keys(seenMap).sort();
  }
  decendentNodeIds(data) {
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
  familyTreeNodeIds(data) {
    let ret = [];
    if (Graph.isEdgeLike(data)) {
      let reactId;
      reactId = this.reactIdFromData(data, true);
      if (reactId) ret.push(reactId);
      reactId = this.reactIdFromData(data, false);
      if (reactId) ret.push(reactId);
    } else {
      ret.push(this.reactIdFromData(data));
    }
    return _.union(
      ret,
      this.ancestorNodeIds(data),
      this.decendentNodeIds(data)
    );
  }

  familyTreeNodeIdsForDatas(datas) {
    let self = this;
    return _.union(
      _.flatMap(datas, function(data) {
        return self.familyTreeNodeIds(data);
      })
    );
  }
  decendentNodeIdsForDatas(datas) {
    let self = this;
    return _.union(
      _.flatMap(datas, function(data) {
        return self.decendentNodeIds(data);
      })
    );
  }
  ancestorNodeIdsForDatas(datas) {
    let self = this;
    return _.union(
      _.flatMap(datas, function(data) {
        return self.ancestorNodeIds(data);
      })
    );
  }

  hoverStatusOnNodeIds(nodeIds, hoverKey, onStatus, offStatus) {
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

  filterGraphOnNodeIds(nodeIds) {
    let nodeMap = {};
    nodeIds.map(function(nodeId) {
      nodeMap[nodeId] = true;
    });

    let self = this;
    // prune nodes
    _.map(this.nodes, function(node, key) {
      if (!_.has(nodeMap, node.reactId)) {
        delete self.nodes[key];
      }
    });
    // prune edges
    _.map(this.edges, function(edge, key) {
      if (
        !(_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId))
      ) {
        delete self.edges[key];
      }
    });
    // prune unique edges
    _.map(this.edgesUnique, function(edge, key) {
      if (
        !(_.has(nodeMap, edge.reactId) && _.has(nodeMap, edge.depOnReactId))
      ) {
        delete self.edgesUnique[key];
      }
    });

    return this;
  }

  addEntry(data) {
    if (data.reactId === "rNoCtx") {
      return;
    }

    let node, lastNodeId, edge;

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
        node = this.nodes[data.reactId];
        node.value = data.value;
        node.valueChangedStatus.setActiveAtStep(data.step);
        break;

      case "invalidateStart":
        node = this.nodes[data.reactId];
        lastNodeId = _.last(this.activeInvalidateEnter);
        if (lastNodeId) {
          this.nodes[lastNodeId].invalidateStatus.resetActive();
        }
        this.activeInvalidateEnter.push(data.reactId);
        switch (node.type) {
          case "observable":
          case "observer":
            node.invalidateStatus.setActiveAtStep(data.step);
            break;
        }
        node.statusAdd(data);
        break;
      case "enter":
        lastNodeId = _.last(this.activeNodeEnter);
        if (lastNodeId) {
          this.nodes[lastNodeId].enterStatus.resetActive();
        }
        this.activeNodeEnter.push(data.reactId);
        node = this.nodes[data.reactId];
        node.enterStatus.setActiveAtStep(data.step);
        switch (node.type) {
          case "observer":
          case "observable":
            node.invalidateStatus.reset();
        }
        node.statusAdd(data);
        break;

      case "isolateInvalidateStart":
      case "isolateEnter":
        this.nodes[data.reactId].statusAdd(data);
        break;

      case "invalidateEnd":
      case "exit":
      case "isolateExit":
      case "isolateInvalidateEnd": {
        node = this.nodes[data.reactId];
        switch (data.action) {
          case "exit":
            this.nodes[_.last(this.activeNodeEnter)].enterStatus.reset();
            this.activeNodeEnter.pop();
            lastNodeId = _.last(this.activeNodeEnter);
            if (lastNodeId) {
              this.nodes[lastNodeId].enterStatus.setActiveAtStep(data.step);
            }
            break;
          case "invalidateEnd":
            // turn off the previously active node
            this.nodes[
              _.last(this.activeInvalidateEnter)
            ].invalidateStatus.resetActive();
            this.activeInvalidateEnter.pop();
            // if another invalidateStart node exists...
            //   set the previous invalidateStart node to active
            lastNodeId = _.last(this.activeInvalidateEnter);
            if (lastNodeId) {
              this.nodes[lastNodeId].invalidateStatus.setActiveAtStep(
                data.step
              );
            }
            node.invalidateStatus.toFinished();
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
        let prevData = node.statusLast();
        let expectedAction = {
          exit: "enter",
          isolateExit: "isolateEnter",
          invalidateEnd: "invalidateStart",
          isolateInvalidateEnd: "isolateInvalidateStart",
        }[data.action];
        StatusArr.expect_prev_status(data, prevData, expectedAction);
        node.statusRemove();
        break;
      }

      case "dependsOn": {
        edge = new Edge(data);
        let edgeKey = edge.key;

        // store unique edges to always display a transparent dependency
        if (!_.has(this.edgesUnique, edge.ghostKey)) {
          this.edgesUnique[edge.ghostKey] = new GhostEdge(data);
        }

        if (!_.has(this.edges, edgeKey)) {
          this.edges[edgeKey] = edge;
        } else {
          edge = this.edges[edgeKey];
        }

        if (this.nodes[edge.reactId].statusLast().action === "isolateEnter") {
          edge.status = "isolate";
        } else {
          edge.status = "normal";
        }
        break;
      }

      case "dependsOnRemove":
        edge = new Edge(data);
        // remove the edge
        delete this.edges[edge.key];
        break;

      case "queueEmpty":
      case "asyncStart":
      case "asyncStop":
        this[data.action] = data.step;
        break;

      default:
        console.error("data.action: ", data.action, data);
        throw data;
    }
  }
}

export default Graph;
