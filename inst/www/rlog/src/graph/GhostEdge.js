// @flow

import type {
  EdgeLikeType,
  EdgeIdType,
  EdgeHoverKeyType,
  EdgeKeyType,
  GraphNodeKeyType,
} from "./Edge";
import { HoverStatus } from "./HoverStatus";

class GhostEdge {
  reactId: string;
  depOnReactId: string;
  time: number;
  hoverStatus: HoverStatus;
  isGhost: boolean;
  session: string;

  constructor(data: EdgeLikeType) {
    if (typeof data.reactId === "undefined")
      throw "data.reactId not provided to new GhostEdge()";
    if (typeof data.depOnReactId === "undefined")
      throw "data.depOnReactId not provided to new GhostEdge()";
    if (typeof data.time === "undefined")
      throw "data.time not provided to new GhostEdge()";
    this.reactId = data.reactId;
    this.depOnReactId = data.depOnReactId;
    this.session = data.session || "Global";
    this.time = data.time;
    this.isGhost = true;
    this.hoverStatus = data.hoverStatus || new HoverStatus();
  }
  get id(): EdgeIdType {
    return `${this.reactId}_${this.depOnReactId}`.replace(/\$/g, "_");
  }
  get source(): GraphNodeKeyType {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target(): GraphNodeKeyType {
    return this.reactId.replace(/\$/g, "_");
  }
  get key(): EdgeKeyType {
    return `${this.reactId} depends on ${this.depOnReactId}`;
  }
  get hoverKey(): EdgeHoverKeyType {
    return this.key;
  }
  get cytoStyle() {
    return {};
    // return graphStyles.ghostEdge.default
  }
  get cytoClasses(): string {
    let classes = ["edgeGhost"];
    switch (this.hoverStatus.state) {
      case HoverStatus.valFocused:
        break;
      case HoverStatus.valNotFocused:
        if (this.hoverStatus.sticky) {
          classes.push("edgeGhostHoverNotFocusedButSticky");
        } else {
          classes.push("edgeGhostHoverNotFocused");
        }
        break;
    }
    if (this.hoverStatus.selected) classes.push("edgeGhostSelected");
    return classes.join(" ");
  }
  get cytoData() {
    let retData = this;
    return {
      group: "edges",
      data: retData,
    };
  }
}

export { GhostEdge };
