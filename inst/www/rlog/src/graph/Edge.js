// @flow

import { HoverStatus } from "./HoverStatus";

import type { ReactIdType, CtxIdType } from "../log/logStates";

class Edge {
  reactId: ReactIdType;
  depOnReactId: ReactIdType;
  ctxId: CtxIdType;
  session: string;
  time: number;
  status: string;
  isGhost: boolean;
  hoverStatus: HoverStatus;

  constructor(data: EdgeInputType) {
    if (typeof data.reactId === "undefined")
      throw "data.reactId not provided to new Edge()";
    if (typeof data.depOnReactId === "undefined")
      throw "data.depOnReactId not provided to new Edge()";
    if (typeof data.ctxId === "undefined")
      throw "data.ctxId not provided to new Edge()";
    if (typeof data.time === "undefined")
      throw "data.time not provided to new Edge()";
    this.reactId = data.reactId;
    this.depOnReactId = data.depOnReactId;
    this.ctxId = data.ctxId;
    this.session = data.session || "Global";
    this.time = data.time;
    this.status = "normal";
    this.isGhost = false;
    this.hoverStatus = data.hoverStatus || new HoverStatus();
  }
  get id(): EdgeIdType {
    return `${this.reactId}_${this.depOnReactId}_${this.ctxId}`.replace(
      /\$/g,
      "_"
    );
  }
  get source(): GraphNodeKeyType {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target(): GraphNodeKeyType {
    return this.reactId.replace(/\$/g, "_");
  }
  get key(): EdgeKeyType {
    return `${this.reactId} depends on ${this.depOnReactId} in ${this.ctxId}`;
  }
  get ghostKey(): EdgeKeyType {
    return `${this.reactId} depends on ${this.depOnReactId}`;
  }
  get hoverKey(): EdgeHoverKeyType {
    return this.ghostKey;
  }
  get inIsolate(): boolean {
    return this.status === "isolate";
  }
  get cytoStyle(): Object {
    return {};
  }
  get cytoClasses(): string {
    let classes = [];
    if (this.inIsolate) classes.push("edgeIsolate");
    switch (this.hoverStatus.state) {
      case HoverStatus.valFocused:
        break;
      case HoverStatus.valNotFocused:
        if (this.hoverStatus.sticky) {
          classes.push("hoverNotFocusedButSticky");
        } else {
          classes.push("hoverNotFocused");
        }
        break;
    }
    if (this.hoverStatus.selected) classes.push("edgeSelected");
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

type EdgeKeyType = string;
type EdgeHoverKeyType = string;
type EdgeIdType = string;

type GraphNodeKeyType = string;

type EdgeInputType = EdgeLikeType & {
  ctxId: CtxIdType,
};

type EdgeLikeType = {
  reactId: ReactIdType,
  depOnReactId: ReactIdType,
  session: ?string,
  time: number,
  hoverStatus?: ?HoverStatus,
};

export { Edge };

export type {
  EdgeLikeType,
  EdgeIdType,
  EdgeKeyType,
  EdgeHoverKeyType,
  GraphNodeKeyType,
};
