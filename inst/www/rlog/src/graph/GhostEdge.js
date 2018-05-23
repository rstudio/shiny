import HoverStatus from "./HoverStatus";

class GhostEdge {
  constructor(data) {
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
  get id() {
    return `${this.reactId}_${this.depOnReactId}`.replace(/\$/g, "_");
  }
  get source() {
    return this.depOnReactId.replace(/\$/g, "_");
  }
  get target() {
    return this.reactId.replace(/\$/g, "_");
  }
  get key() {
    return `${this.reactId} depends on ${this.depOnReactId}`;
  }
  get hoverKey() {
    return this.key;
  }
  get cytoStyle() {
    return {};
    // return graphStyles.ghostEdge.default
  }
  get cytoClasses() {
    let classes = ["edgeGhost"];
    switch (this.hoverStatus.state) {
      case HoverStatus.focused:
        break;
      case HoverStatus.notFocused:
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

export default GhostEdge;
