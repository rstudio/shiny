// @flow

import { rlog } from "../rlog";

let container: JQuery;

let updateLogEntry = function(): void {
  container.text(JSON.stringify(rlog.log[rlog.curTick], null, "  "));
};

let setContainer = function(container_: JQuery): void {
  container = container_;
};

export { updateLogEntry as update, setContainer };
