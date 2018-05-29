// @flow

// initialize all log entries to have a step value

import _ from "lodash";

import type { LogType } from "./logStates";

_.map((window.log: LogType), function(entry, i: number) {
  entry.step = i;
});
