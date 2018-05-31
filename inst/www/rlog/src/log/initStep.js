// @flow

// initialize all log entries to have a step value

import _ from "lodash";

import type { LogType } from "./logStates";

if (window.__APP_DATA__) {
  window.log = window.__APP_DATA__;
}

_.map((window.log: LogType), function(entry, i: number) {
  entry.step = i;
});
