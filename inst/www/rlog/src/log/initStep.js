// @flow

// initialize all log entries to have a step value

import _ from "lodash";

_.map(window.log, function(entry, i: number) {
  entry.step = i;
});
