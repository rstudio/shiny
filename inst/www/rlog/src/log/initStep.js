// initialize all log entries to have a step value

import _ from "lodash";

_.map(window.log, function(entry, i) {
  entry.step = i;
});
