var page = require('webpage').create();
setTimeout(function() {
  page.open('http://localhost:8765', function(status) {
    setTimeout(function() {
      phantom.exit();
    }, 1000);
  });
}, 1000);
