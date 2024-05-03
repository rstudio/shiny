// This of this like the .shiny-busy class that shiny.js puts on the root element,
// except it's added before shiny.js is initialized, connected, etc.
document.documentElement.classList.add("shiny-not-yet-idle");
$(document).one("shiny:idle", function () {
  document.documentElement.classList.remove("shiny-not-yet-idle");
});
