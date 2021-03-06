// nav-navtree is built on a combination of Bootstrap's tab &
// collapse components, but the tab component isn't smart enough to
// know about the deactive when are activated. Note that this logic is
// very similar to shinydashboard's deactivateOtherTabs() (in tab.js)
$(document).on("shown.bs.tab", ".nav-navtree", function(e) {
  var tgt = $(e.target);
  var nav =  tgt.parents(".nav-navtree");
  tgt.tab("show");
  nav.find("li").not(tgt).removeClass("active"); // BS3
  nav.find("li > a").not(tgt).removeClass("active"); // BS4
});
