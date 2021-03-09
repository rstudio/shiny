// nav-navtree is built on a combination of Bootstrap's tab &
// collapse components, but the tab component isn't smart enough to
// know about the deactive when are activated. Note that this logic also
// exists in input_binding_tabinput.js and is repeated here in case this
// component wants to be statically rendered
$(document).on("shown.bs.tab", ".nav-navtree", function(e) {
  var tgt = $(e.target);
  var nav =  tgt.parents(".nav-navtree");
  nav.find("li").not(tgt).removeClass("active"); // BS3
  nav.find("li > a").not(tgt).removeClass("active"); // BS4
});
