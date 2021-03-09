var bootstrapTabInputBinding = new InputBinding();
$.extend(bootstrapTabInputBinding, {
  find: function(scope) {
    return $(scope).find('ul.nav.shiny-tab-input');
  },
  getValue: function(el) {
    let anchor = isBS3() ?
      $(el).find('li:not(.dropdown).active > a'):
      $(el).find('.nav-link:not(.dropdown-toggle).active, .dropdown-menu > .dropdown-item.active');

    if (anchor.length === 1)
      return this._getTabName(anchor);

    return null;
  },
  setValue: function(el, value) {
    let self = this;
    let success = false;
    if (value) {
      let anchors = isBS3() ?
        $(el).find('li:not(.dropdown) > a') :
        $(el).find('.nav-link:not(.dropdown-toggle), .dropdown-menu > .dropdown-item');
      anchors.each(function() {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
          $(this).parents(".collapse").collapse('show');
          success = true;
          return false; // Break out of each()
        }
        return true;
      });
    }
    if (!success) {
      // This is to handle the case where nothing is selected, e.g. the last tab
      // was removed using removeTab.
      $(el).trigger("change");
    }
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);
    $(el).trigger("change");
  },
  subscribe: function(el, callback) {
    var deactivateOtherTabs = this._deactivateOtherTabs;
    $(el).on('change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function(event) {
      deactivateOtherTabs(event);
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.bootstrapTabInputBinding');
  },
  _getTabName: function(anchor) {
    return anchor.attr('data-value') || anchor.text();
  },
  // nav-navtree is built on a combination of Bootstrap's tab &
  // collapse components, but the tab component isn't smart enough to
  // know about the deactive when are activated. Note that this logic is
  // very similar to shinydashboard's deactivateOtherTabs() (in tab.js)
  _deactivateOtherTabs: function(event) {
    var tgt = $(event.target);
    var nav =  tgt.parents(".nav-navtree");
    nav.find("li").not(tgt).removeClass("active"); // BS3
    nav.find("li > a").not(tgt).removeClass("active"); // BS4
  }
});
inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');
