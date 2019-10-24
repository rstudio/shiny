var bootstrapTabInputBinding = new InputBinding();
$.extend(bootstrapTabInputBinding, {
  find: function(scope) {
    return $(scope).find('ul.nav.shiny-tab-input');
  },
  getValue: function(el) {
    var anchor = $(el).find('li:not(.dropdown).active').children('a');
    if (anchor.length === 1)
      return this._getTabName(anchor);

    return null;
  },
  setValue: function(el, value) {
    let self = this;
    let success = false;
    if (value) {
      let anchors = $(el).find('li:not(.dropdown)').children('a');
      anchors.each(function() {
        if (self._getTabName($(this)) === value) {
          $(this).tab('show');
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
  },
  subscribe: function(el, callback) {
    $(el).on('change shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function(event) {
      let $target = $(event.target); // newly selected <a>
      let $oldTarget = $(event.relatedTarget); // previously selected <a>

      // Just in case we're running with Bootstrap 4. Unlike
      // bs3, bs4 puts the .active on the <a>, not the <li>
      // around the <a>. We need it to be on the <li> (it can
      // also be on the <a>) so that the tab manipulation
      // functions can work without major modifications.
      if (!$target.parent("li.active").length) {
        if ($oldTarget.length) {
          $oldTarget.parent("li").removeClass("active");
        } else {
          // There are cases where $oldTarget doesn't exist
          // (e.g., if a dropdown-item is already selected,
          // then a new dropdown-item is selected)
          $("#" + this.id).find("*").removeClass("active");
          $target.addClass("active");
        }
        $target.parent("li").addClass("active");
      }
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off('.bootstrapTabInputBinding');
  },
  _getTabName: function(anchor) {
    return anchor.attr('data-value') || anchor.text();
  }
});
inputBindings.register(bootstrapTabInputBinding, 'shiny.bootstrapTabInput');
