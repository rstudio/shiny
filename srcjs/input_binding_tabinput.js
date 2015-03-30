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
    var self = this;
    var anchors = $(el).find('li:not(.dropdown)').children('a');
    anchors.each(function() {
      if (self._getTabName($(this)) === value) {
        $(this).tab('show');
        return false;
      }
    });
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);
  },
  subscribe: function(el, callback) {
    $(el).on('shown.bootstrapTabInputBinding shown.bs.tab.bootstrapTabInputBinding', function(event) {
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
