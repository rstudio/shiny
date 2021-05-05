var actionButtonInputBinding = new InputBinding();
$.extend(actionButtonInputBinding, {
  find: function(scope) {
    return $(scope).find(".action-button");
  },
  getValue: function(el) {
    return $(el).data('val') || 0;
  },
  setValue: function(el, value) {
    $(el).data('val', value);
  },
  getType: function(el) {
    return 'shiny.action';
  },
  subscribe: function(el, callback) {
    $(el).on("click.actionButtonInputBinding", function(e) {
      var $el = $(this);
      var val = $el.data('val') || 0;
      $el.data('val', val + 1);

      callback();
    });
  },
  getState: function(el) {
    return { value: this.getValue(el) };
  },
  receiveMessage: function(el, data) {
    var $el = $(el);

    // retrieve current label and icon
    var label = $el.text();
    var icon = '';

    // to check (and store) the previous icon, we look for a $el child
    // object that has an i tag, and some (any) class (this prevents
    // italicized text - which has an i tag but, usually, no class -
    // from being mistakenly selected)
    if ($el.find('i[class]').length > 0) {
      var icon_html = $el.find('i[class]')[0];
      if (icon_html === $el.children()[0]) {     // another check for robustness
        icon = $(icon_html).prop('outerHTML');
      }
    }

    // update the requested properties
    if (data.hasOwnProperty('label')) label = data.label;
    if (data.hasOwnProperty('icon')) {
      icon = data.icon;
      // if the user entered icon=character(0), remove the icon
      if (icon.length === 0) icon = '';
    }

    // produce new html
    $el.html(icon + ' ' + label);
  },
  unsubscribe: function(el) {
    $(el).off(".actionButtonInputBinding");
  }
});
inputBindings.register(actionButtonInputBinding, 'shiny.actionButtonInput');


$(document).on('click', 'a.action-button', function(e) {
  e.preventDefault();
});
