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
    if ($el.find('i.fa, i.glyphicon').length > 0) {
      var icon_html = $el.find('i.fa, i.glyphicon')[0];
      icon = $(icon_html).prop('outerHTML');
    }

    // update the requested properties
    if (data.hasOwnProperty('label')) label = data.label;
    if (data.hasOwnProperty('icon')) {
      icon = data.icon;
      // if the user entered icon=icon("") to remove it
      if (icon==='<i class="fa fa-"></i>') icon = '';
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
