var checkboxInputBinding = new InputBinding();
$.extend(checkboxInputBinding, {
  find: function(scope) {
    return $(scope).find('input[type="checkbox"]');
  },
  getValue: function(el) {
    return el.checked;
  },
  setValue: function(el, value) {
    el.checked = value;
  },
  subscribe: function(el, callback) {
    $(el).on('change.checkboxInputBinding', function(event) {
      callback(true);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.checkboxInputBinding');
  },
  getState: function(el) {
    return {
      label: $(el).parent().find('span').text(),
      value: el.checked
    };
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      el.checked = data.value;

    // checkboxInput()'s label works different from other
    // input labels...the label container should always exist
    if (data.hasOwnProperty('label'))
      $(el).parent().find('span').text(data.label);

    $(el).trigger('change');
  }
});
inputBindings.register(checkboxInputBinding, 'shiny.checkboxInput');
