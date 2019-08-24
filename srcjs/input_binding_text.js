var textInputBinding = new InputBinding();
$.extend(textInputBinding, {
  find: function(scope) {
    var $inputs = $(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
    // selectize.js 0.12.4 inserts a hidden text input with an
    // id that ends in '-selectized'. The .not() selector below
    // is to prevent textInputBinding from accidentally picking up
    // this hidden element as a shiny input (#2396)
    return $inputs.not('input[type="text"][id$="-selectized"]');
  },
  getId: function(el) {
    return InputBinding.prototype.getId.call(this, el) || el.name;
  },
  getValue: function(el) {
    return el.value;
  },
  setValue: function(el, value) {
    el.value = value;
  },
  subscribe: function(el, callback) {
    $(el).on('keyup.textInputBinding input.textInputBinding', function(event) {
      callback(true);
    });
    $(el).on('change.textInputBinding', function(event) {
      callback(false);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.textInputBinding');
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('value'))
      this.setValue(el, data.value);

    updateLabel(data.label, this._getLabelNode(el));

    if (data.hasOwnProperty('placeholder'))
      el.placeholder = data.placeholder;

    $(el).trigger('change');
  },
  getState: function(el) {
    return {
      label: this._getLabelNode(el).text(),
      value: el.value,
      placeholder: el.placeholder
    };
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },
  _getLabelNode: function(el) {
    return $(el).parent().find('label[for="' + $escape(el.id) + '"]');
  }
});
inputBindings.register(textInputBinding, 'shiny.textInput');
