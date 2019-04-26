var textInputBinding = new InputBinding();
$.extend(textInputBinding, {
  find: function(scope) {
    return $(scope).find('input[type="text"], input[type="search"], input[type="url"], input[type="email"]');
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

    if (data.hasOwnProperty('label')) {
      var label = $(el).parent().find('label[for="' + $escape(el.id) + '"]');
      // If textInput(label=NULL), then no label tag is provided from the
      // server, so create one if we need to.
      if (label.length === 0) {
        $('<label for="' + $escape(el.id) + '">' + data.label + '</label>').insertBefore(el);
      } else {
        label.text(data.label);
      }
    }

    if (data.hasOwnProperty('placeholder'))
      el.placeholder = data.placeholder;

    $(el).trigger('change');
  },
  getState: function(el) {
    return {
      label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
      value: el.value,
      placeholder: el.placeholder
    };
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});
inputBindings.register(textInputBinding, 'shiny.textInput');
