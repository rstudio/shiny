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


    var labelTag = $(el).parent().find('label[for="' + $escape(el.id) + '"]');
    var hasLabelTag = labelTag.length > 0;

    // If data.label exists, then we may need to insert a label
    // tag into the DOM. If it doesn't exist, then the label tag
    // should be removed to be more consistent with the behavior
    // of `textInput(label = NULL)`
    if (data.hasOwnProperty('label')) {

      if (hasLabelTag) {
        labelTag.text(data.label);
      } else {
        $('<label for="' + $escape(el.id) + '"></label>').text(data.label).insertBefore(el);
      }

    } else {

      if (hasLabelTag) labelTag.remove();

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
