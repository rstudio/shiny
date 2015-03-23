
  var numberInputBinding = {};
  $.extend(numberInputBinding, textInputBinding, {
    find: function(scope) {
      return $(scope).find('input[type="number"]');
    },
    getValue: function(el) {
      var numberVal = $(el).val();
      if (/^\s*$/.test(numberVal))  // Return null if all whitespace
        return null;
      else if (!isNaN(numberVal))   // If valid Javascript number string, coerce to number
        return +numberVal;
      else
        return numberVal;           // If other string like "1e6", send it unchanged
    },
    setValue: function(el, value) {
      el.value = value;
    },
    getType: function(el) {
      return "shiny.number";
    },
    receiveMessage: function(el, data) {
      if (data.hasOwnProperty('value'))  el.value = data.value;
      if (data.hasOwnProperty('min'))    el.min   = data.min;
      if (data.hasOwnProperty('max'))    el.max   = data.max;
      if (data.hasOwnProperty('step'))   el.step  = data.step;

      if (data.hasOwnProperty('label'))
        $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(data.label);

      $(el).trigger('change');
    },
    getState: function(el) {
      return { label: $(el).parent().find('label[for="' + $escape(el.id) + '"]').text(),
               value: this.getValue(el),
               min:   Number(el.min),
               max:   Number(el.max),
               step:  Number(el.step) };
    }
  });
  inputBindings.register(numberInputBinding, 'shiny.numberInput');
