var textOutputBinding = new OutputBinding();
$.extend(textOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-text-output');
  },
  renderValue: function(el, data) {
    $(el).text(data);
  }
});
outputBindings.register(textOutputBinding, 'shiny.textOutput');

