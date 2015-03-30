var textareaInputBinding = {};
$.extend(textareaInputBinding, textInputBinding, {
  find: function(scope) {
    return $(scope).find('textarea');
  }
});
inputBindings.register(textareaInputBinding, 'shiny.textareaInput');
