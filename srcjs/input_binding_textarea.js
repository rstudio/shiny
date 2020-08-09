var textareaInputBinding = {};
$.extend(textareaInputBinding, textInputBinding, {
  find: function(scope) {
    if (exports.bindGenericInputs) {
      return $(scope).find('textarea');
    } else {
      return $(scope).find('textarea.shiny-input-textarea');
    }
  }
});
inputBindings.register(textareaInputBinding, 'shiny.textareaInput');
