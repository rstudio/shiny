var passwordInputBinding = {};
$.extend(passwordInputBinding, textInputBinding, {
  find: function(scope) {
    if (exports.bindGenericInputs) {
      return $(scope).find('input[type="password"]');
    } else {
      return $(scope).find('input[type="password"].shiny-input-password');
    }
  },
  getType: function(el) {
    return "shiny.password";
  }
});
inputBindings.register(passwordInputBinding, 'shiny.passwordInput');
