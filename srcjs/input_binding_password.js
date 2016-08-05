var passwordInputBinding = {};
$.extend(passwordInputBinding, textInputBinding, {
  find: function(scope) {
    return $(scope).find('input[type="password"]');
  },
  getType: function(el) {
    return "shiny.password";
  }
});
inputBindings.register(passwordInputBinding, 'shiny.passwordInput');
