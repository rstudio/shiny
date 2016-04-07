var bookmarkOutputBinding = new OutputBinding();
$.extend(bookmarkOutputBinding, {
  find: function(scope) {
    return $(scope).find('.shiny-bookmark-output');
  },
  renderValue: function(el, data) {
    const $el = $(el);
    const $button = $el.find('button');

    // Initialize clipboardjs for this element, one time only
    if (!$button.data('clipboard-initialized')) {
      new Clipboard($button[0]);
      $button.data('clipboard-initialized', true);
    }

    $el.find('input')[0].value = location.protocol + '//' + location.host +
      location.pathname + '?' + data;
  }
});
outputBindings.register(bookmarkOutputBinding, 'shiny.bookmarkOutput');
