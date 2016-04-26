(function() {
  var bookmarkOutputBinding = new OutputBinding();

  function find(scope) {
    return $(scope).find('.shiny-bookmark-output');
  }

  function renderValue(el, data) {
    const $el = $(el);
    const $button = $el.find('button[data-clipboard-target]');

    // Initialize clipboardjs for this element, one time only
    if (!$button.data('clipboard-initialized')) {
      const clipboard = new Clipboard($button[0]);
      _initTooltip(clipboard, $button);
      $button.data('clipboard-initialized', true);
    }

    $el.find('input')[0].value = location.protocol + '//' + location.host +
      location.pathname + '?' + data;
  }

  function _initTooltip(clipboard, $el) {
    // When clipboard action occurs, show tooltip
    clipboard.on('success', function(e) {
      $el.tooltip({
        title: 'Copied!',
        placement: 'auto bottom',
        trigger: 'manual'
      });
      $el.tooltip('show');
      _scheduleTooltipHide($el);
    });

    // Backup tooltip for platforms that don't support copying to clipboard
    clipboard.on('error', function(e) {
      $el.tooltip({
        title: () => _fallbackMessage(e.action),
        placement: 'auto bottom',
        trigger: 'manual'
      });
      $el.tooltip('show');
      _scheduleTooltipHide($el);
    });
  }

  function _fallbackMessage(action) {
    var actionMsg = '';
    var actionKey = (action === 'cut' ? 'X' : 'C');

    if(/iPhone|iPad/i.test(navigator.userAgent)) {
      actionMsg = 'Not supported';
    } else if (/Mac/i.test(navigator.userAgent)) {
      actionMsg = 'Press ⌘-' + actionKey + ' to ' + action;
    } else {
      actionMsg = 'Press Ctrl-' + actionKey + ' to ' + action;
    }

    return actionMsg;
  }

  function _scheduleTooltipHide($el, delay = 2000) {
    setTimeout(
      () => $el.tooltip('hide'),
      delay
    );
  }


  $.extend(bookmarkOutputBinding, {
    find,
    renderValue
  });
  outputBindings.register(bookmarkOutputBinding, 'shiny.bookmarkOutput');
})();
