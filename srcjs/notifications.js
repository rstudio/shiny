exports.notifications = (function() {

  // Milliseconds to fade in or out
  const fadeDuration = 250;

  function show({ html='', action='', deps=[], duration=5000,
                  id=null, closeButton=true, type=null } = {})
  {
    if (!id)
      id = randomId();

    // Create panel if necessary
    _createPanel();

    // Get existing DOM element for this ID, or create if needed.
    let $notification = _get(id);
    if ($notification.length === 0)
      $notification = _create(id);

    // Render html and dependencies
    const newHtml = `<div class="shiny-notification-content-text">${html}</div>` +
                    `<div class="shiny-notification-content-action">${action}</div>`;
    const $content = $notification.find('.shiny-notification-content');
    exports.renderContent($content, { html: newHtml, deps: deps });

    // Remove any existing classes of the form 'shiny-notification-xxxx'.
    // The xxxx would be strings like 'warning'.
    const classes = $notification.attr('class')
      .split(/\s+/)
      .filter(cls => cls.match(/^shiny-notification-/))
      .join(' ');
    $notification.removeClass(classes);

    // Add class. 'default' means no additional CSS class.
    if (type && type !== 'default')
      $notification.addClass('shiny-notification-' + type);


    // Make sure that the presence/absence of close button matches with value
    // of `closeButton`.
    const $close = $notification.find('.shiny-notification-close');
    if (closeButton && $close.length === 0) {
      $notification.append('<div class="shiny-notification-close">&times;</div>');
    } else if (!closeButton && $close.length !== 0) {
      $close.remove();
    }

    // If duration was provided, schedule removal. If not, clear existing
    // removal callback (this happens if a message was first added with
    // a duration, and then updated with no duration).
    if (duration)
      _addRemovalCallback(id, duration);
    else
      _clearRemovalCallback(id);

    return id;
  }

  function remove(id) {
    _get(id).fadeOut(fadeDuration, function() {

      exports.unbindAll(this);
      $(this).remove();

      // If no more notifications, remove the panel from the DOM.
      if (_ids().length === 0) {
        _getPanel().remove();
      }
    });
  }

  // Returns an individual notification DOM object (wrapped in jQuery).
  function _get(id) {
    if (!id)
      return null;
    return _getPanel().find('#shiny-notification-' + $escape(id));
  }

  // Return array of all notification IDs
  function _ids() {
    return _getPanel()
      .find('.shiny-notification')
      .map(function() { return this.id.replace(/shiny-notification-/, ''); })
      .get();
  }

  // Returns the notification panel DOM object (wrapped in jQuery).
  function _getPanel() {
    return $('#shiny-notification-panel');
  }

  // Create notifications panel and return the jQuery object. If the DOM
  // element already exists, just return it.
  function _createPanel() {
    let $panel = _getPanel();

    if ($panel.length > 0)
      return $panel;

    $(document.body).append('<div id="shiny-notification-panel">');

    return $panel;
  }

  // Create a notification DOM element and return the jQuery object. If the
  // DOM element already exists for the ID, just return it without creating.
  function _create(id) {
    let $notification = _get(id);

    if ($notification.length === 0) {
      $notification = $(
        `<div id="shiny-notification-${id}" class="shiny-notification">` +
        '<div class="shiny-notification-close">&times;</div>' +
        '<div class="shiny-notification-content"></div>' +
        '</div>'
      );

      $notification.find('.shiny-notification-close').on('click', e => {
        e.preventDefault();
        e.stopPropagation();
        remove(id);
      });

      _getPanel().append($notification);
    }

    return $notification;
  }

  // Add a callback to remove a notification after a delay in ms.
  function _addRemovalCallback(id, delay) {
    // If there's an existing removalCallback, clear it before adding the new
    // one.
    _clearRemovalCallback(id);

    // Attach new removal callback
    const removalCallback = setTimeout(function() { remove(id); }, delay);
    _get(id).data('removalCallback', removalCallback);
  }

  // Clear a removal callback from a notification, if present.
  function _clearRemovalCallback(id) {
    const $notification = _get(id);
    const oldRemovalCallback = $notification.data('removalCallback');
    if (oldRemovalCallback) {
      clearTimeout(oldRemovalCallback);
    }
  }

  return {
    show,
    remove
  };
})();
