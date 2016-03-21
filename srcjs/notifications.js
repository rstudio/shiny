exports.notifications = (function() {

  // Milliseconds to fade in or out
  const fadeDuration = 250;

  function show({ html=null, duration=5000, id=null, closeButton=true } = {}) {
    if (!id)
      id = randomId();

    // Create panel if necessary
    _createPanel();

    // Get existing DOM element for this ID, or create if needed.
    let $notification = get(id);
    if ($notification.length === 0)
      $notification = _create(id);

    // Set HTML content
    if (html) {
      $notification
        .find('.shiny-notification-content')
        .html(html)
        .fadeIn(fadeDuration);
    }

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
    get(id).fadeOut(fadeDuration, function() {

      this.remove();

      // If no more notifications, remove the panel from the DOM.
      if (ids().length === 0) {
        _getPanel().remove();
      }
    });
  }

  function removeAll() {
    ids().forEach(id => remove(id));
  }

  // Returns an individual notification DOM object (wrapped in jQuery).
  function get(id) {
    return _getPanel().find('#shiny-notification-' + id);
  }

  // Return array of all notification IDs
  function ids() {
    const $notifications = _getPanel().find('.shiny-notification');
    return $notifications
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

    $('body').append('<div id="shiny-notification-panel">');

    return $panel;
  }

  // Create a notification DOM element and return the jQuery object. If the
  // DOM element already exists for the ID, just return it without creating.
  function _create(id) {
    let $notification = get(id);

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
    get(id).data('removalCallback', removalCallback);
  }

  // Clear a removal callback from a notification, if present.
  function _clearRemovalCallback(id) {
    const $notification = get(id);
    const oldRemovalCallback = $notification.data('removalCallback');
    if (oldRemovalCallback) {
      clearTimeout(oldRemovalCallback);
    }
  }

  return {
    show,
    remove,
    removeAll,
    get,
    ids
  };
})();
