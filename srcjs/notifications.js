exports.notifications = (function() {

  // Milliseconds to fade in or out
  const fadeDuration = 250;

  function show({ html=null, duration=null, id=null } = {}) {
    if (!id)
      id = randomId();

    // Create panel if necessary
    _createPanel();
    showPanel(fadeDuration);

    // Get existing DOM element for this ID, or create if needed.
    let $notification = get(id);
    if ($notification.length === 0)
      $notification = _create(id);

    // Set HTML content
    if (html) {
      $notification
        .html(html)
        .fadeIn(fadeDuration);
    }

    // If duration was provided, schedule removal. If not, clear existing
    // removal callback (this happens if a message was first added with
    // a duration, and then updated with no duration).
    if (duration)
      _addRemovalCallback(id, duration);
    else
      _clearRemovalCallback(id)

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

  function hidePanel() {
    _getPanel().fadeOut(fadeDuration);
  }

  function showPanel() {
    _getPanel().fadeIn(fadeDuration);
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
    return $('#shiny-notifications-panel');
  }

  // Create notifications panel and return the jQuery object. If the DOM
  // element already exists, just return it.
  function _createPanel() {
    let $panel = _getPanel();

    if ($panel.length === 0) {
      $panel = $('<div id="shiny-notifications-panel">');
      $('body').append($panel);
    }

    return $panel;
  }

  // Create a notification DOM element and return the jQuery object. If the
  // DOM element already exists for the ID, just return it without creating.
  function _create(id, hidden = true) {
    let $notification = get(id);

    if ($notification.length === 0) {
      $notification = $(`<div id="shiny-notification-${id}" class="shiny-notification">`);
      if (hidden) {
        $notification.hide();
      }
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
    hidePanel,
    showPanel,
    get,
    ids
  };
})();
