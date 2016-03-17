exports.Notifications = (function() {

  // Milliseconds to fade in or out
  const fadeDuration = 250;

  function add({ html="", duration=null } = {}) {
    const id = randomId();

    // Create panel if necessary
    _createPanel();

    // Create DOM element
    _create(id).html(html);
    show(id);

    if (duration) {
      _addRemovalCallback(id, duration);
    }

    return id;
  }

  function update(id, { html="", duration=null } = {}) {
    if (duration)
      _addRemovalCallback(id, duration);

    if (html)
      get(id).html(html);
  }

  function remove(id) {
    get(id).fadeOut(fadeDuration, function() { this.remove(); } );
  }

  function show(id) {
    showPanel(fadeDuration);
    get(id).fadeIn(fadeDuration);
  }

  function hide(id) {
    get(id).fadeOut(fadeDuration);
  }

  function removeAll() {
    _getPanel().find('.shiny-notification')
      .fadeOut(fadeDuration, function() { this.remove(); } );
  }

  function hideAll() {
    _getPanel().find('.shiny-notification').fadeOut(fadeDuration);
  }

  function showAll() {
    _getPanel().find('.shiny-notification').fadeIn(fadeDuration);
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
    let $panel = $('#shiny-notifications-panel');

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
    add,
    update,
    remove,
    show,
    hide,
    removeAll,
    hideAll,
    showAll,
    hidePanel,
    showPanel,
    get,
    ids
  };
})();
