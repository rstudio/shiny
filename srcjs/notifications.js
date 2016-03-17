exports.Notifications = (function() {

  function add(note, duration = 250) {
    const id = randomId();

    // Create panel if necessary
    _createPanel();

    // Create DOM element
    _create(id).html(note.html);

    show(id, duration);
    return id;
  }

  function update(id, note) {
    get(id).html(note.html);
  }

  function remove(id, duration = 250) {
    get(id).fadeOut(duration, function() { this.remove(); } );
  }

  function show(id, duration = 250) {
    showPanel(duration);
    get(id).fadeIn(duration);
  }

  function hide(id, duration = 250) {
    get(id).fadeOut(duration);
  }

  function removeAll(duration = 250) {
    _getPanel().find('.shiny-notification')
      .fadeOut(duration, function() { this.remove(); } );
  }

  function hideAll(duration = 250) {
    _getPanel().find('.shiny-notification').fadeOut(duration);
  }

  function showAll(duration = 250) {
    _getPanel().find('.shiny-notification').fadeIn(duration);
  }

  function hidePanel(duration = 250) {
    _getPanel().fadeOut(duration);
  }

  function showPanel(duration = 250) {
    _getPanel().fadeIn(duration);
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
