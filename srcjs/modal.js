exports.modal = {

  show: function({ html='', deps=[] } = {}) {
    // Get existing DOM element for this ID, or create if needed.
    let $modal = $('#shiny-modal-wrapper');
    if ($modal.length === 0) {
      $modal = $('<div id="shiny-modal-wrapper"></div>');
      $('body').append($modal);
    }

    // When the inner modal is hidden, remove the entire thing, including
    // wrapper. This is meant to work with Bootstrap modal dialogs.
    $modal.on('hidden.bs.modal', function() {
      exports.unbindAll($modal);
      $modal.remove();
    });

    // Set/replace contents of wrapper with html.
    exports.renderContent($modal, { html: html, deps: deps });
  },

  remove: function() {
    const $modal = $('#shiny-modal-wrapper');

    // Look for a Bootstrap modal dialog inside the modal wrapper
    const $bsmodal = $modal.find(".modal");

    if ($bsmodal.length > 0) {
      // If the modal wrapper contains a Bootstrap modal dialog, trigger hide
      // event. This will trigger the hidden.bs.modal callback taht we set in
      // show(), which unbinds and removes the element.
      $bsmodal.modal('hide');

    } else {
      // If not a Bootstrap modal dialog, simply unbind and remove it.
      exports.unbindAll($modal);
      $modal.remove();
    }
  }
};
