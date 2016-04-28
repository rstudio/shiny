$(document).on('keydown', function(e) {
  if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
    return;
  var url = 'reactlog?w=' + window.escape(exports.shinyapp.config.workerId) +
    "&s=" + window.escape(exports.shinyapp.config.sessionId);
  window.open(url);
  e.preventDefault();
});

// Turns out that Firefox does not support insertAdjacentElement().
// So we have to implement our own version for insertUI.
// Code adapted from here: http://forums.mozillazine.org/viewtopic.php?t=445587
HTMLElement.prototype.insertAdjacentElement = function(where, parsedNode) {
  switch (where) {
    case 'beforeBegin':
      this.parentNode.insertBefore(parsedNode, this);
      break;
    case 'afterBegin':
      this.insertBefore(parsedNode, this.firstChild);
      break;
    case 'beforeEnd':
      this.appendChild(parsedNode);
      break;
    case 'afterEnd':
      if (this.nextSibling) {
        this.parentNode.insertBefore(parsedNode, this.nextSibling);
      }
      else {
        this.parentNode.appendChild(parsedNode);
      }
      break;
  }
};

exports.addCustomMessageHandler("shiny-insert-ui",
  function(message) {
    let targets = $(message.selector);
    if (targets.length === 0) {
      // render the HTML and deps to a null target, so
      // the side-effect of rendering the deps, singletons,
      // and <head> still occur
      exports.renderHtml(
        $([]),
        message.content.html,
        message.content.deps
      );
    } else {
      targets.each((i, target) => {
        let container = document.createElement(message.container);
        target.insertAdjacentElement(message.where, container);
        exports.renderContent(container, message.content);
        $(container).trigger("shown");
        return message.multiple;
      });
    }
  }
);

exports.addCustomMessageHandler("shiny-remove-ui",
  function(message) {
    let els = $(message.selector);
    els.each((i, el) => {
      $(el).remove();
      return message.multiple;
    });
  }
);

