$(document).on('keydown', function(e) {
  if (e.which !== 114 || (!e.ctrlKey && !e.metaKey) || (e.shiftKey || e.altKey))
    return;
  var url = 'reactlog?w=' + window.escape(exports.shinyapp.config.workerId) +
    "&s=" + window.escape(exports.shinyapp.config.sessionId);
  window.open(url);
  e.preventDefault();
});

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

