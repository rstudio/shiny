var downloadLinkOutputBinding = new OutputBinding();
$.extend(downloadLinkOutputBinding, {
  find: function(scope) {
    return $(scope).find('a.shiny-download-link');
  },
  renderValue: function(el, data) {
    $(el).attr('href', data);
  }
});
outputBindings.register(downloadLinkOutputBinding, 'shiny.downloadLink');
