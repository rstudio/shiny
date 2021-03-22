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


// Trigger shiny:filedownload event whenever a downloadButton/Link is clicked
$(document).on('click.shinyDownloadLink', 'a.shiny-download-link', function(e) {
  var evt = jQuery.Event('shiny:filedownload');
  evt.name = this.id;
  evt.href = this.href;
  $(document).trigger(evt);
});
