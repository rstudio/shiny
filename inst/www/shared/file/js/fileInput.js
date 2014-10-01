var SITE = SITE || {};

SITE.fileInputs = function() {
  var $this = $(this),
  $val = $this.val(),
  valArray = $val.split('\\'),
  newVal = valArray[valArray.length-1],
  $button = $this.siblings('.browse-btn'),
  $fakeFile = $this.siblings('.file-holder');
  if(newVal !== '') {
    $fakeFile.text(newVal);
  }
};

$(document).ready(function() {
  $('.file-wrapper input[type=file]').bind('change focus click', SITE.fileInputs);
});
