let jQuery: JQueryStatic;

function setJQuery(jQuery_: JQueryStatic): void {
  jQuery = jQuery_;
}

export { jQuery, jQuery as $, setJQuery };
