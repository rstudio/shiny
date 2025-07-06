function disableDrag(
  $el: JQuery<HTMLElement>,
  $img: JQuery<HTMLElement>,
): void {
  // Make image non-draggable (Chrome, Safari)
  $img.css("-webkit-user-drag", "none");

  // Firefox, IE<=10
  // First remove existing handler so we don't keep adding handlers.
  $img.off("dragstart.image_output");
  $img.on("dragstart.image_output", function () {
    return false;
  });

  // Disable selection of image and text when dragging in IE<=10
  $el.off("selectstart.image_output");
  $el.on("selectstart.image_output", function () {
    return false;
  });
}

export { disableDrag };
