import { imageOutputBinding } from "../bindings/output/image";

function resetBrush(brushId) {
  Shiny.setInputValue(brushId, null);
  imageOutputBinding.find(document).trigger("shiny-internal:brushed", {
    brushId: brushId,
    outputId: null,
  });
}

export { resetBrush };
