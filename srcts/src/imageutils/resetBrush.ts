import { imageOutputBinding } from "../bindings/output/image";
import { shinySetInputValue } from "../shiny/init";

function resetBrush(brushId: string): void {
  shinySetInputValue(brushId, null);
  imageOutputBinding
    .find(document.documentElement)
    .trigger("shiny-internal:brushed", {
      brushId: brushId,
      outputId: null,
    });
}

export { resetBrush };
