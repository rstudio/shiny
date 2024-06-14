// Type definitions for @types-rstudio/shiny
// Project: Shiny <https://shiny.rstudio.com/>
// Definitions by: RStudio <https://www.rstudio.com/>

import type { ShinyClass } from "../src/shiny/index";

declare global {
  // Tell window.Shiny exists
  interface Window {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Shiny: ShinyClass;
  }
}
