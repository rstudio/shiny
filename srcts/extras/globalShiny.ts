// Type definitions for @types-rstudio/shiny
// Project: Shiny <https://shiny.rstudio.com/>
// Definitions by: RStudio <https://www.rstudio.com/>

import type { Shiny as ShinyClass } from "../src/shiny/index";

declare global {
  // Tell Shiny variable globally exists
  // eslint-disable-next-line @typescript-eslint/naming-convention
  const Shiny: ShinyClass;

  // Tell window.Shiny exists
  interface Window {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Shiny: ShinyClass;
  }
}
