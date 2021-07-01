// Type definitions for rstudio/shiny
// Project: Shiny <https://shiny.rstudio.com/>
// Definitions by: RStudio <https://www.rstudio.com/>

import type { Shiny } from "../src/shiny/index";

declare global {
  // eslint-disable-next-line @typescript-eslint/naming-convention
  const Shiny: Shiny;

  interface Window {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    Shiny: Shiny;
  }
}
