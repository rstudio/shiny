import type { Shiny as RStudioShiny } from "../src/shiny/index";
declare global {
    const Shiny: RStudioShiny;
    interface Window {
        Shiny: RStudioShiny;
    }
    type Shiny = RStudioShiny;
}
