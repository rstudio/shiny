import type { Shiny } from "../src/shiny/index";
declare global {
    const Shiny: Shiny;
    interface Window {
        Shiny: Shiny;
    }
}
