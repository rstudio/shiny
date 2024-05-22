import type { Shiny as ShinyClass } from "../src/shiny/index";
declare global {
    const Shiny: ShinyClass;
    interface Window {
        Shiny: ShinyClass;
    }
    type Shiny = ShinyClass;
}
