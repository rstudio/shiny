import type { ShinyClass } from "../src/shiny/index";
declare global {
    const Shiny: ShinyClass;
    interface Window {
        Shiny: ShinyClass;
    }
}
