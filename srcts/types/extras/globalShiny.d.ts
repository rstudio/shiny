import type { ShinyClass } from "../src/shiny/index";
declare global {
    interface Window {
        Shiny: ShinyClass;
    }
}
