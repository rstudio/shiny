import { WindowShiny } from "./external/globals";
import { $escape, compareVersion } from "./utils";

// Tell TS to ignore this line as the _true_ value is defined at compile time
/* eslint "@typescript-eslint/ban-ts-comment": 0 */
// @ts-ignore
WindowShiny.VERSION = SHINY_VERSION;

WindowShiny.$escape = $escape;
WindowShiny.compareVersion = compareVersion;

export { WindowShiny as Shiny };
