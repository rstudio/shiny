import { WindowShiny } from "./external/globals";
import {$escape, compareVersion} from './utils'

WindowShiny.VERSION = "1.6.0.9000"; // @VERSION@


WindowShiny.$escape = $escape
WindowShiny.compareVersion = compareVersion


export { WindowShiny as Shiny };
