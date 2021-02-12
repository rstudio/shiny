import {readSync as dsfReadSync} from "readcontrol"
import {Shiny} from "./external/globals"

const VERSION = dsfReadSync("../../DESCRIPTION")

Shiny.VERSION = VERSION

export {Shiny}
