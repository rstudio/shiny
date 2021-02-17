import "babel-polyfill";

import { main as jquery_main } from "./jquery";
import { main as history_main } from "./window/history";
import { Shiny } from "./shiny";

import { main } from "./main";

history_main();
jquery_main();

main();

window.console.log("Shiny version: ", Shiny.VERSION);
