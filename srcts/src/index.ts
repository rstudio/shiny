import { init } from "./initialize";
import { Shiny } from "./shiny";

import { main } from "./main";

init();

main();

// Set Shiny globally
window["Shiny"] = Shiny;

window.console.log("Shiny version: ", Shiny.version);
