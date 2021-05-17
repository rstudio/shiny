import { init } from "./initialize";
import { Shiny } from "./shiny";

import { main } from "./main";

init();

main();

window.console.log("Shiny version: ", Shiny.version);
