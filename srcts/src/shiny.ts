import {
  inputBindings,
  InputBinding,
  outputBindings,
  OutputBinding,
} from "./bindings";
import { $escape, compareVersion } from "./utils";

interface ShinyType {
  version: string;
  $escape: any;
  compareVersion: any;
  inputBindings: typeof inputBindings;
  InputBinding: typeof InputBinding;
  outputBindings: typeof outputBindings;
  OutputBinding: typeof OutputBinding;
}

let Shiny: ShinyType;

function setShiny(Shiny_: ShinyType): void {
  Shiny = Shiny_;

  // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
  // During testing, the `Shiny.version` will be `"development"`
  Shiny.version = process.env.SHINY_VERSION || "development";

  Shiny.$escape = $escape;
  Shiny.compareVersion = compareVersion;
  Shiny.inputBindings = inputBindings;
  Shiny.outputBindings = outputBindings;
}

export { Shiny, setShiny };

export type { ShinyType };
