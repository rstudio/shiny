import { $escape, compareVersion } from "./utils";

interface ShinyType {
  version: string;
  $escape: any;
  compareVersion: any;
}

let Shiny: ShinyType;

function setShiny(Shiny_: ShinyType): void {
  Shiny = Shiny_;

  // Tell TS to ignore this line as the _true_ value is defined at compile time
  Shiny.version = process.env.SHINY_VERSION || "developement";

  Shiny.$escape = $escape;
  Shiny.compareVersion = compareVersion;
}

export { Shiny, setShiny };

export type { ShinyType };
