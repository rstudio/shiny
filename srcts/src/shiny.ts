import { $escape, compareVersion } from "./utils";

interface ShinyType {
  version: string;
  $escape: any;
  compareVersion: any;
}

let Shiny: ShinyType;

function setShiny(Shiny_: ShinyType): void {
  Shiny = Shiny_;

  // `process.env.SHINY_VERSION` is overwritten to the Shiny version at build time.
  // During testing, the `Shiny.version` will be `"development"`
  Shiny.version = process.env.SHINY_VERSION || "development";

  Shiny.$escape = $escape;
  Shiny.compareVersion = compareVersion;
}

export { Shiny, setShiny };

export type { ShinyType };
