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
  // eslint-disable-next-line @typescript-eslint/ban-ts-comment
  // @ts-ignore
  Shiny.version = SHINY_VERSION;

  Shiny.$escape = $escape;
  Shiny.compareVersion = compareVersion;
}

export { Shiny, setShiny };

export type { ShinyType };
