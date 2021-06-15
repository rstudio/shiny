import type { ShinyType } from "../shiny";

function windowShiny(): ShinyType | null {
  // Use `unknown` type as we know what we are doing is _dangerous_
  // Immediately init shiny on the window
  if (!(window as unknown)["Shiny"]) {
    (window as unknown)["Shiny"] = {};
  }
  return (window as unknown)["Shiny"];
}

export { windowShiny };
