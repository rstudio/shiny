import type { Shiny } from "../shiny";

function windowShiny(): Shiny {
  // Use `any` type as we know what we are doing is _dangerous_
  // Immediately init shiny on the window
  if (!(window as any)["Shiny"]) {
    (window as any)["Shiny"] = {};
  }
  return (window as any)["Shiny"];
}

export { windowShiny };
