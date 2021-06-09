function windowShiny(): any {
  // immediately init shiny on the window
  if (!(window as any)["Shiny"]) {
    (window as any)["Shiny"] = {};
  }
  return (window as any)["Shiny"];
}

export { windowShiny };
