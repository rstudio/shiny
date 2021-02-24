function windowShiny(): any {
  return (window as any)["Shiny"] || {};
}

export { windowShiny };
