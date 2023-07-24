function ancestorBindSetting(el: HTMLElement): string {
  if (!(el instanceof HTMLElement)) return "eager";

  if (el.dataset.shinyBindInputs) {
    // The element itself has a bind setting, use that. It might be useful
    // TODO: should we only consider descendants, not current elements?
    return el.dataset.shinyBindInputs;
  }

  if (!document.querySelector("[data-shiny-bind-inputs]")) {
    // No bind settings declared in this document, everything is "eager"
    return "eager";
  }

  // Knowing we have some areas of the document with bind settings, we then
  // focus on those areas. We're trying to avoid many calls to .closest() and
  // only falling back to DOM traversal when necessary and in a limited scope.
  const setAreas = Array.from(
    document.querySelectorAll("[data-shiny-bind-inputs]")
  ) as HTMLElement[];

  for (const area of setAreas) {
    if (area.contains(el)) {
      const ancestorBindSetting = el.closest(
        "[data-shiny-bind-inputs]"
      ) as HTMLElement;

      if (!(ancestorBindSetting instanceof HTMLElement)) continue;
      if (ancestorBindSetting.dataset.shinyBindInputs) {
        return ancestorBindSetting.dataset.shinyBindInputs as string;
      }
    }
  }

  return "eager";
}

function filterBindingMatchesAllowedOnly(
  matches: JQuery<HTMLElement>
): JQuery<HTMLElement> {
  return matches.filter(function () {
    return ancestorBindSetting(this) !== "none";
  });
}

function filterBindingMatchesIfStrict(
  matches: JQuery<HTMLElement>,
  inputClass: string
): JQuery<HTMLElement> {
  return matches.filter(function () {
    const bindSetting = ancestorBindSetting(this);
    if (bindSetting === "eager") return true;
    if (bindSetting === "strict") return this.classList.contains(inputClass);
    return false;
  });
}

export { filterBindingMatchesAllowedOnly, filterBindingMatchesIfStrict };
