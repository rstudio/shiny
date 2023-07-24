function ancestorBindSetting(el: HTMLElement): string {
  if (!(el instanceof HTMLElement)) return "eager";

  if (el.dataset.shinyBindInputs) {
    return el.dataset.shinyBindInputs;
  }

  const ancestorBindSetting = el.closest("[data-shiny-bind-inputs]");
  if (!ancestorBindSetting || !(ancestorBindSetting instanceof HTMLElement)) {
    return "eager";
  }

  return ancestorBindSetting.dataset.shinyBindInputs;
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
