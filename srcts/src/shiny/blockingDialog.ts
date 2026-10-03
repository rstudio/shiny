// One modal card over a backdrop; the rest of the page is made inert. Plain
// DOM and textContent only, so error text containing markup is shown, not
// run. Esc does nothing: every button is a real decision.

type DialogButton = {
  label: string;
  style: "primary" | "link";
  // data-choice, for tests.
  choice: string;
  onClick: () => void;
};

type DialogSpec = {
  id: string;
  title: string;
  body: string;
  // Shown in monospace above the body when present.
  detail?: string | null;
  buttons: DialogButton[];
};

const inertMark = "data-shiny-dialog-inert";

function showBlockingDialog(spec: DialogSpec): void {
  hideBlockingDialog(spec.id);

  const backdrop = document.createElement("div");
  backdrop.id = spec.id;
  backdrop.className = "shiny-blocking-dialog-backdrop";

  const dialog = document.createElement("div");
  dialog.className = "shiny-blocking-dialog";
  dialog.setAttribute("role", "alertdialog");
  dialog.setAttribute("aria-modal", "true");
  dialog.setAttribute("aria-labelledby", spec.id + "-title");
  dialog.setAttribute(
    "aria-describedby",
    (spec.detail ? spec.id + "-detail " : "") + spec.id + "-body",
  );

  const title = document.createElement("h2");
  title.id = spec.id + "-title";
  title.textContent = spec.title;
  dialog.appendChild(title);

  if (spec.detail) {
    const code = document.createElement("code");
    code.id = spec.id + "-detail";
    code.className = "shiny-blocking-dialog-detail";
    code.textContent = spec.detail;
    dialog.appendChild(code);
  }

  const body = document.createElement("p");
  body.id = spec.id + "-body";
  body.textContent = spec.body;
  dialog.appendChild(body);

  const actions = document.createElement("div");
  actions.className = "shiny-blocking-dialog-actions";
  let firstButton: HTMLButtonElement | null = null;
  for (const b of spec.buttons) {
    const button = document.createElement("button");
    button.type = "button";
    button.dataset.choice = b.choice;
    button.textContent = b.label;
    button.className =
      b.style === "primary" ? "btn btn-primary" : "btn btn-link";
    button.addEventListener("click", b.onClick);
    actions.appendChild(button);
    firstButton ??= button;
  }
  dialog.appendChild(actions);
  backdrop.appendChild(dialog);

  for (const el of Array.from(document.body.children)) {
    if (!el.hasAttribute("inert")) {
      el.setAttribute("inert", "");
      el.setAttribute(inertMark, spec.id);
    }
  }
  document.body.appendChild(backdrop);
  firstButton?.focus();
}

function hideBlockingDialog(id: string): void {
  document.getElementById(id)?.remove();
  for (const el of Array.from(
    document.querySelectorAll("[" + inertMark + '="' + id + '"]'),
  )) {
    el.removeAttribute("inert");
    el.removeAttribute(inertMark);
  }
}

export { hideBlockingDialog, showBlockingDialog };
export type { DialogButton, DialogSpec };
