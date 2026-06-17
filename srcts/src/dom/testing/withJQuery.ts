// shiny:jquery-allowed -- test helper installs a real jquery for adapter parity tests
import $ from "jquery";

/**
 * Toggle `window.jQuery` for the duration of `fn` and restore it afterward
 * (even on throw). Used by dual-path adapter parity tests.
 *
 * When `present` is true, installs the real `jquery` package as
 * `window.jQuery`; when false, sets `window.jQuery` to undefined.
 *
 * Requires a DOM (call `setupDom()` first).
 */
export function withJQuery<T>(present: boolean, fn: () => T): T {
  const win = (globalThis as any).window as { jQuery?: unknown };
  if (win === undefined) {
    throw new Error("withJQuery: no window — call setupDom() first.");
  }
  const saved = win.jQuery;
  win.jQuery = present ? ($ as unknown) : undefined;
  try {
    return fn();
  } finally {
    win.jQuery = saved;
  }
}
