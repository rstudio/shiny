import { Window } from "happy-dom";

/**
 * Install a fresh happy-dom Window onto Node's globals.
 * Returns a teardown function that restores prior values.
 *
 * Call once per test (e.g., in a `before` hook or inline) — happy-dom
 * does not isolate state across calls, so a fresh window per test
 * avoids cross-test pollution.
 */
export function setupDom(html = "<!doctype html><html><body></body></html>"): () => void {
  const win = new Window({ url: "http://localhost/" });
  // Parse the supplied HTML into the document.
  win.document.write(html);

  const saved = {
    window: (globalThis as any).window,
    document: (globalThis as any).document,
    HTMLElement: (globalThis as any).HTMLElement,
    Element: (globalThis as any).Element,
    Node: (globalThis as any).Node,
  };

  (globalThis as any).window = win;
  (globalThis as any).document = win.document;
  (globalThis as any).HTMLElement = win.HTMLElement;
  (globalThis as any).Element = win.Element;
  (globalThis as any).Node = win.Node;

  return function teardownDom(): void {
    (globalThis as any).window = saved.window;
    (globalThis as any).document = saved.document;
    (globalThis as any).HTMLElement = saved.HTMLElement;
    (globalThis as any).Element = saved.Element;
    (globalThis as any).Node = saved.Node;
  };
}
