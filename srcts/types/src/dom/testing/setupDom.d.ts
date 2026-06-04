/**
 * Install a fresh happy-dom Window onto Node's globals.
 * Returns a teardown function that aborts any pending async work on the
 * window (timers, fetch, MutationObservers) and restores prior values.
 *
 * Call once per test (e.g., in a `before` hook or inline) — happy-dom
 * does not isolate state across calls, so a fresh window per test
 * avoids cross-test pollution.
 */
export declare function setupDom(html?: string): () => void;
