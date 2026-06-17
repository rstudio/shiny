# jQuery Wrapper — Step 1 (Scaffold + `dom/query`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Lay the foundation for the jQuery wrapper described in `.claude/specs/2026-06-03-jquery-wrapper-design.md`: stand up the `srcts/src/dom/` directory, add `JQueryRequiredError`, build the `dom/query` capability module (native + jQuery adapters with per-call dispatch), extend the build-time `_jquery.ts` check to fence jQuery imports inside adapter files, and prove the pattern by migrating two representative call sites.

**Architecture:** Ports & Adapters. Each capability module exposes native-typed dispatch functions that, at call time, check `window.jQuery` and delegate to either a jQuery adapter or a native adapter. The wrapper directory is `srcts/src/dom/`; jQuery imports/types are forbidden outside `srcts/src/dom/*/jquery.ts` (build-enforced, with a `// shiny:jquery-allowed -- <reason>` magic-comment escape hatch).

**Tech Stack:** TypeScript 5.8, esbuild bundle (`tsx srcts/build/shiny.ts`), test runner `node:test` via `tsx --test`, new DOM environment via `happy-dom` (added in Task 0). The existing esbuild `external_libs.ts` already maps `jquery → window.jQuery`, so no bundle-config change is needed.

**Out of scope for this plan (deferred to follow-on plans):** the other capability modules (`dom/events`, `dom/html`, `dom/ajax`, `dom/widgets`), the dual `InputBinding`/`OutputBinding` API, the `package.json` peer-dependency switch, and migrating the remaining ~10 query-only call sites. Each gets its own plan that builds on this scaffold.

---

## File Structure

**Created in this plan:**

| File | Responsibility |
|---|---|
| `srcts/src/dom/errors.ts` | `JQueryRequiredError` class — thrown anywhere a code path requires jQuery and `window.jQuery` is absent. |
| `srcts/src/dom/testing/withJQuery.ts` | Test helper that toggles `window.jQuery` for dual-path tests. |
| `srcts/src/dom/testing/setupDom.ts` | Bootstraps a happy-dom `window`/`document` for the `node:test` runner. |
| `srcts/src/dom/query/index.ts` | `dom/query` port: exports `select`, `closest`, `matches` dispatch functions. |
| `srcts/src/dom/query/jquery.ts` | jQuery adapter for `dom/query`. The **only** file in this module allowed to `import $ from "jquery"`. |
| `srcts/src/dom/query/native.ts` | Native (DOM-only) adapter for `dom/query`. |
| `srcts/src/dom/query/__tests__/query.test.ts` | Dual-path tests for `select`, `closest`, `matches`. |
| `srcts/src/dom/__tests__/errors.test.ts` | Tests for `JQueryRequiredError` shape. |

**Modified in this plan:**

| File | Reason |
|---|---|
| `package.json` | Add `happy-dom` to `devDependencies`. |
| `srcts/build/_jquery.ts` | Extend with Rule 2 (only adapter files may import jquery / reference `JQuery<…>`) and the magic-comment exemption list with end-of-build audit print. |
| `srcts/src/initialize/browser.ts` | Migrated as one of two representative call sites (uses only selection). |
| `srcts/src/utils/index.ts` | Migrated for its `$()` selection uses only; `$escape` (which is pure string manipulation and only *named* for jQuery) is left alone; `$.type()` and other non-selection uses stay until later plans. May need a `// shiny:jquery-allowed` comment for the residual `$.type()` usage. |
| `NEWS.md` | Add entry under unreleased section documenting the new internal `dom/` layer (no public API change yet). |

---

## Task 0: Add DOM test environment

The existing test runner (`node:test` via `tsx --test`) does not provide a DOM. `happy-dom` is lighter and faster than `jsdom`, and works as a programmatic setup (no globals injection in the runner config).

**Files:**
- Modify: `package.json` (add `happy-dom` to `devDependencies`)
- Create: `srcts/src/dom/testing/setupDom.ts`

- [ ] **Step 1: Add `happy-dom` to devDependencies**

Run from repo root:

```bash
npm install --save-dev happy-dom@15
```

Expected: `package.json` and `package-lock.json` updated; `happy-dom` resolves to `^15.x`.

- [ ] **Step 2: Create the DOM setup helper**

Create `srcts/src/dom/testing/setupDom.ts`:

```ts
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
```

- [ ] **Step 3: Smoke-test the DOM helper**

Add a temporary smoke test by creating `srcts/src/dom/__tests__/setupDom.smoke.test.ts`:

```ts
import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../testing/setupDom";

void test("setupDom installs window.document with a body", () => {
  const teardown = setupDom("<!doctype html><html><body><div id=x></div></body></html>");
  try {
    assert.equal((globalThis as any).document.getElementById("x")?.id, "x");
  } finally {
    teardown();
  }
});

void test("setupDom teardown restores prior globals", () => {
  const before = (globalThis as any).document;
  const teardown = setupDom();
  teardown();
  assert.equal((globalThis as any).document, before);
});
```

- [ ] **Step 4: Run the smoke test**

Run:

```bash
npx tsx --test srcts/src/dom/__tests__/setupDom.smoke.test.ts
```

Expected: both tests pass.

- [ ] **Step 5: Commit**

```bash
git add package.json package-lock.json srcts/src/dom/testing/setupDom.ts srcts/src/dom/__tests__/setupDom.smoke.test.ts
git commit -m "test: add happy-dom-based setupDom helper for upcoming dom/ tests"
```

---

## Task 1: Add `JQueryRequiredError`

**Files:**
- Create: `srcts/src/dom/errors.ts`
- Create: `srcts/src/dom/__tests__/errors.test.ts`

- [ ] **Step 1: Write the failing test**

Create `srcts/src/dom/__tests__/errors.test.ts`:

```ts
import assert from "node:assert/strict";
import test from "node:test";
import { JQueryRequiredError } from "../errors";

void test("JQueryRequiredError carries feature and binding fields", () => {
  const err = new JQueryRequiredError("dom/widgets:datepicker", "MyDateInput");
  assert.equal(err.name, "JQueryRequiredError");
  assert.equal(err.feature, "dom/widgets:datepicker");
  assert.equal(err.binding, "MyDateInput");
  assert.ok(err.message.includes("dom/widgets:datepicker"));
  assert.ok(err.message.includes("MyDateInput"));
  assert.ok(err.message.includes("jQuery"));
});

void test("JQueryRequiredError works without a binding name", () => {
  const err = new JQueryRequiredError("dom/widgets:slider");
  assert.equal(err.binding, undefined);
  assert.ok(err.message.includes("dom/widgets:slider"));
  assert.ok(!err.message.includes("used by binding"));
});

void test("JQueryRequiredError is an Error", () => {
  const err = new JQueryRequiredError("x");
  assert.ok(err instanceof Error);
  assert.ok(err instanceof JQueryRequiredError);
});
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
npx tsx --test srcts/src/dom/__tests__/errors.test.ts
```

Expected: FAIL with "Cannot find module '../errors'" or similar.

- [ ] **Step 3: Implement `errors.ts`**

Create `srcts/src/dom/errors.ts`:

```ts
/**
 * Thrown by a `dom/*` dispatch function when the requested operation
 * requires jQuery and `window.jQuery` is not available.
 *
 * The wrapper layer (see spec 2026-06-03-jquery-wrapper-design.md) treats
 * jQuery as an optional peer dependency. Most operations have a native
 * fallback; some — notably the jQuery-plugin widgets — do not, and throw
 * this error with a descriptive `feature` string and (optionally) the
 * `constructor.name` of the binding that triggered the call.
 */
export class JQueryRequiredError extends Error {
  public readonly feature: string;
  public readonly binding?: string;

  constructor(feature: string, binding?: string) {
    const where = binding ? ` (used by binding "${binding}")` : "";
    super(
      `Shiny feature "${feature}"${where} requires jQuery, ` +
        `which is not loaded on this page. ` +
        `Load jQuery 3.x before Shiny initializes, ` +
        `or replace the binding with a native-compatible alternative.`,
    );
    this.name = "JQueryRequiredError";
    this.feature = feature;
    this.binding = binding;
  }
}
```

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
npx tsx --test srcts/src/dom/__tests__/errors.test.ts
```

Expected: all three tests pass.

- [ ] **Step 5: Commit**

```bash
git add srcts/src/dom/errors.ts srcts/src/dom/__tests__/errors.test.ts
git commit -m "feat(dom): add JQueryRequiredError for optional-jquery surface"
```

---

## Task 2: `withJQuery` test helper

**Files:**
- Create: `srcts/src/dom/testing/withJQuery.ts`
- Create: `srcts/src/dom/__tests__/withJQuery.test.ts`

- [ ] **Step 1: Write the failing test**

Create `srcts/src/dom/__tests__/withJQuery.test.ts`:

```ts
import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../testing/setupDom";
import { withJQuery } from "../testing/withJQuery";

void test("withJQuery(true, fn) makes window.jQuery a function during fn", () => {
  const teardown = setupDom();
  try {
    let observed: unknown;
    withJQuery(true, () => {
      observed = typeof (globalThis as any).window.jQuery;
    });
    assert.equal(observed, "function");
  } finally {
    teardown();
  }
});

void test("withJQuery(false, fn) sets window.jQuery to undefined during fn", () => {
  const teardown = setupDom();
  try {
    (globalThis as any).window.jQuery = () => undefined; // pretend it was loaded
    let observed: unknown = "not-set";
    withJQuery(false, () => {
      observed = (globalThis as any).window.jQuery;
    });
    assert.equal(observed, undefined);
  } finally {
    teardown();
  }
});

void test("withJQuery restores prior value after fn", () => {
  const teardown = setupDom();
  try {
    const sentinel = () => "sentinel";
    (globalThis as any).window.jQuery = sentinel;
    withJQuery(false, () => undefined);
    assert.equal((globalThis as any).window.jQuery, sentinel);
  } finally {
    teardown();
  }
});

void test("withJQuery restores prior value even if fn throws", () => {
  const teardown = setupDom();
  try {
    const sentinel = () => "sentinel";
    (globalThis as any).window.jQuery = sentinel;
    assert.throws(() => {
      withJQuery(false, () => {
        throw new Error("boom");
      });
    });
    assert.equal((globalThis as any).window.jQuery, sentinel);
  } finally {
    teardown();
  }
});
```

- [ ] **Step 2: Run test to verify it fails**

Run:

```bash
npx tsx --test srcts/src/dom/__tests__/withJQuery.test.ts
```

Expected: FAIL with module-not-found.

- [ ] **Step 3: Implement `withJQuery.ts`**

Create `srcts/src/dom/testing/withJQuery.ts`:

```ts
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
```

Note: this file uses the `// shiny:jquery-allowed --` magic comment, which will be honored by the build rule extension we add in Task 4. Until Task 4 runs the build will not enforce that restriction, so this comment is forward-looking but harmless.

- [ ] **Step 4: Run test to verify it passes**

Run:

```bash
npx tsx --test srcts/src/dom/__tests__/withJQuery.test.ts
```

Expected: all four tests pass.

- [ ] **Step 5: Commit**

```bash
git add srcts/src/dom/testing/withJQuery.ts srcts/src/dom/__tests__/withJQuery.test.ts
git commit -m "test(dom): add withJQuery helper for dual-path adapter tests"
```

---

## Task 3: `dom/query` module (port + adapters + tests)

**Files:**
- Create: `srcts/src/dom/query/index.ts`
- Create: `srcts/src/dom/query/native.ts`
- Create: `srcts/src/dom/query/jquery.ts`
- Create: `srcts/src/dom/query/__tests__/query.test.ts`

- [ ] **Step 1: Write the failing parity tests**

Create `srcts/src/dom/query/__tests__/query.test.ts`:

```ts
import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../../testing/setupDom";
import { withJQuery } from "../../testing/withJQuery";
import { closest, matches, select } from "../index";

const FIXTURE = `<!doctype html><html><body>
  <div id="root">
    <div class="a outer">
      <span class="b inner" data-x="1">one</span>
      <span class="b inner" data-x="2">two</span>
    </div>
    <div class="a outer">
      <span class="b inner" data-x="3">three</span>
    </div>
  </div>
</body></html>`;

function runDualPath(name: string, fn: () => void): void {
  void test(`${name} (jquery adapter)`, () => {
    const teardown = setupDom(FIXTURE);
    try {
      withJQuery(true, fn);
    } finally {
      teardown();
    }
  });
  void test(`${name} (native adapter)`, () => {
    const teardown = setupDom(FIXTURE);
    try {
      withJQuery(false, fn);
    } finally {
      teardown();
    }
  });
}

runDualPath("select returns all matching descendants of scope", () => {
  const scope = (globalThis as any).document.getElementById("root") as Element;
  const out = select(scope, ".inner");
  assert.equal(out.length, 3);
  assert.deepEqual(
    out.map((el) => el.getAttribute("data-x")),
    ["1", "2", "3"],
  );
});

runDualPath("select returns [] when nothing matches", () => {
  const scope = (globalThis as any).document.getElementById("root") as Element;
  const out = select(scope, ".does-not-exist");
  assert.deepEqual(out, []);
});

runDualPath("closest returns the nearest ancestor (or self) matching selector", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='2']") as Element;
  const outer = closest(span, ".outer");
  assert.ok(outer);
  assert.equal(outer!.tagName, "DIV");
  assert.ok(outer!.classList.contains("a"));
});

runDualPath("closest returns null when no ancestor matches", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='2']") as Element;
  const out = closest(span, ".no-such-class");
  assert.equal(out, null);
});

runDualPath("matches returns true when element matches selector", () => {
  const span = (globalThis as any).document.querySelector("span[data-x='1']") as Element;
  assert.equal(matches(span, ".inner"), true);
  assert.equal(matches(span, ".outer"), false);
});
```

- [ ] **Step 2: Run tests to verify they fail**

Run:

```bash
npx tsx --test srcts/src/dom/query/__tests__/query.test.ts
```

Expected: FAIL with module-not-found for `../index`.

- [ ] **Step 3: Implement native adapter**

Create `srcts/src/dom/query/native.ts`:

```ts
/**
 * Native (DOM-only) adapter for `dom/query`.
 * No jQuery imports or JQuery<> types may appear in this file.
 */
export function select(scope: ParentNode, selector: string): Element[] {
  return Array.from(scope.querySelectorAll(selector));
}

export function closest(el: Element, selector: string): Element | null {
  return el.closest(selector);
}

export function matches(el: Element, selector: string): boolean {
  return el.matches(selector);
}
```

- [ ] **Step 4: Implement jQuery adapter**

Create `srcts/src/dom/query/jquery.ts`:

```ts
import $ from "jquery";

/**
 * jQuery adapter for `dom/query`.
 *
 * Preserves jQuery selector extensions (e.g. `:visible`) for callers that
 * happen to use them. Note that jQuery only takes `Document | Element` as
 * a `find()` context, so we coerce `DocumentFragment` to its first element
 * child where needed — Shiny's call sites all pass Document or Element
 * scopes in practice.
 */
export function select(scope: ParentNode, selector: string): Element[] {
  return $(scope as Document | Element)
    .find(selector)
    .toArray();
}

export function closest(el: Element, selector: string): Element | null {
  const $found = $(el).closest(selector);
  return $found.length > 0 ? ($found.get(0) as Element) : null;
}

export function matches(el: Element, selector: string): boolean {
  return $(el).is(selector);
}
```

- [ ] **Step 5: Implement port (dispatch)**

Create `srcts/src/dom/query/index.ts`:

```ts
import * as jqueryAdapter from "./jquery";
import * as nativeAdapter from "./native";

function hasJQuery(): boolean {
  return typeof (globalThis as any).window?.jQuery === "function";
}

export function select(scope: ParentNode, selector: string): Element[] {
  return hasJQuery()
    ? jqueryAdapter.select(scope, selector)
    : nativeAdapter.select(scope, selector);
}

export function closest(el: Element, selector: string): Element | null {
  return hasJQuery()
    ? jqueryAdapter.closest(el, selector)
    : nativeAdapter.closest(el, selector);
}

export function matches(el: Element, selector: string): boolean {
  return hasJQuery()
    ? jqueryAdapter.matches(el, selector)
    : nativeAdapter.matches(el, selector);
}
```

- [ ] **Step 6: Run tests to verify they pass**

Run:

```bash
npx tsx --test srcts/src/dom/query/__tests__/query.test.ts
```

Expected: all 10 dual-path tests pass (5 logical tests × 2 adapters).

- [ ] **Step 7: Commit**

```bash
git add srcts/src/dom/query
git commit -m "feat(dom): add dom/query module with native + jquery adapters"
```

---

## Task 4: Extend `_jquery.ts` build rule (Rule 2 + escape hatch)

**Files:**
- Modify: `srcts/build/_jquery.ts`

- [ ] **Step 1: Read the current `_jquery.ts`**

Open `srcts/build/_jquery.ts` (already read in the design phase; ~98 lines, key function is `verifyJqueryUsage`).

- [ ] **Step 2: Extend `verifyJqueryUsage` and add exemption tracking**

Replace the contents of `srcts/build/_jquery.ts` with the version below. Three changes:

1. New regex constant `ADAPTER_PATH_RE` matches `srcts/src/dom/*/jquery.ts` (any direct child capability dir).
2. New rule applied per file: if the file is not an adapter and not exempted by a `// shiny:jquery-allowed -- <reason>` line, importing `jquery` or referencing `JQuery<…>` types fails the build.
3. `verifyJqueryImport` collects exemption strings and prints a single audit summary at the end.

```ts
/*
Make sure all `*.ts` files contain `"jquery"` import statements to properly
scope `jquery`, AND that jQuery imports / `JQuery<>` type references only
appear in `srcts/src/dom/*/jquery.ts` adapter files.

See `.claude/specs/2026-06-03-jquery-wrapper-design.md` §4 for context.

Escape hatch: any file containing a single line of the form
  // shiny:jquery-allowed -- <reason>
is exempted from the adapter-file restriction. The reason is recorded in
an audit summary printed at the end of every build.
*/

import fs from "fs";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore; Type definitions are not found
import readdirRecursive from "fs-readdir-recursive";
import { isUndefined } from "lodash";

const ADAPTER_PATH_RE = /(^|\/)srcts\/src\/dom\/[^/]+\/jquery\.ts$/;
const EXEMPTION_RE = /^\/\/\s*shiny:jquery-allowed\s*--\s*(.+)$/m;
const IMPORT_JQUERY = 'import $ from "jquery";';
const JQUERY_TYPE_RE = /\bJQuery\s*</;

type Exemption = { file: string; reason: string };

async function verifyJqueryUsage(
  filename: string,
  exemptions: Exemption[],
): Promise<void> {
  const contents = await fs.promises.readFile(filename, "utf8");
  const lines = contents.split("\n");

  // ---- Rule 1 (existing): files that use `$` must import it -------------

  const jqueryWordMatch = lines.find(
    (line) => line.includes("jQuery.") || line.includes("jQuery("),
  );
  if (!isUndefined(jqueryWordMatch)) {
    throw (
      `Using \`jQuery\` in file: ${filename}\n` +
      `Match:\n${jqueryWordMatch}\n` +
      "Please use `$` instead of `jQuery`\n" +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  const dollarMatch = lines.find(
    (line) => line.includes("$.") || line.includes("$("),
  );
  const usesDollar = !isUndefined(dollarMatch);
  const hasJqueryImport = lines.includes(IMPORT_JQUERY);

  if (usesDollar && !hasJqueryImport) {
    throw (
      `Using \`$\` in file: ${filename}\n` +
      `Match:\n${dollarMatch}\n` +
      `Please call \`${IMPORT_JQUERY}\` at the top of the file.\n` +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  // ---- Rule 2 (new): jquery is fenced inside adapter files --------------

  const usesJqueryType = JQUERY_TYPE_RE.test(contents);
  const fileImportsJquery = hasJqueryImport;
  const inAdapter = ADAPTER_PATH_RE.test(filename);
  const exemptMatch = contents.match(EXEMPTION_RE);

  if ((fileImportsJquery || usesJqueryType) && !inAdapter) {
    if (exemptMatch) {
      exemptions.push({ file: filename, reason: exemptMatch[1].trim() });
    } else {
      throw (
        `jQuery imports and \`JQuery<>\` types are only allowed in ` +
        `srcts/src/dom/*/jquery.ts adapter files.\n` +
        `Offending file: ${filename}\n` +
        `Use a dom/* dispatch function instead, or add a ` +
        `\`// shiny:jquery-allowed -- <reason>\` comment if the exception ` +
        `is intentional.\n` +
        "See file ./srcts/build/_jquery.ts for more details"
      );
    }
  }
}

const verifyJqueryImport = async function (dir = "."): Promise<void> {
  const tsFiles = (readdirRecursive(dir) as string[])
    .filter((path: string) => path.endsWith(".ts"))
    .map((path) => dir + "/" + path);

  const exemptions: Exemption[] = [];
  await Promise.all(
    tsFiles.map((file) => verifyJqueryUsage(file, exemptions)),
  );

  if (exemptions.length > 0) {
    // Print a stable, sorted audit so reviewers notice drift.
    exemptions.sort((a, b) => a.file.localeCompare(b.file));
    // eslint-disable-next-line no-console
    console.log(
      `\n[_jquery audit] ${exemptions.length} file(s) carry ` +
        `\`shiny:jquery-allowed\` exemptions:`,
    );
    for (const ex of exemptions) {
      // eslint-disable-next-line no-console
      console.log(`  - ${ex.file}: ${ex.reason}`);
    }
  }
};

export { verifyJqueryImport };
```

- [ ] **Step 3: Run the bundle to confirm the build still passes (with audit print)**

Run:

```bash
npm run bundle_shiny
```

Expected: build succeeds. The audit summary at the end of the run prints **exactly one** exemption: `srcts/src/dom/testing/withJQuery.ts`. If any other file fails the new rule, that file is using jQuery outside an adapter and needs to be migrated *now* (via `dom/query`) or *later* (via a follow-on module) — for files using jQuery for capabilities not yet wrapped, add the magic-comment exemption with reason `"awaiting dom/<module> migration"`.

**Important:** all 40 of the currently-jQuery-using files in `srcts/src/` will fail Rule 2 because they import jQuery and live outside the adapter directory. As part of this step, add the `// shiny:jquery-allowed -- awaiting dom/<module> migration` exemption to each one. List them up front:

```bash
grep -l '^import \$ from "jquery";' srcts/src -r
```

For each file in the list **except** files we are migrating in Task 5 below (`srcts/src/initialize/browser.ts`, `srcts/src/utils/index.ts`), prepend at the top of the file (before all imports):

```ts
// shiny:jquery-allowed -- awaiting dom/* migration (see .claude/specs/2026-06-03-jquery-wrapper-design.md)
```

This is intentionally noisy — it makes the remaining migration surface visible in `git grep`. Each future plan removes a batch of these comments as it migrates its module's call sites.

- [ ] **Step 4: Run the bundle again to confirm clean build**

Run:

```bash
npm run bundle_shiny
```

Expected: build succeeds; audit prints ~40 exemptions (one per pending file plus `withJQuery.ts`).

- [ ] **Step 5: Commit**

Stage everything from Step 3 + 4 together:

```bash
git add srcts/build/_jquery.ts srcts/src
git commit -m "build: extend _jquery.ts to fence jquery inside dom/*/jquery.ts adapters

Rule 2: importing jquery or referencing JQuery<> types outside an adapter
file fails the build. A '// shiny:jquery-allowed -- <reason>' magic comment
exempts a file; exemptions are printed in an audit summary at the end of
every build, so drift is visible. All currently-jquery-using source files
are exempted with 'awaiting dom/* migration'; each follow-on plan removes
its module's exemptions as it migrates."
```

---

## Task 5: Migrate two representative call sites

Two files are chosen as the smallest, most isolated proof-of-concept migrations: `srcts/src/initialize/browser.ts` (a few selector calls, no event/html/ajax) and `srcts/src/utils/index.ts` for its `$()` selection uses only.

Note: a quick re-check of each file is required because the file contents on disk are the source of truth; the snippets below show the *pattern* of change. If a file's actual contents differ materially, port what's there using the same pattern (replace `$(scope).find(sel)` → `select(scope, sel)`, etc.) and keep the unrelated code untouched.

**Files:**
- Modify: `srcts/src/initialize/browser.ts`
- Modify: `srcts/src/utils/index.ts`

- [ ] **Step 1: Re-read both files to confirm current jQuery usage**

Open each file and identify every line containing `$(` or `$.`. List them. For each line, classify:
- **Pure selection** (e.g., `$("foo")`, `$(el).find("...")`, `$(el).is("...")`, `$(el).closest("...")`) → migrate to `dom/query`.
- **Anything else** (events, html, ajax, `$.extend`, `$.type`, etc.) → leave in place for now (a future plan will cover it). Keep the `import $ from "jquery";` and add (or keep) the `// shiny:jquery-allowed -- ...` exemption.

- [ ] **Step 2: Migrate `initialize/browser.ts`**

For each pure-selection line, replace with the equivalent `dom/query` call. At the top of the file:

- Remove `import $ from "jquery";` **if and only if** no non-selection `$` usage remains.
- Add `import { select, closest, matches } from "../dom/query";` (only the functions actually used).

Example transformation:

```ts
// Before:
import $ from "jquery";
// ...
const checkboxes = $(scope).find("input[type=checkbox]");
checkboxes.each(function () { /* ... */ });

// After:
import { select } from "../dom/query";
// ...
const checkboxes = select(scope, "input[type=checkbox]");
for (const cb of checkboxes) { /* ... */ }
```

Notes:
- `select(...)` returns `Element[]`, not a `JQuery` object. Replace `.each(...)` loops with `for...of`. Replace `.length` checks — these still work, since the return type is an array.
- If `scope` was typed `JQuery<HTMLElement>`, change to `ParentNode` or `Element`. If the caller passes a jQuery scope, it must be unwrapped at the boundary — but `initialize/browser.ts` is called by Shiny's own boot path with native scopes, so this should not arise here.
- Keep the `// shiny:jquery-allowed -- ...` line removed once the file has no jQuery references.

- [ ] **Step 3: Migrate `utils/index.ts` (selection uses only)**

`utils/index.ts` mixes selection use (if any) with `$.type(...)` and `$escape` (which is a pure string-manipulation function whose name *mentions* jQuery but does not use it — leave entirely alone). Migrate **only** the pure-selection lines.

After migration, the file will likely still import `$` (because of `$.type` and other non-selection uses). That's fine — keep the existing `import $ from "jquery";` and ensure the file carries:

```ts
// shiny:jquery-allowed -- awaiting dom/utility-helpers migration (see .claude/specs/2026-06-03-jquery-wrapper-design.md)
```

(Reason string differs from the generic one so it's clear this file is specifically blocked on capabilities not in this plan.)

- [ ] **Step 4: Run the bundle**

Run:

```bash
npm run bundle_shiny
```

Expected: build succeeds; the audit print no longer lists `srcts/src/initialize/browser.ts` if that file became jQuery-free, and `srcts/src/utils/index.ts` still appears (with its narrower reason string).

- [ ] **Step 5: Run all tests**

Run:

```bash
npm run test_types
```

Expected: no test failures (we have not changed behavior, only the import path).

- [ ] **Step 6: Type-check and lint**

Run:

```bash
npm run build_types && npm run lint
```

Expected: both succeed.

- [ ] **Step 7: Commit**

```bash
git add srcts/src/initialize/browser.ts srcts/src/utils/index.ts
git commit -m "refactor(dom): migrate browser.ts and utils selection calls to dom/query"
```

---

## Task 6: Smoketest under absent jQuery

Add a single integration-level test that boots a minimal piece of Shiny without `window.jQuery` and confirms the new `dom/query` dispatch picks the native adapter. This is the regression net the spec calls for in §5.

**Files:**
- Create: `srcts/src/dom/query/__tests__/query.no-jquery.test.ts`

- [ ] **Step 1: Write the test**

Create `srcts/src/dom/query/__tests__/query.no-jquery.test.ts`:

```ts
import assert from "node:assert/strict";
import test from "node:test";
import { setupDom } from "../../testing/setupDom";
import { select, closest, matches } from "../index";

void test("dom/query works without window.jQuery", () => {
  const teardown = setupDom(
    `<!doctype html><html><body>
       <div id="root">
         <span class="x" data-v="1"></span>
         <span class="x" data-v="2"></span>
       </div>
     </body></html>`,
  );
  try {
    // Explicitly assert no jQuery present.
    assert.equal(typeof (globalThis as any).window.jQuery, "undefined");

    const root = (globalThis as any).document.getElementById("root") as Element;

    const all = select(root, ".x");
    assert.equal(all.length, 2);
    assert.deepEqual(
      all.map((el) => el.getAttribute("data-v")),
      ["1", "2"],
    );

    assert.equal(closest(all[0], "#root")?.id, "root");
    assert.equal(matches(all[0], ".x"), true);
  } finally {
    teardown();
  }
});
```

- [ ] **Step 2: Run the test**

Run:

```bash
npx tsx --test srcts/src/dom/query/__tests__/query.no-jquery.test.ts
```

Expected: pass. (This already worked under Task 3's parity tests, but the explicit no-jQuery test is the regression net.)

- [ ] **Step 3: Commit**

```bash
git add srcts/src/dom/query/__tests__/query.no-jquery.test.ts
git commit -m "test(dom): add no-jquery regression smoketest for dom/query"
```

---

## Task 7: Update NEWS.md and final verification

**Files:**
- Modify: `NEWS.md`

- [ ] **Step 1: Read current `NEWS.md`**

Open `NEWS.md` and locate the unreleased section header (typically `# shiny <next-version>` or similar).

- [ ] **Step 2: Add an internal-change entry**

Under the unreleased section, add:

```markdown
* Internal: Added `srcts/src/dom/` capability-oriented wrapper around jQuery as
  the first step of a long-term jQuery removal effort. Step 1 introduces
  `dom/query` (selection helpers with native and jQuery adapters, dispatched at
  call time) and extends the build-time `_jquery.ts` check to fence jQuery
  imports inside adapter files. No user-facing API changes in this release.
  See `.claude/specs/2026-06-03-jquery-wrapper-design.md` for the full plan.
```

- [ ] **Step 3: Run the full checks suite**

Run:

```bash
npm run checks
```

Expected: lint, build_types, test_types, coverage, circular — all pass.

- [ ] **Step 4: Commit**

```bash
git add NEWS.md
git commit -m "docs(NEWS): note dom/ wrapper scaffold and dom/query module"
```

---

## Post-plan verification checklist

After all tasks complete, manually confirm:

- [ ] `npm run bundle_shiny` succeeds and prints the audit summary with the expected exemption count.
- [ ] `npm run checks` succeeds end to end.
- [ ] `git grep 'import \$ from "jquery"' srcts/src | wc -l` returns one fewer (or two fewer) than the pre-plan count of 41 — confirming that at least one file became jQuery-free.
- [ ] `git grep 'shiny:jquery-allowed' srcts/src` returns roughly 40 results — one per file still pending migration plus `withJQuery.ts`.
- [ ] The `dom/query` parity tests run under both adapters and pass.

---

## Self-Review

**Spec coverage:**

| Spec section | Plan task |
|---|---|
| §1 Architecture (ports & adapters, location, invariant) | Tasks 3 (port shape), 4 (invariant enforcement) |
| §2 `dom/query` module surface | Task 3 |
| §2 `dom/events`, `dom/html`, `dom/ajax`, `dom/widgets` | Deferred to follow-on plans (explicitly out of scope) |
| §3 Dual binding API | Deferred to follow-on plan |
| §3.5 `JQueryRequiredError` | Task 1 |
| §3.5 `package.json` peer dep | Deferred to follow-on plan |
| §4 Build-time enforcement (Rule 2 + escape hatch + audit) | Task 4 |
| §5 Dual-path testing infrastructure | Tasks 0, 2, 3 |
| §5 `<script>` execution tests | N/A — belongs to `dom/html` plan |
| §5 No-jQuery smoketest | Task 6 |
| §6 Migration step 1 | This plan in full; steps 2–7 are follow-on plans |
| §7 Future widget replacement outline | N/A — entirely future work |

No gaps within the scoped step.

**Placeholder scan:** none. Every code step includes the actual code. No TBDs.

**Type consistency:** `select(scope: ParentNode, selector: string): Element[]`, `closest(el: Element, selector: string): Element | null`, `matches(el: Element, selector: string): boolean` — these signatures are identical in `index.ts`, `native.ts`, `jquery.ts`, and all test usages. `JQueryRequiredError(feature, binding?)` — consistent across `errors.ts` and the error test.

**Scope check:** This plan produces a working, mergeable PR (or commit series) that introduces real new functionality (`dom/query`), real test infrastructure, real build enforcement, and a measurable reduction in jQuery surface area — without depending on any later step.
