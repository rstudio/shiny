---
title: "Client-side Assets and JS Build"
description: "How Shiny's TypeScript client (srcts/) is checked, bundled, and committed into inst/www/shared/, how vendored JS/CSS libraries are updated and patched via tools/, and how R htmlDependency() objects serve them; read before touching srcts/, inst/www/shared/, or tools/update*.R."
---

# Client-side Assets and JS Build

Everything the browser loads lives in `inst/www/shared/` and is **committed**.
It has two sources:

1. **Shiny's own TypeScript** in `srcts/`, bundled by esbuild (`npm run build`).
2. **Vendored third-party libraries**, fetched and patched by R scripts in `tools/`.

`srcts/`, `tools/`, `package.json`, `tsconfig.json` and `node_modules` are all
in `.Rbuildignore`; only `inst/www/shared/` ships in the R package.

## srcts layout

| Path | Contents |
|------|----------|
| `srcts/src/` | Main client. Entry point `srcts/src/index.ts` (calls `init()` from `./initialize`, re-exports `Shiny`/`ShinyClass`). Subdirs: `bindings/`, `components/`, `events/`, `file/`, `imageutils/`, `initialize/`, `inputPolicies/`, `shiny/`, `testing/`, `time/`, `utils/`, `window/` |
| `srcts/extras/` | Separate small bundles: `shiny-autoreload.ts`, `shiny-showcase.ts` + `.scss`, `shiny-testmode.ts`, `busy-indicators/busy-indicators.scss`; plus `globalShiny.ts` (declares `window.Shiny` for the published types; kept outside `src/` so `Shiny` isn't a global inside the source) |
| `srcts/build/` | esbuild scripts run with `tsx`: `_build.ts` (shared `build()`, `outDir = "./inst/www/shared/"`, banner from `DESCRIPTION` via `readcontrol`), `_jquery.ts`, `shiny.ts`, `extras.ts`, `external_libs.ts`, `external_libs_tsconfig.json` |
| `srcts/types/` | **Generated** `.d.ts` output of `tsc` (`declarationDir` in `tsconfig.json`), mirrors `build/`, `extras/`, `src/`. Committed |

Config at repo root: `tsconfig.json`, `tsconfig.eslint.json`, `eslint.config.mjs`,
prettier settings inside `package.json` (`prettier-plugin-organize-imports`).

## npm scripts (`package.json`)

```bash
npm ci                 # install exact lockfile versions
npm run build          # build_shiny && bundle_extras && bundle_external_libs
npm run watch          # bundle_shiny -- --watch  (esbuild context.watch(), no checks)
```

- **`build_shiny`** = `checks` then `bundle_shiny`.
- **`bundle_shiny`** (`srcts/build/shiny.ts`): first runs `verifyJqueryImport("srcts/src")`
  (see below), then bundles `srcts/src/index.ts` twice →
  `inst/www/shared/shiny.js` and `shiny.min.js`, each with a `.map`. jQuery is
  externalized to `window.jQuery` via `esbuild-plugin-globals`;
  `process.env.SHINY_VERSION` is `define`d to the `DESCRIPTION` version (read in
  `srcts/src/shiny/index.ts`, falls back to `"development"`). Target `es2021`.
- **`bundle_extras`** (`srcts/build/extras.ts`), all in parallel:
  - `shiny-autoreload.js`, `shiny-showcase.js`, `shiny-testmode.js` (+ `.map`, minified)
  - `shiny-showcase.css` (sass + postcss autoprefixer/preset-env)
  - `busy-indicators/busy-indicators.css`
  - shells out to `Rscript tools/updateShinyCss.R`, which compiles
    `inst/www/shared/shiny_scss/shiny.scss` → `inst/www/shared/shiny.min.css`
    with the R `sass` package. So **`npm run build` needs R + `sass` + `rprojroot`**.
- **`bundle_external_libs`** (`srcts/build/external_libs.ts`): minifies vendored
  files in place (no bundling, uses `external_libs_tsconfig.json` so esbuild
  doesn't inject `"use strict"`):
  - `datepicker/js/bootstrap-datepicker.min.js` — a temp `temp.js` that
    `require`s `bootstrap-datepicker.js` **plus every file in `js/locales/`** is
    bundled, so the min file contains all locales.
  - `ionrangeslider/js/ion.rangeSlider.min.js`
  - `selectize/js/selectize.min.js` (target `es6`)
  - `selectize/accessibility/js/selectize-plugin-a11y.min.js`
- **`checks`** = `lint && build_types && test_types && coverage && circular`:
  - `lint`: `eslint 'srcts/src/**/*.ts' --fix` (rewrites files)
  - `build_types`: `tsc -p tsconfig.json` (emit declarations only → `srcts/types/`)
  - `test_types`: `tsx --test` on every `srcts/src/**/__tests__/*.test.ts`
  - `coverage`: `type-coverage -p tsconfig.json --at-least 90`
  - `circular`: `npx --yes dpdm --transform ./srcts/src/index.ts`
- `coverage_detailed` is an ad-hoc helper (not part of `checks`).

### jQuery import rule (`srcts/build/_jquery.ts`)

`verifyJqueryImport()` scans every `.ts` under `srcts/src`: any line using
`jQuery.`/`jQuery(` is an error (use `$`), and any file using `$.`/`$(` must
contain the exact line `import $ from "jquery";`. This keeps `$` scoped instead
of relying on the global `@types/jquery` declaration.

## TS unit tests

Tests live next to code in `__tests__/` dirs and use Node's built-in runner
(`import test from "node:test"`, `node:assert/strict`), run by `tsx --test`.
Current files: `srcts/src/shiny/__tests__/outputProgress.test.ts`,
`srcts/src/shiny/__tests__/sendOutputInfo.test.ts`,
`srcts/src/time/__tests__/debounce.test.ts`. `**/__tests__` is excluded in
`tsconfig.json`, so tests produce no `.d.ts`. Run alone with `npm run test_types`.

## Vendored libraries

All under `inst/www/shared/`. Pinned versions for R live in generated
`R/version_*.R` files ("do not edit by hand"), written by the update scripts.

| Library | Location | Update path | Patches |
|---------|----------|-------------|---------|
| jQuery 3.7.1 | `jquery.js`, `jquery.min.js`, `.min.js.map` | `tools/updatejQuery.R` (code.jquery.com CDN; rewrites source map; writes `R/version_jquery.R`; `npm install` `jquery` + `@types/jquery`) | — |
| jQuery 1.12.4 (legacy) | `legacy/` | frozen, no script | — |
| jQuery UI 1.14.1 | `jqueryui/` | `tools/updatejQueryUI.R` (writes `R/version_jqueryui.R`, `@types/jqueryui`) | — |
| selectize 0.15.2 + a11y plugin | `selectize/{js,css,scss,accessibility}` | `tools/updateSelectize.R`: copies from **npm** `node_modules/@selectize/selectize/dist` (git ref pinned in `package.json`), downloads `selectize-plugin-a11y` at a pinned commit, applies patches with `git apply --reject`, writes `R/version_selectize.R`, runs `npm run build` | `tools/selectize-patches/` (000–009) |
| ion.rangeSlider 2.3.1 | `ionrangeslider/{js,scss,css}` | `tools/updateIonRangeSlider.R`: GitHub zip, `npx less2sass`, patches, compiles `css/ion.rangeSlider.css` via `bslib::bs_theme(version = 3)`, writes `R/version_ion_range_slider.R`, `@types/ion-rangeslider`, runs `npm run bundle_external_libs` | `tools/ion.rangeSlider-patches/` (0001–0006; adds the Shiny skin `scss/shiny.scss`) |
| bootstrap-datepicker 1.10.0 | `datepicker/{js,js/locales,scss,css}` | `tools/updateBootstrapDatepicker.R`: GitHub zip, `less2sass`, patches, compiles CSS with bslib, writes `R/version_bs_date_picker.R`, npm installs `bootstrap-datepicker` + types | `tools/datepicker-patches/` (001–003) |
| strftime 0.9.2 | `strftime/strftime-min.js` | `tools/updateStrftime.R` | — |
| Bootstrap 3.4.1 | `bootstrap/{css,js,fonts}` | frozen (`bootstrapVersion` in `R/bootstrap.R`); newer BS comes from bslib | — |
| bootstrap-accessibility-plugin | `bootstrap/accessibility/` | `tools/updateBootstrapAccessibilityPlugin.R` copies/compiles from **bslib**'s `lib/bs-a11y-p` | — |
| DataTables 1.10.22 | `datatables/` | `tools/updateDataTables.R` (CDN); version hard-coded in `dataTableDependency` | — |
| highlight.js "6.2" | `highlight/` | frozen | — |
| SVG spinners | `busy-indicators/spinners/` | `tools/updateSpinnerTypes.R` (also regenerates `R/busy-indicators-spinners.R`) | — |

npm-sourced at update time: selectize (via `node_modules`), plus `@types/*` and the
`jquery`/`bootstrap-datepicker` devDependencies that the update scripts keep in
sync. Everything else is downloaded directly by the R script. Patch workflow
(`tools/README.md`): edit vendored files, `git diff > tools/<lib>-patches/NNN-desc.patch`
(applied in `list.files()` order), re-run the update script, commit patch + output.

Shiny's own Sass source is hand-edited in place at `inst/www/shared/shiny_scss/`
(`shiny.scss`, `shiny.bootstrap{3,4,5}.scss`).

## Committed outputs and CI

`inst/www/shared/*` (including `shiny.js`, `shiny.min.js`, `.map`s) and
`srcts/types/**` are tracked in git. There is no local diff check; instead
`.github/workflows/R-CMD-check.yaml` calls `rstudio/shiny-workflows`'
`routine.yaml`, which (when `package.json` has `build`) deletes `srcts/types`,
runs `npm ci` + `npm run build`, `git add -A` on `./inst` and `./srcts` (plus
`package-lock.json`), commits "`npm run build` (GitHub Actions)" and pushes to
same-repo PRs. On `push` events or fork PRs it fails instead
("verify-no-new-commits"). Then `.github/shiny-workflows/routine.sh` runs
`tools/updatePackageJsonVersion.R` (DESCRIPTION `1.14.0.9000` →
package.json `1.14.0-alpha.9000`) and rebuilds if it changed.

`tests/testthat/test-built-files.R` only asserts `shiny.js`/`shiny.min.js` contain
no `{{ VERSION }}` placeholder.

## Published types (`@posit/shiny`)

`package.json` publishes only `LICENSE` and `srcts/types/**/*.d.ts`;
`"types": "srcts/types/extras/globalShiny.d.ts"` (augments `Window` with
`Shiny: ShinyClass`). `prepack`/`postpack` swap `README-npm.md` in as `README.md`
during `npm pack`. Install instructions for consumers are at the top of
`srcts/README.md`. Runtime `@types/*` and `lit` are `dependencies` so consumers get them.

## How R serves the assets

`R/server.R` maps the URL prefix `shared/` to `system_file("www", "shared")` as an
httpuv static path. Pages get dependencies through `htmlDependency(..., package = "shiny", src = "www/shared/...")`,
turned into URLs by `createWebDependency()` (`R/html-deps.R`, uses `addResourcePath()`).

- `R/shinyui.R`: `renderPage()` adds `jqueryDependency()` (option
  `shiny.jquery.version`, 3 → `jquery.min.js`, 1 → `legacy/`) and
  `shinyDependencies()` = `shinyDependencyCSS` (static `shiny.min.css`, or
  `shiny_scss/*` compiled via `bslib::bs_dependency()` when a `bs_theme` is set),
  `busyIndicatorDependency()` (`R/busy-indicators.R`), and `shiny-javascript`
  (`shiny.min.js` vs `shiny.js` via `get_devmode_option("shiny.minified", TRUE)`).
  Also `shiny-testmode` (test mode) and an inline `shiny-devmode` head script.
- `R/bootstrap.R`: `bootstrapLib()` / `bootstrapDependency()`, `dataTableDependency`.
- `R/input-select.R`: `selectizeDependency()`, `selectizeStaticDependency()`, `selectizeScripts()`.
- `R/input-slider.R`: `ionRangeSliderDependency()` (+ `strftime`), `ionRangeSliderDependencyCSS()`.
- `R/input-date.R`: `datePickerDependency()`, `datePickerCSS()` (adds a `noConflict` head script → `$.fn.bsDatepicker`).
- `R/jqueryui.R`: `jqueryuiDependency()`; `R/showcase.R`: `showcaseHead()` (highlight.js + `shiny-showcase.*`).
- `R/middleware.R` injects `shared/shiny-autoreload.js` directly as a `<script>`.

Themable widgets use `bslib::bs_dependency_defer()`: static CSS without a
`bs_theme`, Sass compiled from the vendored `scss/` with one.

## Pitfalls

- **Don't hand-edit generated files**: `shiny.js`/`shiny.min.js`/`.map`, extras
  JS/CSS, `shiny.min.css`, `*.min.js` of vendored libs, `srcts/types/`, and
  `R/version_*.R`. Edit `srcts/`, `shiny_scss/`, or the unminified vendored file
  + a patch, then rebuild.
- **Vendored edits without a patch are lost** the next time an `update*.R`
  script runs (they `unlink()` the target dir). Always add a `tools/*-patches/` file.
- Edits to unminified vendored JS (`selectize.js`, `ion.rangeSlider.js`,
  `bootstrap-datepicker.js`) do nothing in production until
  `npm run bundle_external_libs` regenerates the `.min.js` that R serves by default.
- `npm run watch` / `bundle_shiny` skip `checks`; `npm run lint` is `--fix`
  and will rewrite source files.
- `npm run build` calls `Rscript`; it fails without R and the `sass` package.
- Using `jQuery(...)`, or `$` without the exact `import $ from "jquery";`,
  fails the `_jquery.ts` check — but `srcts/build/shiny.ts` only `console.error`s
  in its `.catch`, so the process may still exit 0 with no new bundle.
- Forgetting to rebuild is usually auto-fixed by CI on same-repo PRs, but fork
  PRs and pushes fail the routine job instead; commit built files yourself.
- Bump a vendored version via its `tools/update*.R` script, not by editing
  `R/version_*.R`; the scripts also keep `package.json` `@types/*` in sync.
