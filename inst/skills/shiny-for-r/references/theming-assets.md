# Theming and Static Assets in Shiny for R

## Overview

App appearance is controlled by a `bslib::bs_theme()` object passed to a
page function's `theme =` argument. `bs_theme()` sets Bootstrap Sass
**variables** before the CSS compiles, so one change cascades consistently
across every component instead of fighting Bootstrap's specificity
selector by selector. The anti-pattern this reference prevents is
scattering `tags$style()`/inline CSS to hand-tune colors and fonts: those
drift out of sync and never touch server-rendered plots. Theme at the
source with `bs_theme()` (and {thematic} for plots); keep raw CSS for
one-off tweaks.

## bs_theme()

`bs_theme()` returns a theme object; pass it to `theme =` on any `page_*()`
function (`page_sidebar()`, `page_navbar()`, `page_fillable()`, ...).

```r
# Partial snippet: theme = bs_theme(...) inside a page_*() function
bs_theme(
  version = 5,
  preset = "shiny",
  bg = "#FFFFFF", fg = "#333333", primary = "#2C3E50",
  base_font = font_google("Lato"),
  font_scale = 1.1
)
```

`preset` selects `"shiny"` (bslib's polished default), `"bootstrap"`
(vanilla Bootstrap 5), or a Bootswatch name (`"flatly"`, `"darkly"`, ...;
list via `bootswatch_themes()`). `bg`/`fg` set background/foreground,
`primary` the brand accent for links and focus states. `base_font`/
`heading_font`/`code_font` accept a `font_google()` result, which downloads
and caches the font locally; `font_scale` multiplies font sizes uniformly.
Any other named argument via `...` sets a raw Sass variable (e.g.
`"border-radius" = "0.5rem"`).

## Dark mode

`input_dark_mode(id = NULL, mode = NULL)` renders a toggle that flips
Bootstrap 5.3's client-side color mode — a CSS variable swap, no Sass
recompilation, instantaneous. Give it an `id` to read the mode as
`input$<id>` (`"light"`/`"dark"`); drive it from the server with
`toggle_dark_mode(mode = NULL, session = ...)`.

```r
# Partial snippet: input_dark_mode() in the UI, toggle_dark_mode() in the server
input_dark_mode(id = "mode")
observeEvent(input$force_dark, toggle_dark_mode(mode = "dark"))
```

For changes beyond light/dark (different palettes, fonts), swap the whole
theme at runtime with `session$setCurrentTheme(theme)` in an observer —
heavier, since it triggers a full recompilation, but not limited to two
modes.

## Theming plots: thematic

`bs_theme()` only produces CSS; it has no effect on server-rendered plot
images. {thematic} bridges that gap, translating the theme's colors and
fonts into R plotting defaults. Call `thematic_shiny()` once, before
`shinyApp()`; it works with base R graphics, ggplot2, and lattice.

```r
library(shiny)
library(bslib)
library(thematic)
library(ggplot2)

thematic_shiny(font = "auto")

ui <- page_fillable(
  theme = bs_theme(bg = "#0b1021", fg = "#e5e5e5", primary = "#7dd3fc"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(mtcars, aes(mpg, hp)) + geom_point()
  })
}

shinyApp(ui, server)
```

`font = "auto"` also matches `bs_theme()`'s font. {thematic} reacts to
`session$setCurrentTheme()`, not `input_dark_mode()`'s client-side toggle.

## _brand.yml

`bs_theme()` accepts a `brand` argument that auto-discovers a `_brand.yml`
file (shared colors, fonts, logo), searching the app directory and each of
its parents, including a `_brand/` or `brand/` subdirectory at every level.
`brand = NULL` (default) applies it if found, no error if absent; `TRUE`
requires the file; `FALSE` disables discovery; a string gives an explicit
path; a list supplies brand settings inline.

```yaml
# _brand.yml
color:
  palette:
    brand-blue: "#0066cc"
  primary: brand-blue
typography:
  base:
    family: Inter
```

```r
# Partial snippet: theme = ... inside a page_*() function, _brand.yml alongside app.R
bs_theme(brand = TRUE)
```

Brand.yml support needs a recent bslib (verified against 0.12.0) *and* the
{brand.yml} package, which bslib only lists in `Suggests`. Without it,
`bs_theme(brand = TRUE)` fails with `The package "brand.yml" is required.`,
so install it alongside bslib.

## Static assets

Files in a `www/` directory next to `app.R` serve automatically at the
app's root URL — images, downloads, CSS. `includeCSS(path)` (and
`includeScript()`) inlines a file for a stylesheet outside `www/`. To serve
another directory under a custom prefix, call `addResourcePath(prefix,
directoryPath)` once, at the top of `app.R`.

```r
# Partial snippet: top of app.R, before ui/server are defined
addResourcePath("assets", "extra-assets")  # extra-assets/logo.png -> /assets/logo.png
```

Reach for `bs_theme()`/{thematic} first for color/font styling; keep raw
CSS and `www/` for assets theming can't express.

## Quick reference

| Function | Purpose |
|---|---|
| `bs_theme(version = , preset = , bg = , fg = , primary = , base_font = , ...)` | Build a theme; pass to a page's `theme =` |
| `font_google(family, ...)` | Reference a Google Font in a theme |
| `input_dark_mode(id = )` / `toggle_dark_mode(mode)` | Client-side light/dark toggle, read/set |
| `session$setCurrentTheme(theme)` | Recompile and swap the active theme at runtime |
| `thematic_shiny(font = "auto")` | Make server-rendered plots match the theme |
| `bs_theme(brand = TRUE/FALSE/path)` | Apply a `_brand.yml` file's colors/fonts/logo |
| `includeCSS(path)` / `includeScript(path)` | Inline an external stylesheet/script |
| `addResourcePath(prefix, dir)` | Serve a directory under a custom URL prefix |

## Common mistakes

- **Scattering `tags$style()`/inline CSS for brand colors.** Drifts out of
  sync and skips plots — set the Sass variable via `bs_theme()`.
- **Plots looking unthemed while the UI matches.** Add
  `thematic::thematic_shiny()` for server-rendered plots.
- **Expecting `input_dark_mode()` to update `renderPlot()` output.**
  {thematic} reacts to `session$setCurrentTheme()`, not the client toggle.
- **`_brand.yml` silently ignored.** Confirm it's named `_brand.yml`
  (leading underscore) and sits in the app directory, a parent, or a
  `_brand/`/`brand/` subdirectory of either.
- **`bs_theme(brand = TRUE)` erroring on a fresh machine.** The {brand.yml}
  package is a bslib `Suggests`; install it explicitly.
- **Passing `bs_theme()` anywhere but a page's `theme =`.** Only meaningful
  there or in `session$setCurrentTheme()`.
- **Serving files from a directory without `addResourcePath()`.** Only
  `www/` is served automatically.
