# Bookmarking Shiny for R apps

## Overview

Bookmarking snapshots an app's `input` values (plus optional custom values)
and restores them later from a URL. Use it instead of hand-building query
strings or a custom persistence layer — Shiny already serializes inputs,
restores them on load, and gives callbacks for anything extra to save.

## Enable bookmarking

Call `enableBookmarking(store = c("url", "server", "disable"))` before
`shinyApp()`, or pass the same value as `shinyApp()`'s `enableBookmarking`
argument. `"url"` encodes all state in the query string — no server storage
needed, but browsers cap URL length (roughly 2000 characters), so it suits
apps with few inputs. `"server"` saves state to disk (a `shiny_bookmarks`
subdirectory by default) and puts only a short state id in the URL; it
handles many inputs and `fileInput()` uploads, which `"url"` cannot
serialize.

For restore to work, **`ui` must be a function that takes one argument,
`request`** — not a static UI object. Wrapping an existing UI is enough:

```r
library(shiny)

ui <- function(request) {
  fluidPage(
    textInput("txt", "Text"),
    checkboxInput("chk", "Checkbox"),
    bookmarkButton()
  )
}

server <- function(input, output, session) {
}

enableBookmarking("url")
shinyApp(ui, server)
```

With this in place, every input is saved when the user bookmarks and restored
automatically when the bookmarked URL loads — no per-input code required.

## Trigger a bookmark

`bookmarkButton(label = "Bookmark...", id = "._bookmark_")` is an
`actionButton()` that calls `session$doBookmark()` for you when clicked. To
trigger a bookmark from your own code (e.g. on every input change), call
`session$doBookmark()` directly inside an observer.

After state is saved, Shiny calls any registered `onBookmarked()` callback
with the bookmark URL. With none registered, Shiny shows a modal containing
the URL instead; registering `updateQueryString(url)` writes the URL into the
browser's address bar instead of popping a modal:

```r
library(shiny)

ui <- function(req) {
  fluidPage(
    textInput("txt", "Text"),
    checkboxInput("chk", "Checkbox")
  )
}

server <- function(input, output, session) {
  observe({
    reactiveValuesToList(input) # trigger this observer on every input change
    session$doBookmark()
  })
  onBookmarked(function(url) {
    updateQueryString(url)
  })
}

enableBookmarking("url")
shinyApp(ui, server)
```

Avoid combining "bookmark on every change" with `"server"` storage — it would
write a new file to disk on every keystroke.

## Save and restore custom values

`onBookmark(fun)` registers a function called just before state is saved;
`fun` receives a mutable state object whose `values` field (an environment)
holds arbitrary extra data, and whose `dir` field (server storage only) names
a directory for extra files. `onRestore(fun)` runs while a session restores,
after the server function runs but before reactives and outputs fire; its
state object exposes the same `values` (and read-only `input`). `onRestored(fun)`
is similar but runs after outputs have rendered and reached the browser — use
it for `update*Input()` calls that need the client ready.

```r
library(shiny)

ui <- function(req) {
  fluidPage(
    textInput("txt", "Input text"),
    bookmarkButton(),
    textOutput("last_saved")
  )
}

server <- function(input, output, session) {
  saved_time <- reactiveVal(NULL)

  onBookmark(function(state) {
    state$values$time <- as.character(Sys.time())
  })
  onRestore(function(state) {
    saved_time(state$values$time)
  })

  output$last_saved <- renderText({
    if (is.null(saved_time())) "Not restored yet" else paste("Last saved at", saved_time())
  })
}

enableBookmarking("url")
shinyApp(ui, server)
```

Unlike inputs, `state$values` never applies to the UI automatically — update
inputs yourself inside `onRestore()`/`onRestored()`.

## Exclude inputs from bookmarking

Call `setBookmarkExclude(names)` inside `server` to keep specific inputs out
of the saved state — for example, a value that's transient or only makes
sense for the current session. `passwordInput()` values are excluded
automatically.

```r
library(shiny)

ui <- function(request) {
  fluidPage(
    sliderInput("weight", "Slider (excluded)", 1, 100, 50),
    checkboxInput("chk", "Checkbox (saved)"),
    bookmarkButton()
  )
}

server <- function(input, output, session) {
  setBookmarkExclude("weight")
}

enableBookmarking("url")
shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `enableBookmarking(store)` | Turn on bookmarking: `"url"`, `"server"`, or `"disable"` |
| `bookmarkButton()` | Button that calls `session$doBookmark()` |
| `session$doBookmark()` | Trigger a bookmark programmatically |
| `onBookmark(fun)` / `onBookmarked(fun)` | Run just before / just after state is saved |
| `onRestore(fun)` / `onRestored(fun)` | Run while / after a session restores |
| `setBookmarkExclude(names)` | Keep listed inputs out of the saved state |
| `updateQueryString(url)` | Write a bookmark URL into the browser address bar |

## Common mistakes

- Defining `ui` as a plain object instead of `function(request) fluidPage(...)`
  -> restored values never make it into the UI.
- Hand-building a query string from `input` values to "bookmark" state ->
  loses file inputs, passwords, and custom values; call `enableBookmarking()`
  and let Shiny serialize the URL instead.
- Never calling `enableBookmarking()` -> `bookmarkButton()` and
  `session$doBookmark()` silently do nothing.
- Expecting `state$values` set in `onBookmark()` to restore on its own -> it
  does not; read it back and call the matching `update*Input()` inside
  `onRestore()`/`onRestored()`.
- Calling `update*Input()` from `onRestore()` too early -> the client isn't
  ready yet; move that call to `onRestored()`.
- Using `"url"` storage with many inputs or a `fileInput()` -> the URL
  overflows the browser's length limit or can't represent the file; switch to
  `"server"`.
