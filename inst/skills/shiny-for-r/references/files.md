# File uploads and downloads in Shiny for R

## Overview

`fileInput()` lets a user pick one or more files; Shiny copies each upload to
a temp file and hands the server a data frame describing it. `downloadHandler()`
paired with `downloadButton()`/`downloadLink()` streams generated content back
to the browser as a file download. The anti-pattern this reference prevents is
reading `input$file$name` (the browser-supplied filename, not a real path on
disk) instead of `input$file$datapath` (the temp file that actually holds the
uploaded bytes) — `name` only works as a label, never as something to open.

## Upload a file: fileInput()

Add `fileInput(inputId, label, ...)` to the UI; the server reads
`input$<id>` as a data frame with one row per uploaded file, or `NULL` before
any upload. Guard with `req()` before touching it.

```r
library(shiny)

ui <- fluidPage(
  fileInput("file1", "Choose CSV file", accept = ".csv"),
  tableOutput("preview")
)

server <- function(input, output, session) {
  data <- reactive({
    file <- input$file1
    req(file)
    read.csv(file$datapath)
  })

  output$preview <- renderTable({
    head(data())
  })
}

shinyApp(ui, server)
```

Each row of `input$<id>` has `name` (browser-reported filename — for display
only), `size` (bytes), `type` (MIME type, or `""` if unknown), and `datapath`
(the temp file to actually open/read). `accept=` hints the browser's file
picker toward extensions (`".csv"`), MIME types (`"text/plain"`), or wildcards
(`"image/*"`) — it is not enforced on the server, so validate the uploaded
file yourself if it matters. Set `multiple = TRUE` to accept several files at
once; iterate over the rows of `input$<id>` to read each `datapath`. Uploaded
temp files live for the session and are deleted when it ends, or sooner if the
user uploads again to the same input — read the data you need right away,
typically inside a `reactive()`.

## Limit upload size: shiny.maxRequestSize

Shiny caps upload size at 5 MB by default; a larger request errors out before
your server code runs. Raise the limit with an option, set once per app (top
of `app.R`, or inside `server()` if you want a per-app-instance override):

```r
options(shiny.maxRequestSize = 30 * 1024^2) # 30 MB
```

## Download generated content: downloadHandler()

Assign `downloadHandler(filename, content)` to an `output` slot whose id
matches a `downloadButton()`/`downloadLink()` in the UI. `filename` is a
string or a zero-argument function returning one (evaluated per click, so it
can use reactive values); `content` is a function taking a single `file`
argument — a path to a nonexistent temp file — and must write the download's
bytes there.

```r
library(shiny)

ui <- fluidPage(
  downloadButton("download_csv", "Download CSV")
)

server <- function(input, output, session) {
  server_data <- mtcars

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("data-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(server_data, file)
    }
  )
}

shinyApp(ui, server)
```

`downloadButton()`/`downloadLink()` render as a styled link that triggers the
browser's normal download flow — no JavaScript or custom route needed.
`contentType=` on `downloadHandler()` sets the MIME type explicitly; left
`NULL`, it's guessed from the filename extension. Like other outputs, download
handlers are suspended while their button is hidden; set
`outputOptions(output, "download_csv", suspendWhenHidden = FALSE)` if you
trigger the download programmatically instead of by a visible click.

## Full app: CSV upload to filtered download

```r
library(shiny)

ui <- fluidPage(
  fileInput("file1", "Choose CSV file", accept = ".csv"),
  numericInput("min_value", "Minimum value", value = 0),
  downloadButton("download", "Download filtered CSV")
)

server <- function(input, output, session) {
  uploaded <- reactive({
    file <- input$file1
    req(file)
    read.csv(file$datapath)
  })

  filtered <- reactive({
    df <- uploaded()
    req(ncol(df) > 0)
    df[df[[1]] >= input$min_value, , drop = FALSE]
  })

  output$download <- downloadHandler(
    filename = function() "filtered.csv",
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
```

## Quick reference

| Function | Purpose |
|---|---|
| `fileInput(inputId, label, multiple=, accept=)` | Upload control; server value is a data frame per row |
| `input$<id>$datapath` | Path to the temp file holding uploaded bytes (use this, not `$name`) |
| `downloadHandler(filename, content)` | Assign to `output`; writes bytes to `content`'s `file` argument |
| `downloadButton(outputId, label, enabled = "auto")` / `downloadLink()` | UI trigger paired with a `downloadHandler()` output id |
| `options(shiny.maxRequestSize = ...)` | Raise the 5 MB default upload size limit |

## Common mistakes

- **Reading `input$file$name` as a path.** It's the browser's display
  filename, not a location on disk — open `input$file$datapath` instead.
- **Touching `input$file` with no guard.** It's `NULL` until a file is
  picked; wrap access in `req(input$file)`.
- **Upload rejected above 5 MB.** Raise
  `options(shiny.maxRequestSize = ...)`.
- **`content` function returning a value instead of writing to `file`.**
  `downloadHandler()` ignores the return value — write the bytes to the
  `file` path you were given.
- **Download button that never fires, or stays greyed out.** The `output`
  id assigned to `downloadHandler()` must match the `downloadButton()`/
  `downloadLink()` id in the UI. Under the default `enabled = "auto"` the
  button starts disabled and only enables once that handler registers, so a
  mismatched id leaves it permanently disabled. Pass `enabled = TRUE` to
  start enabled regardless, or `FALSE` to manage the state yourself.
- **Trusting `accept=` to enforce file type.** It only hints the browser's
  picker; validate the uploaded file's extension/contents on the server.
