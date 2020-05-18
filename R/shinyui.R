#' @include globals.R
NULL

#' Load the MathJax library and typeset math expressions
#'
#' This function adds MathJax to the page and typeset the math expressions (if
#' found) in the content `...`. It only needs to be called once in an app
#' unless the content is rendered *after* the page is loaded, e.g. via
#' [renderUI()], in which case we have to call it explicitly every
#' time we write math expressions to the output.
#' @param ... any HTML elements to apply MathJax to
#' @export
#' @examples withMathJax(helpText("Some math here $$\\alpha+\\beta$$"))
#' # now we can just write "static" content without withMathJax()
#' div("more math here $$\\sqrt{2}$$")
withMathJax <- function(...) {
  path <- 'https://mathjax.rstudio.com/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
  tagList(
    tags$head(
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

renderPage <- function(ui, connection, showcase=0, testMode=FALSE) {
  # If the ui is a NOT complete document (created by htmlTemplate()), then do some
  # preprocessing and make sure it's a complete document.
  if (!inherits(ui, "html_document")) {
    if (showcase > 0)
      ui <- showcaseUI(ui)

    # Wrap ui in body tag if it doesn't already have a single top-level body tag.
    if (!(inherits(ui, "shiny.tag") && ui$name == "body"))
      ui <- tags$body(ui)

    # Put the body into the default template
    ui <- htmlTemplate(
      system.file("template", "default.html", package = "shiny"),
      body = ui
    )
  }

  jquery <- function() {
    version <- getOption("shiny.jquery.version", 3)
    if (version == 3) {
      return(htmlDependency(
        "jquery", "3.5.1",
        c(href = "shared"),
        script = "jquery.min.js"
      ))
    }
    if (version == 1) {
      return(htmlDependency(
        "jquery", "1.12.4",
        c(href = "shared/legacy"),
        script = "jquery.min.js"
      ))
    }
    stop("Unsupported version of jQuery: ", version)
  }

  shiny_deps <- list(
    htmlDependency("json2", "2014.02.04", c(href="shared"), script = "json2-min.js"),
    jquery(),
    htmlDependency("shiny", utils::packageVersion("shiny"), c(href="shared"),
      script = if (getOption("shiny.minified", TRUE)) "shiny.min.js" else "shiny.js",
      stylesheet = "shiny.css")
  )

  if (testMode) {
    # Add code injection listener if in test mode
    shiny_deps[[length(shiny_deps) + 1]] <-
      htmlDependency("shiny-testmode", utils::packageVersion("shiny"),
        c(href="shared"), script = "shiny-testmode.js")
  }

  html <- renderDocument(ui, shiny_deps, processDep = createWebDependency)
  writeUTF8(html, con = connection)
}

#' Create a Shiny UI handler
#'
#' Historically this function was used in ui.R files to register a user
#' interface with Shiny. It is no longer required as of Shiny 0.10; simply
#' ensure that the last expression to be returned from ui.R is a user interface.
#' This function is kept for backwards compatibility with older applications. It
#' returns the value that is passed to it.
#'
#' @param ui A user interace definition
#' @return The user interface definition, without modifications or side effects.
#' @keywords internal
#' @export
shinyUI <- function(ui) {
  .globals$ui <- list(ui)
  ui
}

uiHttpHandler <- function(ui, uiPattern = "^/$") {

  force(ui)

  function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    if (!isTRUE(grepl(uiPattern, req$PATH_INFO)))
      return(NULL)

    textConn <- file(open = "w+")
    on.exit(close(textConn))

    showcaseMode <- .globals$showcaseDefault
    if (.globals$showcaseOverride) {
      mode <- showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }

    testMode <- .globals$testMode %OR% FALSE

    # Create a restore context using query string
    bookmarkStore <- getShinyOption("bookmarkStore", default = "disable")
    if (bookmarkStore == "disable") {
      # If bookmarking is disabled, use empty context
      restoreContext <- RestoreContext$new()
    } else {
      restoreContext <- RestoreContext$new(req$QUERY_STRING)
    }

    withRestoreContext(restoreContext, {
      uiValue <- NULL

      if (is.function(ui)) {
        if (length(formals(ui)) > 0) {
          # No corresponding ..stacktraceoff.., this is pure user code
          uiValue <- ..stacktraceon..(ui(req))
        } else {
          # No corresponding ..stacktraceoff.., this is pure user code
          uiValue <- ..stacktraceon..(ui())
        }
      } else {
        if (getCurrentRestoreContext()$active) {
          warning("Trying to restore saved app state, but UI code must be a function for this to work! See ?enableBookmarking")
        }
        uiValue <- ui
      }
    })
    if (is.null(uiValue))
      return(NULL)

    renderPage(uiValue, textConn, showcaseMode, testMode)
    html <- paste(readLines(textConn, encoding = 'UTF-8'), collapse='\n')
    return(httpResponse(200, content=enc2utf8(html)))
  }
}
