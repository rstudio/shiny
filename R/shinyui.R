#' @include globals.R
NULL

#' Load the MathJax library and typeset math expressions
#'
#' This function adds MathJax to the page and typeset the math expressions (if
#' found) in the content \code{...}. It only needs to be called once in an app
#' unless the content is rendered \emph{after} the page is loaded, e.g. via
#' \code{\link{renderUI}}, in which case we have to call it explicitly every
#' time we write math expressions to the output.
#' @param ... any HTML elements to apply MathJax to
#' @export
#' @examples withMathJax(helpText("Some math here $$\\alpha+\\beta$$"))
#' # now we can just write "static" content without withMathJax()
#' div("more math here $$\\sqrt{2}$$")
withMathJax <- function(...) {
  path <- 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML'
  tagList(
    tags$head(
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

renderPage <- function(ui, connection, showcase=0) {

  if (showcase > 0)
    ui <- tagList(tags$head(showcaseHead()), ui)

  result <- renderTags(ui)

  deps <- c(
    list(
      htmlDependency("jquery", "1.11.0", c(href="shared"), script = "jquery.js"),
      htmlDependency("shiny", packageVersion("shiny"), c(href="shared"),
        script = "shiny.js", stylesheet = "shiny.css")
    ),
    result$dependencies
  )
  deps <- resolveDependencies(deps)
  deps <- lapply(deps, createWebDependency)
  depStr <- paste(sapply(deps, function(dep) {
    sprintf("%s[%s]", dep$name, dep$version)
  }), collapse = ";")
  depHtml <- renderDependencies(deps, "href")

  # write preamble
  writeLines(c('<!DOCTYPE html>',
               '<html>',
               '<head>',
               '  <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
               sprintf('  <script type="application/shiny-singletons">%s</script>',
                       paste(result$singletons, collapse = ',')
               ),
               sprintf('  <script type="application/html-dependencies">%s</script>',
                       depStr
               ),
               depHtml
              ),
              con = connection)
  writeLines(c(result$head,
               '</head>',
               '<body>',
               recursive=TRUE),
             con = connection)

  if (showcase > 0) {
    # in showcase mode, emit containing elements and app HTML
    writeLines(as.character(showcaseBody(result$html)),
               con = connection)
  } else {
    # in normal mode, write UI html directly to connection
    writeLines(result$html, con = connection)
  }

  # write end document
  writeLines(c('</body>',
               '</html>'),
             con = connection)
}

#' Create a Shiny UI handler
#'
#' Historically this function was used in ui.R files to register a user
#' interface with Shiny. It is no longer required; simply ensure that the last
#' expression to be returned from ui.R is a user interface. This function is
#' kept for backwards compatibility with older applications. It returns the
#' value that is passed to it.
#'
#' @param ui A user interace definition
#' @return The user interface definition, without modifications or side effects.
#'
#' @export
shinyUI <- function(ui) {
  .globals$ui <- list(ui)
  ui
}

uiHttpHandler <- function(ui, path = "/") {

  force(ui)

  function(req) {
    if (!identical(req$REQUEST_METHOD, 'GET'))
      return(NULL)

    if (req$PATH_INFO != path)
      return(NULL)

    textConn <- textConnection(NULL, "w")
    on.exit(close(textConn))

    showcaseMode <- .globals$showcaseDefault
    if (.globals$showcaseOverride) {
      mode <- showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }
    uiValue <- if (is.function(ui)) {
      if (length(formals(ui)) > 0)
        ui(req)
      else
        ui()
    }
    else
      ui
    renderPage(uiValue, textConn, showcaseMode)
    html <- paste(textConnectionValue(textConn), collapse='\n')
    return(httpResponse(200, content=html))
  }
}
