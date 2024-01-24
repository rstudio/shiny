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
  path <- paste0(
    getOption("shiny.mathjax.url", "https://mathjax.rstudio.com/latest/MathJax.js"),
    "?",
    getOption("shiny.mathjax.config", "config=TeX-AMS-MML_HTMLorMML")
  )
  tagList(
    tags$head(
      singleton(tags$script(src = path, type = 'text/javascript'))
    ),
    ...,
    tags$script(HTML('if (window.MathJax) MathJax.Hub.Queue(["Typeset", MathJax.Hub]);'))
  )
}

renderPage <- function(ui, showcase=0, testMode=FALSE) {
  lang <- getLang(ui)

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
      system_file("template", "default.html", package = "shiny"),
      lang = lang,
      body = ui,
      # this template is a complete HTML document
      document_ = TRUE
    )
  }

  shiny_deps <- c(
    list(jqueryDependency()),
    shinyDependencies()
  )

  if (testMode) {
    # Add code injection listener if in test mode
    shiny_deps[[length(shiny_deps) + 1]] <-
      htmlDependency(
        "shiny-testmode",
        get_package_version("shiny"),
        src = "www/shared",
        package = "shiny",
        script = "shiny-testmode.js",
        all_files = FALSE
      )
  }

  if (in_devmode()) {
    # If we're in dev mode, add a simple script to the head that injects a
    # global variable for the client to use to detect dev mode.
    shiny_deps[[length(shiny_deps) + 1]] <-
      htmlDependency(
        "shiny-devmode",
        get_package_version("shiny"),
        src = "www/shared",
        package = "shiny",
        head="<script>window.__SHINY_DEV_MODE__ = true;</script>",
        all_files = FALSE
      )
  }


  html <- renderDocument(ui, shiny_deps, processDep = createWebDependency)
  enc2utf8(paste(collapse = "\n", html))
}

jqueryDependency <- function() {
  version <- getOption("shiny.jquery.version", 3)
  if (version == 3) {
    return(htmlDependency(
      "jquery", version_jquery,
      src = "www/shared",
      package = "shiny",
      script = "jquery.min.js",
      all_files = FALSE
    ))
  }
  if (version == 1) {
    return(htmlDependency(
      "jquery", "1.12.4",
      src = "www/shared/legacy",
      package = "shiny",
      script = "jquery.min.js",
      all_files = FALSE
    ))
  }
  stop("Unsupported version of jQuery: ", version)
}

shinyDependencies <- function() {
  list(
    bslib::bs_dependency_defer(shinyDependencyCSS),
    htmlDependency(
      name = "shiny-javascript",
      version = get_package_version("shiny"),
      src = "www/shared",
      package = "shiny",
      script =
        if (isTRUE(
          get_devmode_option(
            "shiny.minified",
            TRUE
          )
        ))
          "shiny.min.js"
        else
          "shiny.js",
      all_files = FALSE
    )
  )
}

shinyDependencyCSS <- function(theme) {
  version <- get_package_version("shiny")

  if (!is_bs_theme(theme)) {
    return(htmlDependency(
      name = "shiny-css",
      version = version,
      src = "www/shared",
      package = "shiny",
      stylesheet = "shiny.min.css",
      all_files = FALSE
    ))
  }

  bs_version <- bslib::theme_version(theme)
  bootstrap_scss <- paste0("shiny.bootstrap", bs_version, ".scss")

  scss_home <- system_file("www/shared/shiny_scss", package = "shiny")
  scss_files <- file.path(scss_home, c(bootstrap_scss, "shiny.scss"))
  scss_files <- lapply(scss_files, sass::sass_file)

  bslib::bs_dependency(
    input = scss_files,
    theme = theme,
    name = "shiny-sass",
    version = version,
    cache_key_extra = version
  )
}

#' Create a Shiny UI handler
#'
#' @description `r lifecycle::badge("superseded")`
#'
#' @description Historically this function was used in ui.R files to register a user
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
  if (in_devmode()) {
    shinyDeprecated(
      "0.10.0", "shinyUI()",
      details = paste0(
        "When removing `shinyUI()`, ",
        "ensure that the last expression returned from ui.R is a user interface ",
        "normally supplied to `shinyUI(ui)`."
      )
    )
  }

  .globals$ui <- list(ui)
  ui
}

uiHttpHandler <- function(ui, uiPattern = "^/$") {

  force(ui)

  allowed_methods <- "GET"
  if (is.function(ui)) {
    allowed_methods <- attr(ui, "http_methods_supported", exact = TRUE) %||% allowed_methods
  }

  function(req) {
    if (!isTRUE(req$REQUEST_METHOD %in% allowed_methods))
      return(NULL)

    if (!isTRUE(grepl(uiPattern, req$PATH_INFO)))
      return(NULL)

    showcaseMode <- .globals$showcaseDefault
    if (.globals$showcaseOverride) {
      mode <- showcaseModeOfReq(req)
      if (!is.null(mode))
        showcaseMode <- mode
    }

    testMode <- getShinyOption("testmode", default = FALSE)

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

    if (inherits(uiValue, "httpResponse")) {
      return(uiValue)
    } else {
      html <- renderPage(uiValue, showcaseMode, testMode)
      return(httpResponse(200, content=html))
    }
  }
}
