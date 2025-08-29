utils::globalVariables('func', add = TRUE)

#' Mark a function as a render function
#'
#' `r lifecycle::badge("superseded")` Please use [`createRenderFunction()`] to
#' support async execution. (Shiny 1.1.0)
#'
#' Should be called by implementers of `renderXXX` functions in order to mark
#' their return values as Shiny render functions, and to provide a hint to Shiny
#' regarding what UI function is most commonly used with this type of render
#' function. This can be used in R Markdown documents to create complete output
#' widgets out of just the render function.
#'
#' Note that it is generally preferable to use [createRenderFunction()] instead
#' of `markRenderFunction()`. It essentially wraps up the user-provided
#' expression in the `transform` function passed to it, then passes the resulting
#' function to `markRenderFunction()`. It also provides a simpler calling
#' interface. There may be cases where `markRenderFunction()` must be used instead of
#' [createRenderFunction()] -- for example, when the `transform` parameter of
#' [createRenderFunction()] is not flexible enough for your needs.
#'
#' @param uiFunc A function that renders Shiny UI. Must take a single argument:
#'   an output ID.
#' @param renderFunc A function that is suitable for assigning to a Shiny output
#'   slot.
#' @param outputArgs A list of arguments to pass to the `uiFunc`. Render
#'   functions should include `outputArgs = list()` in their own parameter list,
#'   and pass through the value to `markRenderFunction`, to allow app authors to
#'   customize outputs. (Currently, this is only supported for dynamically
#'   generated UIs, such as those created by Shiny code snippets embedded in R
#'   Markdown documents).
#' @param cacheHint One of `"auto"`, `FALSE`, or some other information to
#'   identify this instance for caching using [bindCache()]. If `"auto"`, it
#'   will try to automatically infer caching information. If `FALSE`, do not
#'   allow caching for the object. Some render functions (such as [renderPlot])
#'   contain internal state that makes them unsuitable for caching.
#' @param cacheWriteHook Used if the render function is passed to `bindCache()`.
#'   This is an optional callback function to invoke before saving the value
#'   from the render function to the cache. This function must accept one
#'   argument, the value returned from `renderFunc`, and should return the value
#'   to store in the cache.
#' @param cacheReadHook Used if the render function is passed to `bindCache()`.
#'   This is an optional callback function to invoke after reading a value from
#'   the cache (if there is a cache hit). The function will be passed one
#'   argument, the value retrieved from the cache. This can be useful when some
#'   side effect needs to occur for a render function to behave correctly. For
#'   example, some render functions call [createWebDependency()] so that Shiny
#'   is able to serve JS and CSS resources.
#' @return The `renderFunc` function, with annotations.
#'
#' @seealso [createRenderFunction()]
#' @export
markRenderFunction <- function(
  uiFunc,
  renderFunc,
  outputArgs = list(),
  cacheHint = "auto",
  cacheWriteHook = NULL,
  cacheReadHook = NULL
) {
  # (Do not emit warning for superseded code, "since there’s no risk if you keep using it")
  # # This method is called by the superseding function, createRenderFunction().
  # if (in_devmode()) {
  #   shinyDeprecated("1.1.0", "markRenderFunction()", "createRenderFunction()")
  # }

  force(renderFunc)

  # a mutable object that keeps track of whether `useRenderFunction` has been
  # executed (this usually only happens when rendering Shiny code snippets in
  # an interactive R Markdown document); its initial value is FALSE
  hasExecuted <- Mutable$new()
  hasExecuted$set(FALSE)

  if (is.null(uiFunc)) {
    uiFunc <- function(id) {
      pre(
        "No UI/output function provided for render function. ",
        "Please see ?shiny::markRenderFunction and ?shiny::createRenderFunction."
      )
    }
  }

  if (identical(cacheHint, "auto")) {
    origUserFunc <- attr(renderFunc, "wrappedFunc", exact = TRUE)
    # The result could be NULL, but don't warn now because it'll only affect
    # users if they try to use caching. We'll warn when someone calls
    # bindCache() on this object.
    if (is.null(origUserFunc)) {
      cacheHint <- NULL
    } else {
      # Add in the wrapper render function and they output function, because
      # they can be useful for distinguishing two renderX functions that receive
      # the same user expression but do different things with them (like
      # renderText and renderPrint).
      cacheHint <- list(
        origUserFunc = origUserFunc,
        renderFunc   = renderFunc,
        outputFunc   = uiFunc
      )
    }
  }

  if (!is.null(cacheHint) && !is_false(cacheHint)) {
    if (!is.list(cacheHint)) {
      cacheHint <- list(cacheHint)
    }
    # For functions, remove the env and source refs because they can cause
    #   spurious differences.
    # For expressions, remove source refs.
    # For everything else, do nothing.
    cacheHint <- lapply(cacheHint, function(x) {
      if      (is.function(x)) formalsAndBody(x)
      else if (is_quosure(x)) zap_srcref(quo_get_expr(x))
      else if (is.language(x)) zap_srcref(x)
      else                     x
    })
  }

  wrappedRenderFunc <- function(...) {
    # if the user provided something through `outputArgs` BUT the
    # `useRenderFunction` was not executed, then outputArgs will be ignored,
    # so throw a warning to let user know the correct usage
    if (length(outputArgs) != 0 && !hasExecuted$get()) {
      warning("Unused argument: outputArgs. The argument outputArgs is only ",
              "meant to be used when embedding snippets of Shiny code in an ",
              "R Markdown code chunk (using runtime: shiny). When running a ",
              "full Shiny app, please set the output arguments directly in ",
              "the corresponding output function of your UI code.")
      # stop warning from happening again for the same object
      hasExecuted$set(TRUE)
    }
    if (is.null(formals(renderFunc))) renderFunc()
    else renderFunc(...)
  }

  otelAttrs <- otel_srcref_attributes(attr(renderFunc, "wrappedFunc"))

  ret <- structure(
    wrappedRenderFunc,
    class          = c("shiny.render.function", "function"),
    outputFunc     = uiFunc,
    outputArgs     = outputArgs,
    hasExecuted    = hasExecuted,
    cacheHint      = cacheHint,
    cacheWriteHook = cacheWriteHook,
    cacheReadHook  = cacheReadHook,
    otelAttrs      = otelAttrs
  )

  if (has_otel_bind("output")) {
    ret <- bindOtel(ret)
  }

  ret
}

#' @export
print.shiny.render.function <- function(x, ...) {
  cat_line("<shiny.render.function>")
}

#' Implement custom render functions
#'
#' Developer-facing utilities for implementing a custom `renderXXX()` function.
#' Before using these utilities directly, consider using the [`htmlwidgets`
#' package](http://www.htmlwidgets.org/develop_intro.html) to implement custom
#' outputs (i.e., custom `renderXXX()`/`xxxOutput()` functions). That said,
#' these utilities can be used more directly if a full-blown htmlwidget isn't
#' needed and/or the user-supplied reactive expression needs to be wrapped in
#' additional call(s).
#'
#' To implement a custom `renderXXX()` function, essentially 2 things are needed:
#'   1. Capture the user's reactive expression as a function.
#'      * New `renderXXX()` functions can use `quoToFunction()` for this, but
#'      already existing `renderXXX()` functions that contain `env` and `quoted`
#'      parameters may want to continue using `installExprFunction()` for better
#'      legacy support (see examples).
#'   2. Flag the resulting function (from 1) as a Shiny rendering function and
#'   also provide a UI container for displaying the result of the rendering
#'   function.
#'      * `createRenderFunction()` is currently recommended (instead of
#'      [markRenderFunction()]) for this step (see examples).
#'
#' @param func A function without parameters, that returns user data. If the
#'   returned value is a promise, then the render function will proceed in async
#'   mode.
#' @param transform A function that takes four arguments: `value`,
#'   `session`, `name`, and `...` (for future-proofing). This
#'   function will be invoked each time a value is returned from `func`,
#'   and is responsible for changing the value into a JSON-ready value to be
#'   JSON-encoded and sent to the browser.
#' @param outputFunc The UI function that is used (or most commonly used) with
#'   this render function. This can be used in R Markdown documents to create
#'   complete output widgets out of just the render function.
#' @inheritParams markRenderFunction
#' @return An annotated render function, ready to be assigned to an
#'   `output` slot.
#'
#' @examples
#' # A custom render function that repeats the supplied value 3 times
#' renderTriple <- function(expr) {
#'   # Wrap user-supplied reactive expression into a function
#'   func <- quoToFunction(rlang::enquo0(expr))
#'
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) {
#'       paste(rep(value, 3), collapse=", ")
#'     },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # For better legacy support, consider using installExprFunction() over quoToFunction()
#' renderTripleLegacy <- function(expr, env = parent.frame(), quoted = FALSE) {
#'   func <- installExprFunction(expr, "func", env, quoted)
#'
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) {
#'       paste(rep(value, 3), collapse=", ")
#'     },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Test render function from the console
#' reactiveConsole(TRUE)
#'
#' v <- reactiveVal("basic")
#' r <- renderTriple({ v() })
#' r()
#' #> [1] "basic, basic, basic"
#'
#' # User can supply quoted code via rlang::quo(). Note that evaluation of the
#' # expression happens when r2() is invoked, not when r2 is created.
#' q <- rlang::quo({ v() })
#' r2 <- rlang::inject(renderTriple(!!q))
#' v("rlang")
#' r2()
#' #> [1] "rlang, rlang, rlang"
#'
#' # Supplying quoted code without rlang::quo() requires installExprFunction()
#' expr <- quote({ v() })
#' r3 <- renderTripleLegacy(expr, quoted = TRUE)
#' v("legacy")
#' r3()
#' #> [1] "legacy, legacy, legacy"
#'
#' # The legacy approach also supports with quosures (env is ignored in this case)
#' q <- rlang::quo({ v() })
#' r4 <- renderTripleLegacy(q, quoted = TRUE)
#' v("legacy-rlang")
#' r4()
#' #> [1] "legacy-rlang, legacy-rlang, legacy-rlang"
#'
#' # Turn off reactivity in the console
#' reactiveConsole(FALSE)
#'
#' @export
createRenderFunction <- function(
  func,
  transform = function(value, session, name, ...) value,
  outputFunc = NULL,
  outputArgs = NULL,
  cacheHint = "auto",
  cacheWriteHook = NULL,
  cacheReadHook = NULL
) {
  renderFunc <- function(shinysession, name, ...) {
    hybrid_chain(
      func(),
      function(value) {
        transform(value, shinysession, name, ...)
      }
    )
  }

  # Hoist func's wrappedFunc attribute into renderFunc, so that when we pass
  # renderFunc on to markRenderFunction, it is able to find the original user
  # function.
  if (identical(cacheHint, "auto")) {
    attr(renderFunc, "wrappedFunc") <- attr(func, "wrappedFunc", exact = TRUE)
  }

  markRenderFunction(outputFunc, renderFunc, outputArgs, cacheHint,
                     cacheWriteHook, cacheReadHook)
}

useRenderFunction <- function(renderFunc, inline = FALSE) {
  outputFunction <- attr(renderFunc, "outputFunc")
  outputArgs <- attr(renderFunc, "outputArgs")
  hasExecuted <- attr(renderFunc, "hasExecuted")
  hasExecuted$set(TRUE)

  for (arg in names(outputArgs)) {
    if (!arg %in% names(formals(outputFunction))) {
      stop(paste0("Unused argument: in 'outputArgs', '",
                  arg, "' is not an valid argument for ",
                  "the output function"))
      outputArgs[[arg]] <- NULL
    }
  }

  id <- createUniqueId(8, "out")

  o <- getDefaultReactiveDomain()$output
  if (!is.null(o)) {
    o[[id]] <- renderFunc
    # If there's a namespace, we must respect it
    id <- getDefaultReactiveDomain()$ns(id)
  }

  # Make the id the first positional argument
  outputArgs <- c(list(id), outputArgs)

  if (is.logical(formals(outputFunction)[["inline"]]) && !("inline" %in% names(outputArgs))) {
    outputArgs[["inline"]] <- inline
  }

  do.call(outputFunction, outputArgs)
}

#' @export
#' @method as.tags shiny.render.function
as.tags.shiny.render.function <- function(x, ..., inline = FALSE) {
  useRenderFunction(x, inline = inline)
}

# Get relevant attributes from a render function object.
renderFunctionAttributes <- function(x) {
  attrs <- c("outputFunc", "outputArgs", "hasExecuted", "cacheHint", "otelAttrs")
  names(attrs) <- attrs
  lapply(attrs, function(name) attr(x, name, exact = TRUE))
}

# Add a named list of attributes to an object
addAttributes <- function(x, attrs) {
  nms <- names(attrs)
  for (i in seq_along(attrs)) {
    attr(x, nms[i]) <- attrs[[i]]
  }
  x
}


#' Mark a render function with attributes that will be used by the output
#'
#' @inheritParams markRenderFunction
#' @param snapshotExclude If TRUE, exclude the output from test snapshots.
#' @param snapshotPreprocess A function for preprocessing the value before
#'   taking a test snapshot.
#'
#' @keywords internal
markOutputAttrs <- function(renderFunc, snapshotExclude = NULL,
  snapshotPreprocess = NULL)
{
  # Add the outputAttrs attribute if necessary
  if (is.null(attr(renderFunc, "outputAttrs", TRUE))) {
    attr(renderFunc, "outputAttrs") <- list()
  }

  if (!is.null(snapshotExclude)) {
    attr(renderFunc, "outputAttrs")$snapshotExclude <- snapshotExclude
  }

  if (!is.null(snapshotPreprocess)) {
    attr(renderFunc, "outputAttrs")$snapshotPreprocess <- snapshotPreprocess
  }

  renderFunc
}

#' Image file output
#'
#' Renders a reactive image that is suitable for assigning to an `output`
#' slot.
#'
#' The expression `expr` must return a list containing the attributes for
#' the `img` object on the client web page. For the image to display,
#' properly, the list must have at least one entry, `src`, which is the
#' path to the image file. It may also useful to have a `contentType`
#' entry specifying the MIME type of the image. If one is not provided,
#' `renderImage` will try to autodetect the type, based on the file
#' extension.
#'
#' Other elements such as `width`, `height`, `class`, and
#' `alt`, can also be added to the list, and they will be used as
#' attributes in the `img` object.
#'
#' The corresponding HTML output tag should be `div` or `img` and have
#' the CSS class name `shiny-image-output`.
#'
#' @seealso
#' * For more details on how the images are generated, and how to control
#'   the output, see [plotPNG()].
#' * Use [outputOptions()] to set general output options for an image output.
#'
#' @param expr An expression that returns a list.
#' @inheritParams renderUI
#' @param deleteFile Should the file in `func()$src` be deleted after
#'   it is sent to the client browser? Generally speaking, if the image is a
#'   temp file generated within `func`, then this should be `TRUE`;
#'   if the image is not a temp file, this should be `FALSE`. (For backward
#'   compatibility reasons, if this argument is missing, a warning will be
#'   emitted, and if the file is in the temp directory it will be deleted. In
#'   the future, this warning will become an error.)
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [imageOutput()] when `renderImage` is used in an
#'   interactive R Markdown document.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   sliderInput("n", "Number of observations", 2, 1000, 500),
#'   plotOutput("plot1"),
#'   plotOutput("plot2"),
#'   plotOutput("plot3")
#' )
#'
#' server <- function(input, output, session) {
#'
#'   # A plot of fixed size
#'   output$plot1 <- renderImage({
#'     # A temp file to save the output. It will be deleted after renderImage
#'     # sends it, because deleteFile=TRUE.
#'     outfile <- tempfile(fileext='.png')
#'
#'     # Generate a png
#'     png(outfile, width=400, height=400)
#'     hist(rnorm(input$n))
#'     dev.off()
#'
#'     # Return a list
#'     list(src = outfile,
#'          alt = "This is alternate text")
#'   }, deleteFile = TRUE)
#'
#'   # A dynamically-sized plot
#'   output$plot2 <- renderImage({
#'     # Read plot2's width and height. These are reactive values, so this
#'     # expression will re-run whenever these values change.
#'     width  <- session$clientData$output_plot2_width
#'     height <- session$clientData$output_plot2_height
#'
#'     # A temp file to save the output.
#'     outfile <- tempfile(fileext='.png')
#'
#'     png(outfile, width=width, height=height)
#'     hist(rnorm(input$n))
#'     dev.off()
#'
#'     # Return a list containing the filename
#'     list(src = outfile,
#'          width = width,
#'          height = height,
#'          alt = "This is alternate text")
#'   }, deleteFile = TRUE)
#'
#'   # Send a pre-rendered image, and don't delete the image after sending it
#'   # NOTE: For this example to work, it would require files in a subdirectory
#'   # named images/
#'   output$plot3 <- renderImage({
#'     # When input$n is 1, filename is ./images/image1.jpeg
#'     filename <- normalizePath(file.path('./images',
#'                               paste('image', input$n, '.jpeg', sep='')))
#'
#'     # Return a list containing the filename
#'     list(src = filename)
#'   }, deleteFile = FALSE)
#' }
#'
#' shinyApp(ui, server)
#' }
renderImage <- function(expr, env = parent.frame(), quoted = FALSE,
                        deleteFile, outputArgs=list())
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderImage")

  # missing() must be used directly within the function with the given arg
  if (missing(deleteFile)) {
    deleteFile <- NULL
  }

  # Tracks whether we've reported the `deleteFile` warning yet; we don't want to
  # do it on every invalidation (though we will end up doing it at least once
  # per output per session).
  warned <- FALSE

  createRenderFunction(func,
    transform = function(imageinfo, session, name, ...) {
      shouldDelete <- deleteFile

      # jcheng 2020-05-08
      #
      # Until Shiny 1.5.0, the default for deleteFile was, incredibly, TRUE.
      # Changing it to default to FALSE might cause existing Shiny apps to pile
      # up images in their temp directory (for long lived R processes). Not
      # having a default (requiring explicit value) is the right long-term move,
      # but would break today's apps.
      #
      # Compromise we decided on was to eventually require TRUE/FALSE, but for
      # now, change the default behavior to only delete temp files; and emit a
      # warning encouraging people to not rely on the default.
      if (is.null(shouldDelete)) {
        shouldDelete <- isTRUE(try(silent = TRUE,
          file.exists(imageinfo$src) && isTemp(imageinfo$src, mustExist = TRUE)
        ))

        if (!warned) {
          warned <<- TRUE
          warning("The renderImage output named '",
            getCurrentOutputInfo()$name,
            "' is missing the deleteFile argument; as of Shiny 1.5.0, you must ",
            "use deleteFile=TRUE or deleteFile=FALSE. (This warning will ",
            "become an error in a future version of Shiny.)",
            call. = FALSE
          )
        }
      }

      if (shouldDelete) {
        on.exit(unlink(imageinfo$src), add = TRUE)
      }

      # If contentType not specified, autodetect based on extension
      contentType <- imageinfo$contentType %||% getContentType(imageinfo$src)

      # Extra values are everything in imageinfo except 'src' and 'contentType'
      extra_attr <- imageinfo[!names(imageinfo) %in% c('src', 'contentType')]

      # Return a list with src, and other img attributes
      c(src = session$fileUrl(name, file=imageinfo$src, contentType=contentType),
        extra_attr)
    },
    imageOutput,
    outputArgs,
    cacheHint = FALSE
  )
}

# TODO: If we ever take a dependency on fs, it'd be great to replace this with
# fs::path_has_parent().
isTemp <- function(path, tempDir = tempdir(), mustExist) {
  if (!isTRUE(mustExist)) {
    # jcheng 2020-05-11: I added mustExist just to make it totally obvious that
    # the path must exist. We don't support the case where the file doesn't
    # exist because it makes normalizePath unusable, and it's a bit scary
    # security-wise to compare paths without normalization. Using fs would fix
    # this as it knows how to normalize paths that don't exist.
    stop("isTemp(mustExist=FALSE) is not implemented")
  }

  if (mustExist && !file.exists(path)) {
    stop("path does not exist")
  }

  if (nchar(tempDir) == 0 || !dir.exists(tempDir)) {
    # This should never happen, but just to be super paranoid...
    stop("invalid temp dir")
  }

  path <- normalizePath(path, winslash = "/", mustWork = mustExist)

  tempDir <- normalizePath(tempDir, winslash = "/", mustWork = TRUE)
  if (path == tempDir) {
    return(FALSE)
  }

  tempDir <- ensure_trailing_slash(tempDir)
  if (path == tempDir) {
    return(FALSE)
  }

  return(substr(path, 1, nchar(tempDir)) == tempDir)
}

#' Text Output
#'
#' @description
#' `renderPrint()` prints the result of `expr`, while `renderText()` pastes it
#' together into a single string. `renderPrint()` is equivalent to [print()];
#' `renderText()` is equivalent to [cat()]. Both functions capture all other
#' printed output generated while evaluating `expr`.
#'
#' `renderPrint()` is usually paired with [verbatimTextOutput()];
#' `renderText()` is usually paired with [textOutput()].
#'
#' @details
#' The corresponding HTML output tag can be anything (though `pre` is
#' recommended if you need a monospace font and whitespace preserved) and should
#' have the CSS class name `shiny-text-output`.
#'
#' @return
#' For `renderPrint()`, note the given expression returns `NULL` then `NULL`
#' will actually be visible in the output. To display nothing, make your
#' function return [invisible()].
#'
#' @param expr An expression to evaluate.
#' @inheritParams renderUI
#' @param width Width of printed output.
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [verbatimTextOutput()] or [textOutput()] when the functions are
#'   used in an interactive RMarkdown document.
#'
#' @example res/text-example.R
#' @seealso [outputOptions()]
#' @export
renderPrint <- function(expr, env = parent.frame(), quoted = FALSE,
                        width = getOption('width'), outputArgs=list())
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderPrint")

  # Set a promise domain that sets the console width
  #   and captures output
  # op <- options(width = width)
  # on.exit(options(op), add = TRUE)

  renderFunc <- function(shinysession, name, ...) {
    domain <- createRenderPrintPromiseDomain(width)
    hybrid_chain(
      {
        promises::with_promise_domain(domain, func())
      },
      function(value) {
        res <- withVisible(value)
        if (res$visible) {
          cat(file = domain$conn, paste(utils::capture.output(res$value, append = TRUE), collapse = "\n"))
        }
        paste(readLines(domain$conn, warn = FALSE), collapse = "\n")
      },
      finally = function() {
        close(domain$conn)
      }
    )
  }

  markRenderFunction(
    verbatimTextOutput,
    renderFunc,
    outputArgs,
    cacheHint = list(
      label = "renderPrint",
      origUserExpr = installedFuncExpr(func)
    )
  )
}

createRenderPrintPromiseDomain <- function(width) {
  f <- file()

  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      force(onFulfilled)
      function(...) {
        op <- options(width = width)
        on.exit(options(op), add = TRUE)

        sink(f, append = TRUE)
        on.exit(sink(NULL), add = TRUE)

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      force(onRejected)
      function(...) {
        op <- options(width = width)
        on.exit(options(op), add = TRUE)

        sink(f, append = TRUE)
        on.exit(sink(NULL), add = TRUE)

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      op <- options(width = width)
      on.exit(options(op), add = TRUE)

      sink(f, append = TRUE)
      on.exit(sink(NULL), add = TRUE)

      force(expr)
    },
    conn = f
  )
}

#' @param sep A separator passed to `cat` to be appended after each
#'   element.
#' @export
#' @rdname renderPrint
renderText <- function(expr, env = parent.frame(), quoted = FALSE,
                       outputArgs=list(), sep=" ") {

  func <- installExprFunction(expr, "func", env, quoted, label = "renderText")

  createRenderFunction(
    func,
    function(value, session, name, ...) {
      paste(utils::capture.output(cat(value, sep=sep)), collapse="\n")
    },
    textOutput,
    outputArgs
  )
}

#' UI Output
#'
#' Renders reactive HTML using the Shiny UI library.
#'
#' The corresponding HTML output tag should be `div` and have the CSS class
#' name `shiny-html-output` (or use [uiOutput()]).
#'
#' @param expr An expression that returns a Shiny tag object, [HTML()],
#'   or a list of such objects.
#' @template param-env
#' @templateVar x expr
#' @templateVar env env
#' @templateVar quoted quoted
#' @template param-quoted
#' @templateVar x expr
#' @templateVar quoted quoted
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [uiOutput()] when `renderUI` is used in an
#'   interactive R Markdown document.
#'
#' @seealso [uiOutput()], [outputOptions()]
#' @export
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   uiOutput("moreControls")
#' )
#'
#' server <- function(input, output) {
#'   output$moreControls <- renderUI({
#'     tagList(
#'       sliderInput("n", "N", 1, 1000, 500),
#'       textInput("label", "Label")
#'     )
#'   })
#' }
#' shinyApp(ui, server)
#' }
#'
renderUI <- function(expr, env = parent.frame(), quoted = FALSE,
                     outputArgs = list())
{
  func <- installExprFunction(expr, "func", env, quoted, label = "renderUI")

  createRenderFunction(
    func,
    function(result, shinysession, name, ...) {
      if (is.null(result) || length(result) == 0)
        return(NULL)

      processDeps(result, shinysession)
    },
    uiOutput,
    outputArgs
  )
}

#' File Downloads
#'
#' Allows content from the Shiny application to be made available to the user as
#' file downloads (for example, downloading the currently visible data as a CSV
#' file). Both filename and contents can be calculated dynamically at the time
#' the user initiates the download. Assign the return value to a slot on
#' `output` in your server function, and in the UI use
#' [downloadButton()] or [downloadLink()] to make the
#' download available.
#'
#' @param filename A string of the filename, including extension, that the
#'   user's web browser should default to when downloading the file; or a
#'   function that returns such a string. (Reactive values and functions may be
#'   used from this function.)
#' @param content A function that takes a single argument `file` that is a
#'   file path (string) of a nonexistent temp file, and writes the content to
#'   that file path. (Reactive values and functions may be used from this
#'   function.)
#' @param contentType A string of the download's
#'   [content type](https://en.wikipedia.org/wiki/Internet_media_type), for
#'   example `"text/csv"` or `"image/png"`. If `NULL`, the content type
#'   will be guessed based on the filename extension, or
#'   `application/octet-stream` if the extension is unknown.
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to [downloadButton()] when `downloadHandler` is used
#'   in an interactive R Markdown document.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' ui <- fluidPage(
#'   downloadButton("downloadData", "Download")
#' )
#'
#' server <- function(input, output) {
#'   # Our dataset
#'   data <- mtcars
#'
#'   output$downloadData <- downloadHandler(
#'     filename = function() {
#'       paste("data-", Sys.Date(), ".csv", sep="")
#'     },
#'     content = function(file) {
#'       write.csv(data, file)
#'     }
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'
#' @seealso
#' * The download handler, like other outputs, is suspended (disabled) by
#'   default for download buttons and links that are hidden. Use
#'   [outputOptions()] to control this behavior, e.g. to set
#'   `suspendWhenHidden = FALSE` if the download is initiated by
#'   programmatically clicking on the download button using JavaScript.
#' @export
downloadHandler <- function(filename, content, contentType=NULL, outputArgs=list()) {
  renderFunc <- function(shinysession, name, ...) {
    shinysession$registerDownload(name, filename, contentType, content)
  }
  snapshotExclude(
    markRenderFunction(downloadButton, renderFunc, outputArgs, cacheHint = FALSE)
  )
}

#' Table output with the JavaScript DataTables library
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated, use
#' [DT::renderDT()](https://rstudio.github.io/DT/shiny.html) instead. It
#' provides a superset of functionality, better performance, and better user
#' experience.
#'
#' @param expr An expression that returns a data frame or a matrix.
#' @inheritParams renderTable
#' @param options A list of initialization options to be passed to DataTables,
#'   or a function to return such a list.  You can find a complete list of
#'   options at <https://datatables.net/reference/option/>.
#'
#'   Any top-level strings with class `"AsIs"` (as created by [I()]) will be
#'   evaluated in JavaScript. This is useful when the type of the option value
#'   is not supported in JSON, e.g., a JavaScript function, which can be
#'   obtained by  evaluating a character string. This only applies to the
#'   root-level elements of options list, and does not worked for lower-level
#'   elements in the list.
#' @param searchDelay The delay for searching, in milliseconds (to avoid too
#'   frequent search requests).
#' @param callback A JavaScript function to be applied to the DataTable object.
#'   This is useful for DataTables plug-ins, which often require the DataTable
#'   instance to be available.
#' @param escape Whether to escape HTML entities in the table: `TRUE` means
#'   to escape the whole table, and `FALSE` means not to escape it.
#'   Alternatively, you can specify numeric column indices or column names to
#'   indicate which columns to escape, e.g. `1:5` (the first 5 columns),
#'   `c(1, 3, 4)`, or `c(-1, -3)` (all columns except the first and
#'   third), or `c('Species', 'Sepal.Length')`.
#' @param outputArgs A list of arguments to be passed through to the implicit
#'   call to `dataTableOutput()` when `renderDataTable()` is used
#'   in an interactive R Markdown document.
#' @export
#' @inheritParams renderPlot
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'   # pass a callback function to DataTables using I()
#'   shinyApp(
#'     ui = fluidPage(
#'       fluidRow(
#'         column(12,
#'           dataTableOutput('table')
#'         )
#'       )
#'     ),
#'     server = function(input, output) {
#'       output$table <- renderDataTable(iris,
#'         options = list(
#'           pageLength = 5,
#'           initComplete = I("function(settings, json) {alert('Done.');}")
#'         )
#'       )
#'     }
#'   )
#' }
#' @keywords internal
renderDataTable <- function(expr, options = NULL, searchDelay = 500,
                            callback = 'function(oTable) {}', escape = TRUE,
                            env = parent.frame(), quoted = FALSE,
                            outputArgs = list()) {

  legacy <- useLegacyDataTable(
    from = "shiny::renderDataTable()",
    to = "DT::renderDT()"
  )

  if (!quoted) {
    expr <- substitute(expr)
    quoted <- TRUE
  }

  if (legacy) {

    legacyRenderDataTable(
      expr, env = env, quoted = quoted,
      options = options,
      searchDelay = searchDelay,
      callback = callback,
      escape = escape,
      outputArgs = outputArgs
    )

  } else {

    if (!missing(searchDelay)) {
      warning("Ignoring renderDataTable()'s searchDelay value (since DT::renderDT() has no equivalent).")
    }

    force(options)
    force(callback)
    force(escape)
    force(outputArgs)

    DT::renderDataTable(
      expr, env = env, quoted = quoted,
      options = if (is.null(options)) list() else options,
      # Turn function into a statement
      callback = DT::JS(paste0("(", callback, ")(table)")),
      escape = escape,
      outputArgs = outputArgs
    )
  }
}


legacyRenderDataTable <- function(expr, options = NULL, searchDelay = 500,
                                  callback = 'function(oTable) {}', escape = TRUE,
                                  env = parent.frame(), quoted = FALSE,
                                  outputArgs=list()) {

  func <- installExprFunction(expr, "func", env, quoted, label = "renderDataTable")

  renderFunc <- function(shinysession, name, ...) {
    if (is.function(options)) options <- options()
    options <- checkDT9(options)
    res <- checkAsIs(options)
    hybrid_chain(
      func(),
      function(data) {
        if (length(dim(data)) != 2) return() # expects a rectangular data object
        if (is.data.frame(data)) data <- as.data.frame(data)
        action <- shinysession$registerDataObj(name, data, dataTablesJSON)
        colnames <- colnames(data)
        # if escape is column names, turn names to numeric indices
        if (is.character(escape)) {
          escape <- stats::setNames(seq_len(ncol(data)), colnames)[escape]
          if (any(is.na(escape)))
            stop("Some column names in the 'escape' argument not found in data")
        }
        colnames[escape] <- htmlEscape(colnames[escape])
        if (!is.logical(escape)) {
          if (!is.numeric(escape))
            stop("'escape' must be TRUE, FALSE, or a numeric vector, or column names")
          escape <- paste(escape, collapse = ',')
        }
        list(
          colnames = colnames, action = action, options = res$options,
          evalOptions = if (length(res$eval)) I(res$eval), searchDelay = searchDelay,
          callback = paste(callback, collapse = '\n'), escape = escape
        )
      }
    )
  }

  renderFunc <- markRenderFunction(dataTableOutput, renderFunc, outputArgs,
    cacheHint = FALSE)

  renderFunc <- snapshotPreprocessOutput(renderFunc, function(value) {
    # Remove the action field so that it's not saved in test snapshots. It
    # contains a value that changes every time an app is run, and shouldn't be
    # stored for test snapshots. It will be something like:
    # "session/e0d14d3fe97f672f9655a127f2a1e079/dataobj/table?w=&nonce=7f5d6d54e22450a3"
    value$action <- NULL
    value
  })

  renderFunc
}

# a data frame containing the DataTables 1.9 and 1.10 names
DT10Names <- function() {
  rbind(
    utils::read.table(
      system_file('www/shared/datatables/upgrade1.10.txt', package = 'shiny'),
      stringsAsFactors = FALSE
    ),
    c('aoColumns', 'Removed')  # looks like an omission on the upgrade guide
  )
}

# check DataTables 1.9.x options, and give instructions for upgrading to 1.10.x
checkDT9 <- function(options) {
  nms <- names(options)
  if (length(nms) == 0L) return(options)
  DT10 <- DT10Names()
  # e.g. the top level option name for oLanguage.sSearch should be oLanguage
  i <- nms %in% gsub('[.].*', '', DT10[, 1])
  if (!any(i)) return(options)  # did not see old option names, ready to go!
  msg <- paste(
    'shiny (>= 0.10.2) has upgraded DataTables from 1.9.4 to 1.10.2, ',
    'and DataTables 1.10.x uses different parameter names with 1.9.x. ',
    'Please follow the upgrade guide https://datatables.net/upgrade/1.10-convert',
    ' to change your DataTables parameter names:\n\n',
    paste(utils::formatUL(nms[i]), collapse = '\n'), '\n', sep = ''
  )
  j <- gsub('[.].*', '', DT10[, 1]) %in% nms
  # I cannot help you upgrade automatically in these cases, so I have to stop
  if (any(grepl('[.]', DT10[j, 1])) || any(grepl('[.]', DT10[j, 2]))) stop(msg)
  warning(msg)
  nms10 <- DT10[match(nms[i], DT10[, 1]), 2]
  if (any(nms10 == 'Removed')) stop(
    "These parameters have been removed in DataTables 1.10.x:\n\n",
    paste(utils::formatUL(nms[i][nms10 == 'Removed']), collapse = '\n'),
    "\n\n", msg
  )
  names(options)[i] <- nms10
  options
}
