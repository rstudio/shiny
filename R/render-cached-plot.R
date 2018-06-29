#' Plot output with cached images
#'
#' Renders a reactive plot, with plot images cached to disk.
#'
#' \code{expr} is an expression that generates a plot, similar to that in
#' \code{renderPlot}. Unlike with \code{renderPlot}, this expression does not
#' take reactive dependencies. It is re-executed only when the cache key
#' changes.
#'
#' \code{cacheKeyExpr} is an expression which, when evaluated, returns an object
#' which will be serialized and hashed using the \code{\link[digest]{digest}}
#' function to generate a string that will be used as a cache key. This key is
#' used to identify the contents of the plot: if the cache key is the same as a
#' previous time, it assumes that the plot is the same and can be retrieved from
#' the cache.
#'
#' This \code{cacheKeyExpr} is reactive, and so it will be re-evaluated when any
#' upstream reactives are invalidated. This will also trigger re-execution of
#' the plotting expression, \code{expr}.
#'
#' The key should consist of "normal" R objects, like vectors and lists. Lists
#' should in turn contain other normal R objects. If the key contains
#' environments, external pointers, or reference objects -- or even if it has
#' such objects attached as attributes -- then it is possible that it will
#' change unpredictably even when you do not expect it to. Additionally, because
#' the entire key is serialized and hashed, if it contains a very large object
#' -- a large data set, for example -- there may be a noticeable performance
#' penalty.
#'
#' If you face these issues with the cache key, you can work around them by
#' extracting out the important parts of the objects, and/or by converting them
#' to normal R objects before returning them. Your expression could even
#' serialize and hash that information in an efficient way and return a string,
#' which will in turn be hashed (very quickly) by the
#' \code{\link[digest]{digest}} function.
#'
#'
#' @section Cache scoping:
#'
#'   There are a number of different ways you may want to scope the cache. For
#'   example, you may want each user session to have their own plot cache, or
#'   you may want each run of the application to have a cache (shared among
#'   possibly multiple simultaneous user sessions), or you may want to have a
#'   cache that persists even after the application is shut down and started
#'   again.
#'
#'   To control the scope of the cache, use the \code{cache} parameter. There
#'   are two ways of having Shiny automatically create and clean up the disk
#'   cache.
#'
#' \describe{
#'   \item{1}{To scope the cache to one session, use \code{cache="session"}.
#'     When a new user session starts -- in other words, when a web browser
#'     visits the Shiny application -- a new cache will be created on disk
#'     for that session. When the session ends, the cache will be deleted.
#'     The cache will not be shared across multiple sessions.}
#'   \item{2}{To scope the cache to one run of a Shiny application (shared
#'     among possibly multiple user sessions), use \code{cache="app"}. This
#'     is the default. The cache will be shared across multiple sessions, so
#'     there is potentially a large performance benefit if there are many users
#'     of the application. If plots cannot be safely shared across users, this
#'     should not be used.}
#'  }
#'
#'   If either \code{"session"} or \code{"app"} is used, the cache will be 5 MB
#'   in size, and will be stored stored in a temporary directory. Note that a
#'   single cache will be shared for all plots within a single application.
#'
#'   In some cases, you may want to have finer-grained control over the caching
#'   behavior. For example, you may want to use a larger or smaller cache, or
#'   you may want the cache to persist across multiple runs of an application,
#'   or even across multiple R processes. To use this finer-grained control,
#'   pass a \code{\link{DiskCache}} object as the \code{cache} parameter.
#'
#'   Here are some ways to create a cache with other behaviors:
#'
#' \describe{
#'   \item{3}{To have the cache persist across multiple runs of an R process,
#'     use \code{cache=DiskCache$new(dirname(tempdir()), "plot1_cache")}.
#'     This will create a subdirectory in your system temp directory named
#'     \code{plot1_cache} (replace \code{plot1_cache} with a unique name of
#'     your choosing). On most platforms, this directory will be removed when
#'     your system reboots.}
#'   \item{4}{To have the cache persist even across multiple reboots, you
#'     can create the cache in a location outside of the temp directory.
#'     For example, it could be a subdirectory of the application, as in
#'     \code{cache=DiskCache$new("plot1_cache")}. You may need to manually
#'     remove this directory to clear the cache.}
#' }
#'
#'
#' @inheritParams renderPlot
#' @param cacheKeyExpr An expression that returns a cache key. This key should
#'   be a unique identifier for a plot: the assumption is that if the cache key
#'   is the same, then the plot will be the same.
#' @param sizePolicy A function that takes two arguments, \code{width} and
#'   \code{height}, and returns a list with \code{width} and \code{height}. The
#'   purpose is to round the actual pixel dimensions from the browser to some
#'   other dimensions, so that this will not generate and cache images of every
#'   possible pixel dimension. See \code{\link{sizeGrowthRatio}} for more
#'   information on the default sizing policy.
#' @param res The resolution of the PNG, in pixels per inch.
#' @param cache The scope of the cache, or a cache object. This can be
#'   \code{"app"} (the default), \code{"session"}, or a cache object like
#'   a \code{\link{diskCache}}. See the Cache Scoping section for more
#'   information.
#'
#' @seealso See \code{\link{renderPlot}} for the regular, non-cached version of
#'   this function.
#'
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # A basic example
#' shinyApp(
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         sliderInput("n", "Number of points", 4, 32, value = 8, step = 4)
#'       ),
#'       mainPanel(plotOutput("plot"))
#'     )
#'   ),
#'   function(input, output, session) {
#'     output$plot <- renderCachedPlot({
#'         Sys.sleep(2)  # Add an artificial delay
#'         seqn <- seq_len(input$n)
#'         plot(mtcars$wt[seqn], mtcars$mpg[seqn],
#'              xlim = range(mtcars$wt), ylim = range(mtcars$mpg))
#'       },
#'       cacheKeyExpr = { list(input$n) }
#'     )
#'   }
#' )
#'
#'
#'
#' # An example that allows resetting the cache
#' mydata <- reactiveVal(data.frame(x = rnorm(400), y = rnorm(400)))
#'
#' ui <- fluidPage(
#'   sidebarLayout(
#'     sidebarPanel(
#'       sliderInput("n", "Number of points", 50, 400, 100, step = 50),
#'       actionButton("newdata", "New data")
#'     ),
#'     mainPanel(
#'       plotOutput("plot")
#'     )
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$newdata, {
#'     mydata(data.frame(x = rnorm(400), y = rnorm(400)))
#'   })
#'
#'   output$plot <- renderCachedPlot(
#'     {
#'       Sys.sleep(2)
#'       d <- mydata()
#'       seqn <- seq_len(input$n)
#'       plot(d$x[seqn], d$y[seqn], xlim = range(d$x), ylim = range(d$y))
#'     },
#'     cacheKeyExpr = { list(input$n, mydata()) },
#'     cache = "app"
#'   )
#' }
#'
#' shinyApp(ui, server)
#'
#'
#' }
#'
#' @export
renderCachedPlot <- function(expr,
  cacheKeyExpr,
  sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2),
  res = 72,
  cache = "app",
  ...,
  env = parent.frame(), quoted = FALSE, outputArgs = list()
) {

  # This ..stacktraceon is matched by a ..stacktraceoff.. when plotFunc
  # is called
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)
  # This is so that the expr doesn't re-execute by itself; it needs to be
  # triggered by the cache key (or width/height) changing.
  isolatedFunc <- function() isolate(func())

  args <- list(...)

  cacheKeyExpr <- substitute(cacheKeyExpr)
  cacheKey <- reactive(cacheKeyExpr, env = parent.frame(), quoted = TRUE)

  ensureCacheSetup <- function() {
    # For our purposes, cache objects must support these methods.
    isCacheObject <- function(x) {
      # Use tryCatch in case the object does not support `$`.
      tryCatch(
        is.function(x$get) && is.function(x$set),
        error = function(e) FALSE
      )
    }

    if (isCacheObject(cache)) {
      # If `cache` is already a cache object, do nothing
      return()

    } else if (identical(cache, "app")) {
      cache <<- getShinyOption("cache")

    } else if (identical(cache, "session")) {
      cache <<- session$getCache()

    } else {
      stop('`cache` must either be "app", "session", or a cache object with methods, `$get`, and `$set`.')
    }
  }

  resizeObserver <- NULL
  ensureResizeObserver <- function() {
    if (!is.null(resizeObserver))
      return()

    # Given the actual width/height of the image in the browser, this gets
    # the width/height from sizePolicy() and pushes those
    # values into `fitDims`. It's done this way so that the `fitDims` only
    # change (and cause invalidations) when the rendered image size changes,
    # and not every time the browser's <img> tag changes size.
    resizeObserver <<- observe({
      cat("resize\n")
      width  <- session$clientData[[paste0('output_', outputName, '_width')]]
      height <- session$clientData[[paste0('output_', outputName, '_height')]]

      rect <- sizePolicy(c(width, height))
      fitDims$width  <- rect[1]
      fitDims$height <- rect[2]
    })
  }


  # The width and height of the plot to draw, given from sizePolicy. These
  # values get filled by an observer below.
  fitDims <- reactiveValues(width = NULL, height = NULL)

  # Vars to store session and output, so that they can be accessed from
  # the plotObj() reactive.
  session <- NULL
  outputName <- NULL

  # This can be used to trigger drawReactive() to re-execute. This is
  # necessary in some cases.
  drawReactiveTrigger <- reactiveVal(0)

  # Calls drawPlot, invoking the user-provided `func` (which may or may not
  # return a promise). The idea is that the (cached) return value from this
  # reactive can be used for varying width/heights, as it includes the
  # displaylist, which is resolution independent.
  drawReactive <- reactive(label = "plotObj", {
    hybrid_chain(
      {
        # Get width/height, but don't depend on them.
        isolate({
          # The first execution will have NULL width/height, because they haven't
          # yet been retrieved from clientData.
          req(fitDims$width, fitDims$height, cancelOutput = TRUE)
        })

        drawReactiveTrigger()
        cat("drawReactive()\n")

        cacheKey()
      },
      function(cacheKeyResult) {
        # Get width/height, but don't depend on them.
        isolate({
          width  <- fitDims$width
          height <- fitDims$height
        })

        pixelratio <- session$clientData$pixelratio %OR% 1

        key <- digest::digest(list(outputName, cacheKeyResult, width, height, res, pixelratio), "sha256")

        cached_value <- cache$get(key)

        if (!is.key_missing(cached_value)) {
          cat("drawReactive(): cached\n")
          # This will NOT include the displaylist.
          return(cached_value)

        } else {
          hybrid_chain(
            {
              cat("drawReactive(): drawPlot()\n")
              # This will include the displaylist.
              do.call("drawPlot", c(
                list(
                  name = outputName,
                  session = session,
                  func = isolatedFunc,
                  width = width,
                  height = height,
                  pixelratio = pixelratio,
                  res = res
                ),
                args
              ))
            },
            function(result) {
              # Cache a copy of the result. In the case of the MemoryCache,
              # the result is simply stored in memory and can be restored just
              # fine. For other types of cache (like DiskCache) where the
              # object must be serialized before saving, there's a catch: the
              # recorded displaylist for the plot can't be serialized and
              # restored properly within the same R session, so we NULL it out
              # before saving. (The PNG can of course be saved and restored
              # just fine.) Note that this was fixed in revision 74506
              # (2e6c669), and should be in R 3.6, but we need it to work on
              # older versions. Perhaps in the future we could do a version
              # check and change caching behavior based on that.
              result_copy <- result
              if (!inherits(cache, "MemoryCache")) {
                result_copy$recordedPlot <- NULL
              }
              cache$set(key, result_copy)

              result
            }
          )
        }
      },
      catch = function(reason) {
        # Non-isolating read. A common reason for errors in plotting is because
        # the dimensions are too small. By taking a dependency on width/height,
        # we can try again if the plot output element changes size.
        fitDims$width
        fitDims$height

        # Propagate the error
        stop(reason)
      }
    )
  })

  # This function is the one that's returned from renderPlot(), and gets
  # wrapped in an observer when the output value is assigned.
  renderFunc <- function(shinysession, name, ...) {
    outputName <<- name
    session <<- shinysession
    ensureCacheSetup()
    ensureResizeObserver()
    cat("renderFunc()\n")

    hybrid_chain(
      drawReactive(),
      function(drawReactiveResult) {
        # Before we move on to the next step, we need the results of both
        # drawReactive() and cacheKey(). Each of these can be a promise or
        # not. So we use another hybrid_chain() here and collect the results
        # before passing them along to the next step. This is sort of like
        # doing `promise_all(a=drawReactive(), b=cacheKey())`, but a lot
        # uglier because it needs to support both promises and non-promises.
        hybrid_chain(
          cacheKey(),
          function(cacheKeyResult) {
            list(
              drawReactiveResult = drawReactiveResult,
              cacheKeyResult = cacheKeyResult
            )
          }
        )
      },
      function(results) {
        cat("renderFunc() chain\n")

        # Extract from previous steps
        drawReactiveResult <- results$drawReactiveResult
        cacheKeyResult     <- results$cacheKeyResult

        # Take a reactive dependency on the fitted dimensions
        width  <- fitDims$width
        height <- fitDims$height
        pixelratio <- session$clientData$pixelratio %OR% 1

        key <- digest::digest(list(outputName, cacheKeyResult, width, height, res, pixelratio), "sha256")

        cached_value <- cache$get(key)

        if (!is.key_missing(cached_value)) {
          cat("drawReactive(): cached\n")
          result <- cached_value

        } else {
          if (is.null(drawReactiveResult$recordedPlot)) {
            # This is an uncommon case. (1) The output from drawPlot was saved
            # to RDS (without a recordedPlot, since that can't be properly
            # saved). (2) drawPlot was called with another set of inputs (so
            # it  didn't load from cache). (3) drawPlot was called, getting a
            # cache hit and restoring the first RDS. (4) the plot is resized,
            # so this reactive executes (and not drawPlot). In this situation,
            # there's no recordedPlot that can be replayed, so we have to
            # trigger drawPlot() to run again.
            cat("renderFunc(): drawReactiveTrigger()\n")
            drawReactiveTrigger(drawReactiveTrigger() + 1)
            req(FALSE, cancelOutput = TRUE)

          } else {
            cat("renderFunc(): resizeSavedPlot()\n")
            result <- do.call("resizeSavedPlot", c(
              list(
                name,
                shinysession,
                drawReactiveResult,
                width,
                height,
                pixelratio,
                res
              ),
              args
            ))

            # Cache the result, but without recordedPlot
            result_copy <- result
            result_copy$recordedPlot <- NULL
            cache$set(key, result_copy)
          }
        }

        img <- result$img
        # Replace exact pixel dimensions; instead tell it to fill.
        img$width  <- "100%"
        img$height <- NULL
        img
      }
    )
  }

  # If renderPlot isn't going to adapt to the height of the div, then the
  # div needs to adapt to the height of renderPlot. By default, plotOutput
  # sets the height to 400px, so to make it adapt we need to override it
  # with NULL.
  outputFunc <- plotOutput
  formals(outputFunc)['height'] <- list(NULL)

  markRenderFunction(outputFunc, renderFunc, outputArgs = outputArgs)
}


#' Create a sizing function that grows at a given ratio
#'
#' Returns a function which takes a two-element vector representing an input
#' width and height, and returns a two-element vector of width and height. The
#' possible widths are the base width times the growthRate to any integer power.
#' For example, with a base width of 500 and growth rate of 1.25, the possible
#' widths include 320, 400, 500, 625, 782, and so on, both smaller and larger.
#' Sizes are rounded up to the next pixel. Heights are computed the same way as
#' widths.
#'
#' @param width,height Base width and height.
#' @param growthRate Growth rate multiplier.
#'
#' @seealso This is to be used with \code{\link{renderCachedPlot}}.
#'
#' @examples
#' f <- sizeGrowthRatio(500, 500, 1.25)
#' f(c(400, 400))
#' f(c(500, 500))
#' f(c(530, 550))
#' f(c(625, 700))
#'
#' @export
sizeGrowthRatio <- function(width = 400, height = 400, growthRate = 1.2) {
  round_dim_up <- function(x, base, rate) {
    power <- ceiling(log(x / base, rate))
    ceiling(base * rate^power)
  }

  function(dims) {
    if (length(dims) != 2) {
      stop("dims must be a vector with two numbers, for width and height.")
    }
    c(
      round_dim_up(dims[1], width,  growthRate),
      round_dim_up(dims[2], height, growthRate)
    )
  }
}
