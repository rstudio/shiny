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
#' Internally, the result from \code{cacheKeyExpr} is combined with the name of
#' the output (if you assign it to \code{output$plot1}, it will be combined
#' with \code{"plot1"}) to form the actual key that is used. As a result, even
#' if there are multiple plots that have the same \code{cacheKeyExpr}, they
#' will not have cache key collisions.
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
#'   \item{1}{To scope the cache to one run of a Shiny application (shared
#'     among possibly multiple user sessions), use \code{cache="app"}. This
#'     is the default. The cache will be shared across multiple sessions, so
#'     there is potentially a large performance benefit if there are many users
#'     of the application. When the application stops running, the cache will
#'     be deleted. If plots cannot be safely shared across users, this should
#'     not be used.}
#'   \item{2}{To scope the cache to one session, use \code{cache="session"}.
#'     When a new user session starts -- in other words, when a web browser
#'     visits the Shiny application -- a new cache will be created on disk
#'     for that session. When the session ends, the cache will be deleted.
#'     The cache will not be shared across multiple sessions.}
#'  }
#'
#'   If either \code{"app"} or \code{"session"} is used, the cache will be 10 MB
#'   in size, and will be stored stored in memory, using a
#'   \code{\link{memoryCache}} object. Note that the cache space will be shared
#'   among all cached plots within a single application or session.
#'
#'   In some cases, you may want more control over the caching behavior. For
#'   example, you may want to use a larger or smaller cache, share a cache
#'   among multiple R processes, or you may want the cache to persist across
#'   multiple runs of an application, or even across multiple R processes.
#'
#'   To use different settings for an application-scoped cache, you can call
#'   \code{\link{shinyOptions}()} at the top of your app.R, server.R, or
#'   global.R. For example, this will create a cache with 20 MB of space
#'   instead of the default 10 MB:
#'   \preformatted{
#'   shinyOptions(cache = memoryCache(size = 20e6))
#'   }
#'
#'   To use different settings for a session-scoped cache, you can call
#'   \code{\link{shinyOptions}()} at the top of your server function. To use
#'   the session-scoped cache, you must also call \code{renderCachedPlot} with
#'   \code{scope="session"}. This will create a 20 MB cache for the session:
#'   \preformatted{
#'   function(input, output, session) {
#'     shinyOptions(cache = memoryCache(size = 20e6))
#'
#'     output$plot <- renderCachedPlot(
#'       ...,
#'       scope = "session"
#'     )
#'   }
#'   }
#'
#'   If you want to create a cache that is shared across multiple concurrent
#'   R processes, you can use a \code{\link{diskCache}}. You can create an
#'   application-level shared cache by putting this at the top of your app.R,
#'   server.R, or global.R:
#'   \preformatted{
#'   shinyOptions(cache = diskCache(file.path(dirname(tempdir()), "myapp-cache"))
#'   }
#'
#'   This will create a subdirectory in your system temp directory named
#'   \code{myapp-cache} (replace \code{myapp-cache} with a unique name of
#'   your choosing). On most platforms, this directory will be removed when
#'   your system reboots. This cache will persist across multiple starts and
#'   stops of the R process, as long as you do not reboot.
#'
#'   To have the cache persist even across multiple reboots, you can create the
#'   cache in a location outside of the temp directory. For example, it could
#'   be a subdirectory of the application:
#'   \preformatted{
#'   shinyOptions(cache = diskCache("./myapp-cache"))
#'   }
#'
#'   In this case, resetting the cache will have to be done manually, by deleting
#'   the directory.
#'
#'   You can also scope a cache to just one plot, or selected plots. To do that,
#'   create a \code{\link{memoryCache}} or \code{\link{diskCache}}, and pass it
#'   as the \code{cache} argument of \code{renderCachedPlot}.
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
#'   this function. For more about configuring caches, see
#'   \code{\link{memoryCache}} and \code{\link{diskCache}}.
#'
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # A basic example that uses the default app-scoped memory cache.
#' # The cache will be shared among all simultaneous users of the application.
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
#' # An example uses a data object shared across sessions. mydata() is part of
#' # the cache key, so when its value changes, plots that were previously
#' # stored in the cache will no longer be used (unless mydata() changes back
#' # to its previous value).
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
#'   )
#' }
#'
#' shinyApp(ui, server)
#'
#'
#' # A basic application with two plots, where each plot in each session has
#' # a separate cache.
#' shinyApp(
#'   fluidPage(
#'     sidebarLayout(
#'       sidebarPanel(
#'         sliderInput("n", "Number of points", 4, 32, value = 8, step = 4)
#'       ),
#'       mainPanel(
#'         plotOutput("plot1"),
#'         plotOutput("plot2")
#'       )
#'     )
#'   ),
#'   function(input, output, session) {
#'     output$plot1 <- renderCachedPlot({
#'         Sys.sleep(2)  # Add an artificial delay
#'         seqn <- seq_len(input$n)
#'         plot(mtcars$wt[seqn], mtcars$mpg[seqn],
#'              xlim = range(mtcars$wt), ylim = range(mtcars$mpg))
#'       },
#'       cacheKeyExpr = { list(input$n) },
#'       cache = memoryCache()
#'     )
#'     output$plot2 <- renderCachedPlot({
#'         Sys.sleep(2)  # Add an artificial delay
#'         seqn <- seq_len(input$n)
#'         plot(mtcars$wt[seqn], mtcars$mpg[seqn],
#'              xlim = range(mtcars$wt), ylim = range(mtcars$mpg))
#'       },
#'       cacheKeyExpr = { list(input$n) },
#'       cache = memoryCache()
#'     )
#'   }
#' )
#'
#' }
#'
#' \dontrun{
#' # At the top of app.R, this set the application-scoped cache to be a memory
#' # cache that is 20 MB in size, and where cached objects expire after one
#' # hour.
#' shinyOptions(cache = memoryCache(max_size = 20e6, max_age = 3600))
#'
#' # At the top of app.R, this set the application-scoped cache to be a disk
#' # cache that can be shared among multiple concurrent R processes, and is
#' # deleted when the system reboots.
#' shinyOptions(cache = diskCache(file.path(dirname(tempdir()), "myapp-cache"))
#'
#' # At the top of app.R, this set the application-scoped cache to be a disk
#' # cache that can be shared among multiple concurrent R processes, and
#' # persists on disk across reboots.
#' shinyOptions(cache = diskCache("./myapp-cache"))
#'
#' # At the top of the server function, this set the session-scoped cache to be
#' # a memory cache that is 5 MB in size.
#' server <- function(input, output, session) {
#'   shinyOptions(cache = memoryCache(max_size = 5e6))
#'
#'   output$plot <- renderCachedPlot(
#'     ...,
#'     scope = "session"
#'   )
#' }
#'
#' }
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
      # cat("resize\n")
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
        # cat("drawReactive()\n")

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
          # cat("drawReactive(): cached\n")
          # This will NOT include the displaylist.
          return(cached_value)

        } else {
          hybrid_chain(
            {
              # cat("drawReactive(): drawPlot()\n")
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
    # cat("renderFunc()\n")

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
        # cat("renderFunc() chain\n")

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
          # cat("drawReactive(): cached\n")
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
            # cat("renderFunc(): drawReactiveTrigger()\n")
            drawReactiveTrigger(drawReactiveTrigger() + 1)
            req(FALSE, cancelOutput = TRUE)

          } else {
            # cat("renderFunc(): resizeSavedPlot()\n")
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
        # Replace exact pixel dimensions; instead, the max-height and
        # max-width will be set to 100% from CSS.
        img$width  <- NULL
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
