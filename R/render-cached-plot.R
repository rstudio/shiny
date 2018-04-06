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
#' \code{cacheResetEventExpr} is an expression that uses reactive values like
#' \code{input$click} and/or reactive expressions like \code{data()}. The
#' \code{cacheResetEventExpr} parameter works similarly to the \code{eventExpr}
#' parameter of \code{\link{observeEvent}}: whenever the upstream reactive
#' dependencies are invalidated, they cause this expression to re-execute, and
#' the cache is reset -- the contents are erased. The cache should be reset when
#' something changes so that a plot made with the same cache key as before would
#' have a different result. This may happen when, for example, the underlying
#' data changes. If the plot is based on a data source that changes over time,
#' the plot at time 1 may differ from the plot at time 2, even if both plots use
#' the same cache key.
#'
#' Another way to use \code{cacheResetEventExpr} is to have it clear the cache
#' at a fixed time interval using \code{\link{invalidateLater}}. For example,
#' you might want to have clear the cache once per hour, or once per day.
#'
#' Although both \code{cacheKeyExpr} and \code{cacheResetEventExpr} are reactive
#' -- they re-execute when their upstream reactive dependencies are invalidated
#' -- they differ in how they use the return value. For \code{cacheKeyExpr}, the
#' returned value is used (as a key). In contrast, for
#' \code{cacheResetEventExpr}, the return value is ignored; the invalidation of
#' the expression is used only to signal that the cache should be reset.
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
#'   To control the scope of the cache, use the \code{scope} parameter. There
#'   are two ways of having Shiny automatically create and clean up the disk
#'   cache.
#'
#' \describe{
#'   \item{1}{To scope the cache to one session, use \code{scope="session"}.
#'     When a new user session starts -- in other words, when a web browser
#'     visits the Shiny application -- a new cache will be created on disk
#'     for that session. When the session ends, the cache will be deleted.
#'     The cache will not be shared across multiple sessions.}
#'   \item{2}{To scope the cache to one run of a Shiny application (shared
#'     among possibly multiple user sessions), use \code{scope="app"}. This
#'     is the default. The cache will be shared across multiple sessions, so
#'     there is potentially a large performance benefit if there are many users
#'     of the application. If plots cannot be safely shared across users, this
#'     should not be used.}
#'  }
#'
#'   In some cases, you may want to manually specify the cache directory. This
#'   can be useful if you want the cache to persist across multiple runs of an
#'   application, or even across multiple R processes.
#'
#' \describe{
#'   \item{3}{To have the cache persist across multiple runs of an R process,
#'     use \code{scope=file.path(dirname(tempdir()), "plot1_cache")}.
#'     This will create a subdirectory in your system temp directory named
#'     \code{plot1_cache} (where \code{plot1_cache} is replaced with a unique
#'     name of your choosing). When the R process exits, it will automatically
#'     be removed.}
#'   \item{4}{To have the cache persist even across multiple R processes, you
#'     can set \code{cacheDir} to a location outside of the temp directory.
#'     For example, it could be a subdirectory of the application, as in
#'     \code{scope="plot1_cache"}}.
#' }
#'
#'   Please note that if you specify a directory, that directory should only be
#'   used to plot cache files. If it contains any other files or directories,
#'   they could be removed when the cache is invalidated. Additionally, the
#'   directory will not automatically be cleaned up or removed when the Shiny
#'   application exits.
#'
#' @inheritParams renderPlot
#' @param cacheKeyExpr An expression that returns a cache key. This key should
#'   be a unique identifier for a plot: the assumption is that if the cache key
#'   is the same, then the plot will be the same.
#' @param cacheResetEventExpr An expression or block of code that accesses any
#'   reactives whose invalidation should cause the cached plots to be cleared.
#'   If \code{NULL} (the default) the cache will not get cleared.
#' @param baseWidth A base value for the width of the cached plot.
#' @param aspectRatioRate A multiplier for different possible aspect ratios. For
#'   example, with a value of 1.2, the possible aspect ratios for plots will be
#'   1:1, 1:1.2, 1:1.44, and so on, getting wider, as well as 1.2:1, 1.44:1, and
#'   so on, getting taller.
#' @param growthRate A multiplier for different cached image sizes. For example,
#'   with a \code{width} of 400 and a \code{growthRate} of 1.25, there will be
#'   possible cached images of widths 256, 320, 400, 500, 625, and so on, both
#'   smaller and larger.
#' @param res The resolution of the PNG, in pixels per inch.
#' @param scope The scope of the cache. This can be \code{"app"} (the default),
#'   \code{"session"}, or the path to a directory to store cached plots. See the
#'   Cache Scoping section for more information.
#'
#' @export
renderCachedPlot <- function(expr, cacheKeyExpr, cacheResetEventExpr = NULL,
  baseWidth = 400, aspectRatioRate = 1.2, growthRate = 1.2, res = 72,
  scope = "app",
  env = parent.frame(), quoted = FALSE, outputArgs = list()
) {

  # This ..stacktraceon is matched by a ..stacktraceoff.. when plotFunc
  # is called
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)
  # This is so that the expr doesn't re-execute by itself; it needs to be
  # triggered by the cache key (or width/height) changing.
  isolatedFunc <- function() isolate(func())

  cacheKey <- reactive(substitute(cacheKeyExpr), env = parent.frame(), quoted = TRUE)

  .cacheDir <- NULL
  cacheDir <- function() {
    # Memoize
    if (is.null(.cacheDir)) {
      if (is.null(outputName)) {
        stop("outputName is NULL. cacheDir() was called too early.")
      }

      if (scope %in% c("app", "session")) {
        appCachePath <- file.path(tempdir(), paste0("shinyapp-", getShinyOption("appToken")))

        if (scope == "app") {
          cacheScopePath <- appCachePath
        } else if (scope == "session") {
          cacheScopePath <- file.path(appCachePath, paste0("shinysession-", session$token))
        }

        .cacheDir <<- file.path(cacheScopePath, paste0("output-", outputName))

      } else {
        # User has passed in a directory
        .cacheDir <<- normalizePath2(scope)
      }
    }

    .cacheDir
  }

  ensureCacheDirExists <- function() {
    if (!dirExists(cacheDir())) {
      cat("Creating ", cacheDir(), "\n")
      dir.create(cacheDir(), recursive = TRUE, mode = "0700")

      # Set up removal of cache directory at appropriate time. The removal
      # callback is registered here, paired with the creation of the cache
      # dir, to ensure it's not scheduled multiple times for one directory.
      deleteCacheDir <- function() {
        # Just to be safe, don't try to delete the cache dir if it's already
        # gone.
        if (!dirExists(cacheDir())) {
          return()
        }

        unlink(cacheDir(), recursive = TRUE)

        # Recursively delete empty parent dirs, up to temp dir.
        currentDir <- dirname(cacheDir())
        while (currentDir != tempdir() &&
               length(dir(currentDir, all.files = TRUE, no.. = TRUE)) == 0)
        {
          dirRemove(currentDir)
          currentDir <- dirname(currentDir)
        }
      }

      if (scope == "app") {
        onStop(deleteCacheDir, session = NULL)
      } else if (scope == "session") {
        onSessionEnded(deleteCacheDir)
      }
    }
  }

  # Clear the cacheDir at the appropriate time. Use ignoreInit=TRUE because we
  # don't want it to happen right in the beginning.
  observeEvent(
    substitute(cacheResetEventExpr), event.env = parent.frame(), event.quoted = TRUE,
    ignoreInit = TRUE,
    {
      unlink(file.path(cacheDir(), "*.rds"))

      # Cause drawReactive() to re-execute, so renderFunc doesn't use the
      # cached value.
      drawReactiveTrigger(drawReactiveTrigger() + 1)
    }
  )

  possible_dims <- all_possible_dims(baseWidth, aspectRatioRate, growthRate)

  # The width and height of the plot to draw, taken from possible_dims. These
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
          width  <- fitDims$width
          height <- fitDims$height
        })
        # The first execution will have NULL width/height, because they haven't
        # yet been retrieved from clientData.
        req(width, height, cancelOutput = TRUE)

        drawReactiveTrigger()
        cat("drawReactive()\n")

        pixelratio <- session$clientData$pixelratio %OR% 1

        ensureCacheDirExists()

        key <- digest::digest(list(cacheKey(), width, height, res, pixelratio))
        resultFilePath <- file.path(cacheDir(), paste0(key, ".rds"))

        if (file.exists(resultFilePath)) {
          cat("drawReactive(): cached\n")
          # This will NOT include the displaylist.
          readRDS(resultFilePath)

        } else {
          cat("drawReactive(): drawPlot()\n")
          # This includes the displaylist.
          drawPlot(outputName, session, isolatedFunc, width, height, pixelratio, res,
                   resultfile = resultFilePath)
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

    # Given the actual width/height of the image in the browser, this gets
    # smallest containing rectangle from possible_dims, and pushes those
    # values into `fitDims`. It's done this way so that the `fitDims` only
    # change (and cause invalidations) when the rendered image size changes,
    # and not every time the browser's <img> tag changes size.
    observe({
      width  <- session$clientData[[paste0('output_', outputName, '_width')]]
      height <- session$clientData[[paste0('output_', outputName, '_height')]]

      rect <- find_smallest_containing_rect(width, height, possible_dims)
      fitDims$width  <- rect$width
      fitDims$height <- rect$height
    })

    hybrid_chain(
      drawReactive(),
      function(result) {
        cat("renderFunc()\n")
        # Take a reactive dependency on the fitted dimensions
        width  <- fitDims$width
        height <- fitDims$height
        pixelratio <- session$clientData$pixelratio %OR% 1

        ensureCacheDirExists()

        key <- digest::digest(list(cacheKey(), width, height, res, pixelratio))
        resultFilePath <- file.path(cacheDir(), paste0(key, ".rds"))

        if (file.exists(resultFilePath)) {
          cat("renderFunc(): cached\n")
          cachedPlot <- readRDS(resultFilePath)
          img <- cachedPlot$img

        } else {
          if (is.null(result$recordedPlot)) {
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
            img <- resizeSavedPlot(name, shinysession, result,
                                   width, height, pixelratio, res,
                                   resultfile = resultFilePath)
          }
        }

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


# Given a target rectangle with `width` and `height`, and data frame `dims` of possible
# dimensions, with column `width` and `height, find the smallest possible width x
# height pair from `dims` that fully contains `width` and `height.`
find_smallest_containing_rect <- function(width, height, dims) {
  fit_rows <- width <= dims$width  &  height <= dims$height

  if (sum(fit_rows) == 0) {
    # TODO: handle case where width x height is larger than all dims
  }

  # Drop all the rows where width x height won't fit
  dims <- dims[fit_rows, ]

  # Find the possible rectangle with the smallest area
  dims$area <- dims$width * dims$height
  min_row <- which.min(dims$area)

  list(
    width  = dims$width[min_row],
    height = dims$height[min_row]
  )
}

# Returns a data frame with all possible width-height combinations. This could
# use some fine-tuning in the future.
all_possible_dims <- function(base_width = 400, aspect_ratio_rate = 1.25, growth_rate = 1.25) {
  aspect_ratios <- aspect_ratio_rate ^ (-3:3)
  dims <- expand.grid(width = base_width * (growth_rate ^ (-6:6)), ratio = aspect_ratios)
  dims$height <- dims$width * dims$ratio

  dims$width  <- round(dims$width)
  dims$height <- round(dims$height)
  dims
}
