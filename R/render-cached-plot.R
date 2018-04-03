#' @export
renderCachedPlot <- function(expr, cacheKeyExpr, cacheInvalidationExpr = NULL,
  baseWidth = 400, aspectRatioRate = 1.25, growthRate = 1.25, res = 72,
  ...,
  env = parent.frame(), quoted = FALSE, outputArgs = list()
) {

  cacheKey          <- reactive(substitute(cacheKeyExpr),
                                env = parent.frame(), quoted = TRUE)
  cacheInvalidation <- reactive(substitute(cacheInvalidationExpr),
                                env = parent.frame(), quoted = TRUE)

  cacheDir <- file.path(tempdir(), "plotcache")

  dir.create(cacheDir, recursive = TRUE, mode = "0700") # This shouldn't be here

  # Delete the cacheDir at the appropriate time. Use ignoreInit=TRUE because
  # we don't want it to happen right in the beginning, especially when
  # cacheDir is provided by the user and it might need to persist across R
  # processes.
  observeEvent(
    substitute(cacheInvalidationExpr), event.env = parent.frame(), event.quoted = TRUE,
    ignoreInit = TRUE,
    {
      if (dirExists(cacheDir)) {
        unlink(cacheDir, recursive = TRUE)
      }
      dir.create(cacheDir, recursive = TRUE, mode = "0700")

      # Cause drawReactive() to re-execute, so renderFunc doesn't use the
      # cached value.
      drawReactiveTrigger(drawReactiveTrigger() + 1)
    }
  )

  # This ..stacktraceon is matched by a ..stacktraceoff.. when plotFunc
  # is called
  installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)

  args <- list(...)

  possible_dims <- all_possible_dims(baseWidth, aspectRatioRate, growthRate)

  # Given the actual width/height of the image in the browser, return the
  # smallest containing rectangle from possible_dims.
  getDims <- function() {
    cat("getDims()\n")
    width  <- session$clientData[[paste0('output_', outputName, '_width')]]
    height <- session$clientData[[paste0('output_', outputName, '_height')]]

    find_smallest_containing_rect(width, height, possible_dims)
  }

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
        drawReactiveTrigger()
        cat("drawReactive()\n")

        dims <- isolate(getDims())
        pixelratio <- session$clientData$pixelratio %OR% 1

        key <- digest::digest(list(cacheKey(), dims$width, dims$height, res, pixelratio))
        resultFilePath <- file.path(cacheDir, paste0(key, ".rds"))

        if (file.exists(resultFilePath)) {
          cat("drawReactive(): cached\n")
          # This will NOT include the displaylist.
          readRDS(resultFilePath)

        } else {
          cat("drawReactive(): drawPlot()\n")
          # This includes the displaylist.
          drawPlot(outputName, session, func, dims$width, dims$height, pixelratio, res,
                   resultfile = resultFilePath)
        }
      },
      catch = function(reason) {
        # Non-isolating read. A common reason for errors in plotting is because
        # the dimensions are too small. By taking a dependency on width/height,
        # we can try again if the plot output element changes size.
        getDims()

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

    hybrid_chain(
      drawReactive(),
      function(result) {
        cat("renderFunc()\n")
        # Do take a reactive dependency on the dimensions
        dims <- getDims()
        pixelratio <- session$clientData$pixelratio %OR% 1

        key <- digest::digest(list(cacheKey(), dims$width, dims$height, res, pixelratio))
        resultFilePath <- file.path(cacheDir, paste0(key, ".rds"))

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
                                   dims$width, dims$height, pixelratio, res,
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



#' Disk-based plot cache
#'
#' Creates a read-through cache for plots. The plotting logic is provided as
#' \code{plotFunc}, a function that can have any number/combination of
#' arguments; the return value of \code{plotCache()} is a function that should
#' be used in the place of plotFunc. Each unique combination of inputs will be
#' cached to disk in the location specified by \code{cacheDir}.
#'
#' \code{invalidationExpr} is an expression that uses reactive values like
#' \code{input$click} and/or reactive expressions like \code{data()}. Whenever
#' it changes value, the cache is invalidated (the contents are erased). You
#' typically want to invalidate the cache when a plot made with the same input
#' variables would have a different result. For example, if the plot is a
#' scatter plot and the data set originally had 100 rows, and then changes to
#' have 200 rows, you would want to invalidate the cache so that the plots would
#' be redrawn display the new, larger data set. The \code{invalidationExpr}
#' parameter works just like the \code{eventExpr} parameter of
#' \code{\link{observeEvent}}.
#'
#' Another way to use \code{invalidationExpr} is to have it invalidate the cache
#' at a fixed time interval. For example, you might want to have invalidate the
#' cache once per hour, or once per day. See below for an example.
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
#'   The cache can be scoped automatically, based on where you call
#'   \code{plotCache()}. If automatic scoping is used, the cache will be
#'   automatically deleted when the scope exits. For example if it is scoped to
#'   a session, then the cache will be deleted when the session exits.
#'
#' \describe{
#'   \item{1}{To scope the cache to one session, call \code{plotCache()} inside
#'     of the server function.}
#'   \item{2}{To scope the cache to one run of a Shiny application (shared
#'     among possibly multiple user sessions), call \code{plotCache()} in your
#'     application, but outside of the server function.}
#'   \item{3}{To scope the cache to a single R process (possibly across multiple
#'     runs of applications), call \code{plotCache()} somewhere outside of
#'     code that is run by \code{runApp()}. (This is an uncommon use case, but
#'     can happen during local application development when running code in the
#'     console.)}
#'  }
#'
#'    If you want to set the scope of the cache manually, use the
#'    \code{cacheDir} parameter. This can be useful if you want the cache to
#'    persist across R processes or even system reboots.
#'
#' \describe{
#'   \item{4}{To have the cache persist across different R processes, use
#'     \code{cacheDir=file.path(dirname(tempdir()), "my_cache_id")}.
#'     This will create a subdirectory in your system temp directory named
#'     \code{my_cache_id} (where \code{my_cache_id} is replaced with a unique
#'     name of your choosing).}
#'   \item{5}{To have the cache persist even across system reboots, you can set
#'     \code{cacheDir} to a location outside of the temp directory.}
#' }
#'
#'
#'
#' @param plotFunc Plotting logic, provided as a function that takes zero or
#'   more arguments. Don't worry about setting up a graphics device or creating
#'   a PNG; just write to the graphics device (you must call \code{print()} on
#'   ggplot2 objects).
#' @param invalidationExpr Any expression or block of code that accesses any
#'   reactives whose invalidation should cause cache invalidation. Use
#'   \code{NULL} if you don't want to cause cache invalidation.
#' @param baseWidth A base value for the width of the cached plot.
#' @param aspectRatioRate A multiplier for different possible aspect ratios.
#' @param growthRate A multiplier for different cached image sizes. For
#'   example, with a \code{width} of 400 and a \code{growth_rate} of 1.25, there
#'   will be possible cached images of widths 256, 320, 400, 500, 625, and so
#'   on, both smaller and larger.
#' @param res The resolution of the PNG, in pixels per inch.
#' @param cacheDir The location on disk where the cache will be stored. If
#'   \code{NULL} (the default), it uses a temp directory which will be cleaned
#'   up when the cache scope exits. See the Cache Scoping section for more
#'   information.
#' @param invalidation.env The environment where the \code{invalidationExpr} is
#'   evaluated.
#' @param invalidation.quoted Is \code{invalidationExpr} expression quoted? By
#'   default, this is FALSE. This is useful when you want to use an expression
#'   that is stored in a variable; to do so, it must be quoted with
#'   \code{quote()}.
#'
#' @export
createCachedPlot <- function(plotFunc, invalidationExpr,
  baseWidth = 400, aspectRatioRate = 1.25, growthRate = 1.25, res = 72,
  cacheDir = NULL,
  invalidation.env = parent.frame(),
  invalidation.quoted = FALSE,
  session = getDefaultReactiveDomain()
) {

  # If user didn't supply cacheDir, automatically determine it.
  if (is.null(cacheDir)) {
    if (!is.null(session)) {
      # Case 1: scope to session
      cacheScopePath <- file.path(tempdir(), paste0("shinysession-", session$token))

    } else if (!is.null(getShinyOption("appToken"))) {
      # Case 2: scope to app
      cacheScopePath <- file.path(tempdir(), paste0("shinyapp-", getShinyOption("appToken")))

    } else {
      # Case 3: scope to current R process
      cacheScopePath <- file.path(tempdir(), "shiny")
    }

    cacheDir <- file.path(cacheScopePath, createUniqueId(8))

    # Remove the cache directory when it's no longer needed.
    reg.finalizer(environment(), function(e) {
      unlink(cacheDir, recursive = TRUE)

      # If cacheScopePath is empty, remove it.
      siblingPaths <- setdiff(dir(cacheScopePath, all.files = TRUE), c(".", ".."))
      if (length(siblingPaths) == 0) {
        file.remove(cacheScopePath)
      }
    })
  }

  if (!dirExists(cacheDir)) {
    dir.create(cacheDir, recursive = TRUE, mode = "0700")
  }

  if (!invalidation.quoted) {
    invalidationExpr <- substitute(invalidationExpr)
  }

  possible_dims <- all_possible_dims(baseWidth, aspectRatioRate, growthRate)

  # Delete the cacheDir at the appropriate time. Use ignoreInit=TRUE because we don't
  # want it to happen right in the beginning, especially when cacheDir is provided
  # by the user and it might need to persist across R processes.
  observeEvent(invalidationExpr, event.env = invalidation.env, event.quoted = TRUE,
    ignoreInit = TRUE,
    {
      if (dirExists(cacheDir)) {
        unlink(cacheDir, recursive = TRUE)
      }
      dir.create(cacheDir, recursive = TRUE, mode = "0700")
    }
  )

  function(...) {
    args <- list(...)

    output_info <- getCurrentOutputInfo()
    if (is.null(output_info)) {
      stop("This must be run in a Shiny output.")
    }
    session <- getDefaultReactiveDomain()
    if (is.null(session)) {
      stop("This must be run from a Shiny session.")
    }

    target_width <- output_info$width()
    target_height <- output_info$height()

    dims <- find_smallest_containing_rect(target_width, target_height, possible_dims)

    pixelratio <- session$clientData$pixelratio

    # TODO: What if the args include weird objects like environments or reactive expressions?
    key <- paste0(digest::digest(c(args, width = dims$width, height = dims$height, res = res, pixelratio = pixelratio)), ".png")
    filePath <- file.path(cacheDir, key)
    if (!file.exists(filePath)) {
      plotPNG(
        filename = filePath,
        width    = dims$width  * pixelratio,
        height   = dims$height * pixelratio,
        res      = res    * pixelratio,
        function() {
          do.call("plotFunc", args)
        }
      )
    }
    filePath
  }
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
