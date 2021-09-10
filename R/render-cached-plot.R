#' Plot output with cached images
#'
#' Renders a reactive plot, with plot images cached to disk. As of Shiny 1.6.0,
#' this is a shortcut for using [bindCache()] with [renderPlot()].
#'
#' `expr` is an expression that generates a plot, similar to that in
#' `renderPlot`. Unlike with `renderPlot`, this expression does not
#' take reactive dependencies. It is re-executed only when the cache key
#' changes.
#'
#' `cacheKeyExpr` is an expression which, when evaluated, returns an object
#' which will be serialized and hashed using the [rlang::hash()]
#' function to generate a string that will be used as a cache key. This key is
#' used to identify the contents of the plot: if the cache key is the same as a
#' previous time, it assumes that the plot is the same and can be retrieved from
#' the cache.
#'
#' This `cacheKeyExpr` is reactive, and so it will be re-evaluated when any
#' upstream reactives are invalidated. This will also trigger re-execution of
#' the plotting expression, `expr`.
#'
#' The key should consist of "normal" R objects, like vectors and lists. Lists
#' should in turn contain other normal R objects. If the key contains
#' environments, external pointers, or reference objects --- or even if it has
#' such objects attached as attributes --- then it is possible that it will
#' change unpredictably even when you do not expect it to. Additionally, because
#' the entire key is serialized and hashed, if it contains a very large object
#' --- a large data set, for example --- there may be a noticeable performance
#' penalty.
#'
#' If you face these issues with the cache key, you can work around them by
#' extracting out the important parts of the objects, and/or by converting them
#' to normal R objects before returning them. Your expression could even
#' serialize and hash that information in an efficient way and return a string,
#' which will in turn be hashed (very quickly) by the
#' [rlang::hash()] function.
#'
#' Internally, the result from `cacheKeyExpr` is combined with the name of
#' the output (if you assign it to `output$plot1`, it will be combined
#' with `"plot1"`) to form the actual key that is used. As a result, even
#' if there are multiple plots that have the same `cacheKeyExpr`, they
#' will not have cache key collisions.
#'
#' @section Interactive plots:
#'
#'   `renderCachedPlot` can be used to create interactive plots. See
#'   [plotOutput()] for more information and examples.
#'
#'
#' @inheritParams renderPlot
#' @inheritParams bindCache
#' @param cacheKeyExpr An expression that returns a cache key. This key should
#'   be a unique identifier for a plot: the assumption is that if the cache key
#'   is the same, then the plot will be the same.
#' @param sizePolicy A function that takes two arguments, `width` and
#'   `height`, and returns a list with `width` and `height`. The
#'   purpose is to round the actual pixel dimensions from the browser to some
#'   other dimensions, so that this will not generate and cache images of every
#'   possible pixel dimension. See [sizeGrowthRatio()] for more
#'   information on the default sizing policy.
#' @param res The resolution of the PNG, in pixels per inch.
#' @param width,height not used. They are specified via the argument
#'   `sizePolicy`.
#'
#' @seealso See [renderPlot()] for the regular, non-cached version of this
#'   function. It can be used with [bindCache()] to get the same effect as
#'   `renderCachedPlot()`. For more about configuring caches, see
#'   [cachem::cache_mem()] and [cachem::cache_disk()].
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
#'       cache = cachem::cache_mem()
#'     )
#'     output$plot2 <- renderCachedPlot({
#'         Sys.sleep(2)  # Add an artificial delay
#'         seqn <- seq_len(input$n)
#'         plot(mtcars$wt[seqn], mtcars$mpg[seqn],
#'              xlim = range(mtcars$wt), ylim = range(mtcars$mpg))
#'       },
#'       cacheKeyExpr = { list(input$n) },
#'       cache = cachem::cache_mem()
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
#' shinyOptions(cache = cachem::cache_mem(max_size = 20e6, max_age = 3600))
#'
#' # At the top of app.R, this set the application-scoped cache to be a disk
#' # cache that can be shared among multiple concurrent R processes, and is
#' # deleted when the system reboots.
#' shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))
#'
#' # At the top of app.R, this set the application-scoped cache to be a disk
#' # cache that can be shared among multiple concurrent R processes, and
#' # persists on disk across reboots.
#' shinyOptions(cache = cachem::cache_disk("./myapp-cache"))
#'
#' # At the top of the server function, this set the session-scoped cache to be
#' # a memory cache that is 5 MB in size.
#' server <- function(input, output, session) {
#'   shinyOptions(cache = cachem::cache_mem(max_size = 5e6))
#'
#'   output$plot <- renderCachedPlot(
#'     ...,
#'     cache = "session"
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
  alt = "Plot object",
  outputArgs = list(),
  width = NULL,
  height = NULL
) {

  expr <- substitute(expr)
  if (!is_quosure(expr)) {
    expr <- new_quosure(expr, env = parent.frame())
  }

  cacheKeyExpr <- substitute(cacheKeyExpr)
  if (!is_quosure(cacheKeyExpr)) {
    cacheKeyExpr <- new_quosure(cacheKeyExpr, env = parent.frame())
  }

  if (!is.null(width) || !is.null(height)) {
    warning("Unused argument(s) 'width' and/or 'height'. ",
            "'sizePolicy' is used instead.")
  }

  inject(
    bindCache(
      renderPlot(!!expr, res = res, alt = alt, outputArgs = outputArgs, ...),
      !!cacheKeyExpr,
      sizePolicy = sizePolicy,
      cache = cache
    )
  )
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
#' @seealso This is to be used with [renderCachedPlot()].
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
