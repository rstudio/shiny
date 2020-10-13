#' Create a reactive expression with caching
#'
#' @inheritParams reactive
#' @param cacheKeyExpr An expression that returns a value that will be hashed
#'   and used as a cache key. This key should be a unique identifier for the
#'   value: the assumption is that if the cache key is the same, then the value
#'   of `expr` is the same. If two separate `cachedReactive`s have the same key,
#'   the value is assumed to be the same. To avoid this, you can add an
#'   arbitrary identifier (like an ID string) to the cache key.
#' @param valueExpr The expression that produces the return value of the
#'   `cachedReactive`. It will be executed within an [isolate()] scope.
#' @param cache The scope of the cache, or a cache object. This can be `"app"`
#'   (the default), `"session"`, or a cache object like a [diskCache()]. See the
#'   Cache Scoping section for more information.
#' @param ... Other arguments are ignored.
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     "x * y: ",
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- cachedReactive(
#'       # Use input$x and input$y for the cache key. This is a reactive
#'       # expression
#'       list(input$x, input$y),
#'       {
#'         # The value expression is an expensive computation
#'         message("Doing expensive computation...")
#'         Sys.sleep(2)
#'         input$x * input$y
#'       }
#'     )
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#'
#' # Demo of cache key collisions and how to avoid them
#' shinyApp(
#'   ui = fluidPage(
#'     numericInput("n", "n", 1),
#'     "n * 2: ", verbatimTextOutput("txt2"),
#'     "n * 3: ", verbatimTextOutput("txt3"),
#'     "n * 4: ", verbatimTextOutput("txt4")
#'
#'   ),
#'   server = function(input, output) {
#'     r2 <- cachedReactive(input$n, {
#'       input$n * 2
#'     })
#'     output$txt2 <- renderText(r2())
#'
#'     # BAD: This uses the same cache key as r2 above, and so it will collide with
#'     # r2.
#'     r3 <- cachedReactive(input$n, {
#'       input$n * 3
#'     })
#'     output$txt3 <- renderText(r3())
#'
#'     # GOOD: By adding an ID string,this uses a different cache key from r2,
#'     # and so its keys will not collide with those from r2.
#'     r4 <- cachedReactive(list(input$n, "times4"), {
#'       input$n * 4
#'     })
#'     output$txt4 <- renderText(r4())
#'   }
#' )
#'
#' }
#'
#' @importFrom digest digest
#' @export
cachedReactive <- function(
  cacheKeyExpr,
  valueExpr,
  ...,
  domain = getDefaultReactiveDomain(),
  cache = "app")
{
  if (length(list(...)) != 0) {
    stop("Additional ... arguments are not allowed.")
  }

  cacheKeyExpr <- substitute(cacheKeyExpr)
  cacheKeyFunc <- reactive(cacheKeyExpr, env = parent.frame(), quoted = TRUE,
                           domain = domain, label = "userCacheKey")

  valueExpr <- substitute(isolate(valueExpr))
  valueFunc <- exprToFunction(valueExpr, parent.frame(), quoted = TRUE)
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveHandler", ..stacktraceon = TRUE)

  reactive(label = "cachedReactive",
    hybrid_chain(
      cacheKeyFunc(),
      function(cacheKeyResult) {
        cache <- resolve_cache_object(cache, domain)
        key   <- digest(cacheKeyResult, algo = "sha1")
        value <- cache$get(key)

        # Case 1: cache hit
        if (!is.key_missing(value)) {
          return(value)
        }
        # Case 2: cache miss
        hybrid_chain(
          valueFunc(),
          function(value) {
            cache$set(key, value)

            value
          }
        )
      }
    ),
    domain = domain
  )
}
