utils::globalVariables(".GenericCallEnv", add = TRUE)

#' Add caching with reactivity to an object
#'
#' @description
#'
#' `bindCache()` adds caching [reactive()] expressions and `render*` functions
#' (like [renderText()], [renderTable()], ...).
#'
#' Ordinary [reactive()] expressions automatically cache their _most recent_
#' value, which helps to  avoid redundant computation in downstream reactives.
#' `bindCache()` will cache all previous values (as long as they fit in the
#' cache) and they can be shared across user sessions. This allows
#' `bindCache()` to dramatically improve performance when used correctly.

#' @details
#'
#' `bindCache()` requires one or more expressions that are used to generate a
#' **cache key**, which is used to determine if a computation has occurred
#' before and hence can be retrieved from the cache. If you're familiar with the
#' concept of memoizing pure functions (e.g., the \pkg{memoise} package), you
#' can think of the cache key as the input(s) to a pure function. As such, one
#' should take care to make sure the use of `bindCache()` is _pure_ in the same
#' sense, namely:
#'
#' 1. For a given key, the return value is always the same.
#' 2. Evaluation has no side-effects.
#'
#' In the example here, the `bindCache()` key consists of `input$x` and
#' `input$y` combined, and the value is `input$x * input$y`. In this simple
#' example, for any given key, there is only one possible returned value.
#'
#' ```
#' r <- reactive({ input$x * input$y }) %>%
#'   bindCache(input$x, input$y)
#' ```
#'

#' The largest performance improvements occur when the cache key is fast to
#' compute and the reactive expression is slow to compute. To see if the value
#' should be computed, a cached reactive evaluates the key, and then serializes
#' and hashes the result. If the resulting hashed key is in the cache, then the
#' cached reactive simply retrieves the previously calculated value and returns
#' it; if not, then the value is computed and the result is stored in the cache
#' before being returned.
#'
#' To compute the cache key, `bindCache()` hashes the contents of `...`, so it's
#' best to avoid including large objects in a cache key since that can result in
#' slow hashing. It's also best to avoid reference objects like environments and
#' R6 objects, since the serialization of these objects may not capture relevant
#' changes.
#'
#' If you want to use a large object as part of a cache key, it may make sense
#' to do some sort of reduction on the data that still captures information
#' about whether a value can be retrieved from the cache. For example, if you
#' have a large data set with timestamps, it might make sense to extract the
#' most recent timestamp and return that. Then, instead of hashing the entire
#' data object, the cached reactive only needs to hash the timestamp.
#'
#' ```
#' r <- reactive({ compute(bigdata()) } %>%
#'   bindCache({ extract_most_recent_time(bigdata()) })
#' ```
#'
#' For computations that are very slow, it often makes sense to pair
#' [bindCache()] with [bindEvent()] so that no computation is performed until
#' the user explicitly requests it (for more, see the Details section of
#' [bindEvent()]).

#' @section Cache keys and reactivity:
#'
#'   Because the **value** expression (from the original [reactive()]) is
#'   cached, it is not necessarily re-executed when someone retrieves a value,
#'   and therefore it can't be used to decide what objects to take reactive
#'   dependencies on. Instead, the **key** is used to figure out which objects
#'   to take reactive dependencies on. In short, the key expression is reactive,
#'   and value expression is no longer reactive.
#'
#'   Here's an example of what not to do: if the key is `input$x` and the value
#'   expression is from `reactive({input$x + input$y})`, then the resulting
#'   cached reactive  will only take a reactive dependency on `input$x` -- it
#'   won't recompute `{input$x + input$y}` when just `input$y` changes.
#'   Moreover, the cache won't use `input$y` as part of the key, and so it could
#'   return incorrect values in the future when it retrieves values from the
#'   cache. (See the examples below for an example of this.)
#'
#'   A better cache key would be something like `input$x, input$y`. This does
#'   two things: it ensures that a reactive dependency is taken on both
#'   `input$x` and `input$y`, and it also makes sure that both values are
#'   represented in the cache key.
#'
#'   In general, `key` should use the same reactive inputs as `value`, but the
#'   computation should be simpler. If there are other (non-reactive) values
#'   that are consumed, such as external data sources, they should be used in
#'   the `key` as well. Note that if the `key` is large, it can make sense to do
#'   some sort of reduction on it so that the serialization and hashing of the
#'   cache key is not too expensive.
#'
#'   Remember that the key is _reactive_, so it is not re-executed every single
#'   time that someone accesses the cached reactive. It is only re-executed if
#'   it has been invalidated by one of the reactives it depends on. For
#'   example, suppose we have this cached reactive:
#'
#'   ```
#'   r <- reactive({ input$x * input$y }) %>%
#'     bindCache(input$x, input$y)
#'   ```
#'
#' In this case, the key expression is essentially `reactive(list(input$x,
#' input$y))` (there's a bit more to it, but that's a good enough
#' approximation). The first time `r()` is called, it executes the key, then
#' fails to find it in the cache, so it executes the value expression, `{
#' input$x + input$y }`. If `r()` is called again, then it does not need to
#' re-execute the key expression, because it has not been invalidated via a
#' change to `input$x` or `input$y`; it simply returns the previous value.
#' However, if `input$x` or `input$y` changes, then the reactive expression will
#' be invalidated, and the next time that someone calls `r()`, the key
#' expression will need to be re-executed.
#'
#' Note that if the cached reactive is passed to [bindEvent()], then the key
#' expression will no longer be reactive; instead, the event expression will be
#' reactive.
#'
#'
#' @section Cache scope:
#'
#'   By default, when `bindCache()` is used, it is scoped to the running
#'   application. That means that it shares a cache with all user sessions
#'   connected to the application (within the R process). This is done with the
#'   `cache` parameter's default value, `"app"`.
#'
#'   With an app-level cache scope, one user can benefit from the work done for
#'   another user's session. In most cases, this is the best way to get
#'   performance improvements from caching. However, in some cases, this could
#'   leak information between sessions. For example, if the cache key does not
#'   fully encompass the inputs used by the value, then data could leak between
#'   the sessions. Or if a user sees that a cached reactive returns its value
#'   very quickly, they may be able to infer that someone else has already used
#'   it with the same values.
#'
#'   It is also possible to scope the cache to the session, with
#'   `cache="session"`. This removes the risk of information leaking between
#'   sessions, but then one session cannot benefit from computations performed in
#'   another session.
#'
#'   It is possible to pass in caching objects directly to
#'   `bindCache()`. This can be useful if, for example, you want to use a
#'   particular type of cache with specific cached reactives, or if you want to
#'   use a [cachem::cache_disk()] that is shared across multiple processes and
#'   persists beyond the current R session.
#'
#'   To use different settings for an application-scoped cache, you can call
#'   [shinyOptions()] at the top of your app.R, server.R, or
#'   global.R. For example, this will create a cache with 500 MB of space
#'   instead of the default 200 MB:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_mem(max_size = 500e6))
#'   ```
#'
#'   To use different settings for a session-scoped cache, you can set
#'   `session$cache` at the top of your server function. By default, it will
#'   create a 200 MB memory cache for each session, but you can replace it with
#'   something different. To use the session-scoped cache, you must also call
#'   `bindCache()` with `cache="session"`. This will create a 100 MB cache for
#'   the session:
#'
#'   ```
#'   function(input, output, session) {
#'     session$cache <- cachem::cache_mem(max_size = 100e6)
#'     ...
#'   }
#'   ```
#'
#'   If you want to use a cache that is shared across multiple R processes, you
#'   can use a [cachem::cache_disk()]. You can create a application-level shared
#'   cache by putting this at the top of your app.R, server.R, or global.R:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_disk(file.path(dirname(tempdir()), "myapp-cache")))
#'   ```
#'
#'   This will create a subdirectory in your system temp directory named
#'   `myapp-cache` (replace `myapp-cache` with a unique name of
#'   your choosing). On most platforms, this directory will be removed when
#'   your system reboots. This cache will persist across multiple starts and
#'   stops of the R process, as long as you do not reboot.
#'
#'   To have the cache persist even across multiple reboots, you can create the
#'   cache in a location outside of the temp directory. For example, it could
#'   be a subdirectory of the application:
#'
#'   ```
#'   shinyOptions(cache = cachem::cache_disk("./myapp-cache"))
#'   ```
#'
#'   In this case, resetting the cache will have to be done manually, by deleting
#'   the directory.
#'
#'   You can also scope a cache to just one item, or selected items. To do that,
#'   create a [cachem::cache_mem()] or [cachem::cache_disk()], and pass it
#'   as the `cache` argument of `bindCache()`.
#'

#'
#' @section Computing cache keys:
#'
#'   The actual cache key that is used internally takes value from evaluating
#'   the key expression(s) (from the `...` arguments) and combines it with the
#'   (unevaluated) value expression.
#'
#'   This means that if there are two cached reactives which have the same
#'   result from evaluating the key, but different value expressions, then they
#'   will not need to worry about collisions.
#'
#'   However, if two cached reactives have identical key and value expressions
#'   expressions, they will share the cached values. This is useful when using
#'   `cache="app"`: there may be multiple user sessions which create separate
#'   cached reactive objects (because they are created from the same code in the
#'   server function, but the server function is executed once for each user
#'   session), and those cached reactive objects across sessions can share
#'   values in the cache.



#'
#' @section Async with cached reactives:
#'
#'   With a cached reactive expression, the key and/or value expression can be
#'   _asynchronous_. In other words, they can be promises --- not regular R
#'   promises, but rather objects provided by the
#'   \href{https://rstudio.github.io/promises/}{\pkg{promises}}  package, which
#'   are similar to promises in JavaScript. (See [promises::promise()] for more
#'   information.) You can also use [mirai::mirai()] or [future::future()]
#'   objects to run code in a separate process or even on a remote machine.
#'
#'   If the value returns a promise, then anything that consumes the cached
#'   reactive must expect it to return a promise.
#'
#'   Similarly, if the key is a promise (in other words, if it is asynchronous),
#'   then the entire cached reactive must be asynchronous, since the key must be
#'   computed asynchronously before it knows whether to compute the value or the
#'   value is retrieved from the cache. Anything that consumes the cached
#'   reactive must therefore expect it to return a promise.
#'

#'
#' @section Developing render functions for caching:
#'
#'   If you've implemented your own `render*()` function, it may just work with
#'   `bindCache()`, but it is possible that you will need to make some
#'   modifications. These modifications involve helping `bindCache()` avoid
#'   cache collisions, dealing with internal state that may be set by the,
#'   `render` function, and modifying the data as it goes in and comes out of
#'   the cache.
#'
#'   You may need to provide a `cacheHint` to [createRenderFunction()] (or
#'   `htmlwidgets::shinyRenderWidget()`, if you've authored an htmlwidget) in
#'   order for `bindCache()` to correctly compute a cache key.
#'
#'   The potential problem is a cache collision. Consider the following:
#'
#'   ```
#'   output$x1 <- renderText({ input$x }) %>% bindCache(input$x)
#'   output$x2 <- renderText({ input$x * 2 }) %>% bindCache(input$x)
#'   ```
#'
#'   Both `output$x1` and `output$x2` use `input$x` as part of their cache key,
#'   but if it were the only thing used in the cache key, then the two outputs
#'   would have a cache collision, and they would have the same output. To avoid
#'   this, a _cache hint_ is automatically added when [renderText()] calls
#'   [createRenderFunction()]. The cache hint is used as part of the actual
#'   cache key, in addition to the one passed to `bindCache()` by the user. The
#'   cache hint can be viewed by calling the internal Shiny function
#'   `extractCacheHint()`:
#'
#'   ```
#'   r <- renderText({ input$x })
#'   shiny:::extractCacheHint(r)
#'   ```
#'
#'   This returns a nested list containing an item, `$origUserFunc$body`, which
#'   in this case is the expression which was passed to `renderText()`:
#'   `{ input$x }`. This (quoted)  expression is mixed into the actual cache
#'   key, and it is how `output$x1` does not have collisions with `output$x2`.
#'
#'   For most developers of render functions, nothing extra needs to be done;
#'   the automatic inference of the cache hint is sufficient. Again, you can
#'   check it by calling `shiny:::extractCacheHint()`, and by testing the
#'   render function for cache collisions in a real application.
#'
#'   In some cases, however, the automatic cache hint inference is not
#'   sufficient, and it is necessary to provide a cache hint. This is true
#'   for `renderPrint()`. Unlike `renderText()`, it wraps the user-provided
#'   expression in another function, before passing it to [createRenderFunction()]
#'   (instead of [createRenderFunction()]). Because the user code is wrapped in
#'   another function, `createRenderFunction()` is not able to automatically
#'   extract the user-provided code and use it in the cache key. Instead,
#'   `renderPrint` calls `createRenderFunction()`, it explicitly passes along a
#'   `cacheHint`, which includes a label and the original user expression.
#'
#'   In general, if you need to provide a `cacheHint`, it is best practice to
#'   provide a `label` id, the user's `expr`, as well as any other arguments
#'   that may influence the final value.
#'
#'   For \pkg{htmlwidgets}, it will try to automatically infer a cache hint;
#'   again, you can inspect the cache hint with `shiny:::extractCacheHint()` and
#'   also test it in an application. If you do need to explicitly provide a
#'   cache hint, pass it to `shinyRenderWidget`. For example:
#'
#'   ```
#'   renderMyWidget <- function(expr) {
#'     q <- rlang::enquo0(expr)
#'
#'     htmlwidgets::shinyRenderWidget(
#'       q,
#'       myWidgetOutput,
#'       quoted = TRUE,
#'       cacheHint = list(label = "myWidget", userQuo = q)
#'     )
#'   }
#'   ```
#'
#'   If your `render` function sets any internal state, you may find it useful
#'   in your call to [createRenderFunction()] to use
#'   the `cacheWriteHook` and/or `cacheReadHook` parameters. These hooks are
#'   functions that run just before the object is stored in the cache, and just
#'   after the object is retrieved from the cache. They can modify the data
#'   that is stored and retrieved; this can be useful if extra information needs
#'   to be stored in the cache. They can also be used to modify the state of the
#'   application; for example, it can call [createWebDependency()] to make
#'   JS/CSS resources available if the cached object is loaded in a different R
#'   process. (See the source of `htmlwidgets::shinyRenderWidget` for an example
#'   of this.)
#'
#' @section Uncacheable objects:
#'
#'   Some render functions cannot be cached, typically because they have side
#'   effects or modify some external state, and they must re-execute each time
#'   in order to work properly.
#'
#'   For developers of such code, they should call [createRenderFunction()] (or
#'   [markRenderFunction()]) with `cacheHint = FALSE`.
#'
#'
#' @section Caching with `renderPlot()`:
#'
#'   When `bindCache()` is used with `renderPlot()`, the `height` and `width`
#'   passed to the original `renderPlot()` are ignored. They are superseded by
#'   `sizePolicy` argument passed to `bindCache. The default is:
#'
#'   ```
#'   sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2)
#'   ```
#'
#' `sizePolicy` must be a function that takes a two-element numeric vector as
#' input, representing the width and height of the `<img>` element in the
#' browser window, and it must return a two-element numeric vector, representing
#' the pixel dimensions of the plot to generate. The purpose is to round the
#' actual pixel dimensions from the browser to some other dimensions, so that
#' this will not generate and cache images of every possible pixel dimension.
#' See [sizeGrowthRatio()] for more information on the default sizing policy.
#'
#' @param x The object to add caching to.
#' @param ... One or more expressions to use in the caching key.
#' @param cache The scope of the cache, or a cache object. This can be `"app"`
#'   (the default), `"session"`, or a cache object like a
#'   [cachem::cache_disk()]. See the Cache Scoping section for more information.
#'
#' @seealso [bindEvent()], [renderCachedPlot()] for caching plots.
#'
#' @examples
#' \dontrun{
#' rc <- bindCache(
#'   x = reactive({
#'     Sys.sleep(2)   # Pretend this is expensive
#'     input$x * 100
#'   }),
#'   input$x
#' )
#'
#' # Can make it prettier with the %>% operator
#' library(magrittr)
#'
#' rc <- reactive({
#'   Sys.sleep(2)
#'   input$x * 100
#' }) %>%
#'   bindCache(input$x)
#'
#' }
#'
#' ## Only run app examples in interactive R sessions
#' if (interactive()) {
#'
#' # Basic example
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- reactive({
#'       # The value expression is an _expensive_ computation
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y)
#'
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#'
#' # Caching renderText
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     output$txt <- renderText({
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y)
#'   }
#' )
#'
#'
#' # Demo of using events and caching with an actionButton
#' shinyApp(
#'   ui = fluidPage(
#'     sliderInput("x", "x", 1, 10, 5),
#'     sliderInput("y", "y", 1, 10, 5),
#'     actionButton("go", "Go"),
#'     div("x * y: "),
#'     verbatimTextOutput("txt")
#'   ),
#'   server = function(input, output) {
#'     r <- reactive({
#'       message("Doing expensive computation...")
#'       Sys.sleep(2)
#'       input$x * input$y
#'     }) %>%
#'       bindCache(input$x, input$y) %>%
#'       bindEvent(input$go)
#'       # The cached, eventified reactive takes a reactive dependency on
#'       # input$go, but doesn't use it for the cache key. It uses input$x and
#'       # input$y for the cache key, but doesn't take a reactive dependency on
#'       # them, because the reactive dependency is superseded by addEvent().
#'
#'     output$txt <- renderText(r())
#'   }
#' )
#'
#' }
#'
#' @export
bindCache <- function(x, ..., cache = "app") {
  force(cache)

  UseMethod("bindCache")
}

#' @export
bindCache.default <- function(x, ...) {
  stop("Don't know how to handle object with class ", paste(class(x), collapse = ", "))
}

#' @export
bindCache.reactiveExpr <- function(x, ..., cache = "app") {
  check_dots_unnamed()

  call_srcref <- attr(sys.call(-1), "srcref", exact = TRUE)
  label <- rassignSrcrefToLabel(
    call_srcref,
    defaultLabel = exprToLabel(substitute(x), "cachedReactive")
  )

  domain <- reactive_get_domain(x)

  # Convert the ... to a function that returns their evaluated values.
  keyFunc <- quos_to_func(enquos0(...))

  valueFunc <- reactive_get_value_func(x)
  # Hash cache hint now -- this will be added to the key later on, to reduce the
  # chance of key collisions with other cachedReactives.
  cacheHint <- rlang::hash(extractCacheHint(x))
  valueFunc <- wrapFunctionLabel(valueFunc, "cachedReactiveValueFunc", ..stacktraceon = TRUE)

  # Don't hold on to the reference for x, so that it can be GC'd
  rm(x)
  # Hacky workaround for issue with `%>%` preventing GC:
  # https://github.com/tidyverse/magrittr/issues/229
  if (exists(".GenericCallEnv") && exists(".", envir = .GenericCallEnv, inherits = FALSE)) {
    rm(list = ".", envir = .GenericCallEnv, inherits = FALSE)
  }

  res <- reactive(label = label, domain = domain, {
    cache <- resolve_cache_object(cache, domain)
    hybrid_chain(
      keyFunc(),
      generateCacheFun(valueFunc, cache, cacheHint, cacheReadHook = identity, cacheWriteHook = identity)
    )
  })

  class(res) <- c("reactive.cache", class(res))
  res
}

#' @export
bindCache.shiny.render.function <- function(x, ..., cache = "app") {
  check_dots_unnamed()

  keyFunc <- quos_to_func(enquos0(...))

  cacheHint <- rlang::hash(extractCacheHint(x))

  cacheWriteHook <- attr(x, "cacheWriteHook", exact = TRUE) %||% identity
  cacheReadHook  <- attr(x, "cacheReadHook",  exact = TRUE) %||% identity

  valueFunc <- x

  renderFunc <- function(...) {
    domain <- getDefaultReactiveDomain()
    cache <- resolve_cache_object(cache, domain)

    hybrid_chain(
      keyFunc(),
      generateCacheFun(valueFunc, cache, cacheHint, cacheReadHook, cacheWriteHook, ...)
    )
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- c("shiny.render.function.cache", class(valueFunc))
  renderFunc
}

#' @export
bindCache.shiny.renderPlot <- function(x, ...,
  cache = "app",
  sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.2))
{
  check_dots_unnamed()

  valueFunc <- x

  # Given the actual width/height of the image element in the browser, the
  # resize observer computes the width/height using sizePolicy() and pushes
  # those values into `fitWidth` and `fitHeight`. It's done this way so that the
  # `fitWidth` and `fitHeight` only change (and cause invalidations of the key
  # expression) when the rendered image size changes, and not every time the
  # browser's <img> tag changes size.
  #
  # If the key expression were invalidated every time the image element changed
  # size, even if the resulting key was the same (because `sizePolicy()` gave
  # the same output for a slightly different img element size), it would result
  # in getting the (same) image from the cache and sending it to the client
  # again. This resize observer prevents that.
  fitDims <- reactiveVal(NULL)
  resizeObserverCreated <- FALSE
  outputName <- NULL
  ensureResizeObserver <- function() {
    if (resizeObserverCreated)
      return()

    doResizeCheck <- function() {
      if (is.null(outputName)) {
        outputName <<- getCurrentOutputInfo()$name
      }
      session <- getDefaultReactiveDomain()

      width  <- session$clientData[[paste0('output_', outputName, '_width')]]  %||% 0
      height <- session$clientData[[paste0('output_', outputName, '_height')]] %||% 0

      rect <- sizePolicy(c(width, height))
      fitDims(list(width = rect[1], height = rect[2]))
    }

    # Run it once immediately, then set up the observer
    isolate(doResizeCheck())

    observe({
      doResizeCheck()
    }, label = "plot-resize")
    # TODO: Make sure this observer gets GC'd if output$foo is replaced.
    # Currently, if you reassign output$foo, the observer persists until the
    # session ends. This is generally bad programming practice and should be
    # rare, but still, we should try to clean up properly.

    resizeObserverCreated <<- TRUE
  }

  renderFunc <- function(...) {
    hybrid_chain(
      # Pass in fitDims so that so that the generated plot will be the
      # dimensions specified by the sizePolicy; otherwise the plot would be the
      # exact dimensions of the img element, which isn't what we want for cached
      # plots.
      valueFunc(..., get_dims = fitDims),
      function(img) {
        # Replace exact pixel dimensions; instead, the max-height and max-width
        # will be set to 100% from CSS.
        img$class <- "shiny-scalable"
        img$width  <- NULL
        img$height <- NULL

        img
      }
    )
  }

  renderFunc <- addAttributes(renderFunc, renderFunctionAttributes(valueFunc))
  class(renderFunc) <- class(valueFunc)

  bindCache.shiny.render.function(
    renderFunc,
    ...,
    {
      ensureResizeObserver()
      session <- getDefaultReactiveDomain()
      if (is.null(session) || is.null(fitDims())) {
        req(FALSE)
      }
      pixelratio <- session$clientData$pixelratio %||% 1

      list(fitDims(), pixelratio)
    },
    cache = cache
  )
}

#' @export
bindCache.reactive.cache <- function(x, ...) {
  stop("bindCache() has already been called on the object.")
}

#' @export
bindCache.shiny.render.function.cache <- bindCache.reactive.cache

#' @export
bindCache.reactive.event <- function(x, ...) {
  stop("Can't call bindCache() after calling bindEvent() on an object. Maybe you wanted to call bindEvent() after bindCache()?")
}

#' @export
bindCache.shiny.render.function.event <- bindCache.reactive.event

#' @export
bindCache.Observer <- function(x, ...) {
  stop("Can't bindCache an observer, because observers exist for the side efects, not for their return values.")
}

#' @export
bindCache.function <- function(x, ...) {
  stop(
    "Don't know how to add caching to a plain function. ",
    "If this is a render* function for Shiny, it may need to be updated. ",
    "Please see ?shiny::bindCache for more information."
  )
}

# Returns a function which should be passed as a step in to hybrid_chain(). The
# returned function takes a cache key as input and manages storing and retrieving
# values from the cache, as well as executing the valueFunc if needed.
generateCacheFun <- function(
  valueFunc,
  cache,
  cacheHint,
  cacheReadHook,
  cacheWriteHook,
  ...
) {
  function(cacheKeyResult) {
    key_str <- rlang::hash(list(cacheKeyResult, cacheHint))
    res <- cache$get(key_str)

    # Case 1: cache hit
    if (!is.key_missing(res)) {
      return(hybrid_chain(
        {
          # The first step is just to convert `res` to a promise or not, so
          # that hybrid_chain() knows to propagate the promise-ness.
          if (res$is_promise) promise_resolve(res)
          else                res
        },
        function(res) {
          if (res$error) {
            stop(res$value)
          }

          cacheReadHook(valueWithVisible(res))
        }
      ))
    }

    # Case 2: cache miss
    #
    # valueFunc() might return a promise, or an actual value. Normally we'd
    # use a hybrid_chain() for this, but in this case, we need to have
    # different behavior if it's a promise or not a promise -- the
    # information about whether or not it's a promise needs to be stored in
    # the cache. We need to handle both cases and record in the cache
    # whether it's a promise or not, so that any consumer of the
    # cachedReactive() will be given the correct kind of object (a promise
    # vs. an actual value) in the case of a future cache hit.
    p <- withCallingHandlers(
      withVisible(isolate(valueFunc(...))),
      error = function(e) {
        cache$set(key_str, list(
          is_promise = FALSE,
          value      = e,
          visible    = TRUE,
          error      = TRUE
        ))
      }
    )

    if (is.promising(p$value)) {
      p$value <- as.promise(p$value)
      p$value <- p$value$
        then(function(value) {
          res <- withVisible(value)
          cache$set(key_str, list(
            is_promise = TRUE,
            value      = cacheWriteHook(res$value),
            visible    = res$visible,
            error      = FALSE
          ))
          valueWithVisible(res)
        })$
        catch(function(e) {
          cache$set(key_str, list(
            is_promise = TRUE,
            value      = e,
            visible    = TRUE,
            error      = TRUE
          ))
          stop(e)
        })
      valueWithVisible(p)
    } else {
      # result is an ordinary value, not a promise.
      cache$set(key_str, list(
        is_promise = FALSE,
        value      = cacheWriteHook(p$value),
        visible    = p$visible,
        error      = FALSE
      ))
      return(valueWithVisible(p))
    }
  }
}

extractCacheHint <- function(func) {
  cacheHint <- attr(func, "cacheHint", exact = TRUE)

  if (is_false(cacheHint)) {
    stop(
      "Cannot call `bindCache()` on this object because it is marked as not cacheable.",
      call. = FALSE
    )
  }

  if (is.null(cacheHint)) {
    warning("No cacheHint found for this object. ",
            "Caching may not work properly.")
  }

  cacheHint
}
