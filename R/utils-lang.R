# Given a list of quosures, return a function that will evaluate them and return
# a list of resulting values. If the list contains a single expression, unwrap
# it from the list.
quos_to_func <- function(qs) {
  if (length(qs) == 0) {
    stop("Need at least one item in `...` to use as cache key or event.")
  }

  if (length(qs) == 1) {
    # Special case for one quosure. This is needed for async to work -- that is,
    # when the quosure returns a promise. It needs to not be wrapped into a list
    # for the hybrid_chain stuff to detect that it's a promise. (Plus, it's not
    # even clear what it would mean to mix promises and non-promises in the
    # key.)
    qs <- qs[[1]]
    function() {
      eval_tidy(qs)
    }

  } else {
    function() {
      lapply(qs, eval_tidy)
    }
  }
}

# Given a list of quosures, return a string representation of the expressions.
#
# qs <- list(quo(a+1), quo({ b+2; b + 3 }))
# quos_to_label(qs)
# #> [1] "a + 1, {\n    b + 2\n    b + 3\n}"
quos_to_label <- function(qs) {
  res <- lapply(qs, function(q) {
    paste(deparse(get_expr(q)), collapse = "\n")
  })

  paste(res, collapse = ", ")
}

# Get the formals and body for a function, without source refs. This is used for
# consistent hashing of the function.
formalsAndBody <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  list(
    formals = formals(x),
    body = body(zap_srcref(x))
  )
}


#' Convert a quosure to a function for a Shiny render function
#'
#' This takes a quosure and label, and wraps them into a function that should be
#' passed to [createRenderFunction()].
#'
#' This function was added in Shiny 1.6.0. Previously, it was recommended to use
#' [installExprFunction()] or [exprToFunction()] in render functions, but now we
#' recommend using [quoToFunction()] paired with [`rlang::enquo0()`]. This is because `quoToFunction()`
#' does not require package authors to manually handle the `env` and `quoted` arguments -- that information is captured
#' by quosures provided by \pkg{rlang}. To support legacy `env` and `quoted` variables,
#' please see [`installExprFunction()`] for more details.
#'
#'
#' @param q Quosure of the expression `x`. When capturing expressions to create
#'   your quosure, it is recommended to use [`enquo0()`] to not unquote the
#'   object too early. See [`enquo0()`] for more details.
#' @inheritParams installExprFunction
#' @seealso
#' * [createRenderFunction()] for example usage.
#' * [rlang::enquo0()] for more
#'   information about not immediately unquoting when making `q`.
#' @export
#' @examples
#' # Create a new renderer, similar to `renderText()`.
#' # This is something that toolkit authors will do.
#' renderTriple <- function(x) {
#'   # Create render function given the user-supplied quosure or expression.
#'   func <- quoToFunction(rlang::enquo0(x))
#'
#'   # Wrap up func, with another function which takes the value of func()
#'   # and modifies it to concatinate the value three times
#'   createRenderFunction(
#'     func,
#'     transform = function(value, ...) {
#'       paste(rep(value, 3), collapse=", ")
#'     },
#'     outputFunc = textOutput
#'   )
#' }
#'
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text", B="text")
#'
#' \dontrun{
#' # Create an output object
#' output$tripleA <- renderTriple({
#'   values$A
#' })
#' # Create an output object
#' output$tripleB <- renderTriple({
#'   values$B
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using `isolate()`
#' tripleA <- renderTriple({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#'
#'
#' # If you want to use a quoted expression, use `rlang::inject()`.
#' q <- quote({ values$A })
#' tripleA <- rlang::inject(renderTriple(!!q))
#' # Look at the value
#' isolate(tripleA())
#'
#'
#' # Capturing an expression and an environment, using a quosure and `rlang::inject()`:
#' e <- new.env()
#' e$vals <- reactiveValues(A = "hello")
#' # Create a quosure that captures both the expression and environment.
#' myquo <- rlang::new_quosure(quote({ vals$A }), env = e)
#' myquo
#' # `inject()` the quosure into the render function
#' tripleA <- rlang::inject(renderTriple(!!myquo))
#' # Look at the value
#' isolate(tripleA())
#' # "hello, hello, hello"
quoToFunction <- function(
  q,
  label = sys.call(-1)[[1]],
  ..stacktraceon = FALSE
) {
  func <- quoToSimpleFunction(as_quosure(q))
  wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon)
}

updateFunctionLabel <- function(label) {
  if (all(is.language(label))) {
    # Prevent immediately invoked functions like as.language(a()())
    if (is.language(label) && length(label) > 1) {
      return("wrappedFunction")
    }
    label <- deparse(label, width.cutoff = 500L)
  }
  label <- as.character(label)
  # Prevent function calls that are over one line; (Assignments are hard to perform)
    # Prevent immediately invoked functions like "a()()"
  if (length(label) > 1 || grepl("(", label, fixed = TRUE)) {
    return("wrappedFunction")
  }
  if (label == "NULL") {
    return("wrappedFunction")
  }
  label
}

quoToSimpleFunction <- function(q) {
  # Should not use `new_function(list(), get_expr(q), get_env(q))` as extra logic
  # is done by rlang to convert the quosure to a function within `as_function(q)`
  fun <- as_function(q)

  # If the quosure is empty, then the returned function can not be called.
  # https://github.com/r-lib/rlang/issues/1244
  if (quo_is_missing(q)) {
    fn_body(fun) <- quote({})
  }

  # `as_function()` returns a function that takes `...`. We need one that takes no
  # args.
  fn_fmls(fun) <- list()

  fun
}


#' Convert an expression to a function
#'
#' @description
#' Note: As of Shiny 1.6.0, when using quosure objects to replace the need for `expr` and `env` paramters, see [`quoToFunction()`] to simplify your code.
#'
#' These two methods are be called from within another function, because
#' they will attempt to get an unquoted expression from two calls back.
#'
#' For `exprToFunction()`:
#' * If `expr` is a quoted expression, then this just converts it to a function.
#' * If `expr` is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' * If `expr` was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#' * If `expr` is a quosure, then `quoted` must be `TRUE` to use the quosure as
#'   the expression.
#' * If `expr` is an `rlang::inject()`ed quosure value, then `env` and `quoted`
#'   will be ignored. Ex: `rlang::inject(exprToFunction(!!myquo))`.
#'
#' `installExprFunction` installs an expression in the given environment as a
#' function, and registers debug hooks so that breakpoints may be set in the
#' function.
#'
#' `installExprFunction` can replace `exprToFunction` as follows: we may use
#' `func <- exprToFunction(expr)` if we do not want the debug hooks, or
#' `installExprFunction(expr, "func")` if we do. Both approaches create a
#' function named `func` in the current environment.
#'
#' To simplify `rlang::inject(installExprFunction(!!myquo, "func"))` or
#' `rlang::inject(installExprFunction(myquo, "func", quoted = TRUE))`, toolkit
#' authors can call `func <- quoToFunction(myquo)` instead.
#' See [`quoToFunction()`] for more details.
#'
#'
#' @section `shinyRenderWidget()`:
#'
#' The [`htmlwidgets::shinyRenderWidget()`] function is a wrapper around
#' [`installExprFunction()`]. Before Shiny 1.6.0, widget authors were recommended to force quoting of the `expr`.
#'
#' ```r
#' # shiny render function for a widget named 'foo'
#' renderFoo <- function(expr, env = parent.frame(), quoted = FALSE) {
#'   if (!quoted) { expr <- substitute(expr) } # force quoted
#'   htmlwidgets::shinyRenderWidget(expr, fooOutput, env, quoted = TRUE)
#' }
#' ```
#'
#' Now, when making new render methods that leverage quosures, widget authors
#' can capture the `expr` immedately with a quosure using [`rlang::enquo0()`].
#' When passing the quosure to [`htmlwidgets::shinyRenderWidget()`],
#' provide `quoted = TRUE` so `installExprFunction()` can properly handle the quosure.
#'
#' ```r
#' # shiny render function for a widget named 'foo'
#' renderFoo <- function(expr) {
#'   q <- rlang::enquo0(expr)
#'   inject(htmlwidgets::shinyRenderWidget(q, fooOutput, quoted = TRUE))
#' }
#' ```
#'
#' @param expr A quoted or unquoted expression, or a quosure.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @seealso [`quoToFunction()`], [`rlang::enquo0()`]
#'
#' @examples
#' # Example of a new renderer, similar to renderText
#' # This is something that toolkit authors will do
#'
#' # Version 1: exprToFunction()
#' renderTripleExpr <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- exprToFunction(expr, env, quoted)
#'
#'   # Wrap up func, with another function which takes the value of func()
#'   # and modifies it to concatinate the value three times
#'   createRenderFunction(
#'     func,
#'     transform = function(value, ...) {
#'       paste(rep(value, 3), collapse=", ")
#'     },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Version 2: installExprFunction()
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function and register debug hooks
#'   installExprFunction(expr, "func", env, quoted)
#'
#'   # (Same as above)
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) { paste(rep(value, 3), collapse=", ") },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Version 3: quoToFunction()
#' # This is the recommended way when able to leverage quosures,
#' # as it discards `env` and `quoted` for simplicity
#' renderTripleQuo <- function(expr) {
#'   # Convert expr to a quosure, and then to a function
#'   func <- quoToFunction(rlang::enquo0(expr))
#'
#'   # (Same as above)
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) { paste(rep(value, 3), collapse=", ") },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Example of using a renderer (which calls `installToExpr()`).
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' if (FALSE) {
#' # Create an output object
#' output$tripleA <- renderTriple({
#'   values$A
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using isolate()
#' tripleA <- renderTriple({
#'   values$A
#' })
#' isolate(tripleA())
#' #> "text, text, text"
#'
#'
#' # If you want to use a quosure for your expresssion,
#' # use `quoted = TRUE` or `rlang::inject()`.
#'
#' # Using `quoted = TRUE`
#' q <- quote({ values$A })
#' tripleA <- renderTriple(q, quoted = TRUE)
#' isolate(tripleA())
#' #> "text, text, text"
#'
#' # Using `rlang::inject()`
#' tripleA <- rlang::inject(renderTriple(!!q))
#' isolate(tripleA())
#' #> "text, text, text"
#'
#'
#' # Capturing an expression and an environment, using a quosure
#' e <- new.env()
#' e$vals <- reactiveValues(A = "hello")
#' # Create a quosure that captures both the expression and environment.
#' myquo <- rlang::new_quosure(quote({ vals$A }), env = e)
#' myquo
#' # Using `quoted = TRUE`
#' tripleA <- renderTriple(q, quoted = TRUE)
#' isolate(tripleA())
#' #> "hello, hello, hello"
#' # Using `rlang::inject()`
#' tripleA <- rlang::inject(renderTriple(!!myquo))
#' isolate(tripleA())
#' #> "hello, hello, hello"
#' @export
#' @keywords internal
exprToFunction <- function(expr, env = parent.frame(), quoted = FALSE) {
  # If `expr` is a raw quosure, must say `quoted = TRUE`; (env is ignored)
  # If `inject()` a quosure, env is ignored, and quoted should be FALSE (aka ignored).
  # Make article of usage
  # * (by joe)

  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  if (is_quosure(expr)) {
    # inject()ed quosure
    # do nothing
  } else if (is.language(expr)) {
    expr <- new_quosure(expr, env = env)
  } else {
    stop("Don't know how to convert '", class(expr)[1], "' to a function; a quosure or quoted expression was expected")
  }
  # Can NOT call `new_function()`. MUST call `as_function()`
  # rlang has custom logic for converting quosures to functions
  quoToSimpleFunction(expr)
}

#' @rdname exprToFunction
#'
#' @param name The name the function should be given
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults to
#'   the name of the calling function.
#' @param wrappedWithLabel,..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @export
installExprFunction <- function(expr, name, eval.env = parent.frame(2),
                                quoted = FALSE,
                                assign.env = parent.frame(1),
                                label = sys.call(-1)[[1]],
                                wrappedWithLabel = TRUE,
                                ..stacktraceon = FALSE) {
  if (!quoted) {
    quoted <- TRUE
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }

  func <- exprToFunction(expr, eval.env, quoted)
  if (length(label) > 1) {
    # Just in case the deparsed code is more complicated than we imagine. If we
    # have a label with length > 1 it causes warnings in wrapFunctionLabel.
    label <- paste0(label, collapse = "\n")
  }
  if (wrappedWithLabel) {
    func <- wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon)
  } else {
    registerDebugHook(name, assign.env, label)
  }
  assign(name, func, envir = assign.env)
}

# Utility function for creating a debugging label, given an expression.
# `expr` is a quoted expression.
# `function_name` is the name of the calling function.
# `label` is an optional user-provided label. If NULL, it will be inferred.
exprToLabel <- function(expr, function_name, label = NULL) {
  srcref <- attr(expr, "srcref", exact = TRUE)
  if (is.null(label)) {
    label <- rexprSrcrefToLabel(
      srcref[[1]],
      sprintf('%s(%s)', function_name, paste(deparse(expr), collapse = '\n'))
    )
  }
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- srcFileOfRef(srcref[[1]])
  label
}
