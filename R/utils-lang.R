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
#'  This takes a quosure and label, and wraps them into a function that should be
#' passed to [createRenderFunction()].
#'
#' This function was added in Shiny 1.6.0. Previously, it was recommended to use
#' [installExprFunction()] or [exprToFunction()] in render functions, but now we
#' recommend using [quoToFunction()] paired with [`rlang::enquo0()`]. This is because `quoToFunction()`
#' does not require package authors to manually handle the `env` and `quoted` arguments -- that information is captured
#' by quosures provided by \pkg{rlang}. To support legacy `env` and `quoted` variables,
#' please see [`sustainEnvAndQuoted()`] for more details and examples on how to migrate your code.
#'
#' @inheritParams sustainEnvAndQuoted
#' @inheritParams installExprFunction
#' @seealso
#' * [createRenderFunction()] for example usage.
#' * [rlang::enquo0()] for more
#'   information about not immediately unquoting when making `q`.
#' * [sustainEnvAndQuoted()] for backwards support for `env` and `quoted` parameters
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
#' tripleA <- rlang::inject(renderTripleIdeal(!!q))
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
#' tripleA <- rlang::inject(renderTripleIdeal(!!myquo))
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


#' Convert expressions and quosures to a function
#'
#' `sustainEnvAndQuoted()` and `quoToFunction()` are meant to be used together in a
#' `render` function, to capture user expressions or quosures and convert them
#' to functions. They are meant to replace the older functions
#' [`installExprFunction()`] and [`exprToFunction()`] (although those functions will continue
#' to work in the future, they will not work with quosures). See the examples in [installExprFunction()] for
#' information on how to migrate to `sustainEnvAndQuoted()` and `quoToFunction()`.
#'
#' Although `sustainEnvAndQuoted()` can take `env` and `quoted` parameters, it is
#' recommended that they not be used, except for backward compatibility.
#' The recommended usage of `sustainEnvAndQuoted()` and `quoToFunction()` does not
#' include use of the `env` and `quoted` parameters. If it is necessary to
#' use quoted expressions and/or custom environments for evaluating, it can be
#' done with quosures and [rlang::inject()]. The examples below demonstrate how
#' to do this.
#'
#' If you are updating from [installExprFunction()] or [exprToFunction()] to
#' these functions, see the examples in the documentation for the old functions
#' for how to migrate them. It is strongly recommended to use [`enquo0()`] to create
#' the quosure as to not immediately evaluate the quosure.
#'
#' Note, like [`installExprFunction()`] and [`exprToFunction()`],
#' [`sustainEnvAndQuoted()`] must be called in similar locations as the method
#' _reaches_ up into the calling environments to check for information. This is
#' one of the **big reasons** why a single `quosure` object should be made as soon
#' as possible and passed down into the lower render functions. By using
#' quosures, less _magic_ has to be performed by package authors to avoid
#' prematurely evaluating user expression outside of the `env` context and manually handling quoted
#' expressions from one layer to the next.
#'
#' @section Migration:
#'
#' `env` and `quoted` should be set to [`deprecated()`] to signal to users and package authors
#' that the parameters should not be used (if possible). For example:
#'
#' ```r
#' library(lifecycle)
#' renderTriple <- function(x, env = deprecated(), quoted = deprecated()) {
#'   q <- rlang::enquo0(x)
#'   q <- sustainEnvAndQuoted(q, x, env, quoted)
#'   func <- quoToFunction(q)
#'   # Use `func` like normal...
#'   createRenderFunction(func, transform = function(value, ...) {
#'     paste(rep(value, 3), collapse=", ")
#'   }, outputFunc = textOutput)
#' }
#' ```
#'
#' Ideally, once `env` and `quoted` are no longer needed, they can be removed when creating the render function.
#'
#' ```r
#' renderTriple <- function(x) {
#'   func <- quoToFunction(rlang::enquo0(x))
#'   # Use `func` like normal...
#'   createRenderFunction(func, transform = function(value, ...) {
#'     paste(rep(value, 3), collapse=", ")
#'   }, outputFunc = textOutput)
#' }
#' ```
#'
#' Optionally, package authors could throw an error if the parameters are supplied.
#'
#' ```r
#' library(lifecycle)
#' renderTriple <- function(x, env = deprecated(), quoted = deprecated()) {
#'   if (is_present(env))
#'     deprecate_stop("1.7.0", "shinyEx::renderTriple(env = )",
#'                    details = "To use `env`, please `inject()` a quosure to `x`")
#'   if (is_present(quoted))
#'     deprecate_stop("1.7.0", "shinyEx::renderTriple(quoted = )",
#'                    details = "Please use a quosure instead")
#'   func <- quoToFunction(rlang::enquo0(x))
#'   # Use `func` like normal...
#'   createRenderFunction(func, transform = function(value, ...) {
#'     paste(rep(value, 3), collapse=", ")
#'   }, outputFunc = textOutput)
#' }
#' ```
#'
#' @section `shinyRenderWidget()`:
#'
#' The [`htmlwidgets::shinyRenderWidget()`] function is a wrapper around `sustainEnvAndQuoted()` and `quoToFunction()`.
#' Package authors should try their best to **only** provide a quosure to `shinyRenderWiget(expr=)`.
#'
#' Previously, widget authors were recommended to force quoting of the `expr`.
#'
#' ```r
#' # shiny render function for a widget named 'foo'
#' renderFoo <- function(expr, env = parent.frame(), quoted = FALSE) {
#'   if (!quoted) { expr <- substitute(expr) } # force quoted
#'   htmlwidgets::shinyRenderWidget(expr, fooOutput, env, quoted = TRUE)
#' }
#' ```
#'
#' Now, widget authors can capture the `expr` immedately with a quosure using [`enquo0()`] and still support `env` and `quoted` with `sustainEnvAndQuoted()`. When passing the quosure to [`htmlwidgets::shinyRenderWidget()`], do not provide `env` and `quoted` as
#' `q` encapsulates `env` and `quoted` information.
#'
#' ```r
#' library(lifecycle)
#' # shiny render function for a widget named 'foo'
#' renderFoo <- function(expr, env = deprecated(), quoted = deprecated()) {
#'   q <- enquo0(expr)
#'   q <- sustainEnvAndQuoted(q, expr, env, quoted)
#'   htmlwidgets::shinyRenderWidget(q, fooOutput)
#' }
#' ```
#'
#'
#' @param q Quosure of the expression `x`. When capturing expressions to create
#'   your quosure, it is recommended to use [`enquo0()`] to not unquote the
#'   object too early. See [`enquo0()`] for more details.
#' @param x An expression. This value is only used to validate if the raw
#'   expression is a [`quo()`]sure that was not [`inject()`]ed.
#' @param env An environment. This is provided for backward compatibility. If
#'   the calling environment is not [`missing()`] the variable used for `env`,
#'   then the quosure `q` will have its environment set to `env`.
#' @param quoted A logical indicating whether or not `env` is quoted. This is
#'   provided for backward compatibility. If the calling environment is not
#'   [`missing()`] the variable used for `quoted`, then the quosure `q` will set
#'   its expression to its internal quoted value.
#'
#' @examples
#' # Three versions of the same example renderer, similar to `renderText()`.
#' # This is something that toolkit authors will do.
#'
#' # Shiny <= 1.6.0; No quosure support.
#' renderTriplePre <- function(x, env = parent.frame(), quoted = FALSE) {
#'   # Create `func` from the expression `x`, `env`, and `quoted`
#'   func <- exprToFunction(x, env, quoted)
#'
#'   # Wrap up func, with another function which takes the value of func()
#'   # and modifies it.
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) {
#'       paste(rep(value, 3), collapse=", ")
#'     },
#'     # The outputFunc can be used by rmarkdown shiny apps to automatically
#'     # generate outputs.
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Shiny >= 1.7.0; Support quosures; Supports `env` and `quoted`.
#' library(lifecycle)
#' renderTripleSustain <- function(x, env = deprecated(), quoted = deprecated()) {
#'   # Capture the quosure of `x`
#'   q <- enquo0(x)
#'   # Possibly handle the environment and quoted parameters
#'   q <- sustainEnvAndQuoted(q, x, env, quoted)
#'   # Create the `func` from the quosure `q`
#'   func <- quoToFunction(q)
#'
#'   # (Same as above)
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) { paste(rep(value, 3), collapse=", ") },
#'     outputFunc = textOutput
#'   )
#' }
#'
#' # Shiny >= 1.7.0; Ideal code; Does not involve `env` or `quoted`
#' renderTripleIdeal <- function(x) {
#'   # Capture the quosure of `x` without automatic unquoting
#'   # and turn it into a function
#'   func <- quoToFunction(enquo0(x))
#'
#'   # (Same as above)
#'   createRenderFunction(
#'     func,
#'     transform = function(value, session, name, ...) { paste(rep(value, 3), collapse=", ") },
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
#' output$tripleA <- renderTripleIdeal({
#'   values$A
#' })
#' # Create an output object
#' output$tripleB <- renderTripleSustain({
#'   values$B
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using `isolate()`
#' tripleA <- renderTripleIdeal({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#'
#'
#' # If you want to use a quoted expression, use `rlang::inject()`.
#' q <- quote({ values$A })
#' tripleA <- rlang::inject(renderTripleIdeal(!!q))
#' isolate(tripleA())
#'
#' # `rlang::inject()` also works with `sustainEnvAndQuoted()`.
#' tripleS <- rlang::inject(renderTripleSustain(!!q))
#' isolate(tripleS())
#' # "text, text, text"
#'
#' # Capturing an expression and an environment, using a quosure and `rlang::inject()`:
#' e <- new.env()
#' e$vals <- reactiveValues(A = "hello", B = "world")
#' # Create a quosure that captures both the expression and environment.
#' myquo <- rlang::new_quosure(quote({ vals$A }), env = e)
#' tripleA <- rlang::inject(renderTripleIdeal(!!myquo))
#' isolate(tripleA())
#' # "hello, hello, hello"
#'
#' # Using a quoted expression and environment without quosures
#' expr <- quote({ vals$B })
#' tripleB <- renderTripleSustain(expr, env = e, quoted = TRUE)
#' isolate(tripleB())
#'
#' # Using an `inject()`ed quosure with either `env` or `quoted` throws an error
#' try(
#'   inject(renderTripleSustain(!!myquo, quoted = FALSE))
#' )
#' try(
#'   inject(renderTripleSustain(!!myquo, env = quo_get_env(my_quo)))
#' )
#'
#' @export
sustainEnvAndQuoted <- function(q, x, env, quoted, verbose = TRUE) {
  env_is_present <-
    # env != deprecated()
    is_present(env) &&
    # Check if parent frame had a missing _`env`_ param
    eval(substitute(!missing(env)), parent.frame())

  quoted_is_present <-
    # quoted != deprecated()
    is_present(quoted) &&
    # Check if parent frame had a missing _`quoted`_ param
    eval(substitute(!missing(quoted)), parent.frame())

  # This is only TRUE when the user called `inject(renderFoo(!!myquo))`
  # This is FALSE if a user called `renderFoo(myquo)` as the expression of q is `{myquo}`
  # This check works as we are looking at the `language` of the `x` variable.
  #   `inject()` alters the AST for the language of `x` and directly injects
  #   the quosure `myquo` into the AST.
  #   `inject()` is weird as it is mixing raw values and language, but it works
  x_lang <- eval(substitute(substitute(x)), parent.frame())
  x_is_injected_quosure <- is_quosure(x_lang)

  # This check can NOT be done as it requires the evaluation of `x`.
  # Even if we check if `x_lang` represents a symbol,
  #   due to lazy evaluation of params it is still like touching a bomb
  # Ex: bomb <- function(bombExpr) { renderText(expr, quoted = FALSE) };
  #   bomb(stop("boom"))
  #   # x_lang
  #   #> symbol bombExpr
  #   # is_quosure(eval(x_lang, parent.frame(2)))
  #   #> Error in eval(x_lang, parent.frame(2)) : boom
  # x_is_raw_quosure <- rlang::is_symbol(x_lang) && is_quosure(eval(x_lang, parent.frame(2)))

  if (
    env_is_present ||
    quoted_is_present
  ) {
    if (verbose) deprecatedEnvQuotedMessage()

    # Can't have x be a quosure AND use env/quoted.
    if (x_is_injected_quosure) {
      stop(
        "Can not use an `rlang::inject()`ed quosure with either the `env` or `quoted` parameters.\n",
        "Please alter your quosure object before sending it into the shiny function call and ",
        "do not supply any `env` or `quoted` parameters."
      )
    }
    # if (x_is_raw_quosure) {
    #   stop(
    #     "Can not use a quosure and supply the `env` or `quoted` parameters to the shiny function call.\n",
    #     "To use your quosure object directly, use `rlang::inject()` mixed with `!!`.\n",
    #     "Ex: `inject(renderText(!!myquo))`"
    #   )
    # }

    # In this code path, x is NOT a literal quosure object
    if (quoted_is_present && isTRUE(quoted)) {
      q <- quo_set_expr(q, eval_tidy(q))
    }
    if (env_is_present) {
      q <- quo_set_env(q, env)
    }
  }
  q
}





#' Convert an expression to a function
#'
#' @description
#' `r lifecycle::badge("superseded")` Please see [`sustainEnvAndQuoted()`] and [`quoToFunction()`]
#' for updated usage. (Shiny 1.7.0)
#'
#' Note: as of Shiny 1.7.0, it is
#' recommended to use [`quoToFunction()`] (and if necessary, [`sustainEnvAndQuoted()`]) instead of
#' `exprToFunction()` and `installExprFunction()`. See the examples for
#' information on how to migrate your code using [`sustainEnvAndQuoted()`] and [`quoToFunction()`].
#'
#' This is to be called from another function, because it will attempt to get
#' an unquoted expression from two calls back.
#'
#' For `exprToFunction()`:
#' If `expr` is a quoted expression, then this just converts it to a function.
#' If `expr` is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' If `expr` was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
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
#'
#' @param expr A quoted or unquoted expression, or a quosure.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#'
#' @examples
#' # These examples demonstrate the old method, with exprToFunction() and
#' # installExprFunction(), as well as how to replace them, with sustainEnvAndQuoted().
#'
#' # Version 1: exprToFunction()
#' # The old way of converting the expression to a function, with exprToFunction()
#' # (No quosure support.)
#' renderTripleExpr <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- exprToFunction(expr, env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Version 2: installExprFunction()
#' # The not-quite-as-old way of converting the expression to a function, with
#' # installExprFunction(). (No quosure support.)
#' renderTripleInstall <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   installExprFunction(expr, "func", env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Version 3: Replacing the old functions with sustainEnvAndQuoted() and quoToFunction()
#' # This keeps the `env` and `quoted` arguments, in case they are needed for
#' # backward compatibility
#' # (Supports quosures. Does not support `inject()`ed quosures with `env` or `quoted` params.)
#' renderTripleSustain <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a quosure, and then to a function
#'   q <- enquo0(expr)
#'   q <- sustainEnvAndQuoted(q, expr, env, quoted)
#'   func <- quoToFunction(q)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#'
#' # Version 4: quoToFunction() only
#' # This is the recommended way to only use quoToFunction() (`enquo0()`), and
#' # it discards `env` and `quoted`, for simplicity.
#' renderTripleIdeal <- function(expr) {
#'   # Convert expr to a quosure, and then to a function
#'   func <- quoToFunction(enquo0(expr))
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- renderTripleIdeal(A="text")
#'
#' \dontrun{
#' # Create an output object
#' output$tripleA <- renderTripleIdeal({
#'   values$A
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using isolate()
#' tripleA <- renderTripleIdeal({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#' @export
#' @keywords internal
exprToFunction <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }

  # expr is a quoted expression
  new_function(list(), body = expr, env = env)
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
