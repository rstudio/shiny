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
#' passed to [createRenderFunction()] or [markRenderFunction()].
#'
#' This function was added in Shiny 1.6.0. Previously, it was recommended to use
#' [installExprFunction()] or [exprToFunction()] in render functions, but now we
#' recommend using [quoToFunction()], because it does not require `env` and
#' `quoted` arguments -- that information is captured by quosures provided by
#' \pkg{rlang}.
#'
#' @param q A quosure.
#' @inheritParams installExprFunction
#' @seealso [createRenderFunction()] for example usage.
#'
#' @export
quoToFunction <- function(q,
                          label = sys.call(-1)[[1]],
                          ..stacktraceon = FALSE)
{
  q <- as_quosure(q)
  func <- quoToSimpleFunction(q)
  wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon)
}
updateFunctionLabel <- function(label) {
  # browser()
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
    return(fun)
  }
  # as_function returns a function that takes `...`. We need one that takes no
  # args.
  fn_fmls(fun) <- list()
  fun
}


#' Convert expressions and quosures to functions
#'
#' `handleEnvAndQuoted()` and `quoToFunction()` are meant to be used together in a
#' `render` function, to capture user expressions or quosures and convert them
#' to functions. They are meant to replace the older functions
#' [installExprFunction()] and [exprToFunction()] (although those will continue
#' to work in the future). See the examples in [installExprFunction()] for
#' information on how to migrate to `getQuosure()` and `quoToFunction()`.
#'
#' Although `getQuosure()` can take `env` and `quoted` parameters, it is
#' recommended that they not be used, except for backward compatibility.
#' The recommended usage of `getQuosure()` and `quoToFunction()` does not
#' include use of the `env` and `quoted` parameters. If it is necessary to
#' use quoted expressions and/or custom environments for evaluating, it can be
#' done with quosures and [rlang::inject()]. The examples below demonstrate how
#' to do this.
#'
#' If you are updating from [installExprFunction()] or [exprToFunction()] to
#' these functions, see the examples in the documentation for the old functions
#' for how to migrate them.
#'
#' @param x An expression or quosure.
#' @param env An environment. This is provided for backward compatibility.
#' @param quoted A boolean indicating whether or not `env` is quoted. This is
#'   provided for backward compatibility.
#'
#' @examples
#' # Example of a new renderer, similar to renderText.
#' # This is something that toolkit authors will do.
#' renderTriple <- function(expr) {
#'   # Convert expr to a quosure, and then to a function
#'   expr <- getQuosure(expr)
#'   func <- quoToFunction(expr)
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
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' \dontrun{
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
#'
#' isolate(tripleA())
#' # "text, text, text"
#'
#'
#' # If you want to use a quoted expression, use rlang:inject().
#' a <- 1
#' expr <- quote({ values$A })
#' tripleA <- rlang::inject(renderTriple(!!expr))
#' isolate(tripleA())
#' # "text, text, text"
#'
#' # Capturing an expression and an environment, using a quosure and rlang::inject():
#' e <- new.env()
#' e$vals <- reactiveValues(A="hello")
#' # Create a quosure that captures both the expression and environment.
#' myquo <- rlang::new_quosure(quote({ vals$A }), env = e)
#' tripleA <- rlang::inject(renderTriple(!!myquo))
#' isolate(tripleA())
#' # "hello, hello, hello"
#'
#'
#' @rdname quoToFunction
#' @export
sustainEnvAndQuoted <- function(q, x, env, quoted) {
  ## To avoid possible boilerplate with `deprecated()`...
  env_is_present <-
    if (!is_present(env)) {
      # If `env` is `deprecated()`, set to the parent frame of the caller
      env <- parent.frame(2)

      FALSE # env_is_present
    } else {
      # check of parent frame had a missing _`env`_ param
      eval(substitute(!missing(env)), parent.frame())
    }
  quoted_is_present <-
    if (!is_present(quoted)) {
      # If `quoted` is deprecated(), set to `FALSE`
      quoted <- FALSE

      FALSE # quoted_is_present
    } else {
      # check of parent frame had a missing _`quoted`_ param
      eval(substitute(!missing(quoted)), parent.frame())
    }
  ##
  # This is TRUE when the user called `inject(renderFoo(!!q))`
  x_is_quosure <- is_quosure(eval(substitute(substitute(x)), parent.frame()))

  sustainEnvAndQuoted_(
    q = q, env = env, quoted = quoted,
    env_is_present = env_is_present,
    quoted_is_present = quoted_is_present,
    x_is_quosure = x_is_quosure,
    verbose = TRUE
  )
}

# `sustainEnvAndQuotedInternal()` is to be called from functions like `reactive()`, `observe()`,
# and the various render functions. It handles the following cases:
# - The typical case where x is an unquoted expression, and `env` and `quoted`
#   are not used.
# - New-style metaprogramming cases, where rlang::inject() is used to inline a
#   quosure into the AST, as in `inject(reactive(!!x))`.
# - Old-style metaprogramming cases, where `env` and/or `quoted` are used.
# Same as `sustainEnvAndQuoted()`, but quiet
# Under assumption that `env = deprecated()` and `quoted = deprecated()`
sustainEnvAndQuotedInternal <- function(q, x, env, quoted) {
  ## Can leverage the fact that we know all `env` and `quoted` args are set to `deprecated()`
  env_is_present <- is_present(env) # eval(substitute(!missing(env)), parent.frame())
  quoted_is_present <- is_present(quoted) # eval(substitute(!missing(quoted)), parent.frame())
  x_is_quosure <- is_quosure(eval(substitute(substitute(x)), parent.frame()))
  if (!env_is_present) env <- parent.frame(2)
  if (!quoted_is_present) quoted <- FALSE

  sustainEnvAndQuoted_(
    q = q,
    env = env,
    quoted = quoted,
    env_is_present = env_is_present,
    quoted_is_present = quoted_is_present,
    x_is_quosure = x_is_quosure,
    verbose = FALSE
  )
}
# # Reaches up three calls to check if env / quoted were provided.
# # Can not leverage `is_present()` logic like in `sustainEnvAndQuotedInternal()`

# # This is similar to `sustainEnvAndQuotedInternal()`, but it is intended to be used only by
# # `installExprFunction()` and `exprToFunction()`. Whereas `sustainEnvAndQuotedInternal()` reaches
# # 2 calls back to find the expression passed in, this function reaches 3 calls
# # back, and it is only used internally within Shiny.
# sustainEnvAndQuoted3Internal <- function(q, x, env, quoted) {
#   ## To avoid possible boilerplate with `deprecated()`...
#   env_is_present <-
#     if (!is_present(env)) {
#       # If `env` is `deprecated()`, set to the parent frame of the caller
#       env <- parent.frame(3)

#       FALSE # env_is_present
#     } else {
#       # check of parent frame of the caller had a missing _`env`_ param
#       eval(eval(substitute(substitute(!missing(env))), parent.frame()), parent.frame(2))
#     }
#   quoted_is_present <-
#     if (!is_present(quoted)) {
#       # If `quoted` is deprecated(), set to `FALSE`
#       quoted <- FALSE

#       FALSE # quoted_is_present
#     } else {
#       # check of parent frame had a missing _`quoted`_ param
#       eval(eval(substitute(substitute(!missing(quoted))), parent.frame()), parent.frame(2))
#     }
#   ##
#   x_is_quosure <- is_quosure(eval(eval(substitute(substitute(x)), parent.frame()), parent.frame(2)))

#   sustainEnvAndQuoted_(
#     q = q, env = env, quoted = quoted,
#     env_is_present = env_is_present,
#     quoted_is_present = quoted_is_present,
#     x_is_quosure = x_is_quosure,
#     verbose = FALSE
#   )
# }
sustainEnvAndQuoted_ <- function(
  q, env, quoted,
  env_is_present, quoted_is_present, x_is_quosure,
  verbose = TRUE
) {
  if (
    env_is_present ||
    quoted_is_present
  ) {
    if (verbose) deprecatedEnvQuotedMessage()
    # browser()

    # Can't have x be a quosure and use env/quoted.
    if (x_is_quosure) {
      stop(
        "Can not use a `quosure()` with either the `env` or `quoted` parameters.\n",
        "Please alter your quosure before the shiny function call and ",
        "do not supply any `env` or `quoted` parameters.\n",
        "To use your `quosure()` object directly, use `inject()` mixed with `!!`.\n",
        "Ex: `inject(renderText(!!q))`"
      )
    }

    # In this code path, x is NOT a literal quosure object
    if (isTRUE(quoted)) {
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
#' `r lifecycle::badge("superseded")` Please see [`quoToFunction()`] for updated usage. (Shiny 1.7.0)
#'
#' Note: as of Shiny 1.7.0, it is
#' recommended to use [`quoToFunction()`] (and if necessary, [`sustainEnvAndQuoted()`]) instead of
#' `exprToFunction()` and `installExprFunction()`. See the examples for
#' information on how to migrate to `getQuosure()` and `quoToFunction()`.
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
#' # installExprFunction(), as well as how to replace them, with getQuosure().
#'
#' # Version 1: exprToFunction()
#' # The old way of converting the expression to a quosure, with exprToFunction()
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- shiny::exprToFunction(expr, env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Version 2: installExprFunction()
#' # The not-quite-as-old way of converting the expression to a quosure, with
#' # installExprFunction()
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   installExprFunction(expr, "func", env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Version 3: Replacing the old functions with getQuosure() and quoToFunction()
#' # This keeps the `env` and `quoted` arguments, in case they are needed for
#' # backward compatibility
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a quosure, and then to a function
#'   q <- getQuosure(expr, env, quoted)
#'   func <- quoToFunction(q)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#'
#' # Version 4: getQuosure()
#' # This is the recommended way to use getQuosure() and quoToFunction(), and
#' # it discards `env` and `quoted`, for simplicity.
#' renderTriple <- function(expr) {
#'   # Convert expr to a quosure, and then to a function
#'   q <- getQuosure(expr)
#'   func <- quoToFunction(q)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' \dontrun{
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
#'
#' isolate(tripleA())
#' # "text, text, text"
#' @export
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
