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


# This function is to be called from functions like `reactive()`, `observe()`,
# and the various render functions. It handles the following cases:
# - The typical case where x is an unquoted expression, and `env` and `quoted`
#   are not used.
# - New-style metaprogramming cases, where rlang::inject() is used to inline a
#   quosure into the AST, as in `inject(reactive(!!x))`.
# - Old-style metaprogramming cases, where `env` and/or `quoted` are used.
#
# Much of the complexity is handling old-style metaprogramming cases. The code
# in this function is more complicated because it needs to look at unevaluated
# expressions in the _calling_ function. If this code were put directly in the
# calling function, it would look like this:
#
# if (!missing(env) || !missing(quoted)) {
#   deprecatedEnvQuotedMessage()
#   if (!quoted) x <- substitute(x)
#   x <- new_quosure(x, env)
#
# } else {
#   x <- substitute(x)
#   if (!is_quosure(x)) {
#     x <- new_quosure(x, env = parent.frame())
#   }
# }
#
# In the future, the calling functions will not need to have the `env` and
# `quoted` arguments -- `rlang::inject()` and quosures can be used instead.
# Instead of using this function, `get_quosure()`, the caller can instead use
# just the following code:
#
# x <- substitute(x)
# if (!is_quosure(x)) {
#   x <- new_quosure(x, env = parent.frame())
# }
#
get_quosure <- function(x, env, quoted) {
  if (!eval(substitute(missing(env)), parent.frame()) ||
      !eval(substitute(missing(quoted)), parent.frame()))
  {
    deprecatedEnvQuotedMessage()
    if (!quoted) {
      x <- eval(substitute(substitute(x)), parent.frame())
    }
    x <- new_quosure(x, env)

  } else {
    x <- eval(substitute(substitute(x)), parent.frame())

    # At this point, x can be a quosure if rlang::inject() is used, but the
    # typical case is that x is not a quosure.
    if (!is_quosure(x)) {
      x <- new_quosure(x, env = parent.frame(2L))
    }
  }

  x
}


#' Convert an expression to a function
#'
#' This is to be called from another function, because it will attempt to get
#' an unquoted expression from two calls back. Note: as of Shiny 1.6.0, it is
#' recommended to use [quoToFunction()] instead.
#'
#' If expr is a quoted expression, then this just converts it to a function.
#' If expr is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' If expr was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#
#' @param expr A quoted or unquoted expression, or a quosure.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#'
#' @examples
#' # Example of a new renderer, similar to renderText
#' # This is something that toolkit authors will do
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
  # Note that this block of code is essentially copied from quoToFunction. It
  # reaches two call levels back to get the expression. The reason that the code
  # is copied instead of calling quoToFunction() is because this function
  # reaches two levels back, and quoToFunction() does the same; if we called
  # quoToFunction() from here, that function would need to reach three levels
  # back.
  if (!eval(substitute(missing(env)), parent.frame()) ||
      !eval(substitute(missing(quoted)), parent.frame()))
  {
    deprecatedEnvQuotedMessage()
    if (!quoted) {
      expr <- eval(substitute(substitute(x)), parent.frame())
    }
    expr <- new_quosure(expr, env)

  } else {
    expr <- eval(substitute(substitute(expr)), parent.frame())

    # At this point, x can be a quosure if rlang::inject() is used, but the
    # typical case is that x is not a quosure.
    if (!is_quosure(expr)) {
      expr <- new_quosure(expr, env = parent.frame(2L))
    }
  }

  new_function(list(), get_expr(expr), get_env(expr))
}

#' Install an expression as a function
#'
#' Installs an expression in the given environment as a function, and registers
#' debug hooks so that breakpoints may be set in the function. Note: as of
#' Shiny 1.6.0, it is recommended to use [quoToFunction()] instead.
#'
#' This function can replace `exprToFunction` as follows: we may use
#' `func <- exprToFunction(expr)` if we do not want the debug hooks, or
#' `installExprFunction(expr, "func")` if we do. Both approaches create a
#' function named `func` in the current environment.
#'
#' @seealso Wraps [exprToFunction()]; see that method's documentation
#'   for more documentation and examples.
#'
#' @param expr A quoted or unquoted expression
#' @param name The name the function should be given
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults to
#'   the name of the calling function.
#' @param wrappedWithLabel,..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @export
installExprFunction <- function(expr, name, eval.env = parent.frame(2),
                                quoted = FALSE,
                                assign.env = parent.frame(1),
                                label = deparse(sys.call(-1)[[1]]),
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
    func <- wrapFunctionLabel(func, label, ..stacktraceon = ..stacktraceon)
  } else {
    registerDebugHook(name, assign.env, label)
  }
  assign(name, func, envir = assign.env)
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
quoToFunction <- function(q, label, ..stacktraceon = FALSE) {
  q <- as_quosure(q)
  func <- as_function(q)
  # as_function returns a function that takes `...`. We want one that takes no
  # args.
  formals(func) <- list()
  wrapFunctionLabel(func, label, ..stacktraceon = ..stacktraceon)
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
