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


#' @describeIn createRenderFunction convert a quosure to a function.
#' @param q Quosure of the expression `x`. When capturing expressions to create
#'   your quosure, it is recommended to use [`rlang::enquo0()`] to not unquote
#'   the object too early. See [`rlang::enquo0()`] for more details.
#' @inheritParams installExprFunction
#' @export
quoToFunction <- function(
  q,
  label = sys.call(-1)[[1]],
  ..stacktraceon = FALSE
) {
  func <- quoToSimpleFunction(as_quosure(q))
  wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
}

updateFunctionLabel <- function(label) {
  badFnName <- "anonymous"
  if (all(is.language(label))) {
    # Prevent immediately invoked functions like as.language(a()())
    if (is.language(label) && length(label) > 1) {
      return(badFnName)
    }
    label <- deparse(label, width.cutoff = 500L)
  }
  label <- as.character(label)
  # Prevent function calls that are over one line; (Assignments are hard to perform)
    # Prevent immediately invoked functions like "a()()"
  if (length(label) > 1 || grepl("(", label, fixed = TRUE)) {
    return(badFnName)
  }
  if (label == "NULL") {
    return(badFnName)
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
#' `r lifecycle::badge("superseded")` Please use [`installExprFunction()`] for a better
#' debugging experience (Shiny 0.8.0). If the `expr` and `quoted` parameters are not needed, please see
#' [`quoToFunction()`] (Shiny 1.6.0).
#'
#' Similar to [installExprFunction()] but doesn't register debug hooks.
#'
#' @param expr A quoted or unquoted expression, or a quosure.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @seealso [`installExprFunction()`] for the modern approach to converting an expression to a function
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
  # MUST call with `quoted = TRUE` as exprToQuo() will not reach high enough
  q <- exprToQuo(expr, env, quoted = TRUE)

  # MUST call `as_function()`. Can NOT call `new_function()`
  # rlang has custom logic for handling converting a quosure to a function
  quoToSimpleFunction(q)
}
# For internal use only; External users should be using `exprToFunction()` or `installExprFunction()`
# MUST be the exact same logic as `exprToFunction()`, but without the `quoToSimpleFunction()` call
exprToQuo <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }
  q <-
    if (is_quosure(expr)) {
      # inject()ed quosure
      # do nothing
      expr
    } else if (is.language(expr) || rlang::is_atomic(expr) || is.null(expr)) {
      # Most common case...
      new_quosure(expr, env = env)
    } else {
      stop("Don't know how to convert '", class(expr)[1], "' to a function; a quosure or quoted expression was expected")
    }
  q
}

#' @describeIn createRenderFunction converts a user's reactive `expr` into a
#'   function that's assigned to a `name` in the `assign.env`.
#'
#' @param name The name the function should be given
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults to
#'   the name of the calling function.
#' @param wrappedWithLabel,..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @inheritParams exprToFunction
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
  wrappedWithLabel <- isTRUE(wrappedWithLabel)
  if (wrappedWithLabel) {
    func <- wrapFunctionLabel(func, updateFunctionLabel(label), ..stacktraceon = ..stacktraceon, dots = FALSE)
  }
  assign(name, func, envir = assign.env)
  if (!wrappedWithLabel) {
    registerDebugHook(name, assign.env, label)
  }

  invisible(func)
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
      simpleExprToFunction(expr, function_name),
      function_name
    )
    label <- as_default_label(label)
  }
  if (length(srcref) >= 2) attr(label, "srcref") <- srcref[[2]]
  attr(label, "srcfile") <- srcFileOfRef(srcref[[1]])
  label
}
simpleExprToFunction <- function(expr, function_name) {
  sprintf('%s(%s)', function_name, paste(deparse(expr), collapse='\n'))
}

installedFuncExpr <- function(func) {
  fn_body(attr(func, "wrappedFunc", exact = TRUE))
}

funcToLabelBody <- function(func) {
  paste(deparse(installedFuncExpr(func)), collapse='\n')
}
funcToLabel <- function(func, functionLabel, label = NULL) {
  if (!is.null(label)) return(label)

  as_default_label(
    sprintf(
      '%s(%s)',
      functionLabel,
      funcToLabelBody(func)
    )
  )
}
quoToLabelBody <- function(q) {
  paste(deparse(quo_get_expr(q)), collapse='\n')
}
quoToLabel <- function(q, functionLabel, label = NULL) {
  if (!is.null(label)) return(label)

  as_default_label(
    sprintf(
      '%s(%s)',
      functionLabel,
      quoToLabelBody(q)
    )
  )
}

as_default_label <- function(x) {
  class(x) <- c("default_label", class(x))
  x
}
is_default_label <- function(x) {
  inherits(x, "default_label")
}
