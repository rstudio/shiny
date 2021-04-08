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
    if (!rlang::is_quosure(x)) {
      x <- new_quosure(x, env)
    }

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
