
# Return the ... arguments of the caller, as a list of quosures. If any are
# quosures inlined with inject(), don't change them.
enquos0 <- function(...) {
  dots <- getNamespace('rlang')$captureDots()
  lapply(dots, function(dot) as_quosure(dot$expr, dot$env))
}

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

# Get the formals and body for a function, without source refs. This is used for
# consistent hashing of the function.
formalsAndBody <- function(x) {
  if (is.null(x)) {
    return(list())
  }

  list(
    formals = formals(x),
    body = body(remove_source(x))
  )
}

# Remove source refs from a function or language object. utils::removeSource()
# does the same, but only gained support for language objects in R 3.6.0.
remove_source <- function(x) {
  if (is.function(x)) {
    body(x) <- remove_source(body(x))
    x
  } else if (is.call(x)) {
    attr(x, "srcref") <- NULL
    attr(x, "wholeSrcref") <- NULL
    attr(x, "srcfile") <- NULL

    # `function` calls store the source ref as the fourth element.
    # See https://github.com/r-lib/testthat/issues/1228
    if (x[[1]] == quote(`function`) && length(x) == 4 &&
        inherits(x[[4]], "srcref")) {
      x[[4]] <- NULL
    }

    x[] <- lapply(x, remove_source)
    x
  } else {
    x
  }
}

# Need this here until it is part of rlang.
inject <- function(expr, env = parent.frame()) {
  rlang::eval_bare(rlang::enexpr(expr), env)
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
