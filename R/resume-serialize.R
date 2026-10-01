#' @include utils.R
NULL

snapshot_deparse <- function(x) {
  paste(deparse(zap_srcref(x), width.cutoff = 500L), collapse = "\n")
}

snapshot_hash_expr <- function(x) {
  rlang::hash(snapshot_deparse(x))
}

# A hash part or hazard list computed on first use (snapshot walk, observer
# finalization, adopt) and then memoised by the node; see
# snapshot_observer_parts(). Most sessions never snapshot, so construction
# does no deparse work.
snapshot_lazy <- function(f, x) {
  force(f)
  force(x)
  structure(function() f(x), class = "snapshot_lazy")
}

snapshot_force <- function(x) {
  if (inherits(x, "snapshot_lazy")) x() else x
}

snapshot_deparse_quos <- function(qs) {
  vapply(qs, function(q) snapshot_deparse(quo_get_expr(q)), character(1))
}

# A conductor's path hash. `parts` are what a bindEvent()/bindCache() wrapper
# adds (event or key expressions, flags, the wrapped reactive's own hash),
# because the conductor it builds runs a fixed template body.
snapshot_conductor_hash <- function(body, parts = NULL) {
  if (is.null(parts)) return(snapshot_hash_expr(body))
  rlang::hash(c(list(body = snapshot_deparse(body)), lapply(parts, snapshot_force)))
}

snapshot_conductor_hash_lazy <- function(body, parts) {
  snapshot_lazy(function(src) snapshot_conductor_hash(src$body, src$parts), list(body = body, parts = parts))
}

# The hash part standing in for reactive `x`, whose function a wrapper's
# conductor calls directly. Holds x's body and parts, not its Observable, so
# x can still be collected.
snapshot_wrapped_conductor_part <- function(x) {
  impl <- attr(x, "observable", exact = TRUE)
  snapshot_conductor_hash_lazy(fn_body(impl$.origFunc), impl$.snapshot$hashParts)
}

# Sets `parts` on a wrapper's conductor and notes its full hash with the
# registry. Its template body was already noted at construction; that entry
# is inert, because every path hash recorded for this conductor includes
# `parts`. `parts` is evaluated only when the conductor is tracked.
snapshot_set_conductor_parts <- function(res, parts) {
  state <- attr(res, "observable", exact = TRUE)$.snapshot
  if (!is.null(state)) {
    state$hashParts <- parts
    state$registry$noteConductor(snapshot_conductor_hash_lazy(fn_body(attr(res, "observable", exact = TRUE)$.origFunc), parts))
  }
  invisible(res)
}

# Assignment-derived name from the call's srcref, or NULL when there is none.
snapshot_assigned_label <- function(call_srcref) {
  label <- rassignSrcrefToLabel(call_srcref, defaultLabel = NA_character_)
  if (is.na(label)) NULL else label
}

# Identity label for a source: the explicit label wins, then
# the assignment name, then a hash of the deparsed constructor call.
snapshot_source_label <- function(call_srcref, call, explicit = NULL) {
  if (!is.null(explicit)) {
    return(list(label = snapshot_scalar_label(explicit), source = "explicit"))
  }
  assigned <- snapshot_assigned_label(call_srcref)
  if (!is.null(assigned)) {
    return(list(label = assigned, source = "srcref"))
  }
  list(label = snapshot_hash_expr(call), source = "hash")
}

# An identity label is one string: a vector `label` would build a vector id.
snapshot_scalar_label <- function(label) {
  paste(as.character(label), collapse = ", ")
}

# The shape of a source's initial value. For sources whose identity is
# positional (no srcref, no label) adoption requires the snapshot's shape to
# match, so `#n` cannot hand a variable a sibling's value.
snapshot_shape <- function(kind, initial) {
  if (identical(kind, "reactiveValues")) {
    return(list(keys = sort(names(initial) %||% character(0))))
  }
  list(class = class(initial), type = typeof(initial))
}

# The body of the user's function behind a label wrapper (see
# wrapFunctionLabel(), which keeps the original in "wrappedFunc").
snapshot_user_body <- function(func) {
  wrapped <- attr(func, "wrappedFunc", exact = TRUE)
  fn_body(if (is.function(wrapped)) wrapped else func)
}

# Names assigned with `<<-` or assign("name", ...) anywhere in `expr`
# (detection only: it feeds the report's `hazard` column).
snapshot_hazards <- function(expr) {
  out <- character(0)
  walk <- function(e) {
    if (rlang::is_quosure(e)) return(walk(rlang::quo_get_expr(e)))
    if (!is.call(e)) return()
    fn <- e[[1]]
    if (is.symbol(fn)) {
      name <- as.character(fn)
      if (name == "<<-" && length(e) >= 2) {
        target <- e[[2]]
        while (is.call(target) && length(target) >= 2) target <- target[[2]]
        if (is.symbol(target)) out <<- c(out, as.character(target))
      } else if (name == "assign" && length(e) >= 2 && is.character(e[[2]])) {
        out <<- c(out, e[[2]])
      }
    }
    for (i in seq_along(e)) {
      if (i > 1 || !is.symbol(e[[i]])) walk(e[[i]])
    }
  }
  walk(expr)
  unique(out)
}

# The hash part for an output: the render function's cache hint when it has
# one (user expression, render wrapper, output function), else the wrapped
# user function's body, else the render function itself.
snapshot_render_hash_part <- function(renderFunc) {
  hint <- attr(renderFunc, "cacheHint", exact = TRUE)
  if (is.list(hint)) {
    return(paste(vapply(hint, function(h) paste(deparse(h, width.cutoff = 500L), collapse = "\n"), character(1)), collapse = "\n"))
  }
  wrapped <- attr(renderFunc, "wrappedFunc", exact = TRUE)
  if (is.function(wrapped)) return(snapshot_deparse(fn_body(wrapped)))
  snapshot_deparse(fn_body(renderFunc))
}

snapshot_render_hazards <- function(renderFunc) {
  wrapped <- attr(renderFunc, "wrappedFunc", exact = TRUE)
  if (is.function(wrapped)) snapshot_hazards(fn_body(wrapped)) else character(0)
}

# Sets an output observer's snapshot kind, label, and (lazy) hash part and
# hazards from its render function. Shared by ShinySession and
# MockShinySession. The render function's "snapshotHashParts" attribute holds
# what its bindEvent()/bindCache() wrappers contribute (event and cache-key
# expressions, flags), which its body does not show.
snapshot_define_output <- function(obs, name, renderFunc) {
  state <- obs$.snapshot
  if (is.null(state)) return(invisible(obs))
  state$kind <- "output"
  state$label <- sprintf("output$%s", name)
  state$hashParts <- c(
    list(render = snapshot_lazy(snapshot_render_hash_part, renderFunc)),
    attr(renderFunc, "snapshotHashParts", exact = TRUE)
  )
  state$hazard <- snapshot_lazy(snapshot_render_hazards, renderFunc)
  if (isTRUE(attr(renderFunc, "disableResume", exact = TRUE))) state$skip <- TRUE
  invisible(obs)
}

# Classes known to round-trip through serialize() despite tripping the walk
# (data.table carries an externalptr in its .internal.selfref attribute).
snapshot_serializable_classes <- c("data.table")

snapshot_special_env <- function(e) {
  identical(e, globalenv()) || identical(e, baseenv()) || identical(e, emptyenv()) ||
    isNamespace(e) || startsWith(environmentName(e), "package:")
}

# NULL when `x` survives serialize()/unserialize() without reference
# semantics; else the path of the first offending part.
# Environments other than the special ones are rejected because a reactive's
# closure environment would copy the server function's scope.
snapshot_unserializable_path <- function(x, path = "value", depth = 0L) {
  if (depth > 50L) return(NULL)
  if (inherits(x, snapshot_serializable_classes)) return(NULL)
  if (is.function(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode")) return(path)
  if (inherits(x, "connection")) return(path)
  if (inherits(x, "promise")) return(path)
  if (is.environment(x)) return(if (snapshot_special_env(x)) NULL else path)

  attrs <- attributes(x)
  attrs$names <- NULL
  attrs$class <- NULL
  for (nm in names(attrs)) {
    hit <- snapshot_unserializable_path(attrs[[nm]], paste0(path, "@", nm), depth + 1L)
    if (!is.null(hit)) return(hit)
  }
  if (isS4(x)) {
    for (nm in methods::slotNames(x)) {
      hit <- snapshot_unserializable_path(methods::slot(x, nm), paste0(path, "@", nm), depth + 1L)
      if (!is.null(hit)) return(hit)
    }
    return(NULL)
  }
  if (is.list(x) || is.pairlist(x)) {
    nms <- names(x)
    for (i in seq_along(x)) {
      sub <- if (!is.null(nms) && nzchar(nms[[i]])) paste0(path, "$", nms[[i]]) else paste0(path, "[[", i, "]]")
      hit <- snapshot_unserializable_path(x[[i]], sub, depth + 1L)
      if (!is.null(hit)) return(hit)
    }
  }
  NULL
}

snapshot_serialized_size <- function(x) {
  length(serialize(x, NULL, xdr = FALSE))
}
