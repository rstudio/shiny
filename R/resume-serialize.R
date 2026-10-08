#' @include utils.R
NULL

snapshot_deparse <- function(x) {
  paste(deparse(zap_srcref(x), width.cutoff = 500L), collapse = "\n")
}

snapshot_hash_expr <- function(x) {
  rlang::hash(snapshot_deparse(x))
}

# Assignment-derived name from the call's srcref, or NULL when there is none.
snapshot_assigned_label <- function(call_srcref) {
  label <- rassignSrcrefToLabel(call_srcref, defaultLabel = NA_character_)
  if (is.na(label)) NULL else label
}

# Identity label for a source: the explicit label wins, then the assignment
# name from the srcref; without a srcref (installed packages), the registry
# resolves `source = "body"` from the call site (snapshot_body_label()).
snapshot_source_label <- function(call_srcref, explicit = NULL, site = NULL) {
  if (isTRUE(.globals$snapshotInternal)) return(list(label = NULL, source = "internal"))
  if (!is.null(explicit)) {
    return(list(label = paste(as.character(explicit), collapse = ", "), source = "explicit"))
  }
  assigned <- snapshot_assigned_label(call_srcref)
  if (!is.null(assigned)) return(list(label = assigned, source = "srcref"))
  # A srcref that yields no assignment (an inline `if (x) a <- ...`) falls
  # through to the body walk, exactly like code without srcrefs.
  if (!is.null(site)) {
    return(list(label = NULL, source = "body", site = site, call = snapshot_deparse(site$call)))
  }
  list(label = NULL, source = "none")
}

# Where a constructor was called: its own call, and the function and frame
# that called it. `frame` is the constructor's sys.parent(); 0 is top level.
snapshot_call_site <- function(call, frame) {
  if (frame == 0L) return(NULL)
  list(call = call, fn = sys.function(frame), env = sys.frame(frame))
}

# The label from the assignment in the caller's body whose right-hand side is
# this very call: the only one, or, when several are identical top-level
# statements, the one this construction's position picks. NULL otherwise.
snapshot_body_label <- function(site, ordinal) {
  call <- site$call
  attributes(call) <- NULL
  hits <- Filter(function(s) identical(s$call, call), snapshot_body_sites(site$fn))
  if (length(hits) == 1L) return(hits[[1]]$name)
  if (length(hits) < 2L || !all(vapply(hits, `[[`, logical(1), "toplevel"))) return(NULL)
  k <- ordinal(site$env, call)
  if (k > length(hits)) return(NULL)
  hits[[k]]$name
}

# Assignments `name <- <call>` / `name = <call>` in `fn`'s body, in order.
# `toplevel` is TRUE when only `{` encloses the assignment. One walk per body.
snapshot_body_sites <- function(fn) {
  body <- body(fn)
  if (is.null(.globals$snapshotBodySites)) .globals$snapshotBodySites <- fastmap::fastmap()
  key <- rlang::hash(body)
  cached <- .globals$snapshotBodySites$get(key)
  if (!is.null(cached)) return(cached)
  sites <- snapshot_collect_sites(body, toplevel = TRUE)
  .globals$snapshotBodySites$set(key, sites)
  sites
}

snapshot_collect_sites <- function(expr, toplevel) {
  if (!is.call(expr)) return(list())
  head <- expr[[1]]
  if ((identical(head, quote(`<-`)) || identical(head, quote(`=`))) && length(expr) == 3L) {
    out <- list()
    if (is.symbol(expr[[2]]) && is.call(expr[[3]])) {
      rhs <- expr[[3]]
      attributes(rhs) <- NULL
      out <- list(list(name = as.character(expr[[2]]), call = rhs, toplevel = toplevel))
    }
    return(c(out, snapshot_collect_sites(expr[[3]], FALSE)))
  }
  inner <- toplevel && identical(head, quote(`{`))
  out <- list()
  for (i in seq_along(expr)[-1]) out <- c(out, snapshot_collect_sites(expr[[i]], inner))
  out
}

# The body of the user's function behind any number of label wrappers (see
# wrapFunctionLabel(), which keeps the original in "wrappedFunc").
snapshot_user_body <- function(func) {
  # Wrappers nest (observe() wraps the user's code, observeEvent() wraps that).
  repeat {
    wrapped <- attr(func, "wrappedFunc", exact = TRUE)
    if (!is.function(wrapped)) break
    func <- wrapped
  }
  fn_body(func)
}

# Names assigned with `<<-` or assign("name", ...) anywhere in `expr`.
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

# Classes known to round-trip through serialize() despite tripping the walk
# (data.table carries an externalptr in its .internal.selfref attribute).
snapshot_serializable_classes <- c("data.table")

snapshot_special_env <- function(e) {
  identical(e, globalenv()) || identical(e, baseenv()) || identical(e, emptyenv()) ||
    isNamespace(e) || startsWith(environmentName(e), "package:")
}

snapshot_walk_max_depth <- 50L
snapshot_walk_max_elements <- 100000L
snapshot_too_large <- "too large to check"

# NULL when `x` survives serialize()/unserialize() without reference
# semantics; else the path of the first offending part, or "too large to
# check" when the walk hit its depth or element limit: it fails
# closed. Atomic vectors recurse only into their attributes, so a data
# frame of atomic columns costs one step per column.
snapshot_unserializable_path <- function(x, path = "value") {
  budget <- new.env(parent = emptyenv())
  budget$left <- snapshot_walk_max_elements
  snapshot_walk(x, path, 0L, budget)
}

snapshot_walk <- function(x, path, depth, budget) {
  if (depth > snapshot_walk_max_depth) return(snapshot_too_large)
  budget$left <- budget$left - 1L
  if (budget$left < 0L) return(snapshot_too_large)
  if (is.null(x)) return(NULL)
  if (inherits(x, snapshot_serializable_classes)) return(NULL)
  if (is.function(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode")) return(path)
  if (inherits(x, "connection")) return(path)
  if (inherits(x, c("promise", "reactivevalues"))) return(path)
  if (is.environment(x)) return(if (snapshot_special_env(x)) NULL else path)

  attrs <- attributes(x)
  attrs$names <- NULL
  attrs$class <- NULL
  attrs$row.names <- NULL
  for (nm in names(attrs)) {
    hit <- snapshot_walk(attrs[[nm]], paste0(path, "@", nm), depth + 1L, budget)
    if (!is.null(hit)) return(hit)
  }
  if (isS4(x)) {
    for (nm in methods::slotNames(x)) {
      hit <- snapshot_walk(methods::slot(x, nm), paste0(path, "@", nm), depth + 1L, budget)
      if (!is.null(hit)) return(hit)
    }
    return(NULL)
  }
  if (is.atomic(x)) return(NULL)
  if (is.list(x) || is.pairlist(x)) {
    # Classed lists (POSIXlt, numeric_version, reactivevalues) define names(),
    # length() and [[ methods that recurse, throw, or need a reactive context.
    x <- unclass(x)
    nms <- names(x)
    for (i in seq_along(x)) {
      sub <- if (!is.null(nms) && nzchar(nms[[i]])) paste0(path, "$", nms[[i]]) else paste0(path, "[[", i, "]]")
      hit <- snapshot_walk(.subset2(x, i), sub, depth + 1L, budget)
      if (!is.null(hit)) return(hit)
    }
  }
  NULL
}

snapshot_serialized_size <- function(x) {
  length(serialize(x, NULL, xdr = FALSE))
}
