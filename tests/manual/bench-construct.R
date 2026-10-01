# tests/manual/bench-construct.R
#
# Measures the construction cost of reactiveVal(), reactive() and observe()
# inside a session. Same usage as bench-dependents-register.R.

args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) args[[1]] else "."
pkgload::load_all(pkg, quiet = TRUE)

n <- 2000L
reps <- 7L

times <- vapply(seq_len(reps), function(rep) {
  s <- MockShinySession$new()
  system.time(withReactiveDomain(s, isolate({
    vals <- lapply(seq_len(n), function(i) reactiveVal(i))
    conds <- lapply(seq_len(n), function(i) reactive(vals[[i]]() + 1))
    obs <- lapply(seq_len(n), function(i) observe({ conds[[i]]() }))
  })))[["elapsed"]]
}, numeric(1))

cat(sprintf("%s: median construct %.1f ms for %d of each node kind over %d reps\n",
  normalizePath(pkg), median(times) * 1000, n, reps))
