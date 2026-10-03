# Construction cost of reactiveVal(), reactive() and observe() inside a
# session, with resume off and on. Run from the repo root:
#   Rscript tests/manual/bench-construct.R
#   SHINY_RESUME=TRUE Rscript tests/manual/bench-construct.R
# Expect near parity (spec 13): the registry records identities only.
pkgload::load_all(".", quiet = TRUE)

n <- 2000L
reps <- 7L
times <- vapply(seq_len(reps), function(rep) {
  s <- MockShinySession$new()
  system.time(withReactiveDomain(s, isolate({
    vals <- lapply(seq_len(n), function(i) reactiveVal(i, label = paste0("v", i)))
    conds <- lapply(seq_len(n), function(i) reactive(vals[[i]]() + 1))
    obs <- lapply(seq_len(n), function(i) observe({ conds[[i]]() }))
  })))[["elapsed"]]
}, numeric(1))
cat(sprintf("resume %s: median construct %.1f ms for %d of each node kind over %d reps\n",
  if (isTRUE(resume_settings()$enabled)) "on" else "off", median(times) * 1000, n, reps))

# The record walk over a thousand small sources.
s <- MockShinySession$new()
if (!is.null(s$.snapshotRegistry)) {
  withReactiveDomain(s, isolate(for (i in 1:1000) assign(paste0("x", i), reactiveVal(rnorm(10), label = paste0("x", i)))))
  cat(sprintf("walk: %.1f ms\n", system.time(s$.snapshotRegistry$snapshot())[["elapsed"]] * 1000))
}
