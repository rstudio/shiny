# Measures registry$snapshot() on the dense graph of
# bench-dependents-register.R, inside a MockShinySession.

pkgload::load_all(".", quiet = TRUE)

n_sources <- 200L; n_conductors <- 400L; n_observers <- 400L; fan_in <- 5L; reps <- 7L
set.seed(1)
s <- MockShinySession$new()
withReactiveDomain(s, isolate({
  sources <- lapply(seq_len(n_sources), function(i) reactiveVal(rnorm(100), label = paste0("s", i)))
  conductors <- lapply(seq_len(n_conductors), function(i) {
    deps <- sources[sample.int(n_sources, fan_in)]
    reactive(sum(vapply(deps, function(d) sum(d()), numeric(1))), label = paste0("c", i))
  })
  observers <- lapply(seq_len(n_observers), function(i) {
    deps <- conductors[sample.int(n_conductors, fan_in)]
    observe({ for (d in deps) d() }, label = paste0("o", i))
  })
}))
s$flushReact()
times <- vapply(seq_len(reps), function(rep) system.time(s$.snapshotRegistry$snapshot())[["elapsed"]], numeric(1))
cat(sprintf("median snapshot %.1f ms over %d reps (%d sources, %d conductors, %d observers)\n",
  median(times) * 1000, reps, n_sources, n_conductors, n_observers))
