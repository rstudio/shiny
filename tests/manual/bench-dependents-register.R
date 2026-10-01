# tests/manual/bench-dependents-register.R
#
# Measures the cost of Dependents$register() on a dense graph. Run from a
# package root; pass the path of a second checkout to compare:
#
#   Rscript tests/manual/bench-dependents-register.R            # this checkout
#   Rscript tests/manual/bench-dependents-register.R /path/to/main
#
# Prints the median wall time of one full flush over `reps` repetitions.

args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args)) args[[1]] else "."
pkgload::load_all(pkg, quiet = TRUE)

n_sources <- 200L
n_conductors <- 400L
n_observers <- 400L
fan_in <- 5L
reps <- 7L

set.seed(1)
build <- function() {
  sources <- lapply(seq_len(n_sources), function(i) reactiveVal(i, label = paste0("s", i)))
  conductors <- lapply(seq_len(n_conductors), function(i) {
    deps <- sources[sample.int(n_sources, fan_in)]
    reactive(sum(vapply(deps, function(d) d(), numeric(1))), label = paste0("c", i))
  })
  observers <- lapply(seq_len(n_observers), function(i) {
    deps <- conductors[sample.int(n_conductors, fan_in)]
    observe({ for (d in deps) d() }, label = paste0("o", i))
  })
  list(sources = sources, observers = observers)
}

g <- build()
shiny:::flushReact()
times <- vapply(seq_len(reps), function(rep) {
  for (s in g$sources) s(isolate(s()) + 1)
  system.time(shiny:::flushReact())[["elapsed"]]
}, numeric(1))
for (o in g$observers) o$destroy()

cat(sprintf("%s: median flush %.1f ms (min %.1f, max %.1f) over %d reps\n",
  normalizePath(pkg), median(times) * 1000, min(times) * 1000, max(times) * 1000, reps))
