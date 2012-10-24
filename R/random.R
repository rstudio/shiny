#' Make a random number generator repeatable
#' 
#' Given a function that generates random data, returns a wrapped version of 
#' that function that always uses the same seed when called. The seed to use can
#' be passed in explicitly if desired; otherwise, a random number is used.
#' 
#' @param rngfunc The function that is affected by the R session's seed.
#' @param seed The seed to set every time the resulting function is called.
#' @return A repeatable version of the function that was passed in.
#'   
#' @note When called, the returned function attempts to preserve the R session's
#'   current seed by snapshotting and restoring
#'   \code{\link[base]{.Random.seed}}.
#'
#' @examples
#' rnormA <- repeatable(rnorm)
#' rnormB <- repeatable(rnorm)
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(5)  # [1]  1.8285879 -0.7468041 -0.4639111 -1.6510126 -1.4686924
#' rnormB(5)  # [1] -0.7946034  0.2568374 -0.6567597  1.2451387 -0.8375699
#' 
#' @export
repeatable <- function(rngfunc, seed = runif(1, 0, .Machine$integer.max)) {
  function(...) {
    currentSeed <- .Random.seed
    on.exit(.Random.seed <- currentSeed)
    
    set.seed(seed)
    
    do.call(rngfunc, list(...))
  }
}