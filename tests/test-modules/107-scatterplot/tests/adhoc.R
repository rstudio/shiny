

withr::with_environment(
  shiny::loadSupport("../"),
  {

    if (!exists("alpha_val")) {
      stop("alpha_val was not loaded using loadSupport")
    }

    # add a "broken test" to prove it's working
    if (!identical(alpha_val, 0.01)) {
      # stop("alpha_val does not equal 0.01. Currently: ", alpha_val)
    }
  }
)
