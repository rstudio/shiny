


withr::with_environment(
  shiny::loadSupport(),
  {
    runner2_B <- 2

    if (!identical(helper1, 123)){
      stop("Missing helper1")
    }
    if (!identical(helper2, "abc")){
      stop("Missing helper2")
    }
    if (exists("runner1_A")){
      stop("runner1_A exists -- are we leaking in between test environments?")
    }
  }
)
