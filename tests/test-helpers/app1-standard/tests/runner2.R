
b <- 2


if (!identical(helper1, "abc")){
  stop("Missing helper1")
}
if (!identical(helper2, 123)){
  stop("Missing helper2")
}
if (exists("a")){
  stop("a exists -- are we leaking in between test environments?")
}
