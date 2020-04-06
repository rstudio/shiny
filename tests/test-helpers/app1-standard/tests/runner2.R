
b <- 2

if (!identical(helper1, 123)){
  stop("Missing helper1")
}
if (!identical(helper2, "abc")){
  stop("Missing helper2")
}
if (exists("A")){
  stop("a exists -- are we leaking in between test environments?")
}
