# Given a numeric vector, convert to strings, sort, and convert back to
# numeric.
lexical_sort <- function(x) {
  as.numeric(sort(as.character(x)))
}
