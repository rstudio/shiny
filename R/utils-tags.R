# Check if `x` is a tag(), tagList(), or HTML()
# @param strict If `FALSE`, also consider a normal list() of 'tags' to be a tag list.
isTagLike <- function(x, strict = FALSE) {
  isTag(x) || isTagList(x, strict = strict) || isTRUE(attr(x, "html"))
}

isTag <- function(x) {
  inherits(x, "shiny.tag")
}

isTagList <- function(x, strict = TRUE) {
  if (strict) {
    return(inherits(x, "shiny.tag.list"))
  }

  if (!is.list(x)) {
    return(FALSE)
  }

  all(vapply(x, isTagLike, logical(1)))
}
