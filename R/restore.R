readBookmarkDataURL <- function(url) {
  values <- parseQueryString(url, nested = TRUE)
  mapply(names(values), values,
    FUN = function(name, value) {
      tryCatch(
        jsonlite::fromJSON(value),
        error = function(e) {
          stop("Failed to parse URL parameter \"", name, "\"")
        }
      )
    }
  )
}

saveBookmarkDataURL <- function(input, values, files) {
  vals <- vapply(reactiveValuesToList(input), function(x) {
    toJSON(x, strict_atomic = FALSE)
  }, character(1), USE.NAMES = TRUE)
  paste0(
    encodeURIComponent(names(vals)),
    "=",
    encodeURIComponent(vals),
    collapse = "&"
  )
}


restoreCtxStack <- Stack$new()

withRestoreContext <- function(ctx, expr) {
  restoreCtxStack$push(ctx)
  on.exit(restoreCtxStack$pop(), add = TRUE)

  force(expr)
}

# Call to access the current restore context
getCurrentRestoreContext <- function() {
  ctx <- restoreCtxStack$peek()
  if (is.null(ctx)) {
    stop("No restore context found")
  }
  ctx
}

extractRestoreContext <- function(url) {
  list(
    input = readBookmarkDataURL(url),
    values = list()
  )
}

#' @export
restoreInput <- function(id, defaultValue) {
  ctx <- getCurrentRestoreContext()
  if (id %in% names(ctx$input)) {
    ctx$input[[id]]
  } else {
    defaultValue
  }
}

#' @export
restoreValue <- function(id, defaultValue) {
  ctx <- getCurrentRestoreContext()
  if (id %in% names(ctx$values)) {
    ctx$values[[id]]
  } else {
    defaultValue
  }
}
