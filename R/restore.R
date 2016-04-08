#' @export
decodeBookmarkDataURL <- function(url) {
  values <- parseQueryString(url, nested = TRUE)
  mapply(names(values), values, SIMPLIFY = FALSE,
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

#' @export
encodeBookmarkDataURL <- function(input, values, files) {
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

decodeRestoreContext <- function(url) {
  list(
    input = decodeBookmarkDataURL(url),
    values = list()
  )
}

#' @export
restoreInput <- function(id, default) {
  ctx <- getCurrentRestoreContext()
  if (id %in% names(ctx$input)) {
    ctx$input[[id]]
  } else {
    default
  }
}

#' @export
restoreValue <- function(id, default) {
  ctx <- getCurrentRestoreContext()
  if (id %in% names(ctx$values)) {
    ctx$values[[id]]
  } else {
    default
  }
}

#' @export
bookmarkOutput <- function(id, label = NULL) {
  textId <- paste0("shiny-bookmark-", id)

  tagList(
    if (!is.null(label)) tags$label(label, `for` = textId),

    div(class="input-group shiny-bookmark-output", id = id,
      tags$input(type = "text", id = textId,
        readonly = "readonly",
        class = "form-control",
        placeholder = "Click button"
      ),
      span(class = "input-group-btn",
        tags$button(class = "btn btn-default",
          `data-clipboard-target` = paste0("#", textId),
          icon("copy", lib = "glyphicon")
        )
      )
    ),
    htmlDependency(
      "clipboardjs", "1.5.10", c(href = "shared/clipboardjs"),
      script = "clipboard.min.js"
    )
  )
}

#' @export
updateQueryString <- function(queryString, session = getDefaultReactiveDomain()) {
  session$updateQueryString(queryString)
}
