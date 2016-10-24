#' Register expressions that will be evaluated when a test snapshot occurs
#'
#' This function registers expressions that will be called when a test snapshot
#' occurs. These test snapshots are available at a URL unique to this
#' application.
#'
#' The format of the response can be specified in a POST request to the data
#' URL. The POST body can be a JSON object that specifies the format. For
#' example if the body is \code{\{"format": "json"\}}, then the values will be
#' encoded as JSON. If the body is \code{\{"format": "rds"\}}, then the values
#' will be encoded as RDS. In practice, RDS should be used for programmatic
#' work; JSON can be used for human-readable output for debugging (and is the
#' default).
#'
#' @export
onTestSnapshot <- function(..., quoted_ = FALSE, envir_ = parent.frame(),
  session = getDefaultReactiveDomain())
{
  # Get a named list of unevaluated expressions.
  if (quoted_) {
    dots <- list(...)
  } else {
    dots <- eval(substitute(alist(...)))
  }

  if (anyUnnamed(dots))
    stop("onTestSnapshot: all arguments must be named.")

  force(envir_)

  session$registerDataObj("shinyTestSnapshot", dots, function(data, req) {
    # The format of the response that will be sent back. Default to JSON unless
    # requested otherwise. The only other valid value is "rds".
    format <- "json"

    reqBody <- rawToChar(req$rook.input$read())

    if (nzchar(reqBody)) {
      format <- jsonlite::fromJSON(reqBody)$format
    }

    values <- isolate(lapply(data, eval, envir = envir_))

    if (identical(format, "json")) {
      content <- toJSON(values)
      httpResponse(200, "application/json", content)

    } else if (identical(format, "rds")) {
      tmpfile <- tempfile("shinyTestSnapshot", fileext = ".rds")
      saveRDS(values, tmpfile)
      on.exit(unlink(tmpfile))

      content <- readBin(tmpfile, "raw", n = file.info(tmpfile)$size)
      httpResponse(200, "application/octet-stream", content)

    } else {
      httpResponse(400, "text/plain", paste("Invalid format requested:", format))
    }
  })
}
