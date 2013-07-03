#' @export
writeReactLog <- function(file=stdout()) {
  cat(RJSONIO::toJSON(.graphEnv$log, pretty=TRUE), file=file)
}

#' @export
showReactLog <- function() {
  browseURL(renderReactLog())
}

renderReactLog <- function() {
  templateFile <- system.file('www/reactive-graph.html', package='shiny')
  html <- paste(readLines(templateFile, warn=FALSE), collapse='\r\n')
  tc <- textConnection(NULL, 'w')
  on.exit(close(tc))
  writeReactLog(tc)
  cat('\n', file=tc)
  flush(tc)
  html <- sub('__DATA__', paste(textConnectionValue(tc), collapse='\r\n'), html, fixed=TRUE)
  file <- tempfile(fileext = '.html')
  writeLines(html, file)
  return(file)
}

.graphAppend <- function(logEntry) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphEnv$log <- c(.graphEnv$log, list(logEntry))
}

.graphDependsOn <- function(id, label) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='dep', id=id, dependsOn=label))
}

.graphDependsOnId <- function(id, dependee) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='depId', id=id, dependsOn=dependee))
}

.graphCreateContext <- function(id, label, type, prevId) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(
      action='ctx', id=id, label=paste(label, collapse='\n'), type=type, prevId=prevId
    ))
}

.graphEnterContext <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='enter', id=id))
}

.graphExitContext <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='exit', id=id))
}

.graphValueChange <- function(label, value) {
  if (isTRUE(getOption('shiny.reactlog', FALSE))) {
    .graphAppend(list(
      action = 'valueChange',
      id = label,
      value = paste(capture.output(str(value)), collapse='\n')
    ))
  }
}

.graphInvalidate <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='invalidate', id=id))
}

.graphEnv <- new.env()
.graphEnv$log <- list()
