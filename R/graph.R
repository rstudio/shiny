#' @export
writeReactLog <- function(file=stdout()) {
  cat(RJSONIO::toJSON(.graphEnv$log, pretty=TRUE), file=file)
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

.graphValueChange <- function(label) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='valueChange', id=label))
}

.graphInvalidate <- function(id) {
  if (isTRUE(getOption('shiny.reactlog', FALSE)))
    .graphAppend(list(action='invalidate', id=id))
}

.graphEnv <- new.env()
.graphEnv$log <- list()
