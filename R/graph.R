#' @export
writeReactLog <- function(file=stdout()) {
  cat(RJSONIO::toJSON(.graphEnv$log, pretty=TRUE), file=file)
}

.graphAppend <- function(logEntry) {
  .graphEnv$log <- c(.graphEnv$log, list(logEntry))
}

.graphDependsOn <- function(id, label) {
  .graphAppend(list(action='dep', id=id, dependsOn=label))
}

.graphDependsOnId <- function(id, dependee) {
  .graphAppend(list(action='depId', id=id, dependsOn=dependee))
}

.graphCreateContext <- function(id, label, type, prevId) {
  .graphAppend(list(
    action='ctx', id=id, label=paste(label, collapse='\n'), type=type, prevId=prevId
  ))
}

.graphEnterContext <- function(id) {
  .graphAppend(list(action='enter', id=id))
}

.graphExitContext <- function(id) {
  .graphAppend(list(action='exit', id=id))
}

.graphValueChange <- function(label) {
  .graphAppend(list(action='valueChange', id=label))
}

.graphInvalidate <- function(id) {
  .graphAppend(list(action='invalidate', id=id))
}

.graphEnv <- new.env()
.graphEnv$log <- list()
