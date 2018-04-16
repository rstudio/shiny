# TODO - remove dot syntax

# used to help define truely global node id's.
# should work accross session and in global namespace
.globals$reactIdCounter <- 0L
nextGlobalReactId <- function() {
  .globals$reactIdCounter <- .globals$reactIdCounter + 1L
  as.character(.globals$reactIdCounter)
}



writeReactLog <- function(file=stdout(), sessionToken = NULL) {
  log <- .rlogStack$as_list()
  if (!is.null(sessionToken)) {
    log <- Filter(function(x) {
      is.null(x$session) || identical(x$session, sessionToken)
    }, log)
  }
  cat(toJSON(log, pretty=TRUE), file=file)
}

#' Reactive Log Visualizer
#'
#' Provides an interactive browser-based tool for visualizing reactive
#' dependencies and execution in your application.
#'
#' To use the reactive log visualizer, start with a fresh R session and
#' run the command \code{options(shiny.reactlog=TRUE)}; then launch your
#' application in the usual way (e.g. using \code{\link{runApp}}). At
#' any time you can hit Ctrl+F3 (or for Mac users, Command+F3) in your
#' web browser to launch the reactive log visualization.
#'
#' The reactive log visualization only includes reactive activity up
#' until the time the report was loaded. If you want to see more recent
#' activity, refresh the browser.
#'
#' Note that Shiny does not distinguish between reactive dependencies
#' that "belong" to one Shiny user session versus another, so the
#' visualization will include all reactive activity that has taken place
#' in the process, not just for a particular application or session.
#'
#' As an alternative to pressing Ctrl/Command+F3--for example, if you
#' are using reactives outside of the context of a Shiny
#' application--you can run the \code{showReactLog} function, which will
#' generate the reactive log visualization as a static HTML file and
#' launch it in your default browser. In this case, refreshing your
#' browser will not load new activity into the report; you will need to
#' call \code{showReactLog()} explicitly.
#'
#' For security and performance reasons, do not enable
#' \code{shiny.reactlog} in production environments. When the option is
#' enabled, it's possible for any user of your app to see at least some
#' of the source code of your reactive expressions and observers.
#'
#' @param time A boolean that specifies whether or not to display the
#' time that each reactive.
#' @export
showReactLog <- function(time = TRUE) {
  utils::browseURL(renderReactLog(time = as.logical(time)))
}

renderReactLog <- function(sessionToken = NULL, time = TRUE) {
  templateFile <- system.file('www/reactive-graph.html', package='shiny')
  html <- paste(readLines(templateFile, warn=FALSE), collapse='\r\n')
  tc <- textConnection(NULL, 'w')
  on.exit(close(tc))
  writeReactLog(tc, sessionToken)
  cat('\n', file=tc)
  flush(tc)
  html <- sub('__DATA__', paste(textConnectionValue(tc), collapse='\r\n'), html, fixed=TRUE)
  html <- sub('__TIME__', paste0('"', time, '"'), html, fixed=TRUE)
  file <- tempfile(fileext = '.html')
  writeLines(html, file)
  return(file)
}

.rlogAppend <- function(logEntry, domain = getDefaultReactiveDomain()) {
  if (isTRUE(getOption('shiny.reactlog'))) {
    sessionToken <- if (is.null(domain)) NULL else domain$token
    .rlogStack$push(c(logEntry, list(
      session = sessionToken,
      time = as.numeric(Sys.time())
    )))
  }

  if (!is.null(domain)) {
    domain$reactlog(logEntry)
  }
}


.pmDepth <- 0
.pmDepthIncrement <- function() .pmDepth <<- .pmDepth + 2
.pmDepthDecrement <- function() .pmDepth <<- .pmDepth - 2
displayRLog <- function() {
  for (msg in .pmMessages$as_list()) {
    message(msg)
  }
}
.pmMessages <- Stack$new()
.pm <- function(...) {
  msg <- paste0(paste0(rep(" ", .pmDepth), collapse = ""), " - ", paste0(..., collapse = ""), collapse = "")
  .pmMessages$push(msg)
  if (TRUE) {
    message(msg)
  }
}
.rlogDependsOnReactiveValueKey <- function(reactId, depOnReactId, key) {
  .pm("dependsOnReactiveValueKey: ", pn(reactId), " ", pn(depOnReactId), " ", key)
  .rlogAppend(list(
    action = "depReactiveValueKey",
    reactId = reactId,
    depOnReactId = depOnReactId,
    key = key
  ))
}
.rlogDependsOnReactiveValueNames <- function(reactId, depOnReactId) {
  .pm("dependsOnReactiveValueNames: ", pn(reactId), " ", pn(depOnReactId))
  .rlogAppend(list(
    action = "depReactiveValueNames",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}
.rlogDependsOnReactiveValueToList <- function(reactId, depOnReactId) {
  .pm("dependsOnReactiveValueToList: ", pn(reactId), " ", pn(depOnReactId))
  .rlogAppend(list(
    action = "dependsOnReactiveValuetoList",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}

.rlogDependsOn <- function(reactId, depOnReactId) {
  .pm("dependsOn: ", pn(reactId), " on ", pn(depOnReactId))
  .rlogAppend(list(
    action = "dep",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}
.rlogDependsOnRemove <- function(reactId, depOnReactId) {
  .pm("dependsOnRemove: ", pn(reactId), " on ", pn(depOnReactId))
  .rlogAppend(list(
    action = "depOnRemove",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}

nodeCache <- list()
pn <- function(reactId) {
  nodeInfo <- nodeCache[[reactId]]
  paste(
    nodeInfo$reactId, nodeInfo$type, nodeInfo$label,
    sep = ":"
  )
}
# init a node id with a label
.rlogAddNodeDef <- function(reactId, label, type) {
  if (!is.null(nodeCache[[reactId]])) {
    stop("node definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
  }
  nodeCache[[reactId]] <<- list(reactId = reactId, label = label, type = type)
  .pm("nodeDef: ", pn(reactId))
  .rlogAppend(list(
    action = "nodeDef",
    reactId = reactId,
    label = label,
    type = type
  ))
}
.rlogUpdateNodeLabel <- function(reactId, label) {
  nodeCache[[reactId]]$label <<- label
  .pm("updateNodeLabel: ", pn(reactId))
  .rlogAppend(list(
    action = "updateNodeLabel",
    reactId = reactId,
    label = label
  ))
}

# .rlogCreateContext <- function(id, label, type, prevId, domain) {
#   message("!!createContext: create graph context is deprecated!!")
#   .rlogAppend(list(
#     action='ctx', ctxId = id, label = paste(label, collapse='\n'),
#     srcref=as.vector(attr(label, "srcref")), srcfile=attr(label, "srcfile"),
#     type=type, prevId=prevId
#   ), domain = domain)
# }

.rlogEnter <- function(reactId, ctxId, type) {
  .pm("enter: ", pn(reactId), " ", ctxId, " ", type)
  .pmDepthIncrement()
  .rlogAppend(list(
    action = 'enter',
    reactId = reactId,
    ctxId = ctxId,
    type = type
  ))
}

.rlogExit <- function(reactId, ctxId, type) {
  .pmDepthDecrement()
  .pm("exit: ", pn(reactId), " ", ctxId, " ", type)
  .rlogAppend(list(
    action = 'exit',
    reactId = reactId,
    ctxId = ctxId,
    type = type
  ))
}

# id = ctx id
# domain is like session
.rlogValueChange <- function(reactId, value) {
  .pm("valueChange: ", pn(reactId), " '",  paste(utils::capture.output(utils::str(value)), collapse='\n'), "'")
  .pmDepthIncrement()
  .rlogAppend(
    list(
      action = 'valueChange',
      reactId = reactId,
      value = paste(utils::capture.output(utils::str(value)), collapse='\n')
    )
  )
}
.rlogValueChangeEnd <- function(reactId, value) {
  .pmDepthDecrement()
  .pm("valueChangeEnd: ", pn(reactId), " '",  paste(utils::capture.output(utils::str(value)), collapse='\n'), "'")
  .rlogAppend(
    list(
      action = 'valueChangeEnd',
      reactId = reactId,
      value = paste(utils::capture.output(utils::str(value)), collapse='\n')
    )
  )
}
.rlogReactValueNames <- function(reactId, values) {
  namesStr <- paste(utils::capture.output(utils::str(ls(values, all.names=TRUE))), collapse='\n')
  .pm("valueChangeReactValueNames: ", pn(reactId), " ", namesStr)
  .rlogAppend(list(
    action = 'valueChangeReactValueNames',
    reactId = reactId,
    value = namesStr
  ))
}
.rlogReactValueValues <- function(reactId, values) {
  valuesStr <- paste(utils::capture.output(utils::str(as.list(values))), collapse='\n')
  # pm("valueChangeReactValue: ", reactId, " ", valuesStr)
  .pm("valueChangeReactValueValues: ", pn(reactId))
  .rlogAppend(list(
    action = 'valueChangeReactValueValues',
    reactId = reactId,
    value = valuesStr
  ))
}
.rlogReactValueKey <- function(reactId, key, value) {
  valueStr <- paste(utils::capture.output(utils::str(value)), collapse='\n')
  .pm("valueChangeReactValueKey: ", pn(reactId), " ", key, " ", valueStr)
  .rlogAppend(list(
    action = 'valueChangeReactValueKey',
    reactId = reactId, key = key,
    value = valueStr
  ))
}

# id = ctx id
# domain is like session
.rlogInvalidateStart <- function(reactId, ctxId, type, domain) {
  .pm("invalidateStart: ", pn(reactId), " ", ctxId, " ", type)
  .pmDepthIncrement()
  .rlogAppend(
    list(
      action = 'invalidateStart',
      reactId = reactId,
      ctxId = ctxId,
      type = type
    ),
    domain
  )
}
.rlogInvalidateEnd <- function(reactId, ctxId, type, domain) {
  .pmDepthDecrement()
  .pm("invalidateEnd: ", pn(reactId), " ", ctxId, " ", type)
  .rlogAppend(
    list(
      action = 'invalidateEnd',
      reactId = reactId,
      ctxId = ctxId,
      type = type
    ),
    domain
  )
}

#' @include stack.R
.rlogStack <- Stack$new()



#############################################################################
#############################################################################
#############################################################################
#############################################################################




.graphAppend <- function(logEntry, domain = getDefaultReactiveDomain()) {
  if (isTRUE(getOption('shiny.reactlog'))) {
    sessionToken <- if (is.null(domain)) NULL else domain$token
    .graphStack$push(c(logEntry, list(
      session = sessionToken,
      time = as.numeric(Sys.time())
    )))
  }

  if (!is.null(domain)) {
    domain$reactlog(logEntry)
  }
}

.graphDependsOn <- function(id, label) {
  .graphAppend(list(action='dep', id=id, dependsOn=label))
}

.graphDependsOnId <- function(id, dependee) {
  .graphAppend(list(action='depId', id=id, dependsOn=dependee))
}

.graphCreateContext <- function(id, label, type, prevId, domain) {
  .graphAppend(list(
    action='ctx', id=id, label=paste(label, collapse='\n'),
    srcref=as.vector(attr(label, "srcref")), srcfile=attr(label, "srcfile"),
    type=type, prevId=prevId
  ), domain = domain)
}

.graphEnterContext <- function(id) {
  .graphAppend(list(action='enter', id=id))
}

.graphExitContext <- function(id) {
  .graphAppend(list(action='exit', id=id))
}

.graphValueChange <- function(label, value) {
  .graphAppend(list(
    action = 'valueChange',
    id = label,
    value = paste(utils::capture.output(utils::str(value)), collapse='\n')
  ))
}

.graphInvalidate <- function(id, domain) {
  .graphAppend(list(action='invalidate', id=id), domain)
}

#' @include stack.R
.graphStack <- Stack$new()
