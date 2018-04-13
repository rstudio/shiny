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
.rlogDependsOnReactiveValueKey <- function(nodeId, depOnNodeId, key) {
  .pm("dependsOnReactiveValueKey: ", pn(nodeId), " ", pn(depOnNodeId), " ", key)
  .rlogAppend(list(
    action = "depReactiveValueKey",
    nodeId = nodeId,
    depOnNodeId = depOnNodeId,
    key = key
  ))
}
.rlogDependsOnReactiveValueNames <- function(nodeId, depOnNodeId) {
  .pm("dependsOnReactiveValueNames: ", pn(nodeId), " ", pn(depOnNodeId))
  .rlogAppend(list(
    action = "depReactiveValueNames",
    nodeId = nodeId,
    depOnNodeId = depOnNodeId
  ))
}
.rlogDependsOnReactiveValueToList <- function(nodeId, depOnNodeId) {
  .pm("dependsOnReactiveValuetoList: ", pn(nodeId), " ", pn(depOnNodeId))
  .rlogAppend(list(
    action = 'dependsOnReactiveValuetoList',
    nodeId = nodeId,
    depOnNodeId = depOnNodeId
  ))
}

.rlogDependsOn <- function(nodeId, depOnNodeId) {
  .pm("dependsOn: ", pn(nodeId), " on ", pn(depOnNodeId))
  .rlogAppend(list(
    action = "dep",
    nodeId = nodeId,
    depOnNodeId = depOnNodeId
  ))
}
.rlogDependsOnRemove <- function(nodeId, depOnNodeId) {
  .pm("dependsOnRemove: ", pn(nodeId), " on ", pn(depOnNodeId))
  .rlogAppend(list(
    action = "depOnRemove",
    nodeId = nodeId,
    depOnNodeId = depOnNodeId
  ))
}

nodeCache <- list()
pn <- function(nodeId) {
  nodeInfo <- nodeCache[[nodeId]]
  paste(
    nodeInfo$nodeId, nodeInfo$type, nodeInfo$label,
    sep = ":"
  )
}
# init a node id with a label
.rlogAddNodeDef <- function(nodeId, label, type) {
  if (!is.null(nodeCache[[nodeId]])) {
    stop("node definition for id: ", nodeId, " already found!!", "Label: ", label, "Type: ", type)
  }
  nodeCache[[nodeId]] <<- list(nodeId = nodeId, label = label, type = type)
  .pm("nodeDef: ", pn(nodeId))
  .rlogAppend(list(
    action = "nodeDef",
    nodeId = nodeId,
    label = label,
    type = type
  ))
}
.rlogUpdateNodeLabel <- function(nodeId, label) {
  nodeCache[[nodeId]]$label <<- label
  .pm("updateNodeLabel: ", pn(nodeId))
  .rlogAppend(list(
    action = "updateNodeLabel",
    nodeId = nodeId,
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

.rlogEnter <- function(nodeId, ctxId, type) {
  .pm("enter: ", pn(nodeId), " ", ctxId, " ", type)
  .pmDepthIncrement()
  .rlogAppend(list(
    action = 'enter',
    nodeId = nodeId,
    ctxId = ctxId,
    type = type
  ))
}

.rlogExit <- function(nodeId, ctxId, type) {
  .pmDepthDecrement()
  .pm("exit: ", pn(nodeId), " ", ctxId, " ", type)
  .rlogAppend(list(
    action = 'exit',
    nodeId = nodeId,
    ctxId = ctxId,
    type = type
  ))
}

# id = ctx id
# domain is like session
.rlogValueChangeStart <- function(nodeId, value) {
  .pm("valueChangeStart: ", pn(nodeId), " '",  paste(utils::capture.output(utils::str(value)), collapse='\n'), "'")
  .pmDepthIncrement()
  .rlogAppend(
    list(
      action = 'valueChangeStart',
      nodeId = nodeId,
      value = paste(utils::capture.output(utils::str(value)), collapse='\n')
    )
  )
}
.rlogValueChangeEnd <- function(nodeId, value) {
  .pmDepthDecrement()
  .pm("valueChangeEnd: ", pn(nodeId), " '",  paste(utils::capture.output(utils::str(value)), collapse='\n'), "'")
  .rlogAppend(
    list(
      action = 'valueChangeEnd',
      nodeId = nodeId,
      value = paste(utils::capture.output(utils::str(value)), collapse='\n')
    )
  )
}
.rlogReactValueNames <- function(nodeId, values) {
  namesStr <- paste(utils::capture.output(utils::str(ls(values, all.names=TRUE))), collapse='\n')
  .pm("valueChangeReactValueNames: ", pn(nodeId), " ", namesStr)
  .rlogAppend(list(
    action = 'valueChangeReactValueNames',
    nodeId = nodeId,
    value = namesStr
  ))
}
.rlogReactValueValues <- function(nodeId, values) {
  valuesStr <- paste(utils::capture.output(utils::str(as.list(values))), collapse='\n')
  # pm("valueChangeReactValue: ", nodeId, " ", valuesStr)
  .pm("valueChangeReactValueValues: ", pn(nodeId))
  .rlogAppend(list(
    action = 'valueChangeReactValueValues',
    nodeId = nodeId,
    value = valuesStr
  ))
}
.rlogReactValueKey <- function(nodeId, key, value) {
  valueStr <- paste(utils::capture.output(utils::str(value)), collapse='\n')
  .pm("valueChangeReactValueKey: ", pn(nodeId), " ", key, " ", valueStr)
  .rlogAppend(list(
    action = 'valueChangeReactValueKey',
    nodeId = nodeId, key = key,
    value = valueStr
  ))
}

# id = ctx id
# domain is like session
.rlogInvalidateStart <- function(nodeId, ctxId, type, domain) {
  .pm("invalidateStart: ", pn(nodeId), " ", ctxId, " ", type)
  .pmDepthIncrement()
  .rlogAppend(
    list(
      action = 'invalidateStart',
      nodeId = nodeId,
      ctxId = ctxId,
      type = type
    ),
    domain
  )
}
.rlogInvalidateEnd <- function(nodeId, ctxId, type, domain) {
  .pmDepthDecrement()
  .pm("invalidateEnd: ", pn(nodeId), " ", ctxId, " ", type)
  .rlogAppend(
    list(
      action = 'invalidateEnd',
      nodeId = nodeId,
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
