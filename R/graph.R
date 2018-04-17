# domain is like session

# used to help define truely global node id's.
# should work accross session and in global namespace
.globals$reactIdCounter <- 0L
nextGlobalReactId <- function() {
  .globals$reactIdCounter <- .globals$reactIdCounter + 1L
  paste0("r", .globals$reactIdCounter)
}



writeReactLog <- function(file=stdout(), sessionToken = NULL) {
  log <- rlogStack$as_list()
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

displayReactLogMessages <- function() {
  for (msg in rLogMsg$messages) {
    message(msg)
  }
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

rlogAppend <- function(logEntry, domain = getDefaultReactiveDomain()) {
  if (isTRUE(getOption('shiny.reactlog'))) {
    sessionToken <- if (is.null(domain)) NULL else domain$token
    rlogStack$push(c(logEntry, list(
      session = sessionToken,
      time = as.numeric(Sys.time())
    )))
  }

  if (!is.null(domain)) {
    domain$reactlog(logEntry)
  }
}


# rlogDependsOnReactiveValueKey <- function(reactId, depOnReactId, key) {
#   rLogMsg$log("dependsOnReactiveValueKey: ", rLogMsg$react(reactId), " ", rLogMsg$react(depOnReactId), " ", key)
#   rlogAppend(list(
#     action = "depReactiveValueKey",
#     reactId = reactId,
#     depOnReactId = depOnReactId,
#     key = key
#   ))
# }
# rlogDependsOnReactiveValueNames <- function(reactId, depOnReactId) {
#   rLogMsg$log("dependsOnReactiveValueNames: ", rLogMsg$node(reactId), " ", rLogMsg$node(depOnReactId))
#   rlogAppend(list(
#     action = "depReactiveValueNames",
#     reactId = reactId,
#     depOnReactId = depOnReactId
#   ))
# }
# rlogDependsOnReactiveValueToList <- function(reactId, depOnReactId) {
#   rLogMsg$log("dependsOnReactiveValueToList: ", rLogMsg$node(reactId), " ", rLogMsg$node(depOnReactId))
#   rlogAppend(list(
#     action = "dependsOnReactiveValuetoList",
#     reactId = reactId,
#     depOnReactId = depOnReactId
#   ))
# }

rlogReactivesNamesId     <- function(reactId)      paste0("names(", reactId, ")")
rlogReactivesAsListId    <- function(reactId)      paste0("as.list(", reactId, ", all.names = TRUE)")
rlogReactivesAsListAllId <- function(reactId)      paste0("as.list(", reactId, ")")
rlogReactivesKeyId       <- function(reactId, key) paste0(reactId, "$", key)

rlogDependsOn <- function(reactId, depOnReactId) {
  rLogMsg$log("dependsOn: ", rLogMsg$node(reactId), " on ", rLogMsg$node(depOnReactId))
  rlogAppend(list(
    action = "dep",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}
rlogDependsOnRemove <- function(reactId, depOnReactId) {
  rLogMsg$log("dependsOnRemove: ", rLogMsg$node(reactId), " on ", rLogMsg$node(depOnReactId))
  rlogAppend(list(
    action = "depOnRemove",
    reactId = reactId,
    depOnReactId = depOnReactId
  ))
}

# init a node id with a label
rlogReactDef <- function(reactId, label, type) {
  if (!is.null(rLogMsg$nodeCache[[reactId]])) {
    stop("node definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
  }
  rLogMsg$nodeCache[[reactId]] <- list(reactId = reactId, label = label)
  rLogMsg$log("def: ", rLogMsg$node(reactId))
  rlogAppend(list(
    action = "def",
    reactId = reactId,
    label = label,
    type = type
  ))
}
rlogUpdateNodeLabel <- function(reactId, label) {
  rLogMsg$nodeCache[[reactId]]$label <<- label
  rLogMsg$log("updateNodeLabel: ", rLogMsg$node(reactId))
  rlogAppend(list(
    action = "updateNodeLabel",
    reactId = reactId,
    label = label
  ))
}

# rlogCreateContext <- function(id, label, type, prevId, domain) {
#   message("!!createContext: create graph context is deprecated!!")
#   rlogAppend(list(
#     action='ctx', ctxId = id, label = paste(label, collapse='\n'),
#     srcref=as.vector(attr(label, "srcref")), srcfile=attr(label, "srcfile"),
#     type=type, prevId=prevId
#   ), domain = domain)
# }

rlogEnter <- function(reactId, ctxId, type) {
  rLogMsg$log("enter: ", rLogMsg$node(reactId), " ", ctxId, " ", type)
  rLogMsg$depthIncrement()
  rlogAppend(list(
    action = 'enter',
    reactId = reactId,
    ctxId = ctxId,
    type = type
  ))
}

rlogExit <- function(reactId, ctxId, type) {
  rLogMsg$depthDecrement()
  rLogMsg$log("exit: ", rLogMsg$node(reactId), " ", ctxId, " ", type)
  rlogAppend(list(
    action = 'exit',
    reactId = reactId,
    ctxId = ctxId,
    type = type
  ))
}

# id = ctx id
# domain is like session
rlogValueChange <- function(reactId, value, display = TRUE) {
  valueStr <- paste(utils::capture.output(utils::str(value)), collapse='\n')
  if (isTRUE(display)) {
    rLogMsg$log("valueChange: ", rLogMsg$node(reactId), " '",  valueStr, "'")
  } else {
    rLogMsg$log("valueChange: ", rLogMsg$node(reactId))
  }
  rlogAppend(
    list(
      action = 'valueChange',
      reactId = reactId,
      value = valueStr
    )
  )
}
# rlogReactValueNames <- function(reactId, values) {
#   namesStr <- paste(utils::capture.output(utils::str(ls(values, all.names=TRUE))), collapse='\n')
#   rLogMsg$log("valueChangeReactValueNames: ", rLogMsg$node(reactId), " ", namesStr)
#   rlogAppend(list(
#     action = 'valueChangeReactValueNames',
#     reactId = reactId,
#     value = namesStr
#   ))
# }
# rlogReactValueValues <- function(reactId, values) {
#   valuesStr <- paste(utils::capture.output(utils::str(as.list(values))), collapse='\n')
#   # pm("valueChangeReactValue: ", reactId, " ", valuesStr)
#   rLogMsg$log("valueChangeReactValueValues: ", rLogMsg$node(reactId))
#   rlogAppend(list(
#     action = 'valueChangeReactValueValues',
#     reactId = reactId,
#     value = valuesStr
#   ))
# }
# rlogReactValueKey <- function(reactId, key, value) {
#   valueStr <- paste(utils::capture.output(utils::str(value)), collapse='\n')
#   rLogMsg$log("valueChangeReactValueKey: ", rLogMsg$node(reactId), " ", key, " ", valueStr)
#   rlogAppend(list(
#     action = 'valueChangeReactValueKey',
#     reactId = reactId, key = key,
#     value = valueStr
#   ))
# }

rlogInvalidateStart <- function(reactId, ctxId, type, domain) {
  if (identical(type, "isolate")) {
    rlogType <- "isolateInvalidateStart"
  } else {
    rlogType <- "invalidateStart"
  }
  rLogMsg$log(rlogType, ": ", rLogMsg$node(reactId), " ", ctxId, " ", type)
  rLogMsg$depthIncrement()
  rlogAppend(
    list(
      action = rlogType,
      reactId = reactId,
      ctxId = ctxId
    ),
    domain
  )
}
rlogInvalidateEnd <- function(reactId, ctxId, type, domain) {
  if (identical(type, "isolate")) {
    rlogType <- "isolateInvalidateEnd"
  } else {
    rlogType <- "invalidateEnd"
  }
  rLogMsg$depthDecrement()
  rLogMsg$log(rlogType, ": ", rLogMsg$node(reactId), " ", ctxId, " ", type)
  rlogAppend(
    list(
      action = rlogType,
      reactId = reactId,
      ctxId = ctxId
    ),
    domain
  )
}

rlogQueueEmpty <- function() {
  rLogMsg$log("queueEmpty")
  rlogAppend(
    list(
      action = "queueEmpty"
    )
  )
}

MessageLogger = R6Class(
  'Stack',
  portable = FALSE,
  class = FALSE,
  public = list(
    depth = 0L,
    display = TRUE,
    messages = c(),
    nodeCache = list(),

    initialize = function(display, depth) {
      if (!missing(display)) self$display <- display
      if (!missing(depth)) self$depth <- depth
    },
    depthIncrement = function() {
      self$depth <- self$depth + 1
    },
    depthDecrement = function() {
      self$depth <- self$depth - 1
    },
    node = function(reactId) {
      nodeInfo <- nodeCache[[reactId]]
      paste0(
        nodeInfo$reactId, ":", nodeInfo$label
      )
    },
    log = function(...) {
      msg <- paste0(
        paste0(rep(". ", depth), collapse = ""), " - ", paste0(..., collapse = ""),
        collapse = ""
      )
      self$messages[length(self$messages) + 1] <- msg
      if (display) {
        message(msg)
      }
    }
  )
)
rLogMsg <- MessageLogger$new(TRUE, 0)


#' @include stack.R
rlogStack <- Stack$new()



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
