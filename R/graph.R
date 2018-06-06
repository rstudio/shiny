# domain is like session


# used to help define truely global react id's.
# should work accross session and in global namespace
.globals$reactIdCounter <- 0L
nextGlobalReactId <- function() {
  .globals$reactIdCounter <- .globals$reactIdCounter + 1L
  paste0("r", .globals$reactIdCounter)
}



writeReactLog <- function(file=stdout(), sessionToken = NULL) {
  log <- rLog$logStack$as_list()
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
  templateFile <- system.file("www/reactive-graph.html", package="shiny")
  html <- paste(readLines(templateFile, warn=FALSE), collapse="\r\n")
  tc <- textConnection(NULL, "w")
  on.exit(close(tc))
  writeReactLog(tc, sessionToken)
  cat("\n", file=tc)
  flush(tc)
  html <- sub("__DATA__", paste(textConnectionValue(tc), collapse="\r\n"), html, fixed=TRUE)
  html <- sub("__TIME__", paste0("\"", time, "\""), html, fixed=TRUE)
  file <- tempfile(fileext = ".html")
  writeLines(html, file)

  # copy js and style files
  file.copy(
    system.file("www/rlog", package="shiny"),
    dirname(file),
    recursive = TRUE
  )

  return(file)
}


RLog <- R6Class(
  "RLog",
  portable = FALSE,
  private = list(
    option = "shiny.reactlog",

    appendEntry = function(domain, logEntry) {
      if (self$isLogging) {
        sessionToken <- if (is.null(domain)) NULL else domain$token
        logStack$push(c(logEntry, list(
          session = sessionToken,
          time = as.numeric(Sys.time())
        )))
      }
      if (!is.null(domain)) domain$reactlog(logEntry)
    }
  ),
  active = list(
    isLogging = function() {
      isTRUE(getOption(private$option))
    }
  ),
  public = list(
    msg = "MessageLogger",
    logStack = "Stack",

    ctxIdStr = function(ctxId) paste0("ctx", ctxId),
    namesIdStr     = function(reactId)      paste0("names(", reactId, ")"),
    asListIdStr    = function(reactId)      paste0("as.list(", reactId, ", all.names = TRUE)"),
    asListAllIdStr = function(reactId)      paste0("as.list(", reactId, ")"),
    keyIdStr       = function(reactId, key) paste0(reactId, "$", key),



    initialize = function(option = "shiny.reactlog", display = TRUE, depth = 0) {
      private$option <- option
      self$logStack <- Stack$new()
      self$msg <- MessageLogger$new(display = display, depth = depth, option = option)
    },
    displayMessages = function() {
      for (msg in msg$messages) {
        message(msg)
      }
    },

    define = function(reactId, label, type, domain) {
      if (msg$hasReact(reactId)) {
        stop("react definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
      }
      msg$setReact(list(reactId = reactId, label = label))
      msg$log("define: ", msg$reactStr(reactId), " - ", type)
      private$appendEntry(domain, list(
        action = "define",
        reactId = reactId,
        label = label,
        type = type
      ))
    },
    defineNames = function(reactId, label, domain)
      define(namesIdStr(reactId), namesIdStr(label), "reactiveValuesNames", domain),
    defineAsList = function(reactId, label, domain)
      define(asListIdStr(reactId), asListIdStr(label), "reactiveValuesAsList", domain),
    defineAsListAll = function(reactId, label, domain)
      define(asListAllIdStr(reactId), asListAllIdStr(label), "reactiveValuesAsListAll", domain),
    defineKey = function(reactId, key, label, domain)
      define(keyIdStr(reactId, key), keyIdStr(label, key), "reactiveValuesKey", domain),

    updateReactLabel = function(reactId, label, domain) {
      msgObj <- msg$getReact(reactId)
      if (!is.null(msgObj)) {
        msgObj$label <- label
        msg$setReact(msgObj)
      }
      msg$log("updateLabel: ", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "updateLabel",
        reactId = reactId,
        label = label
      ))
    },
    updateReactLabelNames = function(reactId, label, domain)
      updateReactLabel(namesIdStr(reactId), namesIdStr(label), domain),
    updateReactLabelAsList = function(reactId, label, domain)
      updateReactLabel(asListIdStr(reactId), asListIdStr(label), domain),
    updateReactLabelAsListAll = function(reactId, label, domain)
      updateReactLabel(asListAllIdStr(reactId), asListAllIdStr(label), domain),
    updateReactLabelKey = function(reactId, key, label, domain)
      updateReactLabel(keyIdStr(reactId, key), keyIdStr(label, key), domain),

    dependsOn = function(reactId, depOnReactId, ctxId, domain) {
      ctxId <- ctxIdStr(ctxId)
      msg$log("dependsOn: ", msg$reactStr(reactId), " on ", msg$reactStr(depOnReactId), " in ", ctxId)
      private$appendEntry(domain, list(
        action = "dependsOn",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKey = function(reactId, depOnReactId, key, ctxId, domain)
      dependsOn(reactId, keyIdStr(depOnReactId, key), ctxId, domain),

    dependsOnRemove = function(reactId, depOnReactId, ctxId, domain) {
      ctxId <- ctxIdStr(ctxId)
      msg$log("dependsOnRemove: ", msg$reactStr(reactId), " on ", msg$reactStr(depOnReactId), " in ", ctxId)
      private$appendEntry(domain, list(
        action = "dependsOnRemove",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKeyRemove = function(reactId, depOnReactId, key, ctxId, domain)
      dependsOnRemove(reactId, keyIdStr(depOnReactId, key), ctxId, domain),

    enter = function(reactId, ctxId, type, domain) {
      ctxId <- ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateEnter: ", msg$reactStr(reactId), " in ", ctxId)
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateEnter",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("enter: ", msg$reactStr(reactId), " in ", ctxId, " - ", type)
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "enter",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    exit = function(reactId, ctxId, type, domain) {
      ctxId <- ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateExit: ", msg$reactStr(reactId), " in ", ctxId)
        private$appendEntry(domain, list(
          action = "isolateExit",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("exit: ", msg$reactStr(reactId), " in ", ctxId, " - ", type)
        private$appendEntry(domain, list(
          action = "exit",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },

    valueChange = function(reactId, value, display, domain) {
      valueStr <- paste(utils::capture.output(utils::str(value)), collapse="\n")
      if (isTRUE(display)) {
        msg$log("valueChange: ", msg$reactStr(reactId), " '",  valueStr, "'")
      } else {
        msg$log("valueChange: ", msg$reactStr(reactId))
      }
      private$appendEntry(domain, list(
        action = "valueChange",
        reactId = reactId,
        value = valueStr
      ))
    },
    valueChangeNames = function(reactId, nameValues, domain)
      valueChange(namesIdStr(reactId), nameValues, FALSE, domain),
    valueChangeAsList = function(reactId, listValue, domain)
      valueChange(asListIdStr(reactId), listValue, FALSE, domain),
    valueChangeAsListAll = function(reactId, listValue, domain)
      valueChange(asListAllIdStr(reactId), listValue, FALSE, domain),
    valueChangeKey = function(reactId, key, value, domain)
      valueChange(keyIdStr(reactId, key), value, FALSE, domain),


    invalidateStart = function(reactId, ctxId, type, domain) {
      ctxId <- ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateInvalidateStart: ", msg$reactStr(reactId), " in ", ctxId)
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateInvalidateStart",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("invalidateStart", ": ", msg$reactStr(reactId), " in ", ctxId, " - ", type)
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "invalidateStart",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    invalidateEnd = function(reactId, ctxId, type, domain) {
      ctxId <- ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateInvalidateEnd: ", msg$reactStr(reactId), " in ", ctxId)
        private$appendEntry(domain, list(
          action = "isolateInvalidateEnd",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("invalidateEnd: ", msg$reactStr(reactId), " in ", ctxId, " - ", type)
        private$appendEntry(domain, list(
          action = "invalidateEnd",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },

    queueEmpty = function(domain = NULL) {
      msg$log("queueEmpty")
      private$appendEntry(domain, list(
        action = "queueEmpty"
      ))
    },

    asyncStart = function(domain = NULL) {
      msg$log("asyncStart")
      private$appendEntry(domain, list(
        action = "asyncStart"
      ))
    },
    asyncStop = function(domain = NULL) {
      msg$log("asyncStop")
      private$appendEntry(domain, list(
        action = "asyncStop"
      ))
    },

    freezeReactiveVal = function(reactId, domain) {
      msg$log("freeze: ", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "freeze",
        reactId = reactId
      ))
    },
    freezeReactiveKey = function(reactId, key, domain)
      freezeReactiveVal(keyIdStr(reactId, key), domain),

    thawReactiveVal = function(reactId, domain) {
      msg$log("thaw: ", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "thaw",
        reactId = reactId
      ))
    },
    thawReactiveKey = function(reactId, key, domain)
      thawReactiveVal(keyIdStr(reactId, key), domain),

    markTime = function(domain = NULL) {
      msg$log("markTime")
      private$appendEntry(domain, list(
        action = "markTime"
      ))
    }

  )
)

MessageLogger = R6Class(
  "MessageLogger",
  portable = FALSE,
  active = list(
    isLogging = function() {
      isTRUE(getOption(option))
    },
    isNotLogging = function() {
      ! isTRUE(getOption(option))
    }
  ),
  public = list(
    depth = 0L,
    display = TRUE,
    messages = c(),
    reactCache = list("rNoCtx" = list(label = "<UNKNOWN>", reactId = "<UNKNOWN>")),
    option = "shiny.reactlog",

    initialize = function(display, depth, option) {
      if (!missing(display)) self$display <- display
      if (!missing(depth)) self$depth <- depth
      if (!missing(option)) self$option <- option
    },
    depthIncrement = function() {
      if (self$isNotLogging) return(NULL)
      self$depth <- self$depth + 1
    },
    depthDecrement = function() {
      if (self$isNotLogging) return(NULL)
      self$depth <- self$depth - 1
    },
    hasReact = function(reactId) {
      if (self$isNotLogging) return(FALSE)
      !is.null(getReact(reactId))
    },
    getReact = function(reactId) {
      if (self$isNotLogging) return(NULL)
      reactCache[[reactId]]
    },
    setReact = function(reactObj) {
      if (self$isNotLogging) return(NULL)
      self$reactCache[[reactObj$reactId]] <- reactObj
    },
    reactStr = function(reactId) {
      if (self$isNotLogging) return(NULL)
      reactInfo <- getReact(reactId)
      paste0(
        reactInfo$reactId, ":", reactInfo$label
      )
    },
    log = function(...) {
      if (self$isNotLogging) return(NULL)
      msg <- paste0(
        paste0(rep("· ", depth), collapse = ""), "- ", paste0(..., collapse = ""),
        collapse = ""
      )
      self$messages[length(self$messages) + 1] <- msg
      if (display) {
        message(msg)
      }
    }
  )
)

#' @include stack.R
#' TODO-barret set TRUE to FALSE before release
#' TODO-barret remove option set
options("shiny.reactlog" = TRUE)
rLog <- RLog$new("shiny.reactlog", TRUE, 0)


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
