# domain is like session


# used to help define truly global react id's.
# should work across session and in global namespace
.globals$reactIdCounter <- 0L
nextGlobalReactId <- function() {
  .globals$reactIdCounter <- .globals$reactIdCounter + 1L
  reactIdStr(.globals$reactIdCounter)
}
reactIdStr <- function(num) {
  paste0("r", num)
}


#' Reactive Log Visualizer
#'
#' Provides an interactive browser-based tool for visualizing reactive
#' dependencies and execution in your application.
#'
#' To use the reactive log visualizer, start with a fresh R session and
#' run the command `reactlog::reactlog_enable()`; then launch your
#' application in the usual way (e.g. using [runApp()]). At
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
#' application--you can run the `reactlogShow` function, which will
#' generate the reactive log visualization as a static HTML file and
#' launch it in your default browser. In this case, refreshing your
#' browser will not load new activity into the report; you will need to
#' call `reactlogShow()` explicitly.
#'
#' For security and performance reasons, do not enable
#' `options(shiny.reactlog=TRUE)` (or `reactlog::reactlog_enable()`) in
#' production environments. When the option is enabled, it's possible
#' for any user of your app to see at least some of the source code of
#' your reactive expressions and observers. In addition, reactlog
#' should be considered a memory leak as it will constantly grow and
#' will never reset until the R session is restarted.
#'
#' @name reactlog
NULL


#' @describeIn reactlog Return a list of reactive information.  Can be used in
#'   conjunction with [reactlog::reactlog_show] to later display the reactlog
#'   graph.
#' @export
reactlog <- function() {
  rLog$asList()
}

#' @describeIn reactlog Display a full reactlog graph for all sessions.
#' @param time A boolean that specifies whether or not to display the
#' time that each reactive takes to calculate a result.
#' @export
reactlogShow <- function(time = TRUE) {
  check_reactlog()
  reactlog::reactlog_show(reactlog(), time = time)
}

#' @describeIn reactlog Resets the entire reactlog stack.  Useful for debugging
#' and removing all prior reactive history.
#' @export
reactlogReset <- function() {
  rLog$reset()
}

#' @describeIn reactlog Adds "mark" entry into the reactlog stack. This is
#' useful for programmatically adding a marked entry in the reactlog, rather
#' than using your keyboard's key combination.
#'
#' For example, we can _mark_ the reactlog at the beginning of an
#' `observeEvent`'s calculation:
#' ```r
#' observeEvent(input$my_event_trigger, {
#'   # Add a mark in the reactlog
#'   reactlogAddMark()
#'   # Run your regular event reaction code here...
#'   ....
#' })
#' ```
#' @param session The Shiny session to assign the mark to. Defaults to the
#' current session.
#' @export
reactlogAddMark <- function(session = getDefaultReactiveDomain()) {
  rLog$userMark(session)
}

# called in "/reactlog" middleware
renderReactlog <- function(sessionToken = NULL, time = TRUE) {
  check_reactlog()
  reactlog::reactlog_render(
    reactlog(),
    session_token = sessionToken,
    time = time
  )
}

check_reactlog <- function() {
  if (!is_installed("reactlog", reactlog_min_version)) {
    rlang::check_installed("reactlog", reactlog_min_version)
  }
}

# Should match the (suggested) version in DESCRIPTION file
reactlog_min_version <- "1.0.0"

RLog <- R6Class(
  "RLog",
  portable = FALSE,
  private = list(
    option = "shiny.reactlog",
    msgOption = "shiny.reactlog.console",
    appendEntry = function(domain, logEntry) {
      if (self$isLogging()) {
        sessionToken <- if (is.null(domain)) NULL else domain$token
        logStack$push(c(logEntry, list(
          session = sessionToken,
          time = as.numeric(Sys.time())
        )))
      }
      if (!is.null(domain)) domain$reactlog(logEntry)
    }
  ),
  public = list(
    msg = "<MessageLogger>",
    logStack = "<Stack>",
    noReactIdLabel = "NoCtxReactId",
    noReactId = reactIdStr("NoCtxReactId"),
    dummyReactIdLabel = "DummyReactId",
    dummyReactId = reactIdStr("DummyReactId"),
    asList = function() {
      ret <- self$logStack$as_list()
      attr(ret, "version") <- "1"
      ret
    },
    ctxIdStr = function(ctxId) {
      if (is.null(ctxId) || identical(ctxId, "")) {
        return(NULL)
      }
      paste0("ctx", ctxId)
    },
    namesIdStr = function(reactId) {
      paste0("names(", reactId, ")")
    },
    asListIdStr = function(reactId) {
      paste0("reactiveValuesToList(", reactId, ")")
    },
    asListAllIdStr = function(reactId) {
      paste0("reactiveValuesToList(", reactId, ", all.names = TRUE)")
    },
    keyIdStr = function(reactId, key) {
      paste0(reactId, "$", key)
    },
    valueStr = function(value, n = 200) {
      if (!self$isLogging()) {
        # return a placeholder string to avoid calling str
        return("<reactlog is turned off>")
      }
      output <- try(silent = TRUE, {
        # only capture the first level of the object
        utils::capture.output(utils::str(value, max.level = 1))
      })
      outputTxt <- paste0(output, collapse = "\n")
      msg$shortenString(outputTxt, n = n)
    },
    initialize = function(rlogOption = "shiny.reactlog", msgOption = "shiny.reactlog.console") {
      private$option <- rlogOption
      private$msgOption <- msgOption

      self$reset()
    },
    reset = function() {
      .globals$reactIdCounter <- 0L

      self$logStack <- fastmap::faststack()
      self$msg <- MessageLogger$new(option = private$msgOption)

      # setup dummy and missing react information
      self$msg$setReact(force = TRUE, list(reactId = self$noReactId, label = self$noReactIdLabel))
      self$msg$setReact(force = TRUE, list(reactId = self$dummyReactId, label = self$dummyReactIdLabel))
    },
    isLogging = function() {
      isTRUE(getOption(private$option, FALSE))
    },
    define = function(reactId, value, label, type, domain) {
      valueStr <- self$valueStr(value)
      if (msg$hasReact(reactId)) {
        stop("react definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
      }
      msg$setReact(list(reactId = reactId, label = label))
      msg$log("define:", msg$reactStr(reactId), msg$typeStr(type = type), msg$valueStr(valueStr))
      private$appendEntry(domain, list(
        action = "define",
        reactId = reactId,
        label = msg$shortenString(label),
        type = type,
        value = valueStr
      ))
    },
    defineNames = function(reactId, value, label, domain) {
      self$define(self$namesIdStr(reactId), value, self$namesIdStr(label), "reactiveValuesNames", domain)
    },
    defineAsList = function(reactId, value, label, domain) {
      self$define(self$asListIdStr(reactId), value, self$asListIdStr(label), "reactiveValuesAsList", domain)
    },
    defineAsListAll = function(reactId, value, label, domain) {
      self$define(self$asListAllIdStr(reactId), value, self$asListAllIdStr(label), "reactiveValuesAsListAll", domain)
    },
    defineKey = function(reactId, value, key, label, domain) {
      self$define(self$keyIdStr(reactId, key), value, self$keyIdStr(label, key), "reactiveValuesKey", domain)
    },
    defineObserver = function(reactId, label, domain) {
      self$define(reactId, value = NULL, label, "observer", domain)
    },
    dependsOn = function(reactId, depOnReactId, ctxId, domain) {
      if (is.null(reactId)) {
        return()
      }
      ctxId <- ctxIdStr(ctxId)
      msg$log("dependsOn:", msg$reactStr(reactId), " on", msg$reactStr(depOnReactId), msg$ctxStr(ctxId))
      private$appendEntry(domain, list(
        action = "dependsOn",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKey = function(reactId, depOnReactId, key, ctxId, domain) {
      self$dependsOn(reactId, self$keyIdStr(depOnReactId, key), ctxId, domain)
    },
    dependsOnRemove = function(reactId, depOnReactId, ctxId, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      msg$log("dependsOnRemove:", msg$reactStr(reactId), " on", msg$reactStr(depOnReactId), msg$ctxStr(ctxId))
      private$appendEntry(domain, list(
        action = "dependsOnRemove",
        reactId = reactId,
        depOnReactId = depOnReactId,
        ctxId = ctxId
      ))
    },
    dependsOnKeyRemove = function(reactId, depOnReactId, key, ctxId, domain) {
      self$dependsOnRemove(reactId, self$keyIdStr(depOnReactId, key), ctxId, domain)
    },
    createContext = function(ctxId, label, type, prevCtxId, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      prevCtxId <- self$ctxIdStr(prevCtxId)
      msg$log("createContext:", msg$ctxPrevCtxStr(preCtxIdTxt = " ", ctxId, prevCtxId, type))
      private$appendEntry(domain, list(
        action = "createContext",
        ctxId = ctxId,
        label = msg$shortenString(label),
        type = type,
        prevCtxId = prevCtxId,
        srcref = as.vector(attr(label, "srcref")), srcfile = attr(label, "srcfile")
      ))
    },
    enter = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateEnter:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateEnter",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("enter:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
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
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateExit:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        private$appendEntry(domain, list(
          action = "isolateExit",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("exit:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        private$appendEntry(domain, list(
          action = "exit",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    valueChange = function(reactId, value, domain) {
      valueStr <- self$valueStr(value)
      msg$log("valueChange:", msg$reactStr(reactId), msg$valueStr(valueStr))
      private$appendEntry(domain, list(
        action = "valueChange",
        reactId = reactId,
        value = valueStr
      ))
    },
    valueChangeNames = function(reactId, nameValues, domain) {
      self$valueChange(self$namesIdStr(reactId), nameValues, domain)
    },
    valueChangeAsList = function(reactId, listValue, domain) {
      self$valueChange(self$asListIdStr(reactId), listValue, domain)
    },
    valueChangeAsListAll = function(reactId, listValue, domain) {
      self$valueChange(self$asListAllIdStr(reactId), listValue, domain)
    },
    valueChangeKey = function(reactId, key, value, domain) {
      self$valueChange(self$keyIdStr(reactId, key), value, domain)
    },
    invalidateStart = function(reactId, ctxId, type, domain) {
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$log("isolateInvalidateStart:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        msg$depthIncrement()
        private$appendEntry(domain, list(
          action = "isolateInvalidateStart",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$log("invalidateStart:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
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
      ctxId <- self$ctxIdStr(ctxId)
      if (identical(type, "isolate")) {
        msg$depthDecrement()
        msg$log("isolateInvalidateEnd:", msg$reactStr(reactId), msg$ctxStr(ctxId))
        private$appendEntry(domain, list(
          action = "isolateInvalidateEnd",
          reactId = reactId,
          ctxId = ctxId
        ))
      } else {
        msg$depthDecrement()
        msg$log("invalidateEnd:", msg$reactStr(reactId), msg$ctxStr(ctxId, type))
        private$appendEntry(domain, list(
          action = "invalidateEnd",
          reactId = reactId,
          ctxId = ctxId,
          type = type
        ))
      }
    },
    invalidateLater = function(reactId, runningCtx, millis, domain) {
      msg$log("invalidateLater: ", millis, "ms", msg$reactStr(reactId), msg$ctxStr(runningCtx))
      private$appendEntry(domain, list(
        action = "invalidateLater",
        reactId = reactId,
        ctxId = runningCtx,
        millis = millis
      ))
    },
    idle = function(domain = NULL) {
      msg$log("idle")
      private$appendEntry(domain, list(
        action = "idle"
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
      msg$log("freeze:", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "freeze",
        reactId = reactId
      ))
    },
    freezeReactiveKey = function(reactId, key, domain) {
      self$freezeReactiveVal(self$keyIdStr(reactId, key), domain)
    },
    thawReactiveVal = function(reactId, domain) {
      msg$log("thaw:", msg$reactStr(reactId))
      private$appendEntry(domain, list(
        action = "thaw",
        reactId = reactId
      ))
    },
    thawReactiveKey = function(reactId, key, domain) {
      self$thawReactiveVal(self$keyIdStr(reactId, key), domain)
    },
    userMark = function(domain = NULL) {
      msg$log("userMark")
      private$appendEntry(domain, list(
        action = "userMark"
      ))
    }
  )
)

MessageLogger <- R6Class(
  "MessageLogger",
  portable = FALSE,
  public = list(
    depth = 0L,
    reactCache = list(),
    option = "shiny.reactlog.console",
    initialize = function(option = "shiny.reactlog.console", depth = 0L) {
      if (!missing(depth)) self$depth <- depth
      if (!missing(option)) self$option <- option
    },
    isLogging = function() {
      isTRUE(getOption(self$option))
    },
    isNotLogging = function() {
      !isTRUE(getOption(self$option))
    },
    depthIncrement = function() {
      if (self$isNotLogging()) {
        return(NULL)
      }
      self$depth <- self$depth + 1L
    },
    depthDecrement = function() {
      if (self$isNotLogging()) {
        return(NULL)
      }
      self$depth <- self$depth - 1L
    },
    hasReact = function(reactId) {
      if (self$isNotLogging()) {
        return(FALSE)
      }
      !is.null(self$getReact(reactId))
    },
    getReact = function(reactId, force = FALSE) {
      if (identical(force, FALSE) && self$isNotLogging()) {
        return(NULL)
      }
      self$reactCache[[reactId]]
    },
    setReact = function(reactObj, force = FALSE) {
      if (identical(force, FALSE) && self$isNotLogging()) {
        return(NULL)
      }
      self$reactCache[[reactObj$reactId]] <- reactObj
    },
    shortenString = function(txt, n = 250) {
      if (is.null(txt) || isTRUE(is.na(txt))) {
        return("")
      }
      if (nchar(txt) > n) {
        return(
          paste0(substr(txt, 1, n - 3), "...")
        )
      }
      return(txt)
    },
    singleLine = function(txt) {
      gsub("([^\\])\\n", "\\1\\\\n", txt)
    },
    valueStr = function(valueStr) {
      paste0(
        " '", self$shortenString(self$singleLine(valueStr)), "'"
      )
    },
    reactStr = function(reactId) {
      if (self$isNotLogging()) {
        return(NULL)
      }
      reactInfo <- self$getReact(reactId)
      if (is.null(reactInfo)) {
        return(" <UNKNOWN_REACTID>")
      }
      paste0(
        " ", reactInfo$reactId, ":'", self$shortenString(self$singleLine(reactInfo$label)), "'"
      )
    },
    typeStr = function(type = NULL) {
      self$ctxStr(ctxId = NULL, type = type)
    },
    ctxStr = function(ctxId = NULL, type = NULL) {
      if (self$isNotLogging()) {
        return(NULL)
      }
      self$ctxPrevCtxStr(ctxId = ctxId, prevCtxId = NULL, type = type)
    },
    ctxPrevCtxStr = function(ctxId = NULL, prevCtxId = NULL, type = NULL, preCtxIdTxt = " in ") {
      if (self$isNotLogging()) {
        return(NULL)
      }
      paste0(
        if (!is.null(ctxId)) paste0(preCtxIdTxt, ctxId),
        if (!is.null(prevCtxId)) paste0(" from ", prevCtxId),
        if (!is.null(type) && !identical(type, "other")) paste0(" - ", type)
      )
    },
    log = function(...) {
      if (self$isNotLogging()) {
        return(NULL)
      }
      msg <- paste0(
        paste0(rep("= ", depth), collapse = ""), "- ", paste0(..., collapse = ""),
        collapse = ""
      )
      message(msg)
    }
  )
)

on_load({
  rLog <- RLog$new("shiny.reactlog", "shiny.reactlog.console")
})
