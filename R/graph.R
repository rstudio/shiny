is_installed <- function(package, version) {
  installedVersion <- tryCatch(utils::packageVersion(package), error = function(e) NA)
  !is.na(installedVersion) && installedVersion >= version
}

# Check that the version of an suggested package satisfies the requirements
#
# @param package The name of the suggested package
# @param version The version of the package
check_suggested <- function(package, version, location) {

  if (!is_installed(package, version)) {
    missing_location <- missing(location)
    msg <- paste0(
      sQuote(package),
      if (is.na(version)) "" else paste0("(>= ", version, ")"),
      " must be installed for this functionality.",
      if (!missing_location)
        paste0(
          "\nPlease install the missing package: \n",
          "  source(\"https://install-github.me/", location, "\")"
        )
    )

    if (interactive() && missing_location) {
      message(msg, "\nWould you like to install it?")
      if (utils::menu(c("Yes", "No")) == 1) {
        utils::install.packages(package)
      } else {
        stop(msg, call. = FALSE)
      }
    } else {
      stop(msg, call. = FALSE)
    }
  }
}




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
  check_reactlog()
  reactlog::show_reactlog(rLog$asList(), time = time)
}
# called in "/reactlog" middleware
renderReactlog <- function(sessionToken = NULL, time = TRUE) {
  check_reactlog()
  reactlog::render_reactlog(
    rLog$asList(),
    session_token = sessionToken,
    time = time
  )
}
check_reactlog <- function() {
  check_suggested("reactlog", reactlog_version(), "rstudio/reactlog")
}
# read reactlog version from description file
# prevents version mismatch in code and description file
reactlog_version <- function() {
  desc <- read.dcf(system.file("DESCRIPTION", package = "shiny", mustWork = TRUE))
  suggests <- desc[1,"Suggests"][[1]]
  suggests_pkgs <- strsplit(suggests, "\n")[[1]]

  reactlog_info <- suggests_pkgs[grepl("reactlog", suggests_pkgs)]
  if (length(reactlog_info) == 0) {
    stop("reactlog can not be found in shiny DESCRIPTION file")
  }

  reactlog_info <- sub("^[^\\(]*\\(", "", reactlog_info)
  reactlog_info <- sub("\\)[^\\)]*$", "", reactlog_info)
  reactlog_info <- sub("^[>= ]*", "", reactlog_info)

  package_version(reactlog_info)
}


RLog <- R6Class(
  "RLog",
  portable = FALSE,
  private = list(
    option = "shiny.reactlog",

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

    asList = function() {
      ret <- self$logStack$as_list()
      attr(ret, "version") <- "1"
      ret
    },

    ctxIdStr = function(ctxId) {
      if (is.null(ctxId) || identical(ctxId, "")) return(NULL)
      paste0("ctx", ctxId)
    },
    namesIdStr = function(reactId) {
      paste0("names(", reactId, ")")
    },
    asListIdStr = function(reactId) {
      paste0("as.list(", reactId, ")")
    },
    asListAllIdStr = function(reactId) {
      paste0("as.list(", reactId, ", all.names = TRUE)")
    },
    keyIdStr = function(reactId, key) {
      paste0(reactId, "$", key)
    },



    initialize = function(rlogOption = "shiny.reactlog", msgOption = "shiny.reactlog.console") {
      private$option <- rlogOption
      self$logStack <- Stack$new()
      self$msg <- MessageLogger$new(option = msgOption)

      self$msg$setReact(list(reactId = self$noReactId, label = self$noReactIdLabel))

    },
    isLogging = function() {
      isTRUE(getOption(private$option, FALSE))
    },

    define = function(reactId, label, type, domain) {
      if (msg$hasReact(reactId)) {
        stop("react definition for id: ", reactId, " already found!!", "Label: ", label, "Type: ", type)
      }
      msg$setReact(list(reactId = reactId, label = label))
      msg$log("define:", msg$reactStr(reactId), msg$typeStr(type = type))
      private$appendEntry(domain, list(
        action = "define",
        reactId = reactId,
        label = label,
        type = type
      ))
    },
    defineNames = function(reactId, label, domain) {
      self$define(self$namesIdStr(reactId), self$namesIdStr(label), "reactiveValuesNames", domain)
    },
    defineAsList = function(reactId, label, domain) {
      self$define(self$asListIdStr(reactId), self$asListIdStr(label), "reactiveValuesAsList", domain)
    },
    defineAsListAll = function(reactId, label, domain) {
      self$define(self$asListAllIdStr(reactId), self$asListAllIdStr(label), "reactiveValuesAsListAll", domain)
    },
    defineKey = function(reactId, key, label, domain) {
      self$define(self$keyIdStr(reactId, key), self$keyIdStr(label, key), "reactiveValuesKey", domain)
    },

    dependsOn = function(reactId, depOnReactId, ctxId, domain) {
      if (is.null(reactId)) return()
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
        label = label,
        type = type,
        prevCtxId = prevCtxId,
        srcref = as.vector(attr(label, "srcref")), srcfile=attr(label, "srcfile")
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

    valueChange = function(reactId, value, display, domain) {
      valueStr <- paste(utils::capture.output(utils::str(value)), collapse="\n")
      if (isTRUE(display)) {
        msg$log("valueChange:", msg$reactStr(reactId), " '",  valueStr, "'")
      } else {
        msg$log("valueChange:", msg$reactStr(reactId))
      }
      private$appendEntry(domain, list(
        action = "valueChange",
        reactId = reactId,
        value = valueStr
      ))
    },
    valueChangeNames = function(reactId, nameValues, domain) {
      self$valueChange(self$namesIdStr(reactId), nameValues, FALSE, domain)
    },
    valueChangeAsList = function(reactId, listValue, domain) {
      self$valueChange(self$asListIdStr(reactId), listValue, FALSE, domain)
    },
    valueChangeAsListAll = function(reactId, listValue, domain) {
      self$valueChange(self$asListAllIdStr(reactId), listValue, FALSE, domain)
    },
    valueChangeKey = function(reactId, key, value, domain) {
      self$valueChange(self$keyIdStr(reactId, key), value, FALSE, domain)
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

MessageLogger = R6Class(
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
      ! isTRUE(getOption(self$option))
    },
    depthIncrement = function() {
      if (self$isNotLogging()) return(NULL)
      self$depth <- self$depth + 1L
    },
    depthDecrement = function() {
      if (self$isNotLogging()) return(NULL)
      self$depth <- self$depth - 1L
    },
    hasReact = function(reactId) {
      if (self$isNotLogging()) return(FALSE)
      !is.null(self$getReact(reactId))
    },
    getReact = function(reactId) {
      if (self$isNotLogging()) return(NULL)
      self$reactCache[[reactId]]
    },
    setReact = function(reactObj) {
      if (self$isNotLogging()) return(NULL)
      self$reactCache[[reactObj$reactId]] <- reactObj
    },
    reactStr = function(reactId) {
      if (self$isNotLogging()) return(NULL)
      reactInfo <- self$getReact(reactId)
      paste0(
        " ", reactInfo$reactId, ":", reactInfo$label
      )
    },
    typeStr = function(type = NULL) {
      self$ctxStr(ctxId = NULL, type = type)
    },
    ctxStr = function(ctxId = NULL, type = NULL) {
      if (self$isNotLogging()) return(NULL)
      self$ctxPrevCtxStr(ctxId = ctxId, prevCtxId = NULL, type = type)
    },
    ctxPrevCtxStr = function(ctxId = NULL, prevCtxId = NULL, type = NULL, preCtxIdTxt = " in ") {
      if (self$isNotLogging()) return(NULL)
      paste0(
        if (!is.null(ctxId)) paste0(preCtxIdTxt, ctxId),
        if (!is.null(prevCtxId)) paste0(" from ", prevCtxId),
        if (!is.null(type) && !identical(type, "other")) paste0(" - ", type)
      )
    },
    log = function(...) {
      if (self$isNotLogging()) return(NULL)
      msg <- paste0(
        paste0(rep("= ", depth), collapse = ""), "- ", paste0(..., collapse = ""),
        collapse = ""
      )
      message(msg)
    }
  )
)

#' @include stack.R
rLog <- NULL
# To be used for initial init and within testing only
initializeReactlog <- function() {
  .globals$reactIdCounter <<- 0L
  rLog <<- RLog$new("shiny.reactlog", "shiny.reactlog.console")
}
initializeReactlog()
