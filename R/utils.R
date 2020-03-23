#' @include globals.R
#' @include map.R
NULL

#' Make a random number generator repeatable
#'
#' Given a function that generates random data, returns a wrapped version of
#' that function that always uses the same seed when called. The seed to use can
#' be passed in explicitly if desired; otherwise, a random number is used.
#'
#' @param rngfunc The function that is affected by the R session's seed.
#' @param seed The seed to set every time the resulting function is called.
#' @return A repeatable version of the function that was passed in.
#'
#' @note When called, the returned function attempts to preserve the R session's
#'   current seed by snapshotting and restoring
#'   [base::.Random.seed()].
#'
#' @examples
#' rnormA <- repeatable(rnorm)
#' rnormB <- repeatable(rnorm)
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(5)  # [1]  1.8285879 -0.7468041 -0.4639111 -1.6510126 -1.4686924
#' rnormB(5)  # [1] -0.7946034  0.2568374 -0.6567597  1.2451387 -0.8375699
#' @export
repeatable <- function(rngfunc, seed = stats::runif(1, 0, .Machine$integer.max)) {
  force(seed)

  function(...) {
    # When we exit, restore the seed to its original state
    if (exists('.Random.seed', where=globalenv())) {
      currentSeed <- get('.Random.seed', pos=globalenv())
      on.exit(assign('.Random.seed', currentSeed, pos=globalenv()))
    }
    else {
      on.exit(rm('.Random.seed', pos=globalenv()))
    }

    set.seed(seed)

    rngfunc(...)
  }
}

.globals$ownSeed <- NULL
# Evaluate an expression using Shiny's own private stream of
# randomness (not affected by set.seed).
withPrivateSeed <- function(expr) {
  # Save the old seed if present.
  if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    hasOrigSeed <- TRUE
    origSeed <- .GlobalEnv$.Random.seed
  } else {
    hasOrigSeed <- FALSE
  }

  # Swap in the private seed.
  if (is.null(.globals$ownSeed)) {
    if (hasOrigSeed) {
      # Move old seed out of the way if present.
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
  } else {
    .GlobalEnv$.Random.seed <- .globals$ownSeed
  }

  # On exit, save the modified private seed, and put the old seed back.
  on.exit({
    .globals$ownSeed <- .GlobalEnv$.Random.seed

    if (hasOrigSeed) {
      .GlobalEnv$.Random.seed <- origSeed
    } else {
      rm(.Random.seed, envir = .GlobalEnv, inherits = FALSE)
    }
    # Need to call this to make sure that the value of .Random.seed gets put
    # into R's internal RNG state. (Issue #1763)
    httpuv::getRNGState()
  })

  expr
}

# Version of runif that runs with private seed
p_runif <- function(...) {
  withPrivateSeed(stats::runif(...))
}

# Version of sample that runs with private seed
p_sample <- function(...) {
  withPrivateSeed(sample(...))
}

# Return a random integral value in the range [min, max).
# If only one argument is passed, then min=0 and max=argument.
randomInt <- function(min, max) {
  if (missing(max)) {
    max <- min
    min <- 0
  }
  if (min < 0 || max <= min)
    stop("Invalid min/max values")

  min + sample(max-min, 1)-1
}

p_randomInt <- function(...) {
  withPrivateSeed(randomInt(...))
}

isWholeNum <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !isTRUE(is.na(x)))
    if (!is.null(y) && !isTRUE(is.na(y)))
      return(y)
  return(NULL)
}

`%.%` <- function(x, y) {
  paste(x, y, sep='')
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
# Given a vector or list, drop all the NULL items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

# Given a vector/list, return TRUE if any elements are named, FALSE otherwise.
anyNamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(FALSE)

  # List with name attribute; check for any ""
  any(nzchar(nms))
}

# Given a vector/list, return TRUE if any elements are unnamed, FALSE otherwise.
anyUnnamed <- function(x) {
  # Zero-length vector
  if (length(x) == 0) return(FALSE)

  nms <- names(x)

  # List with no name attribute
  if (is.null(nms)) return(TRUE)

  # List with name attribute; check for any ""
  any(!nzchar(nms))
}


# Given a vector/list, returns a named vector/list (the labels will be blank).
asNamed <- function(x) {
  if (is.null(names(x))) {
    names(x) <- character(length(x))
  }

  x
}

# Given two named vectors, join them together, and keep only the last element
# with a given name in the resulting vector. If b has any elements with the same
# name as elements in a, the element in a is dropped. Also, if there are any
# duplicated names in a or b, only the last one with that name is kept.
mergeVectors <- function(a, b) {
  if (anyUnnamed(a) || anyUnnamed(b)) {
    stop("Vectors must be either NULL or have names for all elements")
  }

  x <- c(a, b)
  drop_idx <- duplicated(names(x), fromLast = TRUE)
  x[!drop_idx]
}

# Sort a vector by the names of items. If there are multiple items with the
# same name, preserve the original order of those items. For empty
# vectors/lists/NULL, return the original value.
sortByName <- function(x) {
  if (anyUnnamed(x))
    stop("All items must be named")

  # Special case for empty vectors/lists, and NULL
  if (length(x) == 0)
    return(x)

  x[order(names(x))]
}

# Wrapper around list2env with a NULL check. In R <3.2.0, if an empty unnamed
# list is passed to list2env(), it errors. But an empty named list is OK. For
# R >=3.2.0, this wrapper is not necessary.
list2env2 <- function(x, ...) {
  # Ensure that zero-length lists have a name attribute
  if (length(x) == 0)
    attr(x, "names") <- character(0)

  list2env(x, ...)
}

# Combine dir and (file)name into a file path. If a file already exists with a
# name differing only by case, then use it instead.
file.path.ci <- function(...) {
  result <- find.file.ci(...)
  if (!is.null(result))
    return(result)

  # If not found, return the file path that was given to us.
  return(file.path(...))
}

# Does a particular file exist? Case-insensitive for filename, case-sensitive
# for path (on platforms with case-sensitive file system).
file.exists.ci <- function(...) {
  !is.null(find.file.ci(...))
}

# Look for a file, case-insensitive for filename, case-sensitive for path (on
# platforms with case-sensitive filesystem). If found, return the path to the
# file, with the correct case. If not found, return NULL.
find.file.ci <- function(...) {
  default <- file.path(...)
  if (length(default) > 1)
    stop("find.file.ci can only check for one file at a time.")
  if (file.exists(default))
    return(default)

  dir <- dirname(default)
  name <- basename(default)

  # If we got here, then we'll check for a directory with the exact case, and a
  # name with any case.
  all_files <- list.files(dir, all.files=TRUE, full.names=TRUE,
                          include.dirs=TRUE)
  match_idx <- tolower(name) == tolower(basename(all_files))
  matches <- all_files[match_idx]
  if (length(matches) == 0)
    return(NULL)

  return(matches[1])
}

# The function base::dir.exists was added in R 3.2.0, but for backward
# compatibility we need to add this function
dirExists <- function(paths) {
  file.exists(paths) & file.info(paths)$isdir
}

# Removes empty directory (vectorized). This is needed because file.remove()
# on Unix will remove empty directories, but on Windows, it will not. On
# Windows, you would need to use unlink(recursive=TRUE), which is not very
# safe. This function does it safely on Unix and Windows.
dirRemove <- function(path) {
  for (p in path) {
    if (!dirExists(p)) {
      stop("Cannot remove non-existent directory ", p, ".")
    }
    if (length(dir(p, all.files = TRUE, no.. = TRUE)) != 0) {
      stop("Cannot remove non-empty directory ", p, ".")
    }
    result <- unlink(p, recursive = TRUE)
    if (result == 1) {
      stop("Error removing directory ", p, ".")
    }
  }
}

# Attempt to join a path and relative path, and turn the result into a
# (normalized) absolute path. The result will only be returned if it is an
# existing file/directory and is a descendant of dir.
#
# Example:
# resolve("/Users/jcheng", "shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "./shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "shiny/../shiny/")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", ".")  # NULL
# resolve("/Users/jcheng", "..")  # NULL
# resolve("/Users/jcheng", "shiny/..")  # NULL
resolve <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalizePath(abs.path, winslash='/', mustWork=TRUE)
  dir <- normalizePath(dir, winslash='/', mustWork=TRUE)
  # trim the possible trailing slash under Windows (#306)
  if (isWindows()) dir <- sub('/$', '', dir)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
      substr(abs.path, nchar(dir)+1, nchar(dir)+1) != '/') {
    return(NULL)
  }
  return(abs.path)
}

# Given a string, make sure it has a trailing slash.
ensure_trailing_slash <- function(path) {
  if (!grepl("/$", path)) {
    path <- paste0(path, "/")
  }
  path
}


isWindows <- function() .Platform$OS.type == 'windows'

# This is a wrapper for download.file and has the same interface.
# The only difference is that, if the protocol is https, it changes the
# download settings, depending on platform.
download <- function(url, ...) {
  # First, check protocol. If http or https, check platform:
  if (grepl('^https?://', url)) {

    # Check whether we are running R 3.2
    isR32 <- getRversion() >= "3.2"

    # Windows
    if (.Platform$OS.type == "windows") {

      if (isR32) {
        method <- "wininet"
      } else {

        # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
        seti2 <- `::`(utils, 'setInternet2')

        # Check whether we are already using internet2 for internal
        internet2_start <- seti2(NA)

        # If not then temporarily set it
        if (!internet2_start) {
          # Store initial settings, and restore on exit
          on.exit(suppressWarnings(seti2(internet2_start)))

          # Needed for https. Will get warning if setInternet2(FALSE) already run
          # and internet routines are used. But the warnings don't seem to matter.
          suppressWarnings(seti2(TRUE))
        }

        method <- "internal"
      }

      # download.file will complain about file size with something like:
      #       Warning message:
      #         In download.file(url, ...) : downloaded length 19457 != reported length 200
      # because apparently it compares the length with the status code returned (?)
      # so we supress that
      suppressWarnings(utils::download.file(url, method = method, ...))

    } else {
      # If non-Windows, check for libcurl/curl/wget/lynx, then call download.file with
      # appropriate method.

      if (isR32 && capabilities("libcurl")) {
        method <- "libcurl"
      } else if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"

        # curl needs to add a -L option to follow redirects.
        # Save the original options and restore when we exit.
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))

        options(download.file.extra = paste("-L", orig_extra_options))

      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }

      utils::download.file(url, method = method, ...)
    }

  } else {
    utils::download.file(url, ...)
  }
}

getContentType <- function(file, defaultType = 'application/octet-stream') {
  subtype <- ifelse(grepl('[.]html?$', file), 'charset=UTF-8', '')
  mime::guess_type(file, unknown = defaultType, subtype = subtype)
}

# Create a zero-arg function from a quoted expression and environment
# @examples
# makeFunction(body=quote(print(3)))
makeFunction <- function(args = pairlist(), body, env = parent.frame()) {
  eval(call("function", args, body), env)
}

#' Convert an expression to a function
#'
#' This is to be called from another function, because it will attempt to get
#' an unquoted expression from two calls back.
#'
#' If expr is a quoted expression, then this just converts it to a function.
#' If expr is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' If expr was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#
#' @param expr A quoted or unquoted expression, or a function.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#'
#' @examples
#' # Example of a new renderer, similar to renderText
#' # This is something that toolkit authors will do
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- shiny::exprToFunction(expr, env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' \dontrun{
#' # Create an output object
#' output$tripleA <- renderTriple({
#'   values$A
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using isolate()
#' tripleA <- renderTriple({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#' @export
exprToFunction <- function(expr, env=parent.frame(), quoted=FALSE) {
  if (!quoted) {
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }

  # expr is a quoted expression
  makeFunction(body=expr, env=env)
}

#' Install an expression as a function
#'
#' Installs an expression in the given environment as a function, and registers
#' debug hooks so that breakpoints may be set in the function.
#'
#' This function can replace `exprToFunction` as follows: we may use
#' `func <- exprToFunction(expr)` if we do not want the debug hooks, or
#' `installExprFunction(expr, "func")` if we do. Both approaches create a
#' function named `func` in the current environment.
#'
#' @seealso Wraps [exprToFunction()]; see that method's documentation
#'   for more documentation and examples.
#'
#' @param expr A quoted or unquoted expression
#' @param name The name the function should be given
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults to
#'   the name of the calling function.
#' @param wrappedWithLabel,..stacktraceon Advanced use only. For stack manipulation purposes; see
#'   [stacktrace()].
#' @export
installExprFunction <- function(expr, name, eval.env = parent.frame(2),
                                quoted = FALSE,
                                assign.env = parent.frame(1),
                                label = deparse(sys.call(-1)[[1]]),
                                wrappedWithLabel = TRUE,
                                ..stacktraceon = FALSE) {
  if (!quoted) {
    quoted <- TRUE
    expr <- eval(substitute(substitute(expr)), parent.frame())
  }

  func <- exprToFunction(expr, eval.env, quoted)
  if (length(label) > 1) {
    # Just in case the deparsed code is more complicated than we imagine. If we
    # have a label with length > 1 it causes warnings in wrapFunctionLabel.
    label <- paste0(label, collapse = "\n")
  }
  if (wrappedWithLabel) {
    func <- wrapFunctionLabel(func, label, ..stacktraceon = ..stacktraceon)
  } else {
    registerDebugHook(name, assign.env, label)
  }
  assign(name, func, envir = assign.env)
}

#' Parse a GET query string from a URL
#'
#' Returns a named list of key-value pairs.
#'
#' @noMd
#' @param str The query string. It can have a leading \code{"?"} or not.
#' @param nested Whether to parse the query string of as a nested list when it
#'   contains pairs of square brackets \code{[]}. For example, the query
#'   \samp{a[i1][j1]=x&b[i1][j1]=y&b[i2][j1]=z} will be parsed as \code{list(a =
#'   list(i1 = list(j1 = 'x')), b = list(i1 = list(j1 = 'y'), i2 = list(j1 =
#'   'z')))} when \code{nested = TRUE}, and \code{list(`a[i1][j1]` = 'x',
#'   `b[i1][j1]` = 'y', `b[i2][j1]` = 'z')} when \code{nested = FALSE}.
#' @export
#' @examples
#' parseQueryString("?foo=1&bar=b%20a%20r")
#'
#' \dontrun{
#' # Example of usage within a Shiny app
#' function(input, output, session) {
#'
#'   output$queryText <- renderText({
#'     query <- parseQueryString(session$clientData$url_search)
#'
#'     # Ways of accessing the values
#'     if (as.numeric(query$foo) == 1) {
#'       # Do something
#'     }
#'     if (query[["bar"]] == "targetstring") {
#'       # Do something else
#'     }
#'
#'     # Return a string with key-value pairs
#'     paste(names(query), query, sep = "=", collapse=", ")
#'   })
#' }
#' }
#'
parseQueryString <- function(str, nested = FALSE) {
  if (is.null(str) || nchar(str) == 0)
    return(list())

  # Remove leading ?
  if (substr(str, 1, 1) == '?')
    str <- substr(str, 2, nchar(str))

  pairs <- strsplit(str, '&', fixed = TRUE)[[1]]
  # Drop any empty items (if there's leading/trailing/consecutive '&' chars)
  pairs <- pairs[pairs != ""]
  pairs <- strsplit(pairs, '=', fixed = TRUE)

  keys   <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  # Replace NA with '', so they don't get converted to 'NA' by URLdecode
  values[is.na(values)] <- ''

  # Convert "+" to " ", since URLdecode doesn't do it
  keys   <- gsub('+', ' ', keys,   fixed = TRUE)
  values <- gsub('+', ' ', values, fixed = TRUE)

  keys   <- URLdecode(keys)
  values <- URLdecode(values)

  res <- stats::setNames(as.list(values), keys)
  if (!nested) return(res)

  # Make a nested list from a query of the form ?a[1][1]=x11&a[1][2]=x12&...
  for (i in grep('\\[.+\\]', keys)) {
    k <- strsplit(keys[i], '[][]')[[1L]]  # split by [ or ]
    res <- assignNestedList(res, k[k != ''], values[i])
    res[[keys[i]]] <- NULL    # remove res[['a[1][1]']]
  }
  res
}

# Assign value to the bottom element of the list x using recursive indices idx
assignNestedList <- function(x = list(), idx, value) {
  for (i in seq_along(idx)) {
    sub <- idx[seq_len(i)]
    if (is.null(x[[sub]])) x[[sub]] <- list()
  }
  x[[idx]] <- value
  x
}

# decide what to do in case of errors; it is customizable using the shiny.error
# option (e.g. we can set options(shiny.error = recover))
#' @include conditions.R
shinyCallingHandlers <- function(expr) {
  withCallingHandlers(captureStackTraces(expr),
    error = function(e) {
      # Don't intercept shiny.silent.error (i.e. validation errors)
      if (inherits(e, "shiny.silent.error"))
        return()

      handle <- getOption('shiny.error')
      if (is.function(handle)) handle()
    }
  )
}

#' Print message for deprecated functions in Shiny
#'
#' To disable these messages, use `options(shiny.deprecation.messages=FALSE)`.
#'
#' @param new Name of replacement function.
#' @param msg Message to print. If used, this will override the default message.
#' @param old Name of deprecated function.
#' @param version The last version of Shiny before the item was deprecated.
#' @keywords internal
shinyDeprecated <- function(new=NULL, msg=NULL,
                            old=as.character(sys.call(sys.parent()))[1L],
                            version = NULL) {

  if (getOption("shiny.deprecation.messages") %OR% TRUE == FALSE)
    return(invisible())

  if (is.null(msg)) {
    msg <- paste(old, "is deprecated.")
    if (!is.null(new)) {
      msg <- paste(msg, "Please use", new, "instead.",
        "To disable this message, run options(shiny.deprecation.messages=FALSE)")
    }
  }

  if (!is.null(version)) {
    msg <- paste0(msg, " (Last used in version ", version, ")")
  }

  # Similar to .Deprecated(), but print a message instead of warning
  message(msg)
}

#' Register a function with the debugger (if one is active).
#'
#' Call this function after exprToFunction to give any active debugger a hook
#' to set and clear breakpoints in the function. A debugger may implement
#' registerShinyDebugHook to receive callbacks when Shiny functions are
#' instantiated at runtime.
#'
#' @param name Name of the field or object containing the function.
#' @param where The reference object or environment containing the function.
#' @param label A label to display on the function in the debugger.
#' @noRd
registerDebugHook <- function(name, where, label) {
  if (exists("registerShinyDebugHook", mode = "function")) {
    registerShinyDebugHook <- get("registerShinyDebugHook", mode = "function")
    params <- new.env(parent = emptyenv())
    params$name <- name
    params$where <- where
    params$label <- label
    registerShinyDebugHook(params)
  }
}

Callbacks <- R6Class(
  'Callbacks',
  portable = FALSE,
  class = FALSE,
  public = list(
    .nextId = integer(0),
    .callbacks = 'Map',

    initialize = function() {
      # NOTE: we avoid using '.Machine$integer.max' directly
      # as R 3.3.0's 'radixsort' could segfault when sorting
      # an integer vector containing this value
      .nextId <<- as.integer(.Machine$integer.max - 1L)
      .callbacks <<- Map$new()
    },
    register = function(callback) {
      if (!is.function(callback)) {
        stop("callback must be a function")
      }
      id <- as.character(.nextId)
      .nextId <<- .nextId - 1L
      .callbacks$set(id, callback)
      return(function() {
        .callbacks$remove(id)
      })
    },
    invoke = function(..., onError=NULL, ..stacktraceon = FALSE) {
      # Ensure that calls are invoked in the order that they were registered
      keys <- as.character(sort(as.integer(.callbacks$keys()), decreasing = TRUE))
      callbacks <- .callbacks$mget(keys)

      for (callback in callbacks) {
        if (is.null(onError)) {
          if (..stacktraceon) {
            ..stacktraceon..(callback(...))
          } else {
            callback(...)
          }
        } else {
          tryCatch(
            captureStackTraces(
              if (..stacktraceon)
                ..stacktraceon..(callback(...))
              else
                callback(...)
            ),
            error = onError
          )
        }
      }
    },
    count = function() {
      .callbacks$size()
    }
  )
)

# convert a data frame to JSON as required by DataTables request
dataTablesJSON <- function(data, req) {
  n <- nrow(data)
  # DataTables requests were sent via POST
  params <- URLdecode(rawToChar(req$rook.input$read()))
  q <- parseQueryString(params, nested = TRUE)
  ci <- q$search[['caseInsensitive']] == 'true'

  # data may have been replaced/updated in the new table while the Ajax request
  # from the previous table is still on its way, so it is possible that the old
  # request asks for more columns than the current data, in which case we should
  # discard this request and return empty data; the next Ajax request from the
  # new table will retrieve the correct number of columns of data
  if (length(q$columns) != ncol(data)) {
    res <- toJSON(list(
      draw = as.integer(q$draw),
      recordsTotal = n,
      recordsFiltered = 0,
      data = NULL
    ))
    return(httpResponse(200, 'application/json', enc2utf8(res)))
  }

  # global searching
  i <- seq_len(n)
  if (length(q$search[['value']]) && q$search[['value']] != '') {
    i0 <- apply(data, 2, function(x) {
      grep2(q$search[['value']], as.character(x),
            fixed = q$search[['regex']] == 'false', ignore.case = ci)
    })
    i <- intersect(i, unique(unlist(i0)))
  }

  # search by columns
  if (length(i)) for (j in names(q$columns)) {
    col <- q$columns[[j]]
    # if the j-th column is not searchable or the search string is "", skip it
    if (col[['searchable']] != 'true') next
    if ((k <- col[['search']][['value']]) == '') next
    j <- as.integer(j)
    dj <- data[, j + 1]
    r  <- commaToRange(k)
    ij <- if (length(r) == 2 && is.numeric(dj)) {
      which(dj >= r[1] & dj <= r[2])
    } else {
      grep2(k, as.character(dj), fixed = col[['search']][['regex']] == 'false',
            ignore.case = ci)
    }
    i <- intersect(ij, i)
    if (length(i) == 0) break
  }
  if (length(i) != n) data <- data[i, , drop = FALSE]

  # sorting
  oList <- list()
  for (ord in q$order) {
    k <- ord[['column']]  # which column to sort
    d <- ord[['dir']]     # direction asc/desc
    if (q$columns[[k]][['orderable']] != 'true') next
    col <- data[, as.integer(k) + 1]
    oList[[length(oList) + 1]] <- (if (d == 'asc') identity else `-`)(
      if (is.numeric(col)) col else xtfrm(col)
    )
  }
  if (length(oList)) {
    i <- do.call(order, oList)
    data <- data[i, , drop = FALSE]
  }
  # paging
  if (q$length != '-1') {
    i <- seq(as.integer(q$start) + 1L, length.out = as.integer(q$length))
    i <- i[i <= nrow(data)]
    fdata <- data[i, , drop = FALSE]  # filtered data
  } else fdata <- data

  fdata <- unname(as.matrix(fdata))
  if (is.character(fdata) && q$escape != 'false') {
    if (q$escape == 'true') {
      # fdata must be a matrix at this point, and we need to preserve
      # dimensions. Note that it could be a 1xn matrix.
      dims <- dim(fdata)
      fdata <- htmlEscape(fdata)
      dim(fdata) <- dims

    } else {
      k <- as.integer(strsplit(q$escape, ',')[[1]])
      # use seq_len() in case escape = negative indices, e.g. c(-1, -5)
      for (j in seq_len(ncol(fdata))[k]) fdata[, j] <- htmlEscape(fdata[, j])
    }
  }

  res <- toJSON(list(
    draw = as.integer(q$draw),
    recordsTotal = n,
    recordsFiltered = nrow(data),
    data = fdata
  ))
  httpResponse(200, 'application/json', enc2utf8(res))
}

# when both ignore.case and fixed are TRUE, we use grep(ignore.case = FALSE,
# fixed = TRUE) to do lower-case matching of pattern on x
grep2 <- function(pattern, x, ignore.case = FALSE, fixed = FALSE, ...) {
  if (fixed && ignore.case) {
    pattern <- tolower(pattern)
    x <- tolower(x)
    ignore.case <- FALSE
  }
  # when the user types in the search box, the regular expression may not be
  # complete before it is sent to the server, in which case we do not search
  if (!fixed && inherits(try(grep(pattern, ''), silent = TRUE), 'try-error'))
    return(seq_along(x))
  grep(pattern, x, ignore.case = ignore.case, fixed = fixed, ...)
}

getExists <- function(x, mode, envir = parent.frame()) {
  if (exists(x, envir = envir, mode = mode, inherits = FALSE))
    get(x, envir = envir, mode = mode, inherits = FALSE)
}

# convert a string of the form "lower,upper" to c(lower, upper)
commaToRange <- function(string) {
  if (!grepl(',', string)) return()
  r <- strsplit(string, ',')[[1]]
  if (length(r) > 2) return()
  if (length(r) == 1) r <- c(r, '')  # lower,
  r <- as.numeric(r)
  if (is.na(r[1])) r[1] <- -Inf
  if (is.na(r[2])) r[2] <- Inf
  r
}

# for options passed to DataTables/Selectize/..., the options of the class AsIs
# will be evaluated as literal JavaScript code
checkAsIs <- function(options) {
  evalOptions <- if (length(options)) {
    nms <- names(options)
    if (length(nms) == 0L || any(nms == '')) stop("'options' must be a named list")
    i <- unlist(lapply(options, function(x) {
      is.character(x) && inherits(x, 'AsIs')
    }))
    if (any(i)) {
      # must convert to character, otherwise toJSON() turns it to an array []
      options[i] <- lapply(options[i], paste, collapse = '\n')
      nms[i]  # options of these names will be evaluated in JS
    }
  }
  list(options = options, eval = evalOptions)
}

srcrefFromShinyCall <- function(expr) {
  srcrefs <- attr(expr, "srcref")
  num_exprs <- length(srcrefs)
  if (num_exprs < 1)
    return(NULL)
  c(srcrefs[[1]][1], srcrefs[[1]][2],
    srcrefs[[num_exprs]][3], srcrefs[[num_exprs]][4],
    srcrefs[[1]][5], srcrefs[[num_exprs]][6])
}

# Indicates whether the given querystring should cause the associated request
# to be handled in showcase mode. Returns the showcase mode if set, or NULL
# if no showcase mode is set.
showcaseModeOfQuerystring <- function(querystring) {
  if (nchar(querystring) > 0) {
    qs <- parseQueryString(querystring)
    if (exists("showcase", where = qs)) {
      return(as.numeric(qs$showcase))
    }
  }
  return(NULL)
}

showcaseModeOfReq <- function(req) {
  showcaseModeOfQuerystring(req$QUERY_STRING)
}

# Returns (just) the filename containing the given source reference, or an
# empty string if the source reference doesn't include file information.
srcFileOfRef <- function(srcref) {
  fileEnv <- attr(srcref, "srcfile")
  # The 'srcfile' attribute should be a non-null environment containing the
  # variable 'filename', which gives the full path to the source file.
  if (!is.null(fileEnv) &&
      is.environment(fileEnv) &&
      exists("filename", where = fileEnv))
    basename(fileEnv[["filename"]])
  else
    ""
}

# Format a number without sci notation, and keep as many digits as possible (do
# we really need to go beyond 15 digits?)
formatNoSci <- function(x) {
  if (is.null(x)) return(NULL)
  format(x, scientific = FALSE, digits = 15)
}

# Returns a function that calls the given func and caches the result for
# subsequent calls, unless the given file's mtime changes.
cachedFuncWithFile <- function(dir, file, func, case.sensitive = FALSE) {
  dir <- normalizePath(dir, mustWork=TRUE)
  mtime <- NA
  value <- NULL
  function(...) {
    fname <- if (case.sensitive)
      file.path(dir, file)
    else
      file.path.ci(dir, file)

    now <- file.info(fname)$mtime
    if (!identical(mtime, now)) {
      value <<- func(fname, ...)
      mtime <<- now
    }
    value
  }
}

# turn column-based data to row-based data (mainly for JSON), e.g. data.frame(x
# = 1:10, y = 10:1) ==> list(list(x = 1, y = 10), list(x = 2, y = 9), ...)
columnToRowData <- function(data) {
  do.call(
    mapply, c(
      list(FUN = function(...) list(...), SIMPLIFY = FALSE, USE.NAMES = FALSE),
      as.list(data)
    )
  )
}

#' Declare an error safe for the user to see
#'
#' This should be used when you want to let the user see an error
#' message even if the default is to sanitize all errors. If you have an
#' error `e` and call `stop(safeError(e))`, then Shiny will
#' ignore the value of `getOption("shiny.sanitize.errors")` and always
#' display the error in the app itself.
#'
#' @param error Either an "error" object or a "character" object (string).
#' In the latter case, the string will become the message of the error
#' returned by `safeError`.
#'
#' @return An "error" object
#'
#' @details An error generated by `safeError` has priority over all
#' other Shiny errors. This can be dangerous. For example, if you have set
#' `options(shiny.sanitize.errors = TRUE)`, then by default all error
#' messages are omitted in the app, and replaced by a generic error message.
#' However, this does not apply to `safeError`: whatever you pass
#' through `error` will be displayed to the user. So, this should only
#' be used when you are sure that your error message does not contain any
#' sensitive information. In those situations, `safeError` can make
#' your users' lives much easier by giving them a hint as to where the
#' error occurred.
#'
#' @seealso [shiny-options()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#' # uncomment the desired line to experiment with shiny.sanitize.errors
#' # options(shiny.sanitize.errors = TRUE)
#' # options(shiny.sanitize.errors = FALSE)
#'
#' # Define UI
#' ui <- fluidPage(
#'   textInput('number', 'Enter your favorite number from 1 to 10', '5'),
#'   textOutput('normalError'),
#'   textOutput('safeError')
#' )
#'
#' # Server logic
#' server <- function(input, output) {
#'   output$normalError <- renderText({
#'     number <- input$number
#'     if (number %in% 1:10) {
#'       return(paste('You chose', number, '!'))
#'     } else {
#'       stop(
#'         paste(number, 'is not a number between 1 and 10')
#'       )
#'     }
#'   })
#'   output$safeError <- renderText({
#'     number <- input$number
#'     if (number %in% 1:10) {
#'       return(paste('You chose', number, '!'))
#'     } else {
#'       stop(safeError(
#'         paste(number, 'is not a number between 1 and 10')
#'       ))
#'     }
#'   })
#' }
#'
#' # Complete app with UI and server components
#' shinyApp(ui, server)
#' }
#' @export
safeError <- function(error) {
  if (inherits(error, "character")) {
    error <- simpleError(error)
  }
  if (!inherits(error, "error")) {
    stop("The class of the `error` parameter must be either 'error' or 'character'")
  }
  class(error) <- c("shiny.custom.error", class(error))
  error
}

#***********************************************************************#
#**** Keep this function internal for now, may chnage in the future ****#
#***********************************************************************#
# #' Propagate an error through Shiny, but catch it before it throws
# #'
# #' Throws a type of exception that is caught by observers. When such an
# #' exception is triggered, all reactive links are broken. So, essentially,
# #' \code{reactiveStop()} behaves just like \code{stop()}, except that
# #' instead of ending the session, it is silently swalowed by Shiny.
# #'
# #' This function should be used when you want to disrupt the reactive
# #' links in a reactive chain, but do not want to end the session. For
# #' example, this enables you to disallow certain inputs, but get back
# #' to business as usual when valid inputs are re-entered.
# #' \code{reactiveStop} is also called internally by Shiny to create
# #' special errors, such as the ones generated by \code{\link{validate}()},
# #' \code{\link{req}()} and \code{\link{cancelOutput}()}.
# #'
# #' @param message An optional error message.
# #' @param class An optional class to add to the error.
# #' @export
# #' @examples
# #' ## Note: the breaking of the reactive chain that happens in the app
# #' ## below (when input$txt = 'bad' and input$allowBad = 'FALSE') is
# #' ## easily visualized with `reactlogShow()`
# #'
# #' ## Only run examples in interactive R sessions
# #' if (interactive()) {
# #'
# #' ui <- fluidPage(
# #'   textInput('txt', 'Enter some text...'),
# #'   selectInput('allowBad', 'Allow the string \'bad\'?',
# #'               c('TRUE', 'FALSE'), selected = 'FALSE')
# #' )
# #'
# #' server <- function(input, output) {
# #'   val <- reactive({
# #'     if (!(as.logical(input$allowBad))) {
# #'       if (identical(input$txt, "bad")) {
# #'         reactiveStop()
# #'       }
# #'     }
## '   })
# #'
# #'   observe({
# #'     val()
# #'   })
# #' }
# #'
# #' shinyApp(ui, server)
# #' }
# #' @export
reactiveStop <- function(message = "", class = NULL) {
  stopWithCondition(c("shiny.silent.error", class), message)
}

#' Validate input values and other conditions
#'
#' For an output rendering function (e.g. [renderPlot()]), you may
#' need to check that certain input values are available and valid before you
#' can render the output. `validate` gives you a convenient mechanism for
#' doing so.
#'
#' The `validate` function takes any number of (unnamed) arguments, each of
#' which represents a condition to test. If any of the conditions represent
#' failure, then a special type of error is signaled which stops execution. If
#' this error is not handled by application-specific code, it is displayed to
#' the user by Shiny.
#'
#' An easy way to provide arguments to `validate` is to use the `need`
#' function, which takes an expression and a string; if the expression is
#' considered a failure, then the string will be used as the error message. The
#' `need` function considers its expression to be a failure if it is any of
#' the following:
#'
#' \itemize{
#'   \item{`FALSE`}
#'   \item{`NULL`}
#'   \item{`""`}
#'   \item{An empty atomic vector}
#'   \item{An atomic vector that contains only missing values}
#'   \item{A logical vector that contains all `FALSE` or missing values}
#'   \item{An object of class `"try-error"`}
#'   \item{A value that represents an unclicked [actionButton()]}
#' }
#'
#' If any of these values happen to be valid, you can explicitly turn them to
#' logical values. For example, if you allow `NA` but not `NULL`, you
#' can use the condition `!is.null(input$foo)`, because `!is.null(NA)
#' == TRUE`.
#'
#' If you need validation logic that differs significantly from `need`, you
#' can create other validation test functions. A passing test should return
#' `NULL`. A failing test should return an error message as a
#' single-element character vector, or if the failure should happen silently,
#' `FALSE`.
#'
#' Because validation failure is signaled as an error, you can use
#' `validate` in reactive expressions, and validation failures will
#' automatically propagate to outputs that use the reactive expression. In
#' other words, if reactive expression `a` needs `input$x`, and two
#' outputs use `a` (and thus depend indirectly on `input$x`), it's
#' not necessary for the outputs to validate `input$x` explicitly, as long
#' as `a` does validate it.
#'
#' @param ... A list of tests. Each test should equal `NULL` for success,
#'   `FALSE` for silent failure, or a string for failure with an error
#'   message.
#' @param errorClass A CSS class to apply. The actual CSS string will have
#'   `shiny-output-error-` prepended to this value.
#' @export
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#' options(device.ask.default = FALSE)
#'
#' ui <- fluidPage(
#'   checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
#'   selectizeInput('in2', 'Select a state', choices = state.name),
#'   plotOutput('plot')
#' )
#'
#' server <- function(input, output) {
#'   output$plot <- renderPlot({
#'     validate(
#'       need(input$in1, 'Check at least one letter!'),
#'       need(input$in2 != '', 'Please choose a state.')
#'     )
#'     plot(1:10, main = paste(c(input$in1, input$in2), collapse = ', '))
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' }
validate <- function(..., errorClass = character(0)) {
  results <- sapply(list(...), function(x) {
    # Detect NULL or NA
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\n"))
    else
      stop("Unexpected validation result: ", as.character(x))
  })

  results <- stats::na.omit(results)
  if (length(results) == 0)
    return(invisible())

  # There may be empty strings remaining; these are message-less failures that
  # started as FALSE
  results <- results[nzchar(results)]
  reactiveStop(paste(results, collapse="\n"), c(errorClass, "validation"))
}

#' @param expr An expression to test. The condition will pass if the expression
#'   meets the conditions spelled out in Details.
#' @param message A message to convey to the user if the validation condition is
#'   not met. If no message is provided, one will be created using `label`.
#'   To fail with no message, use `FALSE` for the message.
#' @param label A human-readable name for the field that may be missing. This
#'   parameter is not needed if `message` is provided, but must be provided
#'   otherwise.
#' @export
#' @rdname validate
need <- function(expr, message = paste(label, "must be provided"), label) {

  force(message) # Fail fast on message/label both being missing

  if (!isTruthy(expr))
    return(message)
  else
    return(invisible(NULL))
}

#' Check for required values
#'
#' Ensure that values are available ("truthy"--see Details) before proceeding
#' with a calculation or action. If any of the given values is not truthy, the
#' operation is stopped by raising a "silent" exception (not logged by Shiny,
#' nor displayed in the Shiny app's UI).
#'
#' The `req` function was designed to be used in one of two ways. The first
#' is to call it like a statement (ignoring its return value) before attempting
#' operations using the required values:
#'
#' \preformatted{rv <- reactiveValues(state = FALSE)
#' r <- reactive({
#'   req(input$a, input$b, rv$state)
#'   # Code that uses input$a, input$b, and/or rv$state...
#' })}
#'
#' In this example, if `r()` is called and any of `input$a`,
#' `input$b`, and `rv$state` are `NULL`, `FALSE`, `""`,
#' etc., then the `req` call will trigger an error that propagates all the
#' way up to whatever render block or observer is executing.
#'
#' The second is to use it to wrap an expression that must be truthy:
#'
#' \preformatted{output$plot <- renderPlot({
#'   if (req(input$plotType) == "histogram") {
#'     hist(dataset())
#'   } else if (input$plotType == "scatter") {
#'     qplot(dataset(), aes(x = x, y = y))
#'   }
#' })}
#'
#' In this example, `req(input$plotType)` first checks that
#' `input$plotType` is truthy, and if so, returns it. This is a convenient
#' way to check for a value "inline" with its first use.
#'
#' **Truthy and falsy values**
#'
#' The terms "truthy" and "falsy" generally indicate whether a value, when
#' coerced to a [base::logical()], is `TRUE` or `FALSE`. We use
#' the term a little loosely here; our usage tries to match the intuitive
#' notions of "Is this value missing or available?", or "Has the user provided
#' an answer?", or in the case of action buttons, "Has the button been
#' clicked?".
#'
#' For example, a `textInput` that has not been filled out by the user has
#' a value of `""`, so that is considered a falsy value.
#'
#' To be precise, `req` considers a value truthy *unless* it is one
#' of:
#'
#' \itemize{
#'   \item{`FALSE`}
#'   \item{`NULL`}
#'   \item{`""`}
#'   \item{An empty atomic vector}
#'   \item{An atomic vector that contains only missing values}
#'   \item{A logical vector that contains all `FALSE` or missing values}
#'   \item{An object of class `"try-error"`}
#'   \item{A value that represents an unclicked [actionButton()]}
#' }
#'
#' Note in particular that the value `0` is considered truthy, even though
#' `as.logical(0)` is `FALSE`.
#'
#' If the built-in rules for truthiness do not match your requirements, you can
#' always work around them. Since `FALSE` is falsy, you can simply provide
#' the results of your own checks to `req`:
#'
#' `req(input$a != 0)`
#'
#' **Using `req(FALSE)`**
#'
#' You can use `req(FALSE)` (i.e. no condition) if you've already performed
#' all the checks you needed to by that point and just want to stop the reactive
#' chain now. There is no advantange to this, except perhaps ease of readibility
#' if you have a complicated condition to check for (or perhaps if you'd like to
#' divide your condition into nested `if` statements).
#'
#' **Using `cancelOutput = TRUE`**
#'
#' When `req(..., cancelOutput = TRUE)` is used, the "silent" exception is
#' also raised, but it is treated slightly differently if one or more outputs are
#' currently being evaluated. In those cases, the reactive chain does not proceed
#' or update, but the output(s) are left is whatever state they happen to be in
#' (whatever was their last valid state).
#'
#' Note that this is always going to be the case if
#' this is used inside an output context (e.g. `output$txt <- ...`). It may
#' or may not be the case if it is used inside a non-output context (e.g.
#' [reactive()], [observe()] or [observeEvent()])
#' --- depending on whether or not there is an `output$...` that is triggered
#' as a result of those calls. See the examples below for concrete scenarios.
#'
#' @param ... Values to check for truthiness.
#' @param cancelOutput If `TRUE` and an output is being evaluated, stop
#'   processing as usual but instead of clearing the output, leave it in
#'   whatever state it happens to be in.
#' @param x An expression whose truthiness value we want to determine
#' @return The first value that was passed in.
#' @export
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'   ui <- fluidPage(
#'     textInput('data', 'Enter a dataset from the "datasets" package', 'cars'),
#'     p('(E.g. "cars", "mtcars", "pressure", "faithful")'), hr(),
#'     tableOutput('tbl')
#'   )
#'
#'   server <- function(input, output) {
#'     output$tbl <- renderTable({
#'
#'       ## to require that the user types something, use: `req(input$data)`
#'       ## but better: require that input$data is valid and leave the last
#'       ## valid table up
#'       req(exists(input$data, "package:datasets", inherits = FALSE),
#'           cancelOutput = TRUE)
#'
#'       head(get(input$data, "package:datasets", inherits = FALSE))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
req <- function(..., cancelOutput = FALSE) {
  dotloop(function(item) {
    if (!isTruthy(item)) {
      if (isTRUE(cancelOutput)) {
        cancelOutput()
      } else {
        reactiveStop(class = "validation")
      }
    }
  }, ...)

  if (!missing(..1))
    ..1
  else
    invisible()
}

#***********************************************************************#
#**** Keep this function internal for now, may chnage in the future ****#
#***********************************************************************#
# #' Cancel processing of the current output
# #'
# #' Signals an error that Shiny treats specially if an output is currently being
# #' evaluated. Execution will stop, but rather than clearing the output (as
# #' \code{\link{req}} does) or showing an error message (as \code{\link{stop}}
# #' does), the output simply remains unchanged.
# #'
# #' If \code{cancelOutput} is called in any non-output context (like in an
# #' \code{\link{observe}} or \code{\link{observeEvent}}), the effect is the same
# #' as \code{\link{req}(FALSE)}.
# #' @export
# #' @examples
# #' ## Only run examples in interactive R sessions
# #' if (interactive()) {
# #'
# #' # uncomment the desired line to experiment with cancelOutput() vs. req()
# #'
# #' ui <- fluidPage(
# #'   textInput('txt', 'Enter text'),
# #'   textOutput('check')
# #' )
# #'
# #' server <- function(input, output) {
# #'   output$check <- renderText({
# #'     # req(input$txt)
# #'     if (input$txt == 'hi') return('hi')
# #'     else if (input$txt == 'bye') return('bye')
# #'     # else cancelOutput()
# #'   })
# #' }
# #'
# #' shinyApp(ui, server)
# #' }
cancelOutput <- function() {
  reactiveStop(class = "shiny.output.cancel")
}

# Execute a function against each element of ..., but only evaluate each element
# after the previous element has been passed to fun_. The return value of fun_
# is discarded, and only invisible() is returned from dotloop.
#
# Can be used to facilitate short-circuit eval on dots.
dotloop <- function(fun_, ...) {
  for (i in 1:(nargs()-1)) {
    fun_(eval(as.symbol(paste0("..", i))))
  }
  invisible()
}

#' @export
#' @rdname req
isTruthy <- function(x) {
  if (inherits(x, 'try-error'))
    return(FALSE)

  if (!is.atomic(x))
    return(TRUE)

  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(stats::na.omit(x))))
    return(FALSE)
  if (inherits(x, 'shinyActionButtonValue') && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(stats::na.omit(x)))
    return(FALSE)

  return(TRUE)
}

# add class(es) to the error condition, which will be used as names of CSS
# classes, e.g. shiny-output-error shiny-output-error-validation
stopWithCondition <- function(class, message) {
  cond <- structure(
    list(message = message),
    class = c(class, 'error', 'condition')
  )
  stop(cond)
}

#' Collect information about the Shiny Server environment
#'
#' This function returns the information about the current Shiny Server, such as
#' its version, and whether it is the open source edition or professional
#' edition. If the app is not served through the Shiny Server, this function
#' just returns `list(shinyServer = FALSE)`.
#'
#' This function will only return meaningful data when using Shiny Server
#' version 1.2.2 or later.
#' @export
#' @return A list of the Shiny Server information.
serverInfo <- function() {
  .globals$serverInfo
}
.globals$serverInfo <- list(shinyServer = FALSE)

setServerInfo <- function(...) {
  infoOld <- serverInfo()
  infoNew <- list(...)
  infoOld[names(infoNew)] <- infoNew
  .globals$serverInfo <- infoOld
}

# assume file is encoded in UTF-8, but warn against BOM
checkEncoding <- function(file) {
  # skip *nix because its locale is normally UTF-8 based (e.g. en_US.UTF-8), and
  # *nix users have to make a conscious effort to save a file with an encoding
  # that is not UTF-8; if they choose to do so, we cannot do much about it
  # except sitting back and seeing them punished after they choose to escape a
  # world of consistency (falling back to getOption('encoding') will not help
  # because native.enc is also normally UTF-8 based on *nix)
  if (!isWindows()) return('UTF-8')
  size <- file.info(file)[, 'size']
  if (is.na(size)) stop('Cannot access the file ', file)
  # BOM is 3 bytes, so if the file contains BOM, it must be at least 3 bytes
  if (size < 3L) return('UTF-8')

  # check if there is a BOM character: this is also skipped on *nix, because R
  # on *nix simply ignores this meaningless character if present, but it hurts
  # on Windows
  if (identical(charToRaw(readChar(file, 3L, TRUE)), charToRaw('\UFEFF'))) {
    warning('You should not include the Byte Order Mark (BOM) in ', file, '. ',
            'Please re-save it in UTF-8 without BOM. See ',
            'http://shiny.rstudio.com/articles/unicode.html for more info.')
    return('UTF-8-BOM')
  }
  x <- readChar(file, size, useBytes = TRUE)
  if (is.na(iconv(x, 'UTF-8', 'UTF-8'))) {
    warning('The input file ', file, ' does not seem to be encoded in UTF8')
  }
  'UTF-8'
}

# read a file using UTF-8 and (on Windows) convert to native encoding if possible
readUTF8 <- function(file) {
  enc <- checkEncoding(file)
  file <- base::file(file, encoding = enc)
  on.exit(close(file), add = TRUE)
  x <- enc2utf8(readLines(file, warn = FALSE))
  tryNativeEncoding(x)
}

# if the UTF-8 string can be represented in the native encoding, use native encoding
tryNativeEncoding <- function(string) {
  if (!isWindows()) return(string)
  string2 <- enc2native(string)
  if (identical(enc2utf8(string2), string)) string2 else string
}

# similarly, try to source() a file with UTF-8
sourceUTF8 <- function(file, envir = globalenv()) {
  lines <- readUTF8(file)
  enc <- if (any(Encoding(lines) == 'UTF-8')) 'UTF-8' else 'unknown'
  src <- srcfilecopy(file, lines, isFile = TRUE)  # source reference info
  # oddly, parse(file) does not work when file contains multibyte chars that
  # **can** be encoded natively on Windows (might be a bug in base R); we
  # rewrite the source code in a natively encoded temp file and parse it in this
  # case (the source reference is still pointed to the original file, though)
  if (isWindows() && enc == 'unknown') {
    file <- tempfile(); on.exit(unlink(file), add = TRUE)
    writeLines(lines, file)
  }
  exprs <- try(parse(file, keep.source = FALSE, srcfile = src, encoding = enc))
  if (inherits(exprs, "try-error")) {
    diagnoseCode(file)
    stop("Error sourcing ", file)
  }

  # Wrap the exprs in first `{`, then ..stacktraceon..(). It's only really the
  # ..stacktraceon..() that we care about, but the `{` is needed to make that
  # possible.
  exprs <- makeCall(`{`, exprs)
  # Need to wrap exprs in a list because we want it treated as a single argument
  exprs <- makeCall(..stacktraceon.., list(exprs))

  eval(exprs, envir)
}

# @param func Name of function, in unquoted form
# @param args An evaluated list of unevaluated argument expressions
makeCall <- function(func, args) {
  as.call(c(list(substitute(func)), args))
}

# a workaround for https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16264
srcfilecopy <- function(filename, lines, ...) {
  if (getRversion() > '3.2.2') return(base::srcfilecopy(filename, lines, ...))
  src <- base::srcfilecopy(filename, lines = '', ...)
  src$lines <- lines
  src
}

# write text as UTF-8
writeUTF8 <- function(text, ...) {
  text <- enc2utf8(text)
  writeLines(text, ..., useBytes = TRUE)
}

URLdecode <- function(value) {
  decodeURIComponent(value)
}

URLencode <- function(value, reserved = FALSE) {
  value <- enc2utf8(value)
  if (reserved) encodeURIComponent(value) else encodeURI(value)
}

# Make user-supplied dates are either NULL or can be coerced
# to a yyyy-mm-dd formatted string. If a date is specified, this
# function returns a string for consistency across locales.
# Also, `as.Date()` is used to coerce strings to date objects
# so that strings like "2016-08-9" are expanded to "2016-08-09"
dateYMD <- function(date = NULL, argName = "value") {
  if (!length(date)) return(NULL)
  if (length(date) > 1) warning("Expected `", argName, "` to be of length 1.")
  tryCatch(date <- format(as.Date(date), "%Y-%m-%d"),
    error = function(e) {
      warning(
        "Couldn't coerce the `", argName,
        "` argument to a date string with format yyyy-mm-dd",
        call. = FALSE
      )
    }
  )
  date
}

# This function takes a name and function, and it wraps that function in a new
# function which calls the original function using the specified name. This can
# be helpful for profiling, because the specified name will show up on the stack
# trace.
wrapFunctionLabel <- function(func, name, ..stacktraceon = FALSE) {
  if (name == "name" || name == "func" || name == "relabelWrapper") {
    stop("Invalid name for wrapFunctionLabel: ", name)
  }
  assign(name, func, environment())
  registerDebugHook(name, environment(), name)

  relabelWrapper <- eval(substitute(
    function(...) {
      # This `f` gets renamed to the value of `name`. Note that it may not
      # print as the new name, because of source refs stored in the function.
      if (..stacktraceon)
        ..stacktraceon..(f(...))
      else
        f(...)
    },
    list(f = as.name(name))
  ))

  relabelWrapper
}


# This is a very simple mutable object which only stores one value
# (which we can set and get). Using this class is sometimes useful
# when communicating persistent changes across functions.
Mutable <- R6Class("Mutable",
  private = list(
    value = NULL
  ),
  public = list(
    set = function(value) { private$value <- value },
    get = function() { private$value }
  )
)

# More convenient way of chaining together promises than then/catch/finally,
# without the performance impact of %...>%.
promise_chain <- function(promise, ..., catch = NULL, finally = NULL,
  domain = NULL, replace = FALSE) {

  do <- function() {
    p <- Reduce(function(memo, func) {
      promises::then(memo, func)
    }, list(...), promise)

    if (!is.null(catch)) {
      p <- promises::catch(p, catch)
    }

    if (!is.null(finally)) {
      p <- promises::finally(p, finally)
    }

    p
  }

  if (!is.null(domain)) {
    promises::with_promise_domain(domain, do(), replace = replace)
  } else {
    do()
  }
}

# Like promise_chain, but if `expr` returns a non-promise, then `...`, `catch`,
# and `finally` are all executed synchronously
hybrid_chain <- function(expr, ..., catch = NULL, finally = NULL,
  domain = NULL, replace = FALSE) {

  do <- function() {
    runFinally <- TRUE
    tryCatch(
      {
        captureStackTraces({
          result <- withVisible(force(expr))
          if (promises::is.promising(result$value)) {
            # Purposefully NOT including domain (nor replace), as we're already in
            # the domain at this point
            p <- promise_chain(setVisible(result), ..., catch = catch, finally = finally)
            runFinally <- FALSE
            p
          } else {
            result <- Reduce(function(v, func) {
              if (".visible" %in% names(formals(func))) {
                withVisible(func(v$value, .visible = v$visible))
              } else {
                withVisible(func(v$value))
              }
            }, list(...), result)

            setVisible(result)
          }
        })
      },
      error = function(e) {
        if (!is.null(catch))
          catch(e)
        else
          stop(e)
      },
      finally = if (runFinally && !is.null(finally)) finally()
    )
  }

  if (!is.null(domain)) {
    promises::with_promise_domain(domain, do(), replace = replace)
  } else {
    do()
  }
}

# Returns `value` with either `invisible()` applied or not, depending on the
# value of `visible`.
#
# If the `visible` is missing, then `value` should be a list as returned from
# `withVisible()`, and that visibility will be applied.
setVisible <- function(value, visible) {
  if (missing(visible)) {
    visible <- value$visible
    value <- value$value
  }

  if (!visible) {
    invisible(value)
  } else {
    (value)
  }
}

createVarPromiseDomain <- function(env, name, value) {
  force(env)
  force(name)
  force(value)

  promises::new_promise_domain(
    wrapOnFulfilled = function(onFulfilled) {
      function(...) {
        orig <- env[[name]]
        env[[name]] <- value
        on.exit(env[[name]] <- orig)

        onFulfilled(...)
      }
    },
    wrapOnRejected = function(onRejected) {
      function(...) {
        orig <- env[[name]]
        env[[name]] <- value
        on.exit(env[[name]] <- orig)

        onRejected(...)
      }
    },
    wrapSync = function(expr) {
      orig <- env[[name]]
      env[[name]] <- value
      on.exit(env[[name]] <- orig)

      force(expr)
    }
  )
}

getSliderType <- function(min, max, value) {
  vals <- dropNulls(list(value, min, max))
  if (length(vals) == 0) return("")
  type <- unique(lapply(vals, function(x) {
    if      (inherits(x, "Date"))   "date"
    else if (inherits(x, "POSIXt")) "datetime"
    else                            "number"
  }))
  if (length(type) > 1) {
    stop("Type mismatch for `min`, `max`, and `value`. Each must be Date, POSIXt, or number.")
  }
  type[[1]]
}

# Reads the `shiny.sharedSecret` global option, and returns a function that can
# be used to test header values for a match.
loadSharedSecret <- function() {
  normalizeToRaw <- function(value, label = "value") {
    if (is.null(value)) {
      raw()
    } else if (is.character(value)) {
      charToRaw(paste(value, collapse = "\n"))
    } else if (is.raw(value)) {
      value
    } else {
      stop("Wrong type for ", label, "; character or raw expected")
    }
  }

  sharedSecret <- normalizeToRaw(getOption("shiny.sharedSecret"))
  if (is.null(sharedSecret)) {
    function(x) TRUE
  } else {
    # We compare the digest of the two values so that their lengths are equalized
    function(x) {
      x <- normalizeToRaw(x)
      # Constant time comparison to avoid timing attacks
      constantTimeEquals(sharedSecret, x)
    }
  }
}

# Compares two raw vectors of equal length for equality, in constant time
constantTimeEquals <- function(raw1, raw2) {
  stopifnot(is.raw(raw1))
  stopifnot(is.raw(raw2))
  if (length(raw1) != length(raw2)) {
    return(FALSE)
  }

  sum(as.integer(xor(raw1, raw2))) == 0
}

cat_line <- function(...) {
  cat(paste(..., "\n", collapse = ""))
}

