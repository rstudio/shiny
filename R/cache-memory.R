#' Create a memory cache object
#'
#' A memory cache object is a key-value store that saves the values in an
#' environment. Objects can be stored and retrieved using the \code{get()} and
#' \code{set()} methods. Objects are automatically pruned from the cache
#' according to the parameters \code{max_size}, \code{max_age}, \code{max_n},
#' and \code{evict}.
#'
#' In a \code{MemoryCache}, R objects are stored directly in the cache; they are
#' not \emph{not} serialized before being stored in the cache. This contrasts
#' with other cache types, like \code{\link{diskCache}}, where objects are
#' serialized, and the serialized object is cached. This can result in some
#' differences of behavior. For example, as long as an object is stored in a
#' MemoryCache, it will not be garbage collected.
#'
#'
#' @section Missing keys:
#'   The \code{missing} and \code{exec_missing} parameters controls what happens
#'   when \code{get()} is called with a key that is not in the cache (a cache
#'   miss). The default behavior is to return a \code{\link{key_missing}}
#'   object. This is a \emph{sentinel value} that indicates that the key was not
#'   present in the cache. You can test if the returned value represents a
#'   missing key by using the \code{\link{is.key_missing}} function. You can
#'   also have \code{get()} return a different sentinel value, like \code{NULL}.
#'   If you want to throw an error on a cache miss, you can do so by providing a
#'   function for \code{missing} that takes one argument, the key, and also use
#'   \code{exec_missing=TRUE}.
#'
#'   When the cache is created, you can supply a value for \code{missing}, which
#'   sets the default value to be returned for missing values. It can also be
#'   overridden when \code{get()} is called, by supplying a \code{missing}
#'   argument. For example, if you use \code{cache$get("mykey", missing =
#'   NULL)}, it will return \code{NULL} if the key is not in the cache.
#'
#'   If your cache is configured so that \code{get()} returns a sentinel value
#'   to represent a cache miss, then \code{set} will also not allow you to store
#'   the sentinel value in the cache. It will throw an error if you attempt to
#'   do so.
#'
#'   Instead of returning the same sentinel value each time there is cache miss,
#'   the cache can execute a function each time \code{get()} encounters missing
#'   key. If the function returns a value, then \code{get()} will in turn return
#'   that value. However, a more common use is for the function to throw an
#'   error. If an error is thrown, then \code{get()} will not return a value.
#'
#'   To do this, pass a one-argument function to \code{missing}, and use
#'   \code{exec_missing=TRUE}. For example, if you want to throw an error that
#'   prints the missing key, you could do this:
#'
#'   \preformatted{
#'   diskCache(
#'     missing = function(key) {
#'       stop("Attempted to get missing key: ", key)
#'     },
#'     exec_missing = TRUE
#'   )
#'   }
#'
#'   If you use this, the code that calls \code{get()} should be wrapped with
#'   \code{\link{tryCatch}()} to gracefully handle missing keys.
#'
#' @section Cache pruning:
#'
#'   Cache pruning occurs when \code{set()} is called, or it can be invoked
#'   manually by calling \code{prune()}.
#'
#'   When a pruning occurs, if there are any objects that are older than
#'   \code{max_age}, they will be removed.
#'
#'   The \code{max_size} and \code{max_n} parameters are applied to the cache as
#'   a whole, in contrast to \code{max_age}, which is applied to each object
#'   individually.
#'
#'   If the number of objects in the cache exceeds \code{max_n}, then objects
#'   will be removed from the cache according to the eviction policy, which is
#'   set with the \code{evict} parameter. Objects will be removed so that the
#'   number of items is \code{max_n}.
#'
#'   If the size of the objects in the cache exceeds \code{max_size}, then
#'   objects will be removed from the cache. Objects will be removed from the
#'   cache so that the total size remains under \code{max_size}. Note that the
#'   size is calculated using the size of the files, not the size of disk space
#'   used by the files -- these two values can differ because of files are
#'   stored in blocks on disk. For example, if the block size is 4096 bytes,
#'   then a file that is one byte in size will take 4096 bytes on disk.
#'
#'   Another time that objects can be removed from the cache is when
#'   \code{get()} is called. If the target object is older than \code{max_age},
#'   it will be removed and the cache will report it as a missing value.
#'
#' @section Eviction policies:
#'
#' If \code{max_n} or \code{max_size} are used, then objects will be removed
#' from the cache according to an eviction policy. The available eviction
#' policies are:
#'
#'   \describe{
#'     \item{\code{"lru"}}{
#'       Least Recently Used. The least recently used objects will be removed.
#'       This uses the filesystem's atime property. Some filesystems do not
#'       support atime, or have a very low atime resolution. The DiskCache will
#'       check for atime support, and if the filesystem does not support atime,
#'       a warning will be issued and the "fifo" policy will be used instead.
#'     }
#'     \item{\code{"fifo"}}{
#'       First-in-first-out. The oldest objects will be removed.
#'     }
#'   }
#'
#' @section Methods:
#'
#'  A disk cache object has the following methods:
#'
#'   \describe{
#'     \item{\code{get(key, missing, exec_missing)}}{
#'       Returns the value associated with \code{key}. If the key is not in the
#'       cache, then it returns the value specified by \code{missing} or,
#'       \code{missing} is a function and \code{exec_missing=TRUE}, then
#'       executes \code{missing}. The function can throw an error or return the
#'       value. If either of these parameters are specified here, then they
#'       will override the defaults that were set when the DiskCache object was
#'       created. See section Missing Keys for more information.
#'     }
#'     \item{\code{set(key, value)}}{
#'       Stores the \code{key}-\code{value} pair in the cache.
#'     }
#'     \item{\code{exists(key)}}{
#'       Returns \code{TRUE} if the cache contains the key, otherwise
#'       \code{FALSE}.
#'     }
#'     \item{\code{size()}}{
#'       Returns the number of items currently in the cache.
#'     }
#'     \item{\code{keys()}}{
#'       Returns a character vector of all keys currently in the cache.
#'     }
#'     \item{\code{reset()}}{
#'       Clears all objects from the cache.
#'     }
#'     \item{\code{destroy()}}{
#'       Clears all objects in the cache, and removes the cache directory from
#'       disk.
#'     }
#'     \item{\code{prune()}}{
#'       Prunes the cache, using the parameters specified by \code{max_size},
#'       \code{max_age}, \code{max_n}, and \code{evict}.
#'     }
#'   }
#'
#' @inheritParams diskCache
#'
#' @export
memoryCache <- function(
  max_size = 10 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = c("lru", "fifo"),
  missing = key_missing(),
  exec_missing = FALSE,
  logfile = NULL)
{
  MemoryCache$new(max_size, max_age, max_n, evict, missing, exec_missing, logfile)
}

MemoryCache <- R6Class("MemoryCache",
  public = list(
    initialize = function(
      max_size = 10 * 1024 ^ 2,
      max_age = Inf,
      max_n = Inf,
      evict = c("lru", "fifo"),
      missing = key_missing(),
      exec_missing = FALSE,
      logfile = NULL)
    {
      if (exec_missing && (!is.function(missing) || length(formals(missing)) == 0)) {
        stop("When `exec_missing` is true, `missing` must be a function that takes one argument.")
      }
      if (!is.numeric(max_size)) stop("max_size must be a number. Use `Inf` for no limit.")
      if (!is.numeric(max_age))  stop("max_age must be a number. Use `Inf` for no limit.")
      if (!is.numeric(max_n))    stop("max_n must be a number. Use `Inf` for no limit.")
      private$cache        <- new.env(parent = emptyenv())
      private$max_size     <- max_size
      private$max_age      <- max_age
      private$max_n        <- max_n
      private$evict        <- match.arg(evict)
      private$missing      <- missing
      private$exec_missing <- exec_missing
      private$logfile      <- logfile
    },

    get = function(key, missing = private$missing, exec_missing = private$exec_missing) {
      private$log(paste0('get: key "', key, '"'))
      validate_key(key)

      private$maybe_prune_single(key)

      if (!self$exists(key)) {
        private$log(paste0('get: key "', key, '" is missing'))
        if (exec_missing) {
          if (!is.function(missing) || length(formals(missing)) == 0) {
            stop("When `exec_missing` is true, `missing` must be a function that takes one argument.")
          }
          return(missing(key))
        } else {
          return(missing)
        }
      }

      private$log(paste0('get: key "', key, '" found'))
      value <- private$cache[[key]]$value
      value
    },

    set = function(key, value) {
      private$log(paste0('set: key "', key, '"'))
      validate_key(key)

      time <- as.numeric(Sys.time())

      # Only record size if we're actually using max_size for pruning.
      if (is.finite(private$max_size)) {
        # Reported size is rough! See ?object.size.
        size <- as.numeric(object.size(value))
      } else {
        size <- NULL
      }

      private$cache[[key]] <- list(
        key = key,
        value = value,
        size = size,
        mtime = time,
        atime = time
      )
      self$prune()
      invisible(self)
    },

    exists = function(key) {
      validate_key(key)
      # Faster than `exists(key, envir = private$cache, inherits = FALSE)
      !is.null(private$cache[[key]])
    },

    keys = function() {
      ls(private$cache, sorted = FALSE)  # Faster with sorted=FALSE
    },

    remove = function(key) {
      private$log(paste0('remove: key "', key, '"'))
      validate_key(key)
      rm(list = key, envir = private$cache)
      invisible(self)
    },

    reset = function() {
      private$log(paste0('reset'))
      rm(list = self$keys(), envir = private$cache)
      invisible(self)
    },

    prune = function() {
      private$log(paste0('prune'))
      info <- private$object_info()

      # 1. Remove any objects where the age exceeds max age.
      if (is.finite(private$max_age)) {
        time <- as.numeric(Sys.time())
        timediff <- time - info$mtime
        rm_idx <- timediff > private$max_age
        if (any(rm_idx)) {
          private$log(paste0("prune max_age: Removing ", paste(info$key[rm_idx], collapse = ", ")))
          rm(list = info$key[rm_idx], envir = private$cache)
          info <- info[!rm_idx, ]
        }
      }

      # Sort objects by priority, according to eviction policy. The sorting is
      # done in a function which can be called multiple times but only does
      # the work the first time.
      info_is_sorted <- FALSE
      ensure_info_is_sorted <- function() {
        if (info_is_sorted) return()

        if (private$evict == "lru") {
          info <<- info[order(info$atime, decreasing = TRUE), ]
        } else if (private$evict == "fifo") {
          info <<- info[order(info$mtime, decreasing = TRUE), ]
        } else {
          stop('Unknown eviction policy "', private$evict, '"')
        }
        info_is_sorted <<- TRUE
      }

      # 2. Remove objects if there are too many.
      if (is.finite(private$max_n) && nrow(info) > private$max_n) {
        ensure_info_is_sorted()
        rm_idx <- seq_len(nrow(info)) > private$max_n
        private$log(paste0("prune max_n: Removing ", paste(info$key[rm_idx], collapse = ", ")))
        rm(list = info$key[rm_idx], envir = private$cache)
        info <- info[!rm_idx, ]
      }

      # 3. Remove objects if cache is too large.
      if (is.finite(private$max_size) && sum(info$size) > private$max_size) {
        ensure_info_is_sorted()
        cum_size <- cumsum(info$size)
        rm_idx <- cum_size > private$max_size
        private$log(paste0("prune max_size: Removing ", paste(info$key[rm_idx], collapse = ", ")))
        rm(list = info$key[rm_idx], envir = private$cache)
        info <- info[!rm_idx, ]
      }

      invisible(self)
    },

    size = function() {
      length(self$keys())
    }
  ),

  private = list(
    cache = NULL,
    max_age = NULL,
    max_size = NULL,
    max_n = NULL,
    evict = NULL,
    missing = NULL,
    exec_missing = NULL,
    logfile = NULL,

    # Prunes a single object if it exceeds max_age. If the object does not
    # exceed max_age, or if the object doesn't exist, do nothing.
    maybe_prune_single = function(key) {
      if (!is.finite(private$max_age)) return()

      obj <- private$cache[[key]]
      if (is.null(obj)) return()

      timediff <- as.numeric(Sys.time()) - obj$mtime
      if (timediff > private$max_age) {
        private$log(paste0("pruning single object exceeding max_age: Removing ", key))
        rm(list = key, envir = private$cache)
      }
    },

    object_info = function() {
      keys <- ls(private$cache, sorted = FALSE)
      data.frame(
        key   = keys,
        size  = vapply(keys, function(key) private$cache[[key]]$size,  0),
        mtime = vapply(keys, function(key) private$cache[[key]]$mtime, 0),
        atime = vapply(keys, function(key) private$cache[[key]]$atime, 0),
        stringsAsFactors = FALSE
      )
    },

    log = function(text) {
      if (is.null(private$logfile)) return()

      text <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%OS3] MemoryCache "), text)
      writeLines(text, private$logfile)
    }
  )
)
