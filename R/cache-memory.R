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
#' with other cache types, like \code{\link{DiskCache}}, where objects are
#' serialized, and the serialized object is cached. This can result in some
#' differences of behavior. For example, as long as an object is stored in a
#' MemoryCache, it will not be garbage collected.
#'
#'
#' @section Missing keys:
#'
#'   The \code{missing} parameter controls what happens when \code{get()} is
#'   called with a key that is not in the cache (a cache miss). The default
#'   behavior is to return a \code{\link{key_missing}} object. This is a
#'   \emph{sentinel value} representing a missing key. You can test if the
#'   returned value represents a missing key by using the
#'   \code{\link{is.key_missing}} function. You can also have \code{get()}
#'   return a different sentinel value, like \code{NULL}, or even throw an error
#'   on a cache miss.
#'
#'   When the cache is created, you can supply a value for \code{missing}, which
#'   sets the default value to be returned for missing values. It can also be
#'   overridden when \code{get()} is called, by supplying a \code{missing}
#'   argument, as in \code{cache$get("mykey", missing = NULL)}.
#'
#'   If your cache is configured so that \code{get()} returns a sentinel value
#'   to represent a cache miss, then \code{set} will also not allow you to store
#'   the sentinel value in the cache. It will throw an error if you attempt to
#'   do so.
#'
#'   If \code{missing} is a quoted expression, then that expression will be
#'   evaluated each time \code{get()} encounters missing key. If the evaluation
#'   of the expression does not throw an error, then \code{get()} will return
#'   the resulting value. However, it is more common for the expression to throw
#'   an error. If an error is thrown, then \code{get()} will not return a value.
#'   For example, you could use \code{quote(stop("Missing key"))}. If you use
#'   this, the code that calls \code{get()} should be wrapped with
#'   \code{\link{tryCatch}()} to gracefully handle missing keys.
#'
#' @section Cache pruning:
#'
#'   Cache pruning occurs each time \code{get()} and \code{set()} are called, or
#'   it can be invoked manually by calling \code{prune()}.
#'
#'   If there are any objects that are older than \code{max_age}, they will be
#'   removed when a pruning occurs.
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
#'   cache so that the total size remains under \code{max_size}. The size is
#'   calculated by calling \code{\link{object.size}} on an object. Note that if
#'   two keys are associated with the same object, the size calculation will
#'   count the object's size twice, even though there is only one copy in
#'   memory.
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
#'     \item{\code{get(key, missing)}}{
#'       Returns the value associated with \code{key}. If the key is not in the
#'       cache, then it returns the value specified by \code{missing}. The
#'       default value for \code{missing} when the DiskCache object is created,
#'       but it can be overridden when \code{get()} is called.
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
  missing = key_missing())
{
  MemoryCache$new(max_size, max_age, max_n, evict, missing)
}

MemoryCache <- R6Class("MemoryCache",
  public = list(
    initialize = function(
      max_size = 10 * 1024 ^ 2,
      max_age = Inf,
      max_n = Inf,
      evict = c("lru", "fifo"),
      missing = key_missing())
    {
      private$cache        <- new.env(parent = emptyenv())
      private$max_size     <- max_size
      private$max_age      <- max_age
      private$max_n        <- max_n
      private$evict        <- match.arg(evict)
      private$missing      <- missing
    },

    get = function(key, missing = private$missing) {
      validate_key(key)
      if (!self$exists(key)) {
        if (is.language(missing)) {
          return(eval(missing))
        } else {
          return(missing)
        }
      }

      value <- private$cache[[key]]$value
      self$prune()
      value
    },

    set = function(key, value) {
      validate_key(key)
      if (!is.language(private$missing) && identical(value, private$missing)) {
        stop("Attempted to store sentinel value representing a missing key.")
      }

      time <- as.numeric(Sys.time())
      private$cache[[key]] <- list(
        key = key,
        value = value,
        size = as.numeric(object.size(value)),  # Reported size is rough! See ?object.size.
        mtime = time,
        atime = time
      )
      self$prune()
      invisible(self)
    },

    exists = function(key) {
      validate_key(key)
      exists(key, envir = private$cache, inherits = FALSE)
    },

    keys = function() {
      ls(private$cache, sorted = FALSE)  # Faster with sorted=FALSE
    },

    remove = function(key) {
      validate_key(key)
      rm(list = key, envir = private$cache)
      invisible(self)
    },

    reset = function() {
      rm(list = self$keys(), envir = private$cache)
      invisible(self)
    },

    prune = function() {
      info <- private$object_info()

      # 1. Remove any objects where the age exceeds max age.
      time <- as.numeric(Sys.time())
      timediff <- time - info$mtime
      rm_idx <- timediff > private$max_age
      if (any(rm_idx)) {
        message("max_age: Removing ", paste(info$key[rm_idx], collapse = ", "))
      }

      # Remove items from cache
      rm(list = info$key[rm_idx], envir = private$cache)
      info <- info[!rm_idx, ]

      # Sort objects by priority, according to eviction policy.
      if (private$evict == "lru") {
        info <- info[order(info[["atime"]], decreasing = TRUE), ]
      } else if (private$evict == "fifo") {
        info <- info[order(info[["mtime"]], decreasing = TRUE), ]
      } else {
        stop('Unknown eviction policy "', private$evict, '"')
      }

      # 2. Remove objects if there are too many.
      if (nrow(info) > private$max_n) {
        rm_idx <- seq_len(nrow(info)) > private$max_n
        if (any(rm_idx)) {
          message("max_n: Removing ", paste(info$key[rm_idx], collapse = ", "))
        }
        rm(list = info$key[rm_idx], envir = private$cache)
        info <- info[!rm_idx, ]
      }

      # 3. Remove objects if cache is too large.
      if (sum(info$size) > private$max_size) {
        cum_size <- cumsum(info$size)
        rm_idx <- cum_size > private$max_size
        if (any(rm_idx)) {
          message("max_size: Removing ", paste(info$key[rm_idx], collapse = ", "))
        }
        rm(list = info$key[rm_idx], envir = private$cache)
        info <- info[!rm_idx, ]
    }
      invisible(self)
    },

    size = function() {
      # TODO: Validate this against metadata?
      length(self$key())
    },

    destroy = function() {
      if (is.null(private$cache)) {
        return(invisible)
      }

      private$cache <- NULL
    },

    is_destroyed = function() {
      is.null(private$cache)
    }
  ),

  private = list(
    cache = NULL,
    meta = NULL,     # Metadata used for pruning
    max_age = NULL,
    max_size = NULL,
    max_n = NULL,
    evict = NULL,
    missing = NULL,

    object_info = function() {
      keys <- ls(private$cache, sorted = FALSE)
      data.frame(
        key   = keys,
        size  = vapply(keys, function(key) private$cache[[key]]$size,  0),
        mtime = vapply(keys, function(key) private$cache[[key]]$mtime, 0),
        atime = vapply(keys, function(key) private$cache[[key]]$atime, 0),
        stringsAsFactors = FALSE
      )
    }
  )
)
