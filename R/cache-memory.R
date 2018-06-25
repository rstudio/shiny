#' Create a memory cache object
#'
#' A memory cache object is a key-value store that saves the values in an
#' environment. Objects can be stored and retrieved using the \code{get()} and
#' \code{set()} methods. Objects are automatically pruned from the cache
#' according to the parameters \code{max_size}, \code{max_age}, \code{max_n},
#' and \code{evict}.
#'
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
#'     \item{\code{get(key)}}{
#'       Returns the value associated with \code{key}. If the key is not in the
#'       cache, this throws an error.
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
#' @param max_age Maximum age of files in cache before they are evicted, in
#'   seconds.
#' @param max_size Maximum size of the cache, in bytes. If the cache exceeds
#'   this size, cached objects will be removed according to the value of the
#'   \code{evict}.
#' @param max_n Maximum number of objects in the cache. If the number of objects
#'   exceeds this value, then cached objects will be removed according to the
#'   value of \code{evict}.
#' @param evict The eviction policy to use to decide which objects are removed
#'   when a cache pruning occurs. Currently, \code{"lru"} and \code{"fifo"} are
#'   supported.
#' @export
memoryCache <- function(
  max_size = 10 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = c("lru", "fifo"))
{
  MemoryCache$new(max_size, max_age, max_n, evict)
}

MemoryCache <- R6Class("MemoryCache",
  public = list(
    initialize = function(max_size = 10 * 1024 ^ 2,
                          max_age = Inf,
                          max_n = Inf,
                          evict = c("lru", "fifo"))
    {
      private$cache    <- new.env(parent = emptyenv())
      private$max_size <- max_size
      private$max_age  <- max_age
      private$max_n    <- max_n
      private$evict    <- match.arg(evict)
    },

    get = function(key) {
      validate_key(key)
      if (!self$exists(key)) {
        stop("Key not available: ", key)
      }
      value <- private$cache[[key]]$value
      self$prune()
      value
    },

    set = function(key, value) {
      validate_key(key)
      self$prune()
      time <- as.numeric(Sys.time())
      private$cache[[key]] <- list(
        key = key,
        value = value,
        size = as.numeric(object.size(value)),  # Reported size is rough! See ?object.size.
        mtime = time,
        atime = time
      )
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
