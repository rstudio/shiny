# A context object for tracking a cache that needs to be dirtied when a set of
# files changes on disk. Each time the cache is dirtied, the set of files is
# cleared. Therefore, the set of files needs to be re-built each time the cached
# code executes. This approach allows for dynamic dependency graphs.
CacheContext <- R6Class(
  'CacheContext',
  portable = FALSE,
  class = FALSE,
  public = list(
    .dirty = TRUE,
    # List of functions that return TRUE if dirty
    .tests = list(),

    addDependencyFile = function(file) {
      if (.dirty)
        return()

      file <- normalizePath(file)

      mtime <- file.info(file)$mtime
      .tests <<- c(.tests, function() {
        newMtime <- try(file.info(file)$mtime, silent=TRUE)
        if (inherits(newMtime, 'try-error'))
          return(TRUE)
        return(!identical(mtime, newMtime))
      })
      invisible()
    },
    forceDirty = function() {
      .dirty <<- TRUE
      .tests <<- list()
      invisible()
    },
    isDirty = function() {
      if (.dirty)
        return(TRUE)

      for (test in .tests) {
        if (test()) {
          forceDirty()
          return(TRUE)
        }
      }

      return(FALSE)
    },
    reset = function() {
      .dirty <<- FALSE
      .tests <<- list()
    },
    with = function(func) {
      oldCC <- .currentCacheContext$cc
      .currentCacheContext$cc <- self
      on.exit(.currentCacheContext$cc <- oldCC)

      return(func())
    }
  )
)

.currentCacheContext <- new.env()

# Indicates to Shiny that the given file path is part of the dependency graph
# for whatever is currently executing (so far, only ui.R). By default, ui.R only
# gets re-executed when it is detected to have changed; this function allows the
# caller to indicate that it should also re-execute if the given file changes.
#
# If NULL or NA is given as the argument, then ui.R will re-execute next time.
dependsOnFile <- function(filepath) {
  if (is.null(.currentCacheContext$cc))
    return()

  if (is.null(filepath) || is.na(filepath))
    .currentCacheContext$cc$forceDirty()
  else
    .currentCacheContext$cc$addDependencyFile(filepath)
}



#' Create a disk cache object
#'
#' A disk cache object is a key-value store that saves the values as files in a
#' directory on disk. Objects can be stored and retrieved using the \code{get()}
#' and \code{set()} methods. Objects are automatically pruned from the cache
#' according to the parameters \code{max_size}, \code{max_age}, \code{max_n},
#' and \code{evict}.
#'
#'
#' @section Cache pruning:
#'
#' Cache pruning occurs each time \code{get()} and \code{set()} are called, or
#' it can be invoked manually by calling \code{prune()}.
#'
#' If there are any objects that are older than \code{max_age}, they will be
#' removed when a pruning occurs.
#'
#' The \code{max_size} and \code{max_n} parameters are applied to the cache as
#' a whole, in contrast to \code{max_age}, which is applied to each object
#' individually.
#'
#' If the number of objects in the cache exceeds \code{max_n}, then objects
#' will be removed from the cache according to the eviction policy, which is
#' set with the \code{evict} parameter. Objects will be removed so that the
#' number of items is \code{max_n}.
#'
#' If the size of the objects in the cache exceeds \code{max_size}, then
#' objects will be removed from the cache. Objects will be removed from
#' the cache so that the total size remains under \code{max_size}. Note that
#' the size is calculated using the size of the files, not the size of disk
#' space used by the files -- these two values can differ because of files
#' are stored in blocks on disk. For example, if the block size is 4096 bytes,
#' then a file that is one byte in size will take 4096 bytes on disk.
#'
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
#'     \item{\code{has(key)}}{
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
#' @param dir Directory to store files for the cache. By default, it will use
#'   a temporary directory.
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
#' @param destroy_on_finalize If \code{TRUE}, then when the DiskCache object is
#'   garbage collected, the cache directory and all objects inside of it will be
#'   deleted from disk.
#' @export
diskCache <- function(dir = tempfile("DiskCache-"),
  max_size = 5 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = "fifo",
  destroy_on_finalize = TRUE)
{
  DiskCache$new(dir, max_size, max_age, max_n, evict, destroy_on_finalize)
}


DiskCache <- R6Class("DiskCache",
  public = list(
    initialize = function(dir = tempfile("DiskCache-"),
                          max_size = 5 * 1024 ^ 2,
                          max_age = Inf,
                          max_n = Inf,
                          evict = c("lru", "fifo"),
                          destroy_on_finalize = TRUE)
    {
      if (!dirExists(dir)) {
        message("Creating ", dir)
        dir.create(dir, recursive = TRUE, mode = "0700")
        private$dir_was_created <- TRUE
      }
      private$dir                 <- absolutePath(dir)
      private$max_size            <- max_size
      private$max_age             <- max_age
      private$max_n               <- max_n
      private$evict               <- match.arg(evict)
      if (private$evict == "lru" && !check_atime_support(private$dir)) {
        # Another possibility for handling lack of atime support would be to
        # create a file on disk that contains atimes. However, this would not
        # be safe when multiple processes are sharing a cache.
        warning("DiskCache: can't use eviction policy \"lru\" because filesystem for ",
          private$dir, " does not support atime, or has low atime resolution. Using \"fifo\" instead."
        )
        private$evict <- "fifo"
      }
      private$destroy_on_finalize <- destroy_on_finalize
    },

    # TODO:
    # Should call has() and return some sentinal object if not present?
    # Should be atomic to avoid race conditions with other processes.
    # Reduce pruning for mset/mget
    get = function(key) {
      validate_key(key)
      if (!self$has(key)) {
        stop("Key not available: ", key)
      }
      value <- readRDS(private$key_to_filename(key))
      self$prune()
      value
    },

    set = function(key, value) {
      validate_key(key)
      self$prune()
      saveRDS(value, file = private$key_to_filename(key))
      invisible(self)
    },

    has = function(key) {
      validate_key(key)
      file.exists(private$key_to_filename(key))
    },

    # Return all keys in the cache
    keys = function() {
      files <- dir(private$dir, "*.rds")
      sub("\\.rds$", "", files)
    },

    remove = function(key) {
      validate_key(key)
      file.remove(private$key_to_filename(key))
      invisible(self)
    },

    reset = function() {
      file.remove(dir(private$dir, "*.rds", full.names = TRUE))
      invisible(self)
    },

    prune = function() {
      files <- file.info(dir(private$dir, "*.rds", full.names = TRUE))
      files <- files[files$isdir == FALSE, ]
      files$name <- rownames(files)
      rownames(files) <- NULL

      # 1. Remove any files where the age exceeds max age.
      timediff <- as.numeric(Sys.time() - files[["mtime"]], units = "secs")
      rm_idx <- timediff > private$max_age
      if (any(rm_idx)) {
        message("max_age: Removing ", paste(files$name[rm_idx], collapse = ", "))
      }
      file.remove(files$name[rm_idx])

      # Remove rows of files that were deleted.
      files <- files[!rm_idx, ]

      # Sort files by priority, according to eviction policy.
      if (private$evict == "lru") {
        files <- files[order(files[["atime"]], decreasing = TRUE), ]
      } else if (private$evict == "fifo") {
        files <- files[order(files[["mtime"]], decreasing = TRUE), ]
      } else {
        stop('Unknown eviction policy "', private$evict, '"')
      }

      # 2. Remove files if there are too many.
      if (nrow(files) > private$max_n) {
        rm_idx <- seq_len(nrow(files)) > private$max_n
        if (any(rm_idx)) {
          message("max_n: Removing ", paste(files$name[rm_idx], collapse = ", "))
        }
        file.remove(files$name[rm_idx])
      }

      # 3. Remove files if cache is too large.
      if (sum(files$size) > private$max_size) {
        cum_size <- cumsum(files$size)
        rm_idx <- cum_size > private$max_size
        if (any(rm_idx)) {
          message("max_size: Removing ", paste(files$name[rm_idx], collapse = ", "))
        }
        file.remove(files$name[rm_idx])
      }
      invisible(self)
    },

    size = function() {
      length(dir(private$dir, "*.rds"))
    },

    destroy = function() {
      if (private$destroyed) {
        return(invisible)
      }

      private$destroyed <- TRUE
      self$reset()
      if (private$dir_was_created) {
        message("Removing ", private$dir)
        dirRemove(private$dir)
      }
    },

    is_destroyed = function() {
      private$destroyed
    },

    finalize = function() {
      if (private$destroy_on_finalize) {
        self$destroy()
      }
    }
  ),

  private = list(
    dir = NULL,
    max_age = NULL,
    max_size = NULL,
    max_n = NULL,
    evict = NULL,
    dir_was_created = FALSE,
    destroy_on_finalize = NULL,
    destroyed = FALSE,

    key_to_filename = function(key) {
      if (! (is.character(key) && length(key)==1) ) {
        stop("Key must be a character vector of length 1.")
      }
      file.path(private$dir, paste0(key, ".rds"))
    }
  )
)

validate_key <- function(key) {
  if (grepl("[^a-z0-9]", key)) {
    stop("Invalid key: ", key, ". Only lowercase letters and numbers are allowed.")
  }
}

# Checks if a filesystem has atime support, by creating a file in a specified
# directory, waiting 0.1 seconds, then reading the file. If the timestamp does
# not change, then the filesystem does not support atime, or has very low atime
# resolution. For example, FAT has an atime resolution of 1 day. If the
# timestamp does change, then the filesystem supports atime. (Although it is
# possible in very rare cases that the filesystem has a low atime resolution and
# the pause just happend to cross a boundary.)
check_atime_support <- function(dir) {
  dir <- "."
  temp_file <- tempfile("check-atime-support-", dir)

  file.create(temp_file)
  on.exit(unlink(temp_file), add = TRUE)
  atime1 <- as.numeric(file.info(temp_file)[["atime"]])

  Sys.sleep(0.1)
  readBin(temp_file, "raw", 1L)
  atime2 <- as.numeric(file.info(temp_file)[["atime"]])

  if (atime1 == atime2) {
    return(FALSE)
  }

  TRUE
}
