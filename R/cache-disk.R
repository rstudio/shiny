#' Create a disk cache object
#'
#' A disk cache object is a key-value store that saves the values as files in a
#' directory on disk. Objects can be stored and retrieved using the \code{get()}
#' and \code{set()} methods. Objects are automatically pruned from the cache
#' according to the parameters \code{max_size}, \code{max_age}, \code{max_n},
#' and \code{evict}.
#'
#'
#' @section Missing Keys:
#'
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
#'   cache so that the total size remains under \code{max_size}. Note that the
#'   size is calculated using the size of the files, not the size of disk space
#'   used by the files -- these two values can differ because of files are
#'   stored in blocks on disk. For example, if the block size is 4096 bytes,
#'   then a file that is one byte in size will take 4096 bytes on disk.
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
#'
#' @section Sharing among multiple processes:
#'
#' The directory for a DiskCache can be shared among multiple R processes. To
#' do this, each R process should have a DiskCache object that uses the same
#' directory. Each DiskCache will do pruning independently of the others, so if
#' they have different pruning parameters, then one DiskCache may remove cached
#' objects before another DiskCache would do so.
#'
#' When multiple processes share a cache directory, there are some potential
#' race conditions. For example, if your code calls \code{exists(key)} to check
#' if an object is in the cache, and then call \code{get(key)}, the object may
#' be removed from the cache in between those two calls, and \code{get(key)}
#' will throw an error. Instead of calling the two functions, it is better to
#' simply call \code{get(key)}, and use \code{tryCatch()} to handle the error
#' that is thrown if the object is not in the cache. This effectively tests for
#' existence and gets the object in one operation.
#'
#' It is also possible for one processes to prune objects at the same time that
#' another processes is trying to prune objects. If this happens, you may see
#' a warning from \code{file.remove()} failing to remove a file that has
#' already been deleted.
#'
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
#' @param dir Directory to store files for the cache. If \code{NULL} (the
#'   default) it will create and use a temporary directory.
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
#'   deleted from disk. If \code{FALSE}, it will do nothing when finalized. If
#'   \code{NULL} (the default), then the behavior depends on the value of
#'   \code{dir}: If \code{destroy_on_finalize=NULL} and \code{dir=NULL}, then a
#'   temporary directory will be created and used for the cache, and it will be
#'   deleted when the DiskCache is finalized.  If
#'   \code{destroy_on_finalize=NULL} and \code{dir} is \emph{not} \code{NULL},
#'   then the directory will not be deleted when the DiskCache is finalized. In
#'   short, when \code{destroy_on_finalize=NULL}, if the cache directory is
#'   automatically created, it will be automatically deleted, and if the cache
#'   directory is not automatically created, it will not be automatically
#'   deleted.
#' @param missing A value to return or a function to execute when
#'   \code{get(key)} is called but the key is not present in the cache. The
#'   default is a \code{\link{key_missing}} object. If it is a function to
#'   execute, the function must take one argument (the key), and you must also
#'   use \code{exec_missing = TRUE}. If it is a function, it is useful in most
#'   cases for it to throw an error, although another option is to return a
#'   value. If a value is returned, that value will in turn be returned by
#'   \code{get()}. See section Missing keys for more information.
#' @param exec_missing If \code{FALSE} (the default), then treat \code{missing}
#'   as a value to return when \code{get()} results in a cache miss. If
#'   \code{TRUE}, treat \code{missing} as a function to execute when
#'   \code{get()} results in a cache miss.
#' @param logfile An optional filename or connection object to where logging
#'   information will be written. To log to the console, use \code{stdout()}.
#'
#' @export
diskCache <- function(
  dir = NULL,
  max_size = 10 * 1024 ^ 2,
  max_age = Inf,
  max_n = Inf,
  evict = c("lru", "fifo"),
  destroy_on_finalize = NULL,
  missing = key_missing(),
  exec_missing = FALSE,
  logfile = NULL)
{
  DiskCache$new(dir, max_size, max_age, max_n, evict, destroy_on_finalize,
                missing, exec_missing, logfile)
}


DiskCache <- R6Class("DiskCache",
  public = list(
    initialize = function(
      dir = NULL,
      max_size = 10 * 1024 ^ 2,
      max_age = Inf,
      max_n = Inf,
      evict = c("lru", "fifo"),
      destroy_on_finalize = NULL,
      missing = key_missing(),
      exec_missing = FALSE,
      logfile = NULL)
    {
      if (exec_missing && (!is.function(missing) || length(formals(missing)) == 0)) {
        stop("When `exec_missing` is true, `missing` must be a function that takes one argument.")
      }
      if (is.null(destroy_on_finalize)) {
        destroy_on_finalize <- is.null(dir)
      }
      if (is.null(dir)) {
        dir <- tempfile("DiskCache-")
      }

      private$dir                 <- absolutePath(dir)
      private$max_size            <- max_size
      private$max_age             <- max_age
      private$max_n               <- max_n
      private$evict               <- match.arg(evict)
      private$destroy_on_finalize <- destroy_on_finalize
      if (private$evict == "lru" && !check_atime_support(private$dir)) {
        # Another possibility for handling lack of atime support would be to
        # create a file on disk that contains atimes. However, this would not
        # be safe when multiple processes are sharing a cache.
        warning("DiskCache: can't use eviction policy \"lru\" because filesystem for ",
          private$dir, " does not support atime, or has low atime resolution. Using \"fifo\" instead."
        )
        private$evict             <- "fifo"
      }
      private$missing             <- missing
      private$exec_missing        <- exec_missing
      private$logfile             <- logfile

      if (!dirExists(dir)) {
        private$log(paste0("initialize: Creating ", dir))
        dir.create(dir, recursive = TRUE, mode = "0700")
      }
    },

    get = function(key, missing = private$missing, exec_missing = private$exec_missing) {
      private$log(paste0('get: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      filename <- private$key_to_filename(key)

      # Instead of calling exists() before fetching the value, just try to
      # fetch the value. This reduces the risk of a race condition when
      # multiple processes share a cache.
      read_error <- FALSE
      tryCatch(
        {
          value <- suppressWarnings(readRDS(filename))
        },
        error = function(e) {
          read_error <<- TRUE
        }
      )
      if (read_error) {
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
      self$prune()
      value
    },

    set = function(key, value) {
      private$log(paste0('set: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      if (!private$exec_missing && identical(value, private$missing)) {
        stop("Attempted to store sentinel value representing a missing key.")
      }

      file <- private$key_to_filename(key)
      temp_file <- paste0(file, "-temp-", shiny::createUniqueId(8))

      save_error <- FALSE
      ref_object <- FALSE
      tryCatch(
        saveRDS(value, file = temp_file,
          refhook = function(x) {
            ref_object <<- TRUE
            NULL
          }
        ),
        error = function(e) {
          save_error <<- TRUE
        }
      )
      if (save_error) {
        private$log(paste0('set: key "', key, '" error'))
        stop('Error setting value for key "', key, '".')
      }
      if (ref_object) {
        private$log(paste0('set: value is a reference object'))
        warning("A reference object was cached in a serialized format. The restored object may not work as expected.")
      }

      file.rename(temp_file, file)
      self$prune()
      invisible(self)
    },

    exists = function(key) {
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      file.exists(private$key_to_filename(key))
    },

    # Return all keys in the cache
    keys = function() {
      self$is_destroyed(throw = TRUE)
      files <- dir(private$dir, "\\.rds$")
      sub("\\.rds$", "", files)
    },

    remove = function(key) {
      private$log(paste0('remove: key "', key, '"'))
      self$is_destroyed(throw = TRUE)
      validate_key(key)
      file.remove(private$key_to_filename(key))
      invisible(self)
    },

    reset = function() {
      private$log(paste0('reset'))
      self$is_destroyed(throw = TRUE)
      file.remove(dir(private$dir, "\\.rds$", full.names = TRUE))
      invisible(self)
    },

    prune = function() {
      # TODO: It would be good to add parameters `n` and `size`, so that the
      # cache can be pruned to `max_n - n` and `max_size - size` before adding
      # an object. Right now we prune after adding the object, so the cache
      # can temporarily grow past the limits. The reason we don't do this now
      # is because it is expensive to find the size of the serialized object
      # before adding it.

      private$log(paste0('prune'))
      self$is_destroyed(throw = TRUE)

      filenames <- dir(private$dir, "\\.rds$", full.names = TRUE)
      files <- file.info(filenames)
      files <- files[files$isdir == FALSE, ]
      files$name <- rownames(files)
      rownames(files) <- NULL
      # Files could be removed between the dir() and file.info() calls. The
      # entire row for such files will have NA values. Remove those rows.
      files <- files[!is.na(files$size), ]

      # 1. Remove any files where the age exceeds max age.
      timediff <- as.numeric(Sys.time() - files[["mtime"]], units = "secs")
      rm_idx <- timediff > private$max_age
      if (any(rm_idx)) {
        private$log(paste0("prune max_age: Removing ", paste(files$name[rm_idx], collapse = ", ")))
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
          private$log(paste0("prune max_n: Removing ", paste(files$name[rm_idx], collapse = ", ")))
        }
        file.remove(files$name[rm_idx])
      }

      # 3. Remove files if cache is too large.
      if (sum(files$size) > private$max_size) {
        cum_size <- cumsum(files$size)
        rm_idx <- cum_size > private$max_size
        if (any(rm_idx)) {
          private$log(paste0("prune max_size: Removing ", paste(files$name[rm_idx], collapse = ", ")))
        }
        file.remove(files$name[rm_idx])
      }
      invisible(self)
    },

    size = function() {
      self$is_destroyed(throw = TRUE)
      length(dir(private$dir, "\\.rds$"))
    },

    destroy = function() {
      if (self$is_destroyed()) {
        return(invisible(self))
      }

      private$log(paste0("destroy: Removing ", private$dir))
      # First create a sentinel file so that other processes sharing this
      # cache know that the cache is to be destroyed. This is needed because
      # the recursive unlink is not atomic: another process can add a file to
      # the directory after unlink starts removing files but before it removes
      # the directory, and when that happens, the directory removal will fail.
      file.create(file.path(private$dir, "__destroyed__"))
      unlink(private$dir, recursive = TRUE)
      private$destroyed <- TRUE
      invisible(self)
    },

    is_destroyed = function(throw = FALSE) {
      if (!dirExists(private$dir) ||
          file.exists(file.path(private$dir, "__destroyed__")))
      {
        # It's possible for another process to destroy a shared cache directory
        private$destroyed <- TRUE
      }

      if (throw) {
        if (private$destroyed) {
          stop("Attempted to use cache which has been destroyed:\n  ", private$dir)
        }

      } else {
        private$destroyed
      }
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
    destroy_on_finalize = NULL,
    destroyed = FALSE,
    missing = NULL,
    exec_missing = FALSE,
    logfile = NULL,

    key_to_filename = function(key) {
      if (! (is.character(key) && length(key)==1) ) {
        stop("Key must be a character vector of length 1.")
      }
      file.path(private$dir, paste0(key, ".rds"))
    },

    log = function(text) {
      if (is.null(private$logfile)) return()

      text <- paste0(format(Sys.time(), "[%Y-%m-%d %H:%M:%OS3] DiskCache "), text)
      writeLines(text, private$logfile)
    }
  )
)

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
