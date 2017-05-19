library(tools)

# For HTML5-capable browsers, file uploads happen through a series of requests.
#
# 1. Client tells server that one or more files are about to be uploaded; the
#    server responds with a "job ID" that the client should use for the rest of
#    the upload.
#
# 2. For each file (sequentially):
#    a. Client tells server the name, size, and type of the file.
#    b. Client sends server a small-ish blob of data.
#    c. Repeat 2b until the entire file has been uploaded.
#    d. Client tells server that the current file is done.
#
# 3. Repeat 2 until all files have been uploaded.
#
# 4. Client tells server that all files have been uploaded, along with the
#    input ID that this data should be associated with.
#
# Unfortunately this approach will not work for browsers that don't support
# HTML5 File API, but the fallback approach we would like to use (multipart
# form upload, i.e. traditional HTTP POST-based file upload) doesn't work with
# the websockets package's HTTP server at the moment.

# The following file names are illegal on Windows. It's also bad to follow them
# with an extension, such as NUL.txt
# https://msdn.microsoft.com/en-us/library/aa365247.aspx
# https://blogs.msdn.microsoft.com/oldnewthing/20031022-00/?p=42073
illegalWindowsNames <-
  c("AUX",
    paste0("COM", 1:9),
    "CON",
    paste0("LPT", 1:9),
    "NUL",
    "PRN")

# @details Removes strings in file names that could cause problems on Windows.
# @param str A character vector to remove illegal strings from.
# @param illegal The vector of words, by themselves or with an extension, to
#   remove from \code{str}
# @return \code{str} with any illegal words removed.
removeIllegalWindowsFilenames <- function(str, illegal) {
  ret <- str
  ret[ret %in% illegal] <- ""
  illegalNames <- file_path_sans_ext(ret) %in% illegal
  ret[illegalNames] <- paste0(".", file_ext(ret[illegalNames]))
  ret
}

# @details Helper function for abbreviating dots and removing unwanted
#   characters and strings from a filename.
# @param str A character vector which is presumably the name of a file.
# @param whitelist The regular expression fragment indicating the characters to
#   keep.
# @param windows A logical indicating whether or not to strip names that are
#   illegal on Windows.
# @return str with dots abbreviated, non-whitelisted characters removed, and
#   optionally illegal Windows names removed.
sanitize <- function(str, whitelist = "a-zA-Z0-9\\.", windows = FALSE) {
  sanitized <- str %>%
    gsub(sprintf("[^%s]", whitelist), "", .) %>%
    gsub("\\.+", "\\.", .)

  if(windows)
    removeIllegalWindowsFilenames(sanitized, illegalWindowsNames)
  else
    sanitized
}

# @details \code{sanitizeFileName} is an effort to safely retain something close
#   to an uploaded file's original name in order to support libraries like
#   readxl that are sensitive to the names of files. A good overview of the
#   risks associated with uploaded files is
#   \url{https://www.owasp.org/index.php/Unrestricted_File_Upload}
# @param name A character vector, the name of a file to sanitize.
# @param default Character vector to return if \code{name} cannot be sanitized.
#   If a file's extension can be preserved, and the total length of
#   \code{default} and the extension is not more than \code{maxSize}, then
#   \code{default} is combined with the extension and returned.
# @param maxSize The maximum allowable size of the name returned by this
#   function. (Default is 255 characters.)
# @return The sanitized \code{name}, or the \code{default} combined with the
#   original extension, if possible. Otherwise, \code{default}.
sanitizeFileName <- function(name, default, maxSize = 255) {
  if (missing(default))
    stop("default is a required argument")

  if (any(nchar(default) > maxSize))
    stop("default can't be longer than maxSize")

  if (length(name) != length(default))
    stop("name and default must be the same length")

  sanitizedNames <- sanitize(name, windows = (.Platform$OS.type == "windows"))

  mapply(function(sanitizedName, defaultName) {
    fileName <- file_path_sans_ext(sanitizedName)
    fileExt  <- file_ext(sanitizedName)
    onlyExt  <- fileName == paste0(".", fileExt)

    # If the filename is empty, return the default.
    if (nchar(sanitizedName) == 0) {
      defaultName
    # If sanitizedName consists of just an extension -- meaning the name was
    # removed but the extension was preserved -- and if defaultName combined
    # with the preserved extension is an allowable size, return that.
    } else if (onlyExt && (nchar(defaultName) + 1 + nchar(fileExt) <= maxSize)) {
      paste(defaultName, fileExt, sep = ".")
    # If sanitizedName is an allowable size and if it doesn't consist of just an
    # extension, return it.
    } else if ((nchar(sanitizedName) <= maxSize) && !onlyExt) {
      sanitizedName
    # In all other cases, return defaultName.
    } else {
      defaultName
    }
  },
  sanitizedNames,
  default,
  USE.NAMES = FALSE)
}

FileUploadOperation <- R6Class(
  'FileUploadOperation',
  portable = FALSE,
  class = FALSE,
  public = list(
    .parent = NULL,
    .id = character(0),
    .files = data.frame(),
    .dir = character(0),
    .currentFileInfo = list(),
    .currentFileData = NULL,
    .pendingFileInfos = list(),

    initialize = function(parent, id, dir, fileInfos) {
      .parent <<- parent
      .id <<- id
      .files <<- data.frame(name=character(),
                            size=numeric(),
                            type=character(),
                            datapath=character(),
                            stringsAsFactors=FALSE)
      .dir <<- dir
      .pendingFileInfos <<- fileInfos
    },
    fileBegin = function() {
      if (length(.pendingFileInfos) < 1)
        stop("fileBegin called too many times")

      file <- .pendingFileInfos[[1]]
      .currentFileInfo <<- file
      .pendingFileInfos <<- tail(.pendingFileInfos, -1)

      basename <- .currentFileInfo$name
      filename <- file.path(.dir, sanitizeFileName(basename, as.character(length(.files$name))))
      row <- data.frame(name=file$name, size=file$size, type=file$type,
                        datapath=filename, stringsAsFactors=FALSE)

      if (length(.files$name) == 0)
        .files <<- row
      else
        .files <<- rbind(.files, row)

      .currentFileData <<- file(filename, open='wb')
    },
    fileChunk = function(rawdata) {
      writeBin(rawdata, .currentFileData)
    },
    fileEnd = function() {
      close(.currentFileData)
    },
    finish = function() {
      if (length(.pendingFileInfos) > 0)
        stop("File upload job was stopped prematurely")
      .parent$onJobFinished(.id)
      return(.files)
    }
  )
)

#' @include map.R
FileUploadContext <- R6Class(
  'FileUploadContext',
  class = FALSE,
  private = list(
    basedir = character(0),
    operations = 'Map',
    ids = character(0)  # Keep track of all ids used for file uploads
  ),
  public = list(
    initialize = function(dir=tempdir()) {
      private$basedir <- dir
      private$operations <- Map$new()
    },
    createUploadOperation = function(fileInfos) {
      while (TRUE) {
        id <- createUniqueId(12)
        private$ids <- c(private$ids, id)
        dir <- file.path(private$basedir, id)
        if (!dir.create(dir))
          next

        op <- FileUploadOperation$new(self, id, dir, fileInfos)
        private$operations$set(id, op)
        return(id)
      }
    },
    getUploadOperation = function(jobId) {
      private$operations$get(jobId)
    },
    onJobFinished = function(jobId) {
      private$operations$remove(jobId)
    },
    # Remove the directories containing file uploads; this is to be called when
    # a session ends.
    rmUploadDirs = function() {
      # Make sure all_paths is underneath the tempdir()
      if (!grepl(normalizePath(tempdir()), normalizePath(private$basedir), fixed = TRUE)) {
        stop("Won't remove upload path ", private$basedir,
          "because it is not under tempdir(): ", tempdir())
      }

      all_paths <- file.path(private$basedir, private$ids)
      unlink(all_paths, recursive = TRUE)
    }
  )
)
