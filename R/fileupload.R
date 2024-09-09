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

# @description Returns a file's extension, with a leading dot, if one can be
#   found. A valid extension contains only alphanumeric characters. If there is
#   no extension, or if it contains non-alphanumeric characters, an empty
#   string is returned.
# @param x character vector giving file paths.
# @return The extension of \code{x}, with a leading dot, if one was found.
#   Otherwise, an empty character vector.
maybeGetExtension <- function(x) {
  ext <- tools::file_ext(x)
  ifelse(ext == "", ext, paste0(".", ext))
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

      fileBasename <- basename(.currentFileInfo$name)
      filename <- file.path(.dir, paste0(as.character(createUniqueId(8)), maybeGetExtension(fileBasename)))
      row <- data.frame(name=fileBasename, size=file$size, type=file$type,
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
      
      afterUpload <- getShinyOption("afterUpload", default = function(f) {})
      afterUpload(.files)

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
