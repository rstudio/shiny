\name{fileInput}
\alias{fileInput}
\title{File Upload Control}
\usage{
  fileInput(inputId, label, multiple = FALSE,
    accept = NULL)
}
\arguments{
  \item{inputId}{Input variable to assign the control's
  value to.}

  \item{label}{Display label for the control.}

  \item{multiple}{Whether the user should be allowed to
  select and upload multiple files at once.}

  \item{accept}{A character vector of MIME types; gives the
  browser a hint of what kind of files the server is
  expecting.}
}
\description{
  Create a file upload control that can be used to upload
  one or more files. \bold{Does not work on older browsers,
  including Internet Explorer 9 and earlier.}
}
\details{
  Whenever a file upload completes, the corresponding input
  variable is set to a dataframe. This dataframe contains
  one row for each selected file, and the following
  columns: \describe{ \item{\code{name}}{The filename
  provided by the web browser. This is \strong{not} the
  path to read to get at the actual data that was uploaded
  (see \code{datapath} column).} \item{\code{size}}{The
  size of the uploaded data, in bytes.}
  \item{\code{type}}{The MIME type reported by the browser
  (for example, \code{text/plain}), or empty string if the
  browser didn't know.} \item{\code{datapath}}{The path to
  a temp file that contains the data that was uploaded.
  This file may be deleted if the user performs another
  upload operation.} }
}

