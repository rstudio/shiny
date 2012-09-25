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
  one or more files. \bold{Experimental feature. Only works
  in some browsers (primarily tested on Chrome and
  Firefox).}
}

