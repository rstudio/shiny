\name{includeHTML}
\alias{includeHTML}
\alias{includeText}
\alias{includeMarkdown}

\usage{
  includeHTML(path)
  includeText(path)
  includeMarkdown(path)
}

\title{Include Content From a File}

\arguments{
  \item{path}{
    The path of the file to be included. It is highly recommended to
    use a relative path (the base path being the Shiny application
    directory), not an absolute path.
  }
}
\description{
  Include HTML, text, or rendered Markdown into a \link[=shinyUI]{Shiny UI}.
}
\details{
  These functions provide a convenient way to include an extensive amount
  of HTML, textual, or Markdown content, rather than using a large literal R
  string.
}
\note{
  \code{includeText} escapes its contents, but does no other processing. This
  means that hard breaks and multiple spaces will be rendered as they usually
  are in HTML: as a single space character. If you are looking for
  preformatted text, wrap the call with \code{\link{pre}}, or consider using
  \code{includeMarkdown} instead.
}
\note{
  The \code{includeMarkdown} function requires the \code{markdown} package.
}
\examples{
doc <- tags$html(
  tags$head(
    tags$title('My first page')
  ),
  tags$body(
    h1('My first heading'),
    p('My first paragraph, with some ',
      strong('bold'),
      ' text.'),
    div(id='myDiv', class='simpleDiv',
        'Here is a div with some attributes.')
  )
)
cat(as.character(doc))
}