\name{builder}
\alias{p}
\alias{h1}
\alias{h2}
\alias{h3}
\alias{h4}
\alias{h5}
\alias{h6}
\alias{a}
\alias{br}
\alias{div}
\alias{span}
\alias{pre}
\alias{code}
\alias{img}
\alias{strong}
\alias{em}
\alias{tags}

\usage{
  p(...)
  h1(...)
  h2(...)
  h3(...)
  h4(...)
  h5(...)
  h6(...)
  a(...)
  br(...)
  div(...)
  span(...)
  pre(...)
  code(...)
  img(...)
  strong(...)
  em(...)
  
  tags
}

\title{HTML Builder Functions}

\arguments{
  \item{...}{Attributes and children of the element. Named arguments
  become attributes, and positional arguments become children. Valid
  children are tags, single-character character vectors (which become
  text nodes), and raw HTML (see \code{\link{HTML}}). You can also
  pass lists that contain tags, text nodes, and HTML.}
}
\description{
  Simple functions for constructing HTML documents.
}
\details{
  The \code{tags} environment contains convenience functions for all
  valid HTML5 tags. To generate tags that are not part of the HTML5
  specification, you can use the \link{tag} function.
  
  Dedicated functions are available for the most common HTML tags
  that do not conflict with common R functions.
  
  The result from these functions is a tag object, which can be
  converted using \code{as.character}.
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