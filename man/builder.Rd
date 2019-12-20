\name{builder}
\alias{builder}
\alias{tags}
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
\alias{hr}
\title{HTML Builder Functions}
\usage{
tags

p(..., .noWS = NULL)

h1(..., .noWS = NULL)

h2(..., .noWS = NULL)

h3(..., .noWS = NULL)

h4(..., .noWS = NULL)

h5(..., .noWS = NULL)

h6(..., .noWS = NULL)

a(..., .noWS = NULL)

br(..., .noWS = NULL)

div(..., .noWS = NULL)

span(..., .noWS = NULL)

pre(..., .noWS = NULL)

code(..., .noWS = NULL)

img(..., .noWS = NULL)

strong(..., .noWS = NULL)

em(..., .noWS = NULL)

hr(..., .noWS = NULL)
}
\arguments{
\item{...}{Attributes and children of the element. Named arguments become
attributes, and positional arguments become children. Valid children are
tags, single-character character vectors (which become text nodes), raw
HTML (see \code{\link{HTML}}), and \code{html_dependency} objects. You can
also pass lists that contain tags, text nodes, or HTML. To use boolean
attributes, use a named argument with a \code{NA} value. (see example)}

\item{.noWS}{A character vector used to omit some of the whitespace that
would normally be written around this tag. Valid options include
\code{before}, \code{after}, \code{outside}, \code{after-begin}, and
\code{before-end}. Any number of these options can be specified.}
}
\description{
Simple functions for constructing HTML documents.
}
\details{
The \code{tags} environment contains convenience functions for all valid
HTML5 tags. To generate tags that are not part of the HTML5 specification,
you can use the \code{\link{tag}()} function.

Dedicated functions are available for the most common HTML tags that do not
conflict with common R functions.

The result from these functions is a tag object, which can be converted using
\code{\link{as.character}()}.
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

# create an html5 audio tag with controls.
# controls is a boolean attributes
audio_tag <- tags$audio(
  controls = NA,
  tags$source(
    src = "myfile.wav",
    type = "audio/wav"
  )
)
cat(as.character(audio_tag))

# suppress the whitespace between tags
oneline <- tags$span(
  tags$strong("I'm strong", .noWS="outside")
)
cat(as.character(oneline))
}
\references{
\itemize{
   \item W3C html specification about boolean attributes
   \url{https://www.w3.org/TR/html5/infrastructure.html#sec-boolean-attributes}
 }
}
