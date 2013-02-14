\name{textOutput}
\alias{textOutput}
\title{Create a text output element}
\usage{
  textOutput(outputId)
}
\arguments{
  \item{outputId}{output variable to read the value from}
}
\value{
  A text output element that can be included in a panel
}
\description{
  Render a reactive output variable as text within an
  application page. The text will be included within an
  HTML \code{div} tag.
}
\details{
  Text is HTML-escaped prior to rendering. This element is
  often used to display \link{renderText} output variables.
}
\examples{
h3(textOutput("caption"))
}

