\name{imageOutput}
\alias{imageOutput}
\title{Create a image output element}
\usage{
  imageOutput(outputId, width = "100\%", height = "400px")
}
\arguments{
  \item{outputId}{output variable to read the image from}

  \item{width}{Image width. Must be a valid CSS unit (like
  \code{"100\%"}, \code{"400px"}, \code{"auto"}) or a
  number, which will be coerced to a string and have
  \code{"px"} appended.}

  \item{height}{Image height}
}
\value{
  An image output element that can be included in a panel
}
\description{
  Render a \link{renderImage} within an application page.
}
\examples{
# Show an image
mainPanel(
  imageOutput("dataImage")
)
}

