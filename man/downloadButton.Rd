\name{downloadButton}
\alias{downloadButton}
\alias{downloadLink}
\title{Create a download button or link}
\usage{
  downloadButton(outputId, label = "Download",
    class = NULL)

  downloadLink(outputId, label = "Download", class = NULL)
}
\arguments{
  \item{outputId}{The name of the output slot that the
  \code{downloadHandler} is assigned to.}

  \item{label}{The label that should appear on the button.}

  \item{class}{Additional CSS classes to apply to the tag,
  if any.}
}
\description{
  Use these functions to create a download button or link;
  when clicked, it will initiate a browser download. The
  filename and contents are specified by the corresponding
  \code{\link{downloadHandler}} defined in the server
  function.
}
\examples{
\dontrun{
# In server.R:
output$downloadData <- downloadHandler(
  filename = function() {
    paste('data-', Sys.Date(), '.csv', sep='')
  },
  content = function(con) {
    write.csv(data, con)
  }
)

# In ui.R:
downloadLink('downloadData', 'Download')
}
}
\seealso{
  downloadHandler
}

