\name{parseQueryString}
\alias{parseQueryString}
\title{Parse a GET query string from a URL}
\usage{
  parseQueryString(str)
}
\arguments{
  \item{str}{The query string. It can have a leading
  \code{"?"} or not.}
}
\description{
  Returns a named character vector of key-value pairs.
}
\examples{
parseQueryString("?foo=1&bar=b\%20a\%20r")

\dontrun{
# Example of usage within a Shiny app
shinyServer(function(input, output, clientData) {

  output$queryText <- renderText({
    query <- parseQueryString(clientData$url_search)

    # Ways of accessing the values
    if (as.numeric(query$foo) == 1) {
      # Do something
    }
    if (query[["bar"]] == "targetstring") {
      # Do something else
    }

    # Return a string with key-value pairs
    paste(names(query), query, sep = "=", collapse=", ")
  })
})
}
}

