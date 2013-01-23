\name{reactivePrint}
\alias{reactivePrint}
\title{Printable Output}
\usage{
  reactivePrint(func)
}
\arguments{
  \item{func}{A function that may print output and/or
  return a printable R object.}
}
\description{
  Makes a reactive version of the given function that
  captures any printed output, and also captures its
  printable result (unless \code{\link{invisible}}), into a
  string. The resulting function is suitable for assigning
  to an \code{output} slot.
}
\details{
  The corresponding HTML output tag can be anything (though
  \code{pre} is recommended if you need a monospace font
  and whitespace preserved) and should have the CSS class
  name \code{shiny-text-output}.

  The result of executing \code{func} will be printed
  inside a \code{\link[utils]{capture.output}} call.

  Note that unlike most other Shiny output functions, if
  the given function returns \code{NULL} then \code{NULL}
  will actually be visible in the output. To display
  nothing, make your function return
  \code{\link{invisible}()}.
}
\examples{
isolate({

# reactivePrint captures any print output, converts it to a string, and
# returns it
visFun <- reactivePrint(function() "foo")
visFun()
# '[1] "foo"'

invisFun <- reactivePrint(function() invisible("foo"))
invisFun()
# ''

multiprintFun <- reactivePrint(function() {
  print("foo");
  "bar"
})
multiprintFun()
# '[1] "foo"\\n[1] "bar"'

nullFun <- reactivePrint(function() NULL)
nullFun()
# 'NULL'

invisNullFun <- reactivePrint(function() invisible(NULL))
invisNullFun()
# ''

vecFun <- reactivePrint(function() 1:5)
vecFun()
# '[1] 1 2 3 4 5'


# Contrast with reactiveText, which takes the value returned from the function
# and uses cat() to convert it to a string
visFun <- reactiveText(function() "foo")
visFun()
# 'foo'

invisFun <- reactiveText(function() invisible("foo"))
invisFun()
# 'foo'

multiprintFun <- reactiveText(function() {
  print("foo");
  "bar"
})
multiprintFun()
# 'bar'

nullFun <- reactiveText(function() NULL)
nullFun()
# ''

invisNullFun <- reactiveText(function() invisible(NULL))
invisNullFun()
# ''

vecFun <- reactiveText(function() 1:5)
vecFun()
# '1 2 3 4 5'

})
}
\seealso{
  \code{\link{reactiveText}} for displaying the value
  returned from a function, instead of the printed output.
}

