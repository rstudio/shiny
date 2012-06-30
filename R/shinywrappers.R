suppressPackageStartupMessages({
  library(caTools)
  library(xtable)
})

reactivePlot <- function(func, ...) {
  reactive(function() {
    png.file <- tempfile(fileext='.png')
    png(filename=png.file, ...)
    func()
    dev.off()
    
    bytes <- file.info(png.file)$size
    if (is.na(bytes))
      return(NULL)
    
    b64 <- base64encode(readBin(png.file, 'raw', n=bytes))
    return(paste("data:image/png;base64,", b64, sep=''))
  })
}

reactiveTable <- function(func, ...) {
  reactive(function() {
    data <- func()
    return(paste(
      capture.output(
        print(xtable(data, ...), 
              type='html', 
              html.table.attributes='class="data"')),
      collapse="\n"))
  })
}

reactiveText <- function(func, ...) {
  reactive(function() {
    x <- withVisible(func())
    if (x$visible)
      return(paste(capture.output(print(x$value)), collapse="\n"))
    else
      return(x)
  })
}