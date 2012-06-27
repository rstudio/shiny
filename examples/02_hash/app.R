library(digest)

input <- Observable$new(function() {
  str <- get.shiny.input('input1')
  if (get.shiny.input('addnewline'))
    str <- paste(str, "\n", sep='')
  return(str)
})

define.shiny.output('md5_hash', function() {
  digest(input$get.value(), algo='md5', serialize=F)
})
define.shiny.output('sha1_hash', function() {
  digest(input$get.value(), algo='sha1', serialize=F)
})