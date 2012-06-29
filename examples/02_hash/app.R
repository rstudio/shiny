library(digest)

input <- Observable$new(function() {
  str <- get.input('input1')
  if (get.input('addnewline'))
    str <- paste(str, "\n", sep='')
  return(str)
})

define.output('md5_hash', function() {
  digest(input$get.value(), algo='md5', serialize=F)
})
define.output('sha1_hash', function() {
  digest(input$get.value(), algo='sha1', serialize=F)
})