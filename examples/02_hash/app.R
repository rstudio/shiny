library(digest)

text <- observable(function() {
  str <- input$input1
  if (input$addnewline)
    str <- paste(str, "\n", sep='')
  return(str)
})

define.output('md5_hash', function() {
  digest(text$get.value(), algo='md5', serialize=F)
})
define.output('sha1_hash', function() {
  digest(text$get.value(), algo='sha1', serialize=F)
})