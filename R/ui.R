

# attribs, text, children

# explicit named args (my attributes)
# special named args (class, style, attribs)
# un-named character string (content)
# children (which you must explicitly render)
# you return a function 

processArgs <- function(args) {
  a <- list()
  a$class = args$class
  a$style = args$style
  a$attribs = args$attribs
  
  return (a)
}

p <- function(...) {
  args <- processArgs(list(...))
}



defineUI <- function(header, inputs, outputs, stylesheet) {
  
}