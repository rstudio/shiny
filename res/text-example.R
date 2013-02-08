isolate({

# renderPrint captures any print output, converts it to a string, and
# returns it
visFun <- renderPrint(function() "foo")
visFun()
# '[1] "foo"'

invisFun <- renderPrint(function() invisible("foo"))
invisFun()
# ''

multiprintFun <- renderPrint(function() {
  print("foo");
  "bar"
})
multiprintFun()
# '[1] "foo"\n[1] "bar"'

nullFun <- renderPrint(function() NULL)
nullFun()
# 'NULL'

invisNullFun <- renderPrint(function() invisible(NULL))
invisNullFun()
# ''

vecFun <- renderPrint(function() 1:5)
vecFun()
# '[1] 1 2 3 4 5'


# Contrast with renderText, which takes the value returned from the function
# and uses cat() to convert it to a string
visFun <- renderText(function() "foo")
visFun()
# 'foo'

invisFun <- renderText(function() invisible("foo"))
invisFun()
# 'foo'

multiprintFun <- renderText(function() {
  print("foo");
  "bar"
})
multiprintFun()
# 'bar'

nullFun <- renderText(function() NULL)
nullFun()
# ''

invisNullFun <- renderText(function() invisible(NULL))
invisNullFun()
# ''

vecFun <- renderText(function() 1:5)
vecFun()
# '1 2 3 4 5'

})
