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
# '[1] "foo"\n[1] "bar"'

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
