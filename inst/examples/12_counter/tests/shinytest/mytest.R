app <- ShinyDriver$new("../../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(`mymodule1-button` = "click")
app$setInputs(`mymodule1-button` = "click")
app$snapshot()
