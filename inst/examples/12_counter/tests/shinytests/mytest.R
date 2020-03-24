app <- ShinyDriver$new("../../")
app$snapshotInit("counter-test")

app$snapshot()
app$setInputs(`counter1-button` = "click")
app$setInputs(`counter1-button` = "click")
app$snapshot()
