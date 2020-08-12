app <- ShinyDriver$new("../../")
app$snapshotInit("one-child-weight")

app$setInputs(`single-age_years` = 4)
app$setInputs(`single-wt` = 16)
app$snapshot()
