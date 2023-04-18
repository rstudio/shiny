test_that("Radio buttons and checkboxes work with modules", {
  session <- MockShinySession$new()

  updateRadioButtons(session, "test1", label = "Label", choices = letters[1:5])
  session$verifyInputMessage("test1",
    expect_equal(.$label, "Label"),
    expect_equal(.$value, "a"),
    expect_true(grepl('"mock-session-test1"', .$options)),
    !expect_false(grepl('"test1"', .$options)) ## negate returned FALSE from expect_false
  )

  updateCheckboxGroupInput(session, "test2", label = "Label", choices = LETTERS[1:5])
  session$verifyInputMessage("test2",
     expect_equal(.$label, "Label"),
     expect_null(.$value),
     expect_true(grepl('"mock-session-test2"', .$options)),
     !expect_false(grepl('"test2"', .$options))
  )
})
