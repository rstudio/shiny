test_that("Render functions correctly handle quosures", {
  # Normally, quosures are not unwrapped at creation time.
  # However, using inject() will make it unwrap at creation time.

  a <- 1
  r1 <- inject(renderText({ !!a }))
  r2 <- renderText({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "1")
  expect_identical(r2(), "2")

  a <- 1
  r1 <- inject(renderPrint({ !!a }))
  r2 <- renderPrint({ eval_tidy(quo(!!a)) })
  a <- 2
  expect_identical(r1(), "[1] 1")
  expect_identical(r2(), "[1] 2")

  a <- 1
  r1 <- inject(renderUI({ tags$p(!!a) }))
  r2 <- renderUI({ eval_tidy(quo(tags$p(!!a))) })
  a <- 2
  res1 <- r1(shinysession = MockShinySession$new(), name = "foo")
  expect_identical(as.character(res1$html), "<p>1</p>")
  res2 <- r2(shinysession = MockShinySession$new(), name = "foo")
  expect_identical(as.character(res2$html), "<p>2</p>")

  a <- 1
  r1 <- inject(renderTable({ pressure[!!a, ] }, digits = 1))
  r2 <- renderTable({ eval_tidy(quo(pressure[!!a, ])) }, digits = 1)
  a <- 2
  expect_true(grepl("0\\.0", r1()))
  expect_true(grepl("20\\.0", r2()))
})
