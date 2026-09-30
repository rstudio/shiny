skip_if_not_shinytest2()
library(shinytest2)

# For #4281: `datesdisabled` must work with a non-default `format`, and must
# not corrupt the value of inputs with a 2-digit-year `format` (bootstrap-
# datepicker's setDatesDisabled() re-parses the input text with the display
# format, which turns "12" into year 12 AD).
app <- launch_test_app(function() {
  library(shiny)

  dd <- c("2012-03-01", "2012-03-02")

  ui <- fluidPage(
    dateInput("long", "dd/mm/yyyy + datesdisabled",
      value = "2012-02-29", format = "dd/mm/yyyy", datesdisabled = dd),
    dateInput("yy_plain", "mm/dd/yy, no datesdisabled",
      value = "2012-02-29", format = "mm/dd/yy"),
    dateInput("yy_dd", "mm/dd/yy + datesdisabled",
      value = "2012-02-29", format = "mm/dd/yy", datesdisabled = dd)
  )

  server <- function(input, output, session) {}

  shinyApp(ui, server)
})

# Open the picker, go to the next month (March 2012), and return the
# (in-month) days that are shown as disabled.
disabled_days_in_next_month <- function(id) {
  app$get_js(sprintf(
    "(function() {
       var $i = $('#%s input');
       $i.bsDatepicker('show');
       $('.datepicker-days .next').last().click();
       var days = $('.datepicker-days td.day.disabled')
         .not('.old, .new').map(function() { return $(this).text(); }).get();
       $i.bsDatepicker('hide');
       return days;
     })()", id
  ))
}

test_that("dateInput() keeps its value and disables dates for any format", {
  expect_equal(app$get_value(input = "long"), as.Date("2012-02-29"))
  expect_equal(app$get_value(input = "yy_plain"), as.Date("2012-02-29"))
  expect_equal(app$get_value(input = "yy_dd"), as.Date("2012-02-29"))

  expect_equal(unlist(disabled_days_in_next_month("long")), c("1", "2"))
  expect_equal(unlist(disabled_days_in_next_month("yy_dd")), c("1", "2"))
  expect_length(disabled_days_in_next_month("yy_plain"), 0)
})
