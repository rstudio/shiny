test_that("Action button accepts class arguments", {
  make_button <- function(class) {
    if (missing(class)) {
      actionButton("id", "label")
    } else {
      actionButton("id", "label", class = class)
    }
  }
  act <- make_button()
  get_class <- function(act) {
    act_html <- format(act)
    regmatches(act_html, regexec("class=\"[^\"]\"", act_html))[[1]]
  }
  act_class <- get_class(act)
  expect_equal(
    get_class(make_button(NULL)), act_class
  )
  expect_equal(
    get_class(make_button(NA)), act_class
  )
  expect_equal(
    get_class(make_button("extra")), sub("\"$", " extra\"", act_class)
  )
  expect_equal(
    get_class(make_button("extra extra2")), sub("\"$", " extra extra2\"", act_class)
  )
})



test_that("Action link accepts class arguments", {
  make_link <- function(class) {
    if (missing(class)) {
      actionLink("id", "label")
    } else {
      actionLink("id", "label", class = class)
    }
  }
  act <- make_link()
  get_class <- function(act) {
    act_html <- format(act)
    regmatches(act_html, regexec("class=\"[^\"]\"", act_html))[[1]]
  }
  act_class <- get_class(act)
  expect_equal(
    get_class(make_link(NULL)), act_class
  )
  expect_equal(
    get_class(make_link(NA)), act_class
  )
  expect_equal(
    get_class(make_link("extra")), sub("\"$", " extra\"", act_class)
  )
  expect_equal(
    get_class(make_link("extra extra2")), sub("\"$", " extra extra2\"", act_class)
  )
})

test_that("Action button allows icon customization", {
  # No separator between icon and label
  expect_snapshot(actionButton("foo", "Click me"))

  # Should include separator between icon and label
  expect_snapshot(
    actionButton("foo", "Click me", icon = icon("star"))
  )

  # Warn on a non-HTML icon
  expect_warning(
    actionButton("foo", "Click me", icon = "not an icon"),
    "non-HTML value was provided"
  )

  # Allows for arbitrary HTML as icon
  btn <- expect_no_warning(
    actionButton("foo", "Click me", icon = tags$svg())
  )
  btn2 <- expect_no_warning(
    actionButton("foo", "Click me", icon = tagList(tags$svg()))
  )
  btn3 <- expect_no_warning(
    actionButton("foo", "Click me", icon = list(tags$svg()))
  )
  btn4 <- expect_no_warning(
    actionButton("foo", "Click me", icon = HTML("<svg></svg>"))
  )

  # Ignore newlines+indentation for comparison
  as_character <- function(x) {
    gsub("\\n\\s*", "", as.character(x))
  }

  expect_equal(as_character(btn), as_character(btn2))
  expect_equal(as_character(btn2), as_character(btn3))
  expect_equal(as_character(btn3), as_character(btn4))
})

test_that("actionLink uses .noWS to prevent underline rendering issues", {
  # actionLink should generate compact HTML without whitespace between tags
  # This prevents the underline from extending beyond the visible text

  # Test without icon
  link <- actionLink("test_link", "Click me")
  link_html <- as.character(link)

  # Verify no newlines/whitespace between closing > and opening <span
  expect_false(
    grepl(">\n\\s+<span", link_html),
    info = "actionLink should not have whitespace between tags"
  )

  # Test with icon
  link_icon <- actionLink("test_link2", "Click me", icon = icon("star"))
  link_icon_html <- as.character(link_icon)

  # Should also have no whitespace between icon span and label span
  expect_false(
    grepl(">\n\\s+<span", link_icon_html),
    info = "actionLink with icon should not have whitespace between tags"
  )
  expect_snapshot(actionLink("foo", "Click me"))
  expect_snapshot(actionLink("foo", "Click me", icon = icon("star")))
})
