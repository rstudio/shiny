# tabsetPanel() et al. use p_randomInt() to generate ids (which uses withPrivateSeed()),
# so we need to fix Shiny's private seed in order to make their HTML output deterministic
navlist_panel <- function(...) {
  withPrivateSeed(set.seed(100))
  navlistPanel(...)
}
navbar_page <- function(...) {
  withPrivateSeed(set.seed(100))
  navbarPage(...)
}
tabset_panel <- function(...) {
  withPrivateSeed(set.seed(100))
  tabsetPanel(...)
}

expect_snapshot2 <- function(...) {
  if (getRversion() < "3.6.0") {
    skip("Skipping snapshots on R < 3.6 because of different RNG method")
  }
  if (packageVersion("htmltools") <= "0.5.6" && getRversion() > "4.3.1") {
    skip("Skipping snapshots since htmltools is 'outdated'")
  }
  expect_snapshot(...)
}

expect_snapshot_bslib <- function(x, ...) {
  expect_snapshot2(bslib_tags(x), ...)
}

# Simulates the UI tags that would be produced by
# shinyApp(bootstrapPage(theme), function(...) {})
bslib_tags <- function(ui, theme = bslib::bs_theme()) {
  skip_if_not_installed("bslib", "0.5.1.9000")
  old_theme <- getCurrentTheme()
  on.exit(setCurrentTheme(old_theme), add = TRUE)
  setCurrentTheme(theme)
  htmltools::renderTags(ui)$html
}

panels <- list(
  tabPanel("A", "a"),
  tabPanel("B", "b", icon = icon("github")),
  navbarMenu("Menu", tabPanel("C", "c"))
)

test_that("tabsetPanel() markup is correct", {
  default <- tabset_panel(!!!panels)
  pills <- tabset_panel(
    !!!panels, type = "pills", selected = "B",
    header = div(class = "content-header"),
    footer = div(class = "content-footer")
  )
  # BS3
  expect_snapshot2(default)
  expect_snapshot2(pills)
  # BS4
  expect_snapshot_bslib(default)
  expect_snapshot_bslib(pills)

  # Make sure .active class gets added to both the .dropdown as well as the
  # .dropdown-menu's tab
  dropdown_active <- tabset_panel(!!!panels, selected = "C")
  expect_snapshot2(dropdown_active)
})

test_that("navbarPage() markup is correct", {
  nav_page <- navbar_page("Title", !!!panels)
  expect_snapshot2(nav_page)
  expect_snapshot_bslib(nav_page)
})

# navlistPanel() can handle strings, but the others can't
test_that("String input is handled properly", {
  nav_list <- navlist_panel(!!!c(list("A header"), panels))
  expect_snapshot2(nav_list)
  expect_snapshot_bslib(nav_list)
  expect_error(
    tabsetPanel(!!!c(list("A header"), panels)),
    "tabPanel"
  )
})

test_that("Shiny.tag input produces a warning", {
  panels3 <- c(list(div("A div")), panels)
  expect_warning(tab_tags <- tabset_panel(!!!panels3))
  # Carson March 12th, 2021: Yes, he 'empty nav' output here isn't
  # sensible (which is why we now throw a warning), but it's probably
  # too late to change the behavior (it could break user code to do
  # anything different)
  expect_snapshot2(tab_tags)
})

test_that("tabPanelBody validates it's input", {
  expect_silent(tabPanelBody("a", "content1", "content2", icon = icon("table")))
  expect_silent(tabPanelBody(value = "a", "content1", "content2", icon = icon("table")))

  expect_error(tabPanelBody())
  expect_error(tabPanelBody(NULL), "single, non-empty string")
  expect_error(tabPanelBody(1), "single, non-empty string")
  expect_error(tabPanelBody(TRUE), "single, non-empty string")
  expect_error(tabPanelBody(NA), "single, non-empty string")
  expect_error(tabPanelBody(NA_character_), "single, non-empty string")
  expect_error(tabPanelBody(""), "single, non-empty string")
  expect_error(tabPanelBody(letters[1:2]), "single, non-empty string")
})


# https://github.com/rstudio/shiny/issues/3352
test_that("tabItem titles can contain tag objects", {
  title <- tagList(tags$i("Hello"), "world")
  x <- tabsetPanel(tabPanel(title, "tab content"))
  x <- renderTags(x)

  # Result should contain (with different whitespace):
  #   "<a ....> <i>Hello</i> world"
  # As opposed to:
  #   "<a ....>&lt;i&gt;Hello&lt;/i&gt; world
  expect_match(x$html, "<a [^>]+>\\s*<i>Hello</i>\\s+world")
})
