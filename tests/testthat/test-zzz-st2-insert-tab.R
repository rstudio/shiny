skip_if_not_shinytest2()
library(shinytest2)

app <- launch_test_app(function() {
  library(shiny)
  library(bslib)

  ui <- page_fluid(
    actionButton("add", "Add tab"),
    navset_tab(
      id = "tabs",
      nav_panel(
        "Initial",
        value = "initial",
        "Initial content"
      )
    )
  )

  server <- function(input, output, session) {
    number <- reactiveVal(0)

    observeEvent(input$add, {
      n <- number() + 1
      number(n)

      nav_insert(
        id = "tabs",
        nav = nav_panel(
          paste("Dynamic", n),
          value = paste0("dynamic-", n),
          paste("Content", n)
        )
      )
    })
  }

  shinyApp(ui, server)
})

tab_relationships <- function() {
  app$get_js(
    '(function() {
      return Array.from(
        document.querySelectorAll("#tabs a[data-value^=\\"dynamic-\\"]")
      ).map(function(tab) {
        var href = tab.getAttribute("href");
        var panelId =
          href && href.startsWith("#") ? href.slice(1) : null;
        var panel =
          panelId ? document.getElementById(panelId) : null;
        var controls = tab.getAttribute("aria-controls");
        var labelledby =
          panel ? panel.getAttribute("aria-labelledby") : null;

        return {
          value: tab.dataset.value,
          tabId: tab.id,
          href: href,
          controls: controls,
          panelId: panel ? panel.id : null,
          labelledby: labelledby,
          controlsTargetExists:
            controls !== null &&
            document.getElementById(controls) !== null,
          labelledbyTargetExists:
            labelledby !== null &&
            document.getElementById(labelledby) !== null
        };
      });
    })()'
  )
}

test_that("inserted tabs have unique final ids and valid relationships", {
  # Insert twice to verify final ids are unique across dynamic tabs.
  app$click("add")
  app$wait_for_idle()

  app$click("add")
  app$wait_for_idle()

  tabs <- tab_relationships()

  expect_length(tabs, 2)

  for (tab in tabs) {
    expect_equal(tab$href, paste0("#", tab$panelId))
    expect_equal(tab$controls, tab$panelId)
    expect_equal(tab$labelledby, tab$tabId)

    expect_true(tab$controlsTargetExists)
    expect_true(tab$labelledbyTargetExists)

    expect_false(tab$controls == "tab-tsid-id")
    expect_false(tab$tabId == "tab-tsid-id-tab")
  }

  tab_ids <- vapply(tabs, function(x) x$tabId, character(1))
  panel_ids <- vapply(tabs, function(x) x$panelId, character(1))

  expect_length(unique(tab_ids), 2)
  expect_length(unique(panel_ids), 2)
})
