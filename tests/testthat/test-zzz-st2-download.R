skip_if_not_shinytest2()
library(shinytest2)

# The app is started manually via callr and AppDriver connects to the running
# URL rather than using AppDriver$new(fn). This is necessary because
# AppDriver$new(fn) navigates to the app immediately after the port opens,
# but a complex app hasn't finished serving all static resources yet at that
# point. In the devtools-loaded context this causes a race where shinyjs's
# inline handlers fire before shiny.min.js has loaded, leaving Shiny
# undefined. Polling with httr::GET() ensures the app is fully ready before
# AppDriver connects.
#
# Note: bslib is avoided for the toggle controls (checkboxInput is used
# instead) because bslib adds a <script type="module"> tag that is deferred
# and also contributes to the resource-loading race described above.
app_process <- callr::r_bg(
  function(port) {
    pkgload::load_all(quiet = TRUE)
    library(shinyjs)

    handler <- function() {
      downloadHandler(
        filename = "test.txt",
        content = function(file) writeLines("test", file)
      )
    }

    ui <- fluidPage(
      useShinyjs(),

      h3("downloadButton"),

      uiOutput("btn_auto_ui"),
      checkboxInput("toggle_btn_auto", "Enabled", value = TRUE),

      uiOutput("btn_off_ui"),
      checkboxInput("toggle_btn_off", "Enabled", value = FALSE),

      uiOutput("btn_on_ui"),
      checkboxInput("toggle_btn_on", "Enabled", value = TRUE),

      uiOutput("btn_shinyjs_ui"),
      checkboxInput("toggle_btn_shinyjs", "Enabled", value = FALSE),

      h3("downloadLink"),

      uiOutput("lnk_auto_ui"),
      checkboxInput("toggle_lnk_auto", "Enabled", value = TRUE),

      uiOutput("lnk_off_ui"),
      checkboxInput("toggle_lnk_off", "Enabled", value = FALSE),

      uiOutput("lnk_on_ui"),
      checkboxInput("toggle_lnk_on", "Enabled", value = TRUE),

      uiOutput("lnk_shinyjs_ui"),
      checkboxInput("toggle_lnk_shinyjs", "Enabled", value = FALSE)
    )

    server <- function(input, output, session) {
      output$btn_auto <- handler()
      output$btn_off <- handler()
      output$btn_on <- handler()
      output$btn_shinyjs <- handler()
      output$lnk_auto <- handler()
      output$lnk_off <- handler()
      output$lnk_on <- handler()
      output$lnk_shinyjs <- handler()

      output$btn_auto_ui <- renderUI({
        if (isTRUE(input$toggle_btn_auto)) {
          downloadButton("btn_auto", "Auto (default)")
        } else {
          downloadButton("btn_auto", "Auto (default)", enabled = FALSE)
        }
      })

      output$btn_off_ui <- renderUI({
        if (isTRUE(input$toggle_btn_off)) {
          downloadButton("btn_off", "Disabled", enabled = TRUE)
        } else {
          downloadButton("btn_off", "Disabled", enabled = FALSE)
        }
      })

      output$btn_on_ui <- renderUI({
        if (isTRUE(input$toggle_btn_on)) {
          downloadButton("btn_on", "Pre-enabled", enabled = TRUE)
        } else {
          downloadButton("btn_on", "Pre-enabled", enabled = FALSE)
        }
      })

      output$btn_shinyjs_ui <- renderUI({
        btn <- downloadButton("btn_shinyjs", "shinyjs-disabled")
        if (!isTRUE(input$toggle_btn_shinyjs)) {
          shinyjs::disabled(btn)
        } else {
          btn
        }
      })

      output$lnk_auto_ui <- renderUI({
        if (isTRUE(input$toggle_lnk_auto)) {
          downloadLink("lnk_auto", "Auto (default)")
        } else {
          downloadLink("lnk_auto", "Auto (default)", enabled = FALSE)
        }
      })

      output$lnk_off_ui <- renderUI({
        if (isTRUE(input$toggle_lnk_off)) {
          downloadLink("lnk_off", "Disabled", enabled = TRUE)
        } else {
          downloadLink("lnk_off", "Disabled", enabled = FALSE)
        }
      })

      output$lnk_on_ui <- renderUI({
        if (isTRUE(input$toggle_lnk_on)) {
          downloadLink("lnk_on", "Pre-enabled", enabled = TRUE)
        } else {
          downloadLink("lnk_on", "Pre-enabled", enabled = FALSE)
        }
      })

      output$lnk_shinyjs_ui <- renderUI({
        lnk <- downloadLink("lnk_shinyjs", "shinyjs-disabled")
        if (!isTRUE(input$toggle_lnk_shinyjs)) {
          shinyjs::disabled(lnk)
        } else {
          lnk
        }
      })
    }

    shiny::runApp(
      shinyApp(ui, server),
      port = port,
      host = "127.0.0.1",
      launch.browser = FALSE,
      quiet = TRUE,
      test.mode = TRUE
    )
  },
  args = list(port = 7315L)
)
withr::defer(app_process$kill())

# Wait for the app to be ready before connecting
for (i in seq_len(20)) {
  Sys.sleep(0.5)
  ready <- tryCatch(
    {
      httr::GET("http://127.0.0.1:7315", httr::timeout(1))
      TRUE
    },
    error = function(e) FALSE
  )
  if (ready) break
}
if (!ready) skip("Download button test app failed to start")

# Start up app once and share across all tests
app <- AppDriver$new("http://127.0.0.1:7315")
withr::defer({ app$stop() })
app$wait_for_idle()

is_disabled <- function(id) {
  app$get_js(sprintf(
    "var el = document.querySelector('#%s');
     el.classList.contains('disabled') &&
     el.getAttribute('aria-disabled') === 'true' &&
     el.getAttribute('tabindex') === '-1';", id
  ))
}

click_toggle <- function(id) {
  input_name <- paste0("toggle_", id)
  current <- isTRUE(app$get_value(input = input_name))
  do.call(app$set_inputs, setNames(list(!current), input_name))
  app$wait_for_idle()
}

test_download_enabled_behavior <- function(type, prefix) {
  id <- function(suffix) paste0(prefix, suffix)

  test_that(paste(type, "(enabled='auto') auto-enables after server init"), {
    expect_false(is_disabled(id("auto")))
    expect_null(app$get_js(sprintf(
      "document.querySelector('#%s').getAttribute('aria-disabled')", id("auto")
    )))
  })

  test_that(paste(type, "(enabled=FALSE) stays disabled after server init"), {
    expect_true(is_disabled(id("off")))
  })

  test_that(paste(type, "(enabled=TRUE) starts and stays enabled"), {
    expect_false(is_disabled(id("on")))
    expect_null(app$get_js(sprintf(
      "document.querySelector('#%s').getAttribute('aria-disabled')", id("on")
    )))
  })

  test_that(paste(type, "disabled via shinyjs stays disabled after server init"), {
    expect_true(is_disabled(id("shinyjs")))
  })

  test_that(paste(type, "(enabled='auto') can be toggled off and back on"), {
    click_toggle(id("auto"))
    expect_true(is_disabled(id("auto")))

    click_toggle(id("auto"))
    expect_false(is_disabled(id("auto")))
  })

  test_that(paste(type, "(enabled=FALSE) can be toggled on and back off"), {
    click_toggle(id("off"))
    expect_false(is_disabled(id("off")))

    click_toggle(id("off"))
    expect_true(is_disabled(id("off")))
  })

  test_that(paste(type, "(enabled=TRUE) can be toggled off and back on"), {
    click_toggle(id("on"))
    expect_true(is_disabled(id("on")))

    click_toggle(id("on"))
    expect_false(is_disabled(id("on")))
  })

  test_that(paste(type, "disabled via shinyjs can be toggled on and back off"), {
    click_toggle(id("shinyjs"))
    expect_false(is_disabled(id("shinyjs")))

    click_toggle(id("shinyjs"))
    expect_true(is_disabled(id("shinyjs")))
  })
}

# ---------------------------------------------------------------------------
# downloadButton
# ---------------------------------------------------------------------------

test_download_enabled_behavior("downloadButton", "btn_")

# ---------------------------------------------------------------------------
# downloadLink
# ---------------------------------------------------------------------------

test_download_enabled_behavior("downloadLink", "lnk_")
