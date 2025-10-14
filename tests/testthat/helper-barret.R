# Personal debugging function -------------------------------
# system("air format ./R/bind-otel.R")
# Rscript -e "devtools::load_all(); devtools::load_all(\"~/rstudio/ellmer/ellmer.nosync\"); dev_barret()"

# TODO: Remove this function when done debugging
dev_barret <- function() {
  ## Ospan pkgs
  # pak::pak("cran::mirai", upgrade = TRUE)
  # pak::pak("r-lib/httr2#729")
  # pak::pak("tidyverse/ellmer#526")
  ## Prettier tool calls
  # pak::pak("rstudio/shinychat/pkg-r")

  withr::with_options(
    list(
      OTEL_TRACES_EXPORTER = Sys.getenv("LOGFIRE_OTEL_TRACES_EXPORTER"),
      OTEL_EXPORTER_OTLP_ENDPOINT = Sys.getenv(
        "LOGFIRE_OTEL_EXPORTER_OTLP_ENDPOINT"
      ),
      OTEL_EXPORTER_OTLP_HEADERS = Sys.getenv(
        "LOGFIRE_OTEL_EXPORTER_OTLP_HEADERS"
      ),
      OTEL_LOGS_EXPORTER = Sys.getenv("LOGFIRE_OTEL_LOGS_EXPORTER"),
      OTEL_LOG_LEVEL = Sys.getenv("LOGFIRE_OTEL_LOG_LEVEL"),
      OTEL_METRICS_EXPORTER = Sys.getenv("LOGFIRE_OTEL_METRICS_EXPORTER")
    ),
    {
      mirai::daemons(1)

      bind_val <- "none"
      # bind_val <- "all"

      # Enhanced from: https://posit-dev.github.io/shinychat/r/articles/tool-ui.html#alternative-html-display
      get_weather_forecast <- ellmer::tool(
        function(lat, lon, location_name) {
          mirai::mirai(
            {
              otel::log_info(
                "Getting weather forecast",
                logger = otel::get_logger("weather-app")
              )
              forecast_data <- weathR::point_tomorrow(lat, lon, short = FALSE)
              forecast_table <- gt::as_raw_html(gt::gt(forecast_data))
              list(data = forecast_data, table = forecast_table)
            },
            lat = lat,
            lon = lon
          ) |>
            promises::then(function(forecast_info) {
              ellmer::ContentToolResult(
                forecast_info$data,
                extra = list(
                  display = list(
                    html = forecast_info$table,
                    title = paste("Weather Forecast for", location_name)
                  )
                )
              )
            })
        },
        name = "get_weather_forecast",
        description = "Get the weather forecast for a location.",
        arguments = list(
          lat = ellmer::type_number("Latitude"),
          lon = ellmer::type_number("Longitude"),
          location_name = ellmer::type_string(
            "Name of the location for display to the user"
          )
        ),
        annotations = ellmer::tool_annotations(
          title = "Weather Forecast",
          icon = bsicons::bs_icon("cloud-sun")
        )
      )

      client <- ellmer::chat_anthropic("Be terse.")
      client$register_tool(get_weather_forecast)

      ui <- bslib::page_fillable(
        shinychat::chat_mod_ui("chat", height = "100%"),
        actionButton(
          "close_btn",
          label = "",
          class = "btn-close",
          style = "position: fixed; top: 6px; right: 6px;"
        )
      )
      server <- function(input, output, session) {
        with_no_otel_bind({
          chat_server <- shinychat::chat_mod_server("chat", client, session)
        })
        with_no_otel_bind({
          observeEvent(input$close_btn, {
            stopApp()
          })
        })

        # with_no_otel_bind({
        #   output$boom <- renderUI({
        #     stop("Boom!")
        #   })
        # })

        with_no_otel_bind({
          counter <- reactiveVal(1)
          observeEvent(chat_server$last_turn(), {
            counter(counter() + 1)
          })
          observeEvent(counter(), label = "barret_is_lazy", {
            if (counter() == 1) {
              later::later(
                function() {
                  chat_server$update_user_input(
                    value = "What is the weather in Atlanta, GA?",
                    submit = TRUE
                  )
                },
                delay = 1
              )
            } else {
              later::later(
                function() {
                  later::later(
                    function() {
                      message("Stopping app")
                      stopApp()
                    },
                    delay = 0.5
                  )
                  message("Stopping session")
                  session$close()
                },
                delay = 3
              )
            }
          })
        })
      }
      app <- shinyApp(ui, server)
      runApp(app, port = 8080, launch.browser = TRUE)
    }
  )
}

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

# - Kitchen sink app ---------------------------------

dev_barret_kitchen <- function() {
  library(mirai)
  mirai::daemons(2)

  # Inspiration from
  # * https://github.com/r-lib/otel/commit/a2ef493ae4b97701e4e178ac527f313580539080
  # * https://github.com/r-lib/otel/commit/09c0eb6c80d5b907976de8fbaf89798cb11f8e6e#diff-169b8f234d0b208affb106fce375f86fefe2f16dba4ad66495a1dc06c8a4cd7b

  # TODO: Maybe the name is the folder name, similar to shinyapps.io naming
  # Maybe set from a function call somewhere?
  # otel_tracer <- otel::get_tracer("my-app")
  otel_logger <- otel::get_logger("my-app-logger")
  # options("shiny.otel.tracer" = otel_tracer)

  # withr::with_environment(globalenv(), {
  otel_tracer_name <- "my-app"
  # })

  log_and_msg <- function(..., .envir = parent.frame()) {
    msg <- paste(...)
    message("  -- ", msg)
    # otel::log_info(msg, tracer = session$userData[["_otel_tracer"]])
    # TODO: Remove the logger param once function is removed from Shiny package
    otel_log(msg, logger = otel_logger)
  }

  my_global_reactive <- reactiveVal(0)

  app <- shinyApp(
    ui = fluidPage(
      sliderInput("mymod-x", "x", 1, 10, 5),
      sliderInput("mymod-y", "y", 1, 10, 5),
      div("x * y: "),
      verbatimTextOutput("mymod-txt"),
      # bslib::input_task_button("recalculate", "Recalculate"),
      verbatimTextOutput("task_result")
    ),
    server = function(input, output, session) {
      log_and_msg("Start new Shiny session")

      b <- reactiveVal(1)
      observe(b(42))

      # shiny::bindOtel(TRUE)

      shutdown <- function() {
        later::later(
          function() {
            message("\n\nClosing session for minimal logfire graphs")
            # session$close()
            # httpuv::stopAllServers()
            stopApp()
            mirai::daemons(0)
          },
          delay = 100 / 1000
        )
      }

      xMod <- function(id) {
        moduleServer(id, function(input, output, session) {
          xVal <- reactiveVal(NULL)
          yVal <- reactiveVal(NULL)
          rv <- reactiveValues(x = NULL, y = NULL)

          log_and_msg("Shiny module")

          x_raw <- reactive({
            isolate({
              my_global_reactive(my_global_reactive() + 1)
            })

            x_val <- xVal()
            req(x_val)
            log_and_msg(sprintf("X Val: %s", x_val))
            x_val
          })
          x <- debounce(x_raw, 100)
          y_raw <- reactive({
            y_val <- input$y
            log_and_msg(sprintf("Y Val: %s", y_val))
            # Sys.sleep(0.5)
            y_val
          }) |> bindCache(input$y)
          y <- throttle(y_raw, 100)

          calc <- reactive(label = "barret_calc", {
            log_and_msg("Doing expensive computation...")
            x() * y()
          })

          observe({
            log_and_msg("x: ", x())
          })

          output$txt <- renderText({
            calc()
          }) |>
            bindCache(x(), y())

          rand_task <- ExtendedTask$new(function() {
            mirai::mirai(
              {
                # Slow operation goes here
                Sys.sleep(100 / 1000)
                sample(1:100, 1)
              }
            )
          })

          # # Make button state reflect task.
          # # If using R >=4.1, you can do this instead:
          # # rand_task <- ExtendedTask$new(...) |> bind_task_button("recalculate")
          # bslib::bind_task_button(rand_task, "recalculate")

          observeEvent(input$x, {
            # Invoke the extended in an observer
            rand_task$invoke()
          }, label = "invoke_rand_task")

          output$task_result <- renderText({
            # React to updated results when the task completes
            number <- rand_task$result()
            paste0("Your number is ", number, ".")
          })

          mydesc <- reactiveFileReader(
            1000,
            session,
            filePath = system.file("DESCRIPTION", package = "shiny"),
            readFunc = read.dcf
          )
          observe({
            mydesc()
          })

          myfile <- reactivePoll(
            1000,
            session,
            checkFunc = function() {
              Sys.time()
            },
            # This function returns the content of log_file
            valueFunc = function() {
              read.dcf(system.file("DESCRIPTION", package = "shiny"))
            }
          )

          observe({
            myfile()
          })

          x_prom <- reactive({
            # t0
            x_span_id <- force(otel::get_active_span_context()$get_span_id())
            # message("x_prom span id: ", x_span_id)
            x_val <- x()
            log_and_msg("x_prom init")
            p <- promises::promise(function(resolve, reject) {
              log_and_msg("x_prom 0")
              resolve(x_val)
            })
            p <- promises::then(p, function(x_val) {
              log_and_msg("x_prom 1")
              log_and_msg("Launching mirai")
              x_val
              # mirai::mirai_map(seq_len(x_val), function(i) {
              #   otel::start_local_active_span("slow compute")
              #   Sys.sleep(i / 10 / 1000)
              #   i
              # }) |>
              #   promises::then(function(vals) {
              #     max(unlist(vals))
              #   })

              # mirai::mirai(
              #   {
              #     otel::start_local_active_span("slow compute")
              #     # val
              #     # Sys.sleep(0.2)
              #     val
              #   },
              #   val = x_val
              # )
            })
            p <- promises::then(p, function(x_val) {
              log_and_msg("x_prom 2")
              x_val
            })
            p <- promises::then(p, function(x_val) {
              log_and_msg("x_prom 3")
              x_val
            })
          })

          y_prom <- reactive({
            y_span_id <- force(otel::get_active_span_context()$get_span_id())
            # message("y_prom span id: ", y_span_id)
            y_val <- y()
            log_and_msg("y_prom init")
            yp <- promises::promise(function(resolve, reject) {
              log_and_msg("y_prom 0")
              resolve(y_val)
            })
            log_and_msg("make y_prom 1")
            yp <- promises::then(yp, function(y_val) {
              log_and_msg("y_prom 1")
              y_val
            })
            log_and_msg("make y_prom 2")
            yp <- promises::then(yp, function(y_val) {
              log_and_msg("y_prom 2")
              y_val + calc()
            })
            log_and_msg("make y_prom 3")
            yp <- promises::then(yp, function(y_val) {
              log_and_msg("y_prom 3")
              y_val
            })

            log_and_msg(
              "done y_prom - ",
              getCurrentContext()$id,
              " - ",
              getCurrentContext()$.label
            )
            yp
          })

          observe(label = "proms_observer", {
            p <- promises::promise_all(
              x_prom(),
              y_prom()
            )
            p <- promises::then(p, function(vals) {
              log_and_msg("Vals[1]: ", vals[[1]])
              log_and_msg("Vals[2]: ", vals[[2]])

              # cat(force)

              # Shut down the app so the telemetry can be seen easily
              if (vals[[1]] < 6) {
                updateSliderInput(
                  "x",
                  value = vals[[1]] + 1,
                  session = session
                )
              } else {
                shutdown()
              }
            })
            log_and_msg(
              "done proms_observer - ",
              getCurrentContext()$id,
              " - ",
              getCurrentContext()$.label
            )
            p
          })

          # |>
          # bindOtel()

          # Set the value late in the reactive calc
          observeEvent(
            {
              input$x
            },
            {
              rv$x <- input$x
            },
            label = "singleObserveEvent"
          )

          tmp_val <- reactiveVal(NULL)

          # TODO: Not recording updates within the span!!
          x_calc <- eventReactive(
            {
              isolate(tmp_val(1))
              rv$x
            },
            {
              tmp_val(2)
              rv$x
            }
          )
          y_calc <- eventReactive(
            {
              isolate(tmp_val(3))
              input$y * 2
            },
            {
              # x_calc()
              tmp_val(4)
              input$y * 2 / 2
            }
          )
          # observeEvent(label = "set_y", {
          #   rv$y <- input$y
          # })
          observe(label = "set xVal", {
            x_calc()
            xVal(rv$x)
          })
          observe(label = "set yVal", {
            yVal(y_calc())
          })
        })
      }
      xMod("mymod")
    }
  )

  app
}
