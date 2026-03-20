# devtools::load_all(); dev_otel_kitchen()

dev_otel_kitchen <- function() {
  library(mirai)
  mirai::daemons(2)

  # Inspiration from
  # * https://github.com/r-lib/otel/commit/a2ef493ae4b97701e4e178ac527f313580539080
  # * https://github.com/r-lib/otel/commit/09c0eb6c80d5b907976de8fbaf89798cb11f8e6e#diff-169b8f234d0b208affb106fce375f86fefe2f16dba4ad66495a1dc06c8a4cd7b

  otel_logger <- otel::get_logger("my-app-logger")
  otel_tracer_name <- "my-app"

  log_and_msg <- function(..., .envir = parent.frame()) {
    msg <- paste(...)
    message("  -- ", msg)

    otel_log(msg, logger = otel_logger)
  }

  my_global_reactive <- reactiveVal(0)

  app <- shinyApp(
    ui = fluidPage(
      sliderInput("mymod-x", "x", 1, 10, 5),
      sliderInput("mymod-y", "y", 1, 10, 5),
      div("x * y: "),
      verbatimTextOutput("mymod-txt1"),
      verbatimTextOutput("mymod-txt2"),
      verbatimTextOutput("mymod-txt3"),
      verbatimTextOutput("task_result")
    ),
    server = function(input, output, session) {
      log_and_msg("Start new Shiny session")

      b <- reactiveVal(1)
      observe(b(42))

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

      later::later(
        function() {
          if (!session$closed) {
            log_and_msg("Invoking shutdown after 5s")
            shutdown()
          }
        },
        delay = 5
      )

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
          }) |> bindCache(input$y) |> bindEvent(input$y)
          y <- throttle(y_raw, 100)

          calc <- reactive(label = "barret_calc", {
            log_and_msg("Doing expensive computation...")
            x() * y()
          })

          observe({
            log_and_msg("x: ", x())
          })

          output$txt1 <- renderText({
            calc()
          }) |>
            bindCache(x(), y())
          output$txt2 <- renderText({
            calc()
          }) |>
            bindEvent(list(x(), y()))
          output$txt3 <- renderText({
            calc()
          }) |>
            bindCache(x(), y()) |>
            bindEvent(list(x(), y()))

          rand_task <- ExtendedTask$new(function() {
            mirai::mirai(
              {
                # Slow operation goes here
                Sys.sleep(100 / 1000)
                sample(1:100, 1)
              }
            )
          })

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
