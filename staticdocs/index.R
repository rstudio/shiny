list(
  index = list(
    sd_section("UI Inputs",
      "Use for inputs in ui.R",
      c(
        "checkboxGroupInput",
        "checkboxInput",
        "dateInput",
        "dateRangeInput",
        "fileInput",
        "numericInput",
        "radioButtons",
        "selectInput",
        "sliderInput",
        "animationOptions",
        "textInput",
        "actionButton",
        "submitButton",
        "updateCheckboxGroupInput",
        "updateCheckboxInput",
        "updateDateInput",
        "updateDateRangeInput",
        "updateNumericInput",
        "updateRadioButtons",
        "updateSelectInput",
        "updateSliderInput",
        "updateTabsetPanel",
        "updateTextInput"
      )
    ),
    sd_section("UI Outputs",
      "Use for outputs in ui.R",
      c(
        "htmlOutput",
        "imageOutput",
        "plotOutput",
        "outputOptions",
        "tableOutput",
        "textOutput",
        "verbatimTextOutput",
        "downloadButton"
      )
    ),
    sd_section("UI Layout",
      "Use for layout out your app UI",
      c(
        "bootstrapPage",
        "column",
        "conditionalPanel",
        "fixedPage",
        "fluidPage",
        "headerPanel",
        "helpText",
        "icon",
        "mainPanel",
        "navbarPage",
        "navlistPanel",
        "pageWithSidebar",
        "sidebarLayout",
        "sidebarPanel",
        "tabPanel",
        "tabsetPanel",
        "titlePanel",
        "verticalLayout",
        "wellPanel"
      )
    ),
    sd_section("Interface builder functions",
      "Functions for building up HTML",
      c(
        "builder",
        "HTML",
        "includeHTML",
        "singleton",
        "tag",
        "validateCssUnit",
        "withTags"
      )
    ),
    sd_section("Rendering functions",
      "These functions are used for rendering... obviously.",
      c(
        "renderPlot", 
        "renderText", 
        "renderPrint",
        "renderDataTable",
        "renderImage",
        "renderTable",
        "renderUI",
        "downloadHandler",
        "reactivePlot",
        "reactivePrint",
        "reactiveTable",
        "reactiveText",
        "reactiveUI"
      )
    ),
    sd_section("Reactive constructs",
      "Reactive programming support",
      c(
        "invalidateLater",
        "is.reactivevalues",
        "isolate",
        "makeReactiveBinding",
        "observe",
        "reactive",
        "reactiveFileReader",
        "reactivePoll",
        "reactiveTimer",
        "reactiveValues",
        "reactiveValuesToList",
        "showReactLog"
      )
    ),
    sd_section("Boilerplate",
      "Functions for declaring UI and server",
      c(
        "shinyUI",
        "shinyServer"
      )
    ),
    sd_section("Running",
      "Launch Shiny apps",
      c(
        "runApp",
        "runExample",
        "runGist",
        "runGitHub",
        "runUrl",
        "stopApp"
      )
    ),
    sd_section("Extending Shiny",
      "Build your own extensions",
      c(
        "addResourcePath",
        "registerInputHandler",
        "removeInputHandler"
      )
    ),
    sd_section("Utility functions",
      "Miscellaneous utilities",
      c(
        "exprToFunction",
        "installExprFunction",
        "parseQueryString",
        "plotPNG",
        "repeatable",
        "shinyDeprecated"
      )
    )
  )
)

