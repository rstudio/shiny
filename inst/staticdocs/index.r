sd_section("UI Layout",
  "Functions for laying out the user interface for your application.",
  c(
    "absolutePanel",
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
    "flowLayout",
    "splitLayout",
    "verticalLayout",
    "wellPanel",
    "withMathJax"
  )
)
sd_section("UI Inputs",
  "Functions for creating user interface elements that prompt the user for input values or interaction.",
  c(
    "actionButton",
    "checkboxGroupInput",
    "checkboxInput",
    "dateInput",
    "dateRangeInput",
    "fileInput",
    "numericInput",
    "radioButtons",
    "selectInput",
    "sliderInput",
    "submitButton",
    "textInput",
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
)
sd_section("UI Outputs",
  "Functions for creating user interface elements that, in conjunction with rendering functions, display different kinds of output from your application.",
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
)
sd_section("Interface builder functions",
  "A sub-library for writing HTML using R functions. These functions form the foundation on which the higher level user interface functions are built, and can also be used in your Shiny UI to provide custom HTML, CSS, and JavaScript.",
  c(
    "builder",
    "HTML",
    "include",
    "singleton",
    "tag",
    "validateCssUnit",
    "withTags"
  )
)
sd_section("Rendering functions",
  "Functions that you use in your application's server side code, assigning them to outputs that appear in your user interface.",
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
)
sd_section("Reactive constructs",
  "A sub-library that provides reactive programming facilities for R.",
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
    "domains",
    "showReactLog"
  )
)
sd_section("Boilerplate",
  "Functions that are required boilerplate in ui.R and server.R.",
  c(
    "shinyUI",
    "shinyServer"
  )
)
sd_section("Running",
  "Functions that are used to run or stop Shiny applications.",
  c(
    "runApp",
    "runExample",
    "runGist",
    "runGitHub",
    "runUrl",
    "stopApp"
  )
)
sd_section("Extending Shiny",
  "Functions that are intended to be called by third-party packages that extend Shiny.",
  c(
    "addResourcePath",
    "registerInputHandler",
    "removeInputHandler",
    "markRenderFunction"
  )
)
sd_section("Utility functions",
  "Miscellaneous utilities that may be useful to advanced users or when extending Shiny.",
  c(
    "exprToFunction",
    "installExprFunction",
    "parseQueryString",
    "plotPNG",
    "repeatable",
    "shinyDeprecated"
  )
)
sd_section("Embedding",
  "Functions that are intended for third-party packages that embed Shiny applications.",
  c(
    "shinyApp",
    "maskReactiveContext",
    "knitr_methods",
    "getProvidedHtmlDependencies"
  )
)
