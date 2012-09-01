library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("CSV Viewer"),
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 list(Comma=',',
                      Semicolon=';',
                      Tab='\t'),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 list(None='',
                      'Double Quote'='"',
                      'Single Quote'="'"),
                 'Double Quote')
  ),
  mainPanel(
    tableOutput('contents')
  )
))
