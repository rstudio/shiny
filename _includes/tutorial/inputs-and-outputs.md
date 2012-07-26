

## Inputs & Outputs


### Accepting Input

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Miles Per Gallon&quot;),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput(&quot;variable&quot;, &quot;Variable:&quot;,
                list(&quot;Cylinders&quot; = &quot;cyl&quot;, 
                     &quot;Transmission&quot; = &quot;am&quot;, 
                     &quot;Gears&quot; = &quot;gear&quot;)),

    checkboxInput(&quot;outliers&quot;, &quot;Show outliers&quot;, FALSE)
  ),

  mainPanel()
))
</code></pre>

![MPG Screenshot](screenshots/mpg-with-inputs.png)


### Showing Output

#### server.R

<pre><code class="r">library(shiny)
library(datasets)

# We tweak the &quot;am&quot; field to have nicer factor labels. Since this doesn&#39;t
# rely on any user inputs we can do this once at startup and then use the
# value throughout the lifetime of the application
mpgData &lt;- mtcars
mpgData$am &lt;- factor(mpgData$am, labels = c(&quot;Automatic&quot;, &quot;Manual&quot;))


# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  # Compute the forumla text in a reactive function since it is 
  # shared by the output$caption and output$mpgPlot functions
  formulaText &lt;- reactive(function() {
    paste(&quot;mpg ~&quot;, input$variable)
  })

  # Return the formula text for printing as a caption
  output$caption &lt;- reactiveText(function() {
    formulaText()
  })

  # Generate a plot of the requested variable against mpg and only 
  # include outliers if requested
  output$mpgPlot &lt;- reactivePlot(function() {
    boxplot(as.formula(formulaText()), 
            data = mpgData,
            outline = input$outliers)
  })
})
</code></pre>

#### ui.R

<pre><code class="r">library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Miles Per Gallon&quot;),

  # Sidebar with controls to select the variable to plot against mpg
  # and to specify whether outliers should be included
  sidebarPanel(
    selectInput(&quot;variable&quot;, &quot;Variable:&quot;,
                list(&quot;Cylinders&quot; = &quot;cyl&quot;, 
                     &quot;Transmission&quot; = &quot;am&quot;, 
                     &quot;Gears&quot; = &quot;gear&quot;)),

    checkboxInput(&quot;outliers&quot;, &quot;Show outliers&quot;, FALSE)
  ),

  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    h3(textOutput(&quot;caption&quot;)),

    plotOutput(&quot;mpgPlot&quot;)
  )
))


</code></pre>

![MPG Screenshot](screenshots/mpg-with-outputs.png)