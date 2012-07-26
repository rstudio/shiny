
## UI & Server


#### ui.R
<pre><code class="r">library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel(&quot;Miles Per Gallon&quot;),

  sidebarPanel(),

  mainPanel()
))
</code></pre>

#### server.R
<pre><code class="r">library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {


})
</code></pre>


![MPG Screenshot](screenshots/mpg-empty.png)
