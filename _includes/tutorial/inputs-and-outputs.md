

## Inputs & Outputs

### Adding Inputs to the Sidebar

The application we'll be building uses the mtcars data from the R datasets package, and allows users to see a box-plot that explores the relationship between miles-per-gallon and three other variables (Cylinders, Transmission, and Gears). 

We want to provide a way to select which variable to plot mpg against as well as provide an option to include or exclude outliers from the plot. To do this we'll add two elements to the sidebar, a `selectInput` to specify the variable and a `checkboxInput` to control display of outliers. Our ui.R file looks like this after adding these elements:

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

If you run the application again after making these changes you'll see the two user-inputs we defined displayed within the sidebar:

![MPG Screenshot](screenshots/mpg-with-inputs.png)


### Creating the Server Script

Next we need to define the server-side of the application which will accept inputs and compute outputs. Our server.R file is shown below, and illustrates some important concepts:
* Accessing input using slots on the `input` object and generating output by assigning to slots on the `output` object.
* Initializing data at startup that can be accessed throughout the lifetime of the application.
* Using a reactive function to compute a value shared by more than one output function.

The basic task of a Shiny server script is to define the relationship between inputs and outputs. Our script does this by accessing inputs to perform computations and by assigning reactive functions to output slots. 

Here is the source code for the full server script (the inline comments explain the implementation technqiues in more detail):

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

The use of `reactiveText` and `reactivePlot` to generate output (rather than just assigning values directly) is what makes the application reactive. These reactive wrappers return special functions that are only re-executed when their dependencies change. This behavior is what enables Shiny to automatically update output whenever input changes.


### Displaying Outputs

The server script assigned two output values: `output$caption` and `output$mpgPlot`. To update our user interface to display the output we need to add some elements to the main UI panel. 

In the updated ui.R source file below you can see that we've added the caption as an h3 element (level 3 header) and filled in it's value using the `textOutput` function, and also rendered the plot by calling the `plotOutput` function:

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

Running the application now shows it in its final form including inputs and dynamically updating outputs:

![MPG Screenshot](screenshots/mpg-with-outputs.png)

Now that we've got a simple application running we'll probably want to make some changes. The next topic covers the basic cycle of editing, running, and debugging Shiny applications.

