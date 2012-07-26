
## UI & Server

Let's walk through the steps of building a simple Shiny application. As with the previous examples we'll be building our user interface using R, however it's also possible to define a user interface directly in HTML (a possibility covered in the  *HTML UI* example later in the tutorial).

A Shiny application is simply a directory containing a user-interface definition, a  server script, and any additional data, scripts, or other resources required to support the application. 

For this example we'll assume you've chosen to create the application within a sub-directory of your home directory named "shinyapp". To get started first create this directory and then create ui.R and server.R files within it:

<pre><code>~/shinyapp
|-- ui.R
|-- server.R
</code></pre>

Now we'll create minimal versions of each of these files. For ui.R we'll define our user interface by calling the function `pageWithSidebar` and passing it's result to the `shinyUI` function:

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

The three functions `headerPanel`, `sidebarPanel`, and `mainPanel` define the various regions of the user-interface, which are empty for now save for the title we passed to the header panel.

Now let's define a skeletal server implementation. To do this we call `shinyServer` and pass it a function that accepts two parameters: `input` and `output`:

#### server.R
<pre><code class="r">library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

})
</code></pre>

Our server function is empty for now but later we'll use it to define the relationship between our inputs and outputs.

We've now create the most minimal possible Shiny application. You can run the application by calling the `runApp` function as follows:

<pre><code class="console">&gt; library(shiny)
&gt; runApp(&quot;~/shinyapp&quot;)
</code></pre>

If everything is working correctly you'll see the application appear in your browser looking something like this: 

![MPG Screenshot](screenshots/mpg-empty.png)

We now have a running Shiny application however it doesn't do much yet. In the next section we'll complete the application by specifying the user-interface and implementing the server script.