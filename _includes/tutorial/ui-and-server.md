
## UI & Server

Let's walk through the steps of building a simple Shiny application. A Shiny application is simply a directory containing a user-interface definition, a server script, and any additional data, scripts, or other resources required to support the application. 

To get started building the application, create a new empty directory wherever you'd like, then create empty `ui.R` and `server.R` files within in. For purposes of illustration we'll assume you've chosen to create the application at ~/shinyapp:

<pre><code>~/shinyapp
|-- ui.R
|-- server.R
</code></pre>

Now we'll add the minimal code required in each source file. We'll first define the user interface by calling the function `pageWithSidebar` and passing it's result to the `shinyUI` function:

#### ui.R

{% highlight r %}
library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Miles Per Gallon"),

  sidebarPanel(),

  mainPanel()
))
{% endhighlight %}


The three functions `headerPanel`, `sidebarPanel`, and `mainPanel` define the various regions of the user-interface. The application will be called "Miles Per Gallon" so we specify that as the title when we create the header panel. The other panels are empty for now.

Now let's define a skeletal server implementation. To do this we call `shinyServer` and pass it a function that accepts two parameters: `input` and `output`:

#### server.R

{% highlight r %}
library(shiny)

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

})
{% endhighlight %}


Our server function is empty for now but later we'll use it to define the relationship between our inputs and outputs.

We've now created the most minimal possible Shiny application. You can run the application by calling the `runApp` function as follows:

{% highlight console %}
> library(shiny)
> runApp("~/shinyapp")
{% endhighlight %}

If everything is working correctly you'll see the application appear in your browser looking something like this: 

![MPG Screenshot](screenshots/mpg-empty.png)

We now have a running Shiny application however it doesn't do much yet. In the next section we'll complete the application by specifying the user-interface and implementing the server script.