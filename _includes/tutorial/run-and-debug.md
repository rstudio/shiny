

## Run & Debug

Throughout the tutorial you've been calling `runApp` to run examples and the MPG application. This function starts the Shiny application and opens up your default web browser to view it. The call to runApp is blocking, meaning that it prevents traditional interaction with the console while the applciation is running.

To stop the application you simply interupt R -- you can do this by pressing the Escape key in all R front ends as well as by clicking the stop button if your R environment provides one.

### Running in a Separate Process

If you don't want to block access to the console while running your Shiny application you can also run it in a separate process. You can do this by opening a terminal or console window and executing the following:

<pre><code class="console">R -e &quot;shiny::runApp('~/shinyapp')&quot;
</code></pre>

By default `runApp` starts the application on port 8100. If you are using this default then you can connect to the running application by navigating your browser to [http://localhost:8100](http://localhost:8100).

### Live Reloading

When you make changes to your underlying user-interface defintiion or server script you don't need to stop and restart your application to see the changes. Simply save your changes and then reload the browser to the updated application in action.

One qualification to this: when a browser reload occurs Shiny explicitly checks the timestamps of the ui.R and server.R files to see if they need to be re-sources. If you have other scripts or data files that change Shiny can't see those, so a full stop and restart of the application is necessary to see those changes.

### Debugging Techniques

#### Printing 
There are several potential techniques for debugging Shiny applications. The first is to add calls to the [cat](http://stat.ethz.ch/R-manual/R-devel/library/base/html/cat.html) function that print diagnostics where appropriate. For example, these two calls to cat print diagnostics to standard output and standard error respectively:

<pre><code class="r">cat(&quot;foo\n&quot;)
cat(&quot;bar\n&quot;, stderr())
</code></pre>

#### Using browse
The second technique is to add explicit calls to the [browser](http://stat.ethz.ch/R-manual/R-devel/library/base/html/browser.html) function to interrupt execution and inspect the environment where browser was called from. Note that using browser requires that you start the application from an interactive session (as opposed to using R -e as described above).

For example, to unconditionally stop execution at a certain point in the code:

<pre><code class="r"># Always stop execution here
browser() 
</code></pre>

You can also use this technique to stop only on certain conditions. For example, to stop the MPG application only when the user selects "Transmission" as the variable:

<pre><code class="r"># Stop execution when the user selects &quot;am&quot;
browser(expr = identical(input$variable, &quot;am&quot;))
</code></pre>

#### Establishing a custom error handler
You can also set the R &quot;error&quot; option to automatically enter the browser when an error occurs:

<pre><code class="r"># Immediately enter the browser when an error occurs
options(error = browser)
</code></pre>

Alternatively, you can specify the [recover](http://stat.ethz.ch/R-manual/R-devel/library/utils/html/recover.html) function as your error handler, which will print a list of the call stack and allow you to browse at any point in he stack:

<pre><code class="r"># Call the recover function when an error occurs
options(error = recover)
</code></pre>

If you want to set the error option automatically for every R session, you can do this in your .Rprofile file as described in this article on [R Startup](http://stat.ethz.ch/R-manual/R-patched/library/base/html/Startup.html).




