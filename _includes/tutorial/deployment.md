## Delivering Shiny Apps

Once you've written your Shiny app, tested it locally, and gotten it working just right, chances are you're going to want to share your work with others.

You have several options:

### Gist

One easy way is to put your code on [gist.github.com](https://gist.github.com), a code pasteboard service from [GitHub](https://github.com/). Both server.R and ui.R must be included in the same gist, and you must use their proper filenames. See [https://gist.github.com/3239667](https://gist.github.com/3239667) for an example.

Your recipient must have R and the Shiny package installed, and then running the app is as easy as entering the following command:

<pre><code class="r">shiny::runGist('3239667')</code></pre>

In place of `'3239667'` you will use your gist's ID; or, you can use the entire URL of the gist (e.g. `'https://gist.github.com/3239667'`).

#### Pros
* Source code is easily visible by recipient (if desired)
* Easy to run (for R users)
* Easy to post and update

#### Cons
* Code is published to a third-party server

### Zip File

Another way is to simply zip up your project directory and send it to your recipient(s), where they can unzip the file and run it the same way you do (`shiny::runApp`).

#### Pros
* Share apps using e-mail, USB flash drive, or any other way you can transfer a file

#### Cons
* Updates to app must be sent manually

### Package

If your Shiny app is useful to a broader audience, it might be worth the effort to turn it into an R package. Put your Shiny application directory under the package's `inst` directory, then create and export a function that contains something like this:

<pre><code class="r">shiny::runApp(system.file('<em>appdir</em>', package='<em>packagename</em>'))</code></pre>

where `appdir` is the name of your app's subdirectory in `inst`, and `packagename` is the name of your package.

#### Pros
* Publishable on CRAN
* Easy to run (for R users)

#### Cons
* More work to set up
* Source code is visible by recipient (if not desired)

### Over the Web

The most convenient possibility for non-R users would be to access your application directly using their web browsers, without needing to run the application themselves.

Unfortunately, R alone does not currently have the pieces necessary to run a production-quality web service. While nothing prevents you from running an R process on a server and calling `runApp`, the websockets package that Shiny uses as its HTTP listener is not well suited for deploying directly on the open web. Furthermore, any error in your application will cause the `runApp` loop to exit and you will need to restart the R process yourself.

We are currently beta testing a Shiny server platform that will make it possible to host Shiny applications on the web robustly. Unlike the Shiny framework itself, which is completely free and open source, the Shiny server will almost certainly be a subscription-based hosting service and/or commercial software. If you'd like to help us beta test Shiny server, please [register](https://rstudio.wufoo.com/forms/shiny-server-beta-program/).
