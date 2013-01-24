## Sharing Apps to Run Locally

Once you've written your Shiny app, you can distribute it for others to run on their own computers&mdash;they can download and run Shiny apps with a single R command. This requires that they have R and Shiny installed on their computers.

If you want your Shiny app to be accessible over the web, so that users only need a web browser, see <a href="#deployment-web">Deploying Shiny Apps over the Web</a>.

Here are some ways to deliver Shiny apps to run locally:

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


### GitHub repository

If your project is stored in a git repository on GitHub, then others can download and run your app directly. An example repository is at [https://github.com/rstudio/shiny_example](https://github.com/rstudio/shiny_example). The following command will download and run the application:

<pre><code class="r">shiny::runGitHub('shiny_example', 'rstudio')</code></pre>

In this example, the GitHub account is `'rstudio'` and the repository is `'shiny_example'`; you will need to replace them with your account and repository name.

#### Pros
* Source code is easily visible by recipient (if desired)
* Easy to run (for R users)
* Very easy to update if you already use GitHub for your project
* Git-savvy users can clone and fork your repository

#### Cons
* Developer must know how to use git and GitHub
* Code is hosted by a third-party server


### Zip File, delivered over the web

If you store a zip or tar file of your project on a web or FTP server, users can download and run it with a command like this:

<pre><code class="r">runUrl('https://github.com/rstudio/shiny_example/archive/master.zip')</code></pre>

The URL in this case is a zip file that happens to be stored on GitHub; replace it with the URL to your zip file.

#### Pros
* Only requires a web server for delivery

#### Cons
* To view the source, recipient must first download and unzip it


### Zip File, copied to recipient's computer

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
