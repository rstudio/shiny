## Deploying Over the Web

Once you've written your Shiny app, you can make it available to anyone who has a web browser, using our Shiny Server software. You can either host the applications on your own server, or let us host your Shiny applications for you.

If you want a simple way to distribute your Shiny app so that users can run them on their own computers, see <a href="#deployment-local">Deploying Shiny Apps to Run Locally</a>.


### Self-hosted Shiny Server

With our [Shiny Server](http://rstudio.com/shiny/server/) software, you can deploy Shiny applications over the web so that users need only a web browser and your application's URL. You'll need a Linux server and [Shiny Server](http://rstudio.com/shiny/server/).

Shiny Server is free and open source, though in the future we will offer a commercially licensed edition with additional features for larger organizations.

#### Pros
* Easiest for your users&mdash;only a web browser is required
* Take advantage of centralized computing resources

#### Cons
* Requires server setup and maintenance of a Linux server


### RStudio-hosted Shiny Apps

Want to deploy over the web but prefer not to run your own server? We're currently alpha testing ShinyApps, a subscription-based hosting service. To apply for a free alpha test account, [register now](http://www.shinyapps.io/signup.html).

#### Pros
* Easiest for your users&mdash;only a web browser is required
* No need to run your own server
* Easy deployment to the web with one line of R code

#### Cons
* Code and data must be copied to our servers
