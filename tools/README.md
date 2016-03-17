This directory contains build tools for Shiny.


## Grunt

Grunt is a build tool that runs on node.js. In Shiny, it is used for concatenating, minifying, and linting Javascript code.

### Installing Grunt

Grunt requires Node.js and npm (the Node.js package manager). Installation of these programs differs across platforms and is generally pretty easy, so I won't include instructions here.

Once node and npm are installed, install grunt:

```
# Install grunt command line tool globally
sudo npm install -g grunt-cli

# Install grunt plus modules for this project
npm install

# Temporarily need to install estraverse-fb because of a bug in eslint:
# https://github.com/eslint/eslint/issues/5476
npm install estraverse-fb

# To update modules in the future
npm update
```

### Using Grunt

To run all default grunt tasks (concatenation, minification, and jshint), simply go into the `tools` directory and run:

```
grunt
```

Sometimes grunt gets confused about whether the output files are up to date, and won't overwrite them even if the input files have changed. If this happens, run:

```
grunt clean
```

It's also useful to run `grunt` so that it monitors files for changes and run tasks as necessary. This is done with:

```
grunt watch
```

One of the tasks concatenates all the .js files in `/srcjs` together into `/inst/www/shared/shiny.js`. Another task minifies `shiny.js` to generate `shiny.min.js`. The minified file is supplied to the browser, along with a source map file, `shiny.min.js.map`, which allows a user to view the original Javascript source when using the debugging console in the browser.

During development of Shiny's Javascript code, it's best to use `grunt watch` so that the minified file will get updated whenever you make changes the Javascript sources.



Updating web libraries
======================

## babel-polyfill

To update the version of babel-polyfill:

* Check if there is a newer version available by running `npm outdated babel-polyfill`. (If there's no output, then you have the latest version.)
* Run `npm install babel-polyfill --save-dev --save-exact`.
* Edit R/shinyui.R. The `renderPage` function has an `htmlDependency` for
  `babel-polyfill`. Update this to the new version number.
