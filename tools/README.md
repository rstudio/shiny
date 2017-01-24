This directory contains build tools for Shiny.


## JavaScript build tools


### First-time setup

Shiny's JavaScript build tools use Node.js, along with [yarn](https://yarnpkg.com/) to manage the JavaScript packages.

Installation of Node.js differs across platforms and is generally pretty easy, so I won't include instructions here.

Install yarn using the [official instructions](https://yarnpkg.com/en/docs/install).

Then, in this directory (tools/), run the following to install the packages:

```
yarn
```

If in the future you want to upgrade or add a package, run:

```
yarn add --dev [packagename]
```

If someone else updates the package.json and/or yarn.lock files, simply run `yarn` to update your packages.

For information about upgrading or installing new packages, see the [yarn workflow documentation](https://yarnpkg.com/en/docs/yarn-workflow).


### Grunt

Grunt is a build tool that runs on node.js and will be installed. In Shiny, it is used for concatenating, minifying, and linting Javascript code.

#### Installing Grunt

```
# Install grunt command line tool globally
sudo yarn global add grunt-cli
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

* Check if there is a newer version available by running `yarn outdated babel-polyfill`. (If there's no output, then you have the latest version.)
* Run `yarn add --dev babel-polyfill --exact`.
* Edit R/shinyui.R. The `renderPage` function has an `htmlDependency` for
  `babel-polyfill`. Update this to the new version number.
