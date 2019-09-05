This directory contains build tools for Shiny.


## JavaScript build tools

### First-time setup
Shiny's JavaScript build tools use Node.js, along with [yarn](https://yarnpkg.com/) to manage the JavaScript packages.

Installation of Node.js differs across platforms, see [the official Node.js website](https://nodejs.org/) for instructions on downloading and installing. We presume that you have Node.js installed on your machine before continuing.

Install yarn using the [official instructions](https://yarnpkg.com/en/docs/install).

You can test that Node.js and yarn are installed properly by running the following commands:

```
node --version
yarn --version
```

Once both are installed, run the following in this directory (`tools/`) to install the packages :

```
yarn
```

### Adding packages
If in the future you want to upgrade or add a package, run:

```
yarn add --dev [packagename]
```

This will automatically add the package to the dependencies in package.json, and it will also update the yarn.lock to reflect that change. If someone other than yourself does this, simply run `yarn` to update your local packages to match the new package.json.

### Upgrading packages
Periodically, it's good to upgrade the packages to a recent version. There's two ways of doing this, depending on your intention:

1. Use `yarn upgrade` to upgrade all dependencies to their latest version based on the version range specified in the package.json file (the yarn.lock file will be recreated as well. Yarn packages use [semantic versioning](https://yarnpkg.com/en/docs/dependency-versions), i.e. each version is writen with a maximum of 3 dot-separated numbers such that: `major.minor.patch`. For example in the version `3.1.4`, 3 is the major version number, 1 is the minor version number and 4 is the patch version number. Here are the most used operators (these appear before the version number):

  - `~` is for upgrades that keep the minor version the same (assuming that was specified);

  - `^` is for upgrades that keep the major version the same (more or less -- more specifically, it allow changes that do not modify the first non-zero digit in the version, either the 3 in 3.1.4 or the 4 in 0.4.2.). This is the default operator added to the package.json when you run `yarn add [package-name]`.

2. Use `yarn upgrade [package]` to upgrade a single named package to the version specified by the latest tag (potentially upgrading the package across major versions).

For more information about upgrading or installing new packages, see the [yarn workflow documentation](https://yarnpkg.com/en/docs/yarn-workflow).

### Grunt
Grunt is a build tool that runs on node.js (and installed using `yarn`). In Shiny, it is used for concatenating, minifying, and linting Javascript code.

### Using Grunt
To run all default grunt tasks specified in the Gruntfile (concatenation, minification, and jshint), simply go into the `tools` directory and run:

```
yarn build
```

Sometimes grunt gets confused about whether the output files are up to date, and won't overwrite them even if the input files have changed. If this happens, run:

```
yarn clean
```

It's also useful to run `grunt` so that it monitors files for changes and run tasks as necessary. This is done with:

```
yarn watch
```

One of the tasks concatenates all the .js files in `/srcjs` together into `/inst/www/shared/shiny.js`. Another task minifies `shiny.js` to generate `shiny.min.js`. The minified file is supplied to the browser, along with a source map file, `shiny.min.js.map`, which allows a user to view the original Javascript source when using the debugging console in the browser.

During development of Shiny's Javascript code, it's best to use `yarn watch` so that the minified file will get updated whenever you make changes the Javascript sources.

#### Auto build and browser refresh

An alternative to `yarn watch` is to use `entr` to trigger `grunt` when sources change. `entr` can be installed with `brew install entr` on a Mac, or on Linux using your distribution's package manager. Using this technique, it's possible to both automatically rebuild sources and reload Chrome at the same time:

*macOS*:

```
find ../srcjs/ | entr bash -c './node_modules/grunt/bin/grunt && osascript -e "tell application \"Google Chrome\" to reload active tab of window 1"'
```

*Linux*:

For this to work you must first install `xdotool` using your distribution's package manager.

```
find ../srcjs/ | entr bash -c './node_modules/grunt/bin/grunt && xdotool search --onlyvisible --class Chrome windowfocus key ctrl+r'
```

Updating web libraries
======================

## babel-polyfill

To update the version of babel-polyfill:

* Check if there is a newer version available by running `yarn outdated babel-polyfill`. (If there's no output, then you have the latest version.)
* Run `yarn add --dev babel-polyfill --exact`.
* Edit R/shinyui.R. The `renderPage` function has an `htmlDependency` for
  `babel-polyfill`. Update this to the new version number.

## Updating and patching `bootstrap-datepicker`

### Updating

[bootstrap-datepicker](https://github.com/uxsolutions/bootstrap-datepicker) can be updated with the script `updateBootstrapDatepicker.R`.

After updating, our patches to `bootstrap-datepicker` must be applied using the script `applyDatepickerPatches.R`

After updating and applying patches, `yarn grunt` should be run per the instructions above in order to generate a minified JavaScript file.

### Making a new patch

To create a new patch:

1. Make any necessary changes to files in `inst/www/shared/datepicker`
1. **Do not commit your changes.**
1. Instead, create a patch with a command like `git diff > tools/datepicker-patches/012-a-description.patch`. Patches are applied in alphabetic order (per `list.files`), so you should name your patch based on the last one in `tools/datepicker-patches` so that it's applied last.
1. Revert `bootstrap-datepicker` to its unpatched state by running `updateBootstrapDatepicker.R`
1. Apply all patches, including the one you just made, by running `applyDatepickerPatches.R`
1. Run `yarn grunt`
1. Test your changes
1. `git add` the new `.patch` and any resulting changes


## Updating and patching ion.rangeSlider

### Updating

[ion.rangeSlider](https://github.com/IonDen/ion.rangeSlider) can be updated with the script `updateBootstrapDatepicker.R`. That script downloads a specific version of ion.rangeSlider and applies our patches in tools/ion.rangeSlider-patches.

After updating and applying patches, `yarn grunt` should be run per the instructions above in order to generate a minified JavaScript file.

### Making a new patch

To create a new patch:

1. Make any necessary changes to files in `inst/www/shared/ion.rangeSlider`
1. **Do not commit your changes.**
1. Instead, create a patch with a command like `git diff > tools/ion.rangeSlider-patches/0004-a-description.patch`. Patches are applied in alphabetic order (per `list.files`), so you should name your patch based on the last one in `tools/ion.rangeSlider-patches` so that it's applied last.
1. Run `updateIonRangeSlider.R` to download the library and apply patches.
1. Run `yarn grunt`
1. Test your changes
1. `git add` the new `.patch` and any resulting changes
