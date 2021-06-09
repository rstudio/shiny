# Development Rules
* Put Imports at top
* Put Exports at bottom
* File / Folder structure
  * Lean towards using more many files / folder vs larger files
  * Nest folders inside larger _ideas_
  * Each folder should generally have an `**/index.ts` to export most everything for the folder
    * Exception: `./src/window` folder. Must call methods directly.
* Any `window.***` calls are done in `./src/window` folder only.
  * Add exported values / function if necessary.
  * This helps keep each file self contained. Trying not to have random inputs from anywhere
* jQuery
  * Always import `./src/jquery` instead of `"jquery"`
    * Exeption: Test files. There, you can import `"jquery"` directly as the browser is not available to already have jQuery loaded.
  * Prevents from installing local jquery in addition to global jquery
    * WAY to many packages exist on the assumption that jquery is available at run time. Therefore, it can not be removed globally. :-(
* Anything that needs to be initialized on start **must** exist in the `./src/initialize` folder.
  * No file should produce any side effects.
  * To capture initializations, export a `setFoo(foo_)` method that updates a locally defined `foo` variable.


# TODO

* √ Move everything into a single ts file. This will allow for the functions to find themselves
  * √ Except the utils.js already converted
* √ es6 shiny.js
  * √ Pass in version using esbuild
  * √ Move all shiny files in order to main.ts
  * √ validate polyfills are working by finding them in the code
* √ Produce minified shiny js
* √ Disable $ from being found without an import
  * √ Using a patch with yarn v2
* √ Document `./package.json` scripts
* √ Verify that `babel` is configurable
  * √ Use targeting browsers
  * √ Verify it works on phantomjs / shinytest
* √ Set up initial jest tests
* √ Use a global shim to avoid importing jquery directly, but make testing easy to test
* Update the /tools/update*.R scripts to produce a version and install node dependencies
  * √ jquery
  * √ ion range slider
  * √ selectize
  * √ strftime
  * √ bootstrap date picker
  * font awesome?
  * bootstrap accessilbility plugin?

# Round #2
* Convert registered bindings
  * input
    * actionbutton
    * checkbox
    * checkboxgroup
    * date
    * number
    * password
    * radio
    * selectInput
    * slider
    * text
    * textarea
  * output
* Add default value to `subscribe(callback)` callback function of `false`. B/c if the value was not provided, it was not truthy, therefore equivalent to `false`.
  * √ radio
  * √ checkboxgroup
  * √ daterange
  * √ actionbutton
  * √ bootstraptabinput
* √ snake_case to camelCase conversions.
* √ globally import strftime from `window.strftime`
* Remove `evt` from jQuery.on callbacks where `evt` was not used.
  * √ checkbox.subscribe
  * √ checkboxgroup.subscribe
  * √ radio.subscribe
  * √ slider.subscribe
  * √ date.subscribe
  * √ selectInput.subscribe
  * √ actionButton.subscribe
  * √ bootstraptabinput.subscribe
* Convert usage of `+x` to `Number(x)`
  * https://stackoverflow.com/a/15872631/591574
  * √ slider.getValue()
* Adjust tabinput.ts `setValue()` to return either `false | void`, not `false | true`.
  * √ number.getValue()
  * What matters is that `false` is returned, or nothing is returned. Replaced `return true;` with `return;`
* Questions
  * Why does `receiveMessage(data)` sometimes have a `label`?
  * Should we have a update datatables script?


# Later TODO

* Use --strictNullChecks in tsconfig.json

* Make `_*()` methods `private *()`
* Each _file_ will be pulled out as possible into smaller files in separate PRs
* Convert `FileProcessor` to a true class definition
* Break up `./utils` into many files
  * Remove any `: any` types
* Make `@typescript-eslint/explicit-module-boundary-types` an error
* Fix all `// eslint-disable-next-line no-prototype-builtins` lines
* TypeScript other shiny files (ex: showcasemode)
* Completely remove `parcel` from `./package.json` and only use `esbuild`
* Delete 'shiny-es5' files
* Delete 'old' folder
* _Uglify_ js files (like in previous Gruntfile.js)
  * datepicker
  * ionrangeslider
  * selectize



# Eventual TODO
* Use yarn PnP
  * See `./patch/yarn_pnp.patch`
  * Use [esbuild](https://github.com/yarnpkg/berry/tree/master/packages/esbuild-plugin-pnp#yarnpkgesbuild-plugin-pnp)
  * Known problems:
    * `@yarnpkg/esbuild-plugin-pnp@0.0.1` gives full file paths, not relative file paths
    * `@testing-library/jest-dom/extend-expect` can not be found.
