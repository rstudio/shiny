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

# Later TODO

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


# Eventual TODO
* Use yarn PnP
  * Use [esbuild](https://github.com/yarnpkg/berry/tree/master/packages/esbuild-plugin-pnp#yarnpkgesbuild-plugin-pnp)
  * Remove `./.yarnrc.yaml` `nodeLinker` key
  * TODO - Figure out how to call the esbuild command with the missing packages. Currently Yarn can't ifnd `esbuild` and suggests `esbuild-X.Y.Z-SHA` (or something other than `esbuild`) which does not make sense.
    * Calling `yarn node esbuild.config.mjs` does not work
    * Calling `yarn pnpify node esbuild.config.mjs` does not work
