# Development Rules
* Put Imports at top
* Put Exports at bottom
* Any `window.***` calls are done in `./src/external` folder only.
  * Add exported values / function if necessary.
  * This helps keep each file self contained. Trying not to have random inputs from anywhere
* Anything that needs to be initialized on start **must** exist in the `./src/initialize` folder.
  * No file should produce any side effects.
  * To capture initializations, export a `setFoo(foo)` method that updates a locally defined `foo` variable.
*


# Approach
# TODO

* √Move everything into a single ts file. This will allow for the functions to find themselves
  * √Except the utils.js already converted
* √ es6 shiny.js
  * √ Pass in version using esbuild
  * √ Move all shiny files in order to main.ts
  * √ validate polyfills are working by finding them in the code
* √ Produce minified shiny js
* √ Disable $ from being found without an import
  * √ Using a patch with yarn v2
* Document `./package.json` scripts
* Set up initial jest tests
* Verify that `babel` is configurable
  * Meaning targeting browsers

# Later TODO

* After merging this PR, each _file_ will be pulled out as possible into smaller files
* √ Remove parcel from package.json and only use esbuild
* Break up `./utils` into many files
* Remove any `: any` types
* Fix all `// eslint-disable-next-line no-prototype-builtins` lines
* Convert `FileProcessor` to a true class definition
* es6 other shiny files (ex: showcasemode)
* Delete 'shiny-es5' files
* Delete 'old' folder
