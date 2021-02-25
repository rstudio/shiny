# TypeScript build tools

## First-time setup
Shiny's TypeScript build tools use Node.js, along with [yarn](https://yarnpkg.com/) v2 to manage the JavaScript packages.

Installation of Node.js differs across platforms, see [the official Node.js website](https://nodejs.org/) for instructions on downloading and installing. We presume that you have Node.js installed on your machine before continuing.

Install yarn using the [official instructions](https://yarnpkg.com/en/docs/install).

You can test that Node.js and yarn are installed properly by running the following commands:

```bash
node --version
yarn --version
```

Once both are installed, run the following in this directory (`srcts/`) to install the packages :

```bash
yarn install
```

### Yarn v2

This repo uses Yarn v2. If you have the latest yarn installed from brew (v1.x), Yarn will pick up on the fact that this repo is a Yarn v2 (`berry`) repo.

For compatibility with `esbuild`, the `./node_modules` is still maintained.

## Updating Node

To avoid finding and determining which is the best `node` version to use, use a package called `n` to help facilitate installing the latest stable version of node.

> `npx` is a command that allows you to run a package and command even if the package is not installed. It is distributed with the latest releases of node.

```bash
# Update to the latest stable node version
npx n stable

# View installed versions
npx n ls
```

## Adding packages
If in the future you want to upgrade or add a package, run:

```bash
yarn add --dev [packagename]
```

This will automatically add the package to the dependencies in `./package.json`, and it will also update the `./yarn.lock` to reflect that change. If someone other than yourself does this, simply run `yarn` to update your local packages to match the new `./package.json`.

## Upgrading packages
Periodically, it's good to upgrade the packages to a recent version. There's two ways of doing this, depending on your intention:

1. Use `yarn up` to upgrade all dependencies to their latest version based on the version range specified in the package.json file (the `./yarn.lock` file will be recreated as well. Yarn packages use [semantic versioning](https://yarnpkg.com/en/docs/dependency-versions), i.e. each version is writen with a maximum of 3 dot-separated numbers such that: `major.minor.patch`. For example in the version `3.1.4`, 3 is the major version number, 1 is the minor version number and 4 is the patch version number. Here are the most used operators (these appear before the version number):

  - `~` is for upgrades that keep the minor version the same (assuming that was specified);

  - `^` is for upgrades that keep the major version the same (more or less -- more specifically, it allow changes that do not modify the first non-zero digit in the version, either the 3 in 3.1.4 or the 4 in 0.4.2.). This is the default operator added to the package.json when you run `yarn add [package-name]`.

2. Use `yarn up [package]` to upgrade a single named package to the version specified by the latest tag (potentially upgrading the package across major versions).

3. To see all outdated packages, run `yarn outdated`

# Bundling the TypeScript

[esbuild](https://esbuild.github.io/) is a build tool that (for Shiny's purposes) compiles the TypeScript into a single JavaScript file.

## Using `esbuild`

To run all default build tasks, simply go into the `./srcts` directory and run:

```bash
yarn build
```

It's also useful to have `esbuild` watch for updated files and immediately re-build `shiny.js` as necessary during development. This is done with:

```bash
yarn watch
```

Both JavaScript files will produce a sourcemap (`**.js.map`) that the browser will understand.  This will help you debug Shiny's JavaScript code within the browser and point back to the original TypeScript files.

<!-- #### Auto build and browser refresh

An alternative to `yarn watch` is to use `entr` to trigger `grunt` when sources change. `entr` can be installed with `brew install entr` on a Mac, or on Linux using your distribution's package manager. Using this technique, it's possible to both automatically rebuild sources and reload Chrome at the same time:

*macOS*:

```bash
find ../srcts/ | entr bash -c './node_modules/grunt/bin/grunt && osascript -e "tell application \"Google Chrome\" to reload active tab of window 1"'
```

*Linux*:

For this to work you must first install `xdotool` using your distribution's package manager.

```bash
find ../srcts/ | entr bash -c './node_modules/grunt/bin/grunt && xdotool search --onlyvisible --class Chrome windowfocus key ctrl+r'
``` -->



# Development in VSCode

VSCode does not like to develop TypeScript with the configuration files in a subfolder. To leverage full VSCode capabilities, it is recommended to open the `./srcts` folder as the root folder of a VSCode project. This will enable VSCode to readily find all of the configuration files.

## Config files

* `.browserslistrc`: Used with `browserslist` and `core-js` to determine which polyfills should be incorporated.
* `.eslintrc.yml`: Used with `eslint` and `prettier` to determine how the TypeScript files should be formatted and which lint failures should cause warnings, errors, or be ignored.
* `.prettierrc.yml`: Used by `prettier` to know how to adjust code when a file is saved in VSCode or within `eslint`'s linting process.
* `yarnrc.yml`: Notifies `yarn` to use `yarn` v2, install `./node_modules` folder for esbuild, and any plugins that may be used.
* `babel.config.json`: Used within `babel` transpilation of TypeScript -> JavaScript -> polyfilled JavaScript. `core-js` polyfills are only added as necessary and the `core-js` library is directly ignored to [avoid being processed by `babel`](https://github.com/zloirock/core-js/issues/743#issuecomment-571983318).
* `esbuild.config.mjs`: Script that will build `shiny.js` and `shiny.min.js` with their sourcemaps
* `jest.config.js`: Used to configure `jest` testing
* `package.json`: Contains useful scripts that can be run by `yarn` via `yarn run SCRIPTNAME`. The scripts described below are inteded for developer use. All other scripts are means to an end.
  * `yarn run watch`: Watch `./src` for changes and rebuild the JavaScript files.
  * `yarn run build`: Build `shiny.js` and `shiny.min.js` in `../inst/www/shared`. Both files will have a corresponding sourcemap
  * `yarn run lint`: Fix all TypeScript lints using [`eslint`](https://eslint.org/) and [`prettier`](https://prettier.io/)
  * `yarn run test`: Run all TypeScript tests
* `tsconfig.json`: Used by TypeScript.
  * `target: ES5`: Compile to es5, so babel has an easier job.
  * `preserveConstEnums: false`: Do no preserve enum values into the final code. (If true, produces bloat / unused code)
  * `isolatedModules: true`: Requested by esbuild. This allows for esbuild to compile the files in parallel.

# Updating web libraries
## `@types/jquery`

As of v3.5.5, `@types/jquery` produces a globally available constant of `$` and `jQuery`. This is problematic as TypeScript is there to enforce that all variables are accounted for. Declaring that these two variables exist globally removes the requirement to import `$` (or `jQuery`). This is bad for Shiny as the `$` would not be enforced to the "within package" `$`.

To overcome this, a patch is used to remove the globally defined `$` (and `Symbol`) variable declarations. Yarn v2 has a [`patch` protocol](https://yarnpkg.com/features/protocols#patch) that allows a local patch to be applied to the publically available `@types/jquery` package upon installation.

If in the future where the variables are not globally declared anymore, the patch may be removed and `@types/jquery` can be imported directly.

In regular code files, remember to only use `"./jquery"` module and not `"jquery"` directly to avoid double importing jQuery on the browser.

## `core-js`

To update the version of `core-js`:

* Check if there is a newer version available by running `yarn outdated core-js`. (If there's no output, then you have the latest version.)
* Run `yarn add --dev core-js --exact`.
