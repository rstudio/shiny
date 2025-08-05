# Using Shiny TypeScript Definitions

When developing TypeScript projects that use `window.Shiny`, we recommend installing the Shiny TypeScript definitions to your package. To install the latest stable definitions, run:

```bash
npm install https://github.com/rstudio/shiny\#v1.11.0 --save-dev
```

, matching the GitHub tag to your current the Shiny CRAN release (ex: `v1.11.0`).  If you are asked to select a version of `@types/jquery`, please select the closest matching version.

This will provide a global type definition of `window.Shiny`. In your code, you can access the Shiny object via `window.Shiny` or just `Shiny`. However, note that if you are using TypeScript, it will be OK with `window.Shiny` but it will flag uses of `Shiny` (without the `window.` prefix), because TypeScript won't know that it's a global variable. We consider it better practice to use `window.Shiny` instead of `Shiny`, but if you want TypeScript to know that `Shiny` is available as a global variable, you can add the following to a TypeScript file in your code base.

```ts
 declare global {
  const Shiny: ShinyClass;
}
```

When loading your compiled file, it should be loaded after `shiny.js` is loaded. If you are using an `htmlDependency()` to add your code to the page, your script will automatically be loaded after has been loaded.


----------------------------------------------------

# TypeScript build tools (Shiny Developers)

All files will be described as if the working directory is the root folder of `rstudio/shiny`, not relative to this `README.md` file.

## First-time setup

Shiny's TypeScript build tools use Node.js and `npm`.

Installation of Node.js differs across platforms, see [the official Node.js website](https://nodejs.org/) for instructions on downloading and installing. We presume that you have Node.js installed on your machine before continuing.

Install npm using the [official instructions](https://docs.npmjs.com/getting-started/installing-node).

You can test that Node.js and `npm` are installed properly by running the following commands:

```bash
node --version
npm --version
```

Once both are installed, run the following in the root repo directory to install the packages :

```bash
# Sitting in `rstudio/shiny` repo
npm install
```

### Reproducible builds

To ensure that the same versions of packages are used between users, install the package lock dependencies by running:

```bash
npm ci
```

This command will install the exact versions of packages specified in the `package-lock.json` file, ensuring that all developers are using the same versions of dependencies.

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
npm install --save-dev [packagename]
```

This will automatically add the package to the dependencies in `package.json`, and it will also update the `package-lock.json` to reflect that change. If someone other than yourself does this, simply run `npm ci` to update your local packages to match the new `package.json`.

## Upgrading packages

Periodically, it's good to upgrade the packages to a recent version. There's two ways of doing this, depending on your intention:

1. Use `npm outdated` to see outdated dependencies based on the version range specified in the `package.json` file. Npm packages use [semantic versioning](https://docs.npmjs.com/about-semantic-versioning), i.e. each version is writen with a maximum of 3 dot-separated numbers such that: `major.minor.patch`. For example in the version `3.1.4`, 3 is the major version number, 1 is the minor version number and 4 is the patch version number. Please use an `x` to denote where packages can automatically bump their version. For example, `3.1.x` will accept any patch version for `3.1`. Or `3.x` to accept any v3 minor version.

2. Use `npm update [package]` to upgrade a single named package to the version specified by the latest tag (potentially upgrading the package across major versions).

# TypeScript

## Learn about TypeScript

The documentation by [TypeScript](https://www.typescriptlang.org/docs/) is a solid resource to know each and every bell and whistle. Most features have examples and convey the thoughts well.

[TypeScript Deep Dive](https://basarat.gitbook.io/typescript/) is an online `bookdown`-like approach to TypeScript by "Microsoft MVP for TypeScript", Basarat Ali Syed. In his book, he goes through many examples of what you "should do", not necessarily "what is possible" like the [TypeScript docs](https://www.typescriptlang.org/docs/).

## TypeScript StyleGuide

Using the style guid from [TypeScript Deep Dive / StyleGuide](https://basarat.gitbook.io/typescript/styleguide), we extend it to have the usage be more familiar to R developers and preexisting Shiny development.  The goal is to produce consistent code that can be injested quickly.


### StyleGuide

* `null` vs. `undefined`
  * Do not use `x === null` unless you truly mean it.
  * Safer to use _truthy_ or _falsey_ checks instead. Ex: `if (x) {}`
* `type` vs `interface`
  * > Use `type` when you might need a union or intersection: `type Foo = number | { someProperty: number }`
  * > Use `interface` when you want extends or implements: `interface FooBar extends Foo { bar: string;}`
  * > Otherwise use whatever makes you happy that day.
* Namespace
  * `PascalCase`
  * Ex: `Shiny`

### Enforced (by `eslint`) StyleGuide

* Variable
  * `camelCase`
  * Ex: `const hello = "world`
* Class
  * `PascalCase`
  * Ex: `class InputBinding {}`
* Type, Interface definitions:
  * `PascalCase`
  * Ex: `type BindingBase = {name: string}`
  * Ex: `interface ShinyEventMessage extends JQuery.Event {}`
* Enum
  * `PascalCase`
  * (Currently unused)
* Single vs. Double Quotes
  * While the JS community has decided on single quotes, R has decided on double quotes.
  * > When you can't use double quotes, try using back ticks (`).
* Annotate Arrays as `Type[]`
  * Ex: `Foo[]` (vs `Array<Foo>`)
* Annotate Records as `{[key: string]: valueType}`
  * Ex: `const x: {[key: string]: number} = {a: 4}`
  * Ex: Extend the unknown key definition with static keys: `const x: {known: string, [key: string]: number} = {known: "yes", a: 4}`
* File Names
  * `camelCase` - Enforced by `eslint`

## Config files

The JavaScript community likes to build many small, effective packages that do minimal work. The unfortunate side effect is needing a config file for everything.

All config files are located in the root folder to avoid opening two separate VS Code projects.

* `.eslintrc.yml`
  * Used with `eslint` and `prettier` to determine how the TypeScript files should be formatted and which lint failures should cause warnings, errors, or be ignored.
* `.prettierrc.yml`
  * Used by `prettier` to know how to adjust code when a file is saved in VSCode or within `eslint`'s linting process.
* `package.json`
  * Contains useful scripts that can be run by `npm` via `npm run SCRIPTNAME`.
  * The scripts described below are inteded for developer use. All other scripts are means to an end.
    * `npm run watch` - Watch `srcts/src` for changes and rebuild the JavaScript files.
    * `npm run build` - Build `shiny.js` and `shiny.min.js` in `inst/www/shared`. Both files will have a corresponding sourcemap
    * `npm run lint` - Fix all TypeScript lints using [`eslint`](https://eslint.org/) and [`prettier`](https://prettier.io/)
* `tsconfig.json` -
  * TypeScript config file
  * Notable options set:
    * `target: ES2021` - Compile to es2021.
    * `preserveConstEnums: false` - Do no preserve enum values into the final code. (If true, produces bloat / unused code)
    * `isolatedModules: true` & `esModuleInterop: true` - Requested by `esbuild`. This [allows for `esbuild`](https://esbuild.github.io/content-types/#typescript) to safely compile the files in parallel

## Bundle TypeScript

[esbuild](https://esbuild.github.io/) is a build tool that (for Shiny's purposes) compiles the TypeScript into a single JavaScript file.

To run all build tasks, run:

```bash
npm run build
```

It's also useful to have `esbuild` watch for updated files and immediately re-build `shiny.js` as necessary during development. This is done with:

```bash
npm run watch
```

Both JavaScript files will produce a sourcemap (`**.js.map`) that the browser will understand.  This will help you debug Shiny's JavaScript code within the browser and point back to the original TypeScript files.

### Exported types

`./extras/globalShiny.ts` contains global declarations to define `window.Shiny`, a globally available `Shiny` variable, and a globally available `ShinyClass` type. This file is in a parallel folder to `./src` to avoid `Shiny` from being globally accessable within the source code. However, this file is the default type definition when the Type definitions are installed by external developers.

### GitHub Actions

On push to the `main` branch or push to a Pull Request to the `main` branch, a GitHub Action will be run to make sure the bundled JavaScript code is up to date. If the source code does not compile to the exact same file, it will be committed an pushed back to the outdated branch. (This makes it so the full build tools are not necessary for small tweaks and comments. 🎉)


# Updating dependencies


### External libraries

Shiny already has a handful of html dependencies that should NOT be bundled within `shiny.js`.  To update the dependencies below, see the directions in [`tools/README.md`](../tools).
* `jquery` / `@types/jquery`
* `bootstrap` / `@types/bootstrap`
  * Bootstrap is not being updated anymore. Only bootstrap 3.4 will be utilized within shiny.js. To use the latest bootstrap, see [`rstudio/bslib`](https://github.com/rstudio/bslib)
* `bootstrap-datepicker` / `@types/bootstrap-datepicker`
* `ion-rangeslider` / `@types/ion-rangeslider`
* `selectize` / `@types/selectize`
* `strftime`
