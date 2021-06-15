# `yarn` Patch files

* `types-jquery.patch`
  * Do not export `$` as a globally available variable. When developing TS code, I like to have full control over the variables. It is good to have a record of where everything comes from. Shiny can not use the latest jQuery loaded onto the page and needs to use a scoped `jQuery` variable. In the end, we can shim the `jquery` import to be `window.jQuery`
* `yarn_pnp.patch`
  * This file is currently not used and is outdated.
  * This provides a good game plan on how to use PnP with Yarn once esbuild can easily be integrated with Yarn PnP.
  * Using PnP removes the `node_modules` folder, but adds a zip of each package. I **do not** like Yarn's suggestion to commit these zip files to support their [Zero Installs](https://next.yarnpkg.com/features/zero-installs) philosophy.
  * Reference:
    * https://next.yarnpkg.com/features/pnp
    * https://yarnpkg.com/api/modules/esbuild_plugin_pnp.html
