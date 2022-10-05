# `yarn` Patch files

* `yarn_pnp.patch`
  * This file is currently not used and is outdated.
  * This provides a good game plan on how to use PnP with Yarn once esbuild can easily be integrated with Yarn PnP.
  * Using PnP removes the `node_modules` folder, but adds a zip of each package. I **do not** like Yarn's suggestion to commit these zip files to support their [Zero Installs](https://next.yarnpkg.com/features/zero-installs) philosophy.
  * Reference:
    * https://next.yarnpkg.com/features/pnp
    * https://yarnpkg.com/api/modules/esbuild_plugin_pnp.html
