// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

// - TypeScript -----------------------------------------------------------

import { banner, build, outDir } from "./_build";
import babelPlugin from "esbuild-plugin-babel";

build({
  bundle: true,
  sourcemap: true,
  minify: true,
  plugins: [babelPlugin()],
  banner: banner,
  entryPoints: [
    "srcts/extras/shiny-autoreload.ts",
    "srcts/extras/shiny-showcase.ts",
    "srcts/extras/shiny-testmode.ts",
  ],
  outdir: outDir,
});

// - Sass -----------------------------------------------------------

import autoprefixer from "autoprefixer";
import postCssPlugin from "@deanc/esbuild-plugin-postcss";
import sassPlugin from "esbuild-plugin-sass";

const sassOpts = {
  minify: true,
  banner: banner,
  plugins: [
    sassPlugin(),
    postCssPlugin({
      plugins: [autoprefixer],
    }),
  ],
};

build({
  ...sassOpts,
  entryPoints: ["srcts/extras/shiny-showcase.scss"],
  outfile: outDir + "shiny-showcase.css",
});
build({
  ...sassOpts,
  entryPoints: [
    // Must keep shiny.scss within `inst` to be able to use as htmldependency
    outDir + "shiny_scss/shiny.scss",
  ],
  outfile: outDir + "shiny.min.css",
});
