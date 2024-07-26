// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

// - TypeScript -----------------------------------------------------------

import { banner, build, outDir, babelPlugin } from "./_build";

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
import sassPlugin from "esbuild-plugin-sass";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore; Type definitions are not found. This occurs when `strict: true` in tsconfig.json
import postCssPlugin from "@deanc/esbuild-plugin-postcss";

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
build({
  ...sassOpts,
  entryPoints: ["srcts/extras/busy-indicators/busy-indicators.scss"],
  outfile: outDir + "busy-indicators/busy-indicators.css",
  plugins: [sassPlugin()],
  bundle: false,
  metafile: true,
});
