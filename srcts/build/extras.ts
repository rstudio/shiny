// This build script must be executed from the root repo directory via
// ```
// npm run build
// ```

// - TypeScript -----------------------------------------------------------

import { banner, build, outDir } from "./_build.js";

await build({
  bundle: true,
  sourcemap: true,
  minify: true,
  plugins: [],
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
import { sassPlugin } from "esbuild-sass-plugin";
import postcss from "postcss";
import postcssPresetEnv from "postcss-preset-env";

const sassOpts = {
  minify: true,
  banner: banner,
  plugins: [
    sassPlugin({
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
      async transform(source: string, resolveDir: string) {
        const { css } = await postcss([
          autoprefixer,
          postcssPresetEnv({ stage: 0 }),
        ]).process(source, { from: undefined });
        return css;
      },
    }),
  ],
};

await build({
  ...sassOpts,
  entryPoints: ["srcts/extras/shiny-showcase.scss"],
  outfile: outDir + "shiny-showcase.css",
});
await build({
  ...sassOpts,
  entryPoints: [
    // Must keep shiny.scss within `inst` to be able to use as htmldependency
    outDir + "shiny_scss/shiny.scss",
  ],
  outfile: outDir + "shiny.min.css",
});
await build({
  ...sassOpts,
  entryPoints: ["srcts/extras/busy-indicators/busy-indicators.scss"],
  outfile: outDir + "busy-indicators/busy-indicators.css",
  plugins: [sassPlugin()],
  bundle: false,
  metafile: true,
});
