// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

import { banner, build, outDir, shinyDesc, babelPlugin } from "./_build";
import globalsPlugin from "esbuild-plugin-globals";
import type { BuildOptions } from "esbuild";

const opts: BuildOptions = {
  entryPoints: ["srcts/src/index.ts"],
  bundle: true,
  sourcemap: true,
  plugins: [
    globalsPlugin({
      jquery: "window.jQuery",
      //// Loaded dynamically. MUST use `window.strftime` within code
      // strftime: "window.strftime",
    }),
    babelPlugin(),
  ],
  define: {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    "process.env.SHINY_VERSION": `"${shinyDesc.version}"`,
  },
  banner: banner,
};

build({
  ...opts,
  outfile: outDir + "shiny.js",
});
build({
  ...opts,
  outfile: outDir + "shiny.min.js",
  minify: true,
});
