// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

import type { BuildOptions } from "esbuild";
import globalsPlugin from "esbuild-plugin-globals";
import { banner, build, outDir, shinyDesc } from "./_build";
import { verifyJqueryImport } from "./_jquery";

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
  ],
  define: {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    "process.env.SHINY_VERSION": `"${shinyDesc.version}"`,
  },
  banner: banner,
};

// Make sure all ts files contain jquery import statements before building
verifyJqueryImport("srcts/src")
  .then(() => {
    Promise.all([
      build({
        ...opts,
        outfile: outDir + "shiny.js",
      }),
      build({
        ...opts,
        outfile: outDir + "shiny.min.js",
        minify: true,
      }),
    ]);
  })
  .catch((err) => {
    console.error("Error:\n" + err);
  });
