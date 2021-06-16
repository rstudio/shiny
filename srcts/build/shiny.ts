// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

import { build, outDir } from "./_build";
import globalsPlugin from "esbuild-plugin-globals";
import babelPlugin from "esbuild-plugin-babel";
import readcontrol from "readcontrol";
import { BuildOptions } from "esbuild";

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
    //
    babelPlugin(),
  ],
  define: {
    "process.env.SHINY_VERSION": `"${
      readcontrol.readSync("./DESCRIPTION").version
    }"`,
  },
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
