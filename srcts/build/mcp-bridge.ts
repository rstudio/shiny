// Builds the MCP Apps bridge (inlined into the ui://shiny/app resource):
// ```
// npm run bundle_mcp
// ```

import type { BuildOptions } from "esbuild";
import { banner, build, outDir, shinyDesc } from "./_build";

const opts: BuildOptions = {
  entryPoints: ["srcts/src/mcp/index.ts"],
  bundle: true,
  format: "iife",
  sourcemap: false,
  define: {
    // eslint-disable-next-line @typescript-eslint/naming-convention
    "process.env.SHINY_VERSION": `"${shinyDesc.version}"`,
  },
  banner: banner,
};

// eslint-disable-next-line @typescript-eslint/no-floating-promises
build({
  ...opts,
  outfile: outDir + "shiny-mcp-bridge.js",
  minify: true,
});
