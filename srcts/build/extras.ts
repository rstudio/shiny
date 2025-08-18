// This build script must be executed from the root repo directory via
// ```
// npm run build
// ```

import autoprefixer from "autoprefixer";
import { exec } from "child_process";
import { sassPlugin } from "esbuild-sass-plugin";
import postcss from "postcss";
import postcssPresetEnv from "postcss-preset-env";
import { promisify } from "util";

import { banner, build, outDir } from "./_build.js";

const execAsync = promisify(exec);

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

async function main(): Promise<void> {
  await Promise.all([
    // - TypeScript -----------------------------------------------------------
    // TypeScript builds
    build({
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
    }),

    // Shiny sass MUST be built using `Rscript tools/updateShinyCss.R`
    execAsync("Rscript tools/updateShinyCss.R").then(({ stdout, stderr }) => {
      if (stdout) console.log(stdout);
      if (stderr) console.error(stderr);
    }),

    // Sass builds
    build({
      ...sassOpts,
      entryPoints: ["srcts/extras/shiny-showcase.scss"],
      outfile: outDir + "shiny-showcase.css",
    }),
    build({
      ...sassOpts,
      entryPoints: ["srcts/extras/busy-indicators/busy-indicators.scss"],
      outfile: outDir + "busy-indicators/busy-indicators.css",
      plugins: [sassPlugin()],
      bundle: false,
      metafile: true,
    }),
  ]);
}

main().catch((err) => {
  console.error("Error:\n" + err);
  process.exit(1);
});
