import esbuild from "esbuild";
import babel from "esbuild-plugin-babel";
import readcontrol from "readcontrol";

import process from "process";

let watch = process.argv.length >= 3 && process.argv[2] == "--watch";

let outdir = "../inst/www/shared/";
let opts = {
  entryPoints: ["src/index.ts"],
  bundle: true,
  watch: watch,
  plugins: [babel()],
  target: "es5",
  sourcemap: true,
  define: {
    SHINY_VERSION: `"${readcontrol.readSync("../DESCRIPTION").version}"`,
  },
};

console.log("Building shiny.js");
esbuild
  .build({
    ...opts,
    outfile: outdir + "shiny.js",
  })
  .then(function () {
    console.log("Building shiny.min.js");
    esbuild.build({
      ...opts,
      outfile: outdir + "shiny.min.js",
      minify: false,
    });
  })
  .catch(() => process.exit(1));
