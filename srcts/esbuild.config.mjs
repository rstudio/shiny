import esbuild from "esbuild";
import babel from "esbuild-plugin-babel";
import readcontrol from "readcontrol";
import process from "process";
import globalsPlugin from "esbuild-plugin-globals";

let watch = process.argv.length >= 3 && process.argv[2] == "--watch";

let outdir = "../inst/www/shared/";
let opts = {
  entryPoints: ["src/index.ts"],
  bundle: true,
  watch: watch,
  plugins: [
    globalsPlugin({
      jquery: "window.jQuery",
      strftime: "window.strftime",
    }),
    babel(),
  ],
  target: "es5",
  sourcemap: true,
  define: {
    "process.env.SHINY_VERSION": `"${
      readcontrol.readSync("../DESCRIPTION").version
    }"`,
  },
};

console.log("Building shiny.js");
await esbuild.build({
  ...opts,
  outfile: outdir + "shiny.js",
});

console.log("Building shiny.min.js");
await esbuild.build({
  ...opts,
  outfile: outdir + "shiny.min.js",
  minify: true,
});
