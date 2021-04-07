import {readdirSync, unlinkSync, writeFileSync} from "fs";
import esbuild from "esbuild";
// import process from "process";
// let watch = process.argv.length >= 3 && process.argv[2] == "--watch";

let instdir = "../inst/";
let outdir = instdir + "/www/shared/";

let opts = {
  bundle: false,
  watch: false,
  plugins: [],
  target: "es5",
  sourcemap: false,
};

console.log("Building datepicker");
const locale_files = readdirSync(instdir + "www/shared/datepicker/js/locales/")

let require_files = locale_files.map(function(filename) {
  return `require("./locales/${ filename }");`;
}).join("\n");

let tmpfile = instdir + "www/shared/datepicker/js/temp.js";
writeFileSync(tmpfile,
`require("./bootstrap-datepicker.js");
${require_files}`)
await esbuild.build({
  ...opts,
  bundle: true,
  entryPoints: [tmpfile],
  outfile: instdir + "www/shared/datepicker/js/bootstrap-datepicker.min.js",
  external: ['jquery'],
  minify: true,
});
// Clean up
unlinkSync(tmpfile);

console.log("Building ionrangeslider");
await esbuild.build({
  ...opts,
  entryPoints: [
    instdir + "www/shared/ionrangeslider/js/ion.rangeSlider.js"
  ],
  outfile: instdir + "www/shared/ionrangeslider/js/ion.rangeSlider.min.js",
  minify: true,
});

console.log("Building selectize");
await esbuild.build({
  ...opts,
  entryPoints: [
    instdir + "www/shared/selectize/accessibility/js/selectize-plugin-a11y.js"
  ],
  outfile: instdir + "www/shared/selectize/accessibility/js/selectize-plugin-a11y.min.js",
  minify: true,
});
