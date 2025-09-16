// This build script must be executed from the root repo directory via
// ```
// npm run build
// ```

import globalsPlugin from "esbuild-plugin-globals";
import { readdir, unlink, writeFile } from "fs/promises";
import { build, outDir } from "./_build";

const opts = {
  bundle: false,
  sourcemap: false,
  // Oddly, esbuild seems to use the top-level tsconfig.json file even when just
  // minifying JS to JS. Because that tsconfig file has "strict":true, esbuild
  // ends up adding "use strict" to the top of each minified JS file, which can
  // alter behavior. To avoid this, we have a separate tsconfig file with
  // "alwaysStrict":false.
  tsconfig: "srcts/build/external_libs_tsconfig.json",
};

readdir(outDir + "datepicker/js/locales/").then(async (localeFiles) => {
  const requireFiles = localeFiles
    .map(function (filename) {
      return `require("./locales/${filename}");`;
    })
    .join("\n");

  const tmpFile = outDir + "datepicker/js/temp.js";

  await writeFile(
    tmpFile,
    `require("./bootstrap-datepicker.js");\n${requireFiles}`
  );

  await build({
    ...opts,
    plugins: [
      globalsPlugin({
        jquery: "window.jQuery",
      }),
    ],
    bundle: true,
    entryPoints: [tmpFile],
    outfile: outDir + "datepicker/js/bootstrap-datepicker.min.js",
    minify: true,
  });
  // Clean up
  unlink(tmpFile);
});

build({
  ...opts,
  entryPoints: [outDir + "ionrangeslider/js/ion.rangeSlider.js"],
  outfile: outDir + "ionrangeslider/js/ion.rangeSlider.min.js",
  minify: true,
});

build({
  ...opts,
  entryPoints: [outDir + "selectize/js/selectize.js"],
  outfile: outDir + "selectize/js/selectize.min.js",
  minify: true,
  target: "es6",
});

build({
  ...opts,
  entryPoints: [outDir + "selectize/accessibility/js/selectize-plugin-a11y.js"],
  outfile: outDir + "selectize/accessibility/js/selectize-plugin-a11y.min.js",
  minify: true,
});
