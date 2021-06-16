// This build script must be executed from the root repo directory via
// ```
// yarn build
// ```

import { build, outDir } from "./_build";
import { readdir, unlink, writeFile } from "fs/promises";
import globalsPlugin from "esbuild-plugin-globals";

const opts = {
  bundle: false,
  sourcemap: false,
  target: "es5",
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
  entryPoints: [outDir + "selectize/accessibility/js/selectize-plugin-a11y.js"],
  outfile: outDir + "selectize/accessibility/js/selectize-plugin-a11y.min.js",
  minify: true,
});
