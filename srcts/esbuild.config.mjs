import esbuild from "esbuild";
import babel from "esbuild-plugin-babel";
import readcontrol from "readcontrol";
import process from "process";
import globalsPlugin from "esbuild-plugin-globals";

async function buildFile(
  fileName,
  extraOpts = {},
  strSize = "shiny.min.js".length
) {
  let watch = process.argv.length >= 3 && process.argv[2] == "--watch";
  let incremental = false;

  let printName = fileName;

  while (printName.length < strSize) {
    printName = printName + " ";
  }

  const onRebuild = function (error, result) {
    if (error) {
      console.error("Watch build failed:", error);
    } else {
      console.log("√ -", printName, "-", new Date().toJSON());
    }
    return;
  };

  if (watch) {
    incremental = true;
    watch = {
      onRebuild: onRebuild,
    };
  }

  const outdir = "../inst/www/shared/";

  console.log("Building " + fileName);
  await esbuild.build({
    outfile: outdir + fileName,
    entryPoints: ["src/index.ts"],
    bundle: true,
    incremental: incremental,
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
    ...extraOpts,
  });
  onRebuild();
}

buildFile("shiny.js");
buildFile("shiny.min.js", { minify: true });
