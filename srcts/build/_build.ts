import {
  build as esbuildBuild,
  BuildIncremental,
  BuildOptions,
  BuildResult,
} from "esbuild";
import process from "process";
import { basename } from "path";

const outDir = "./inst/www/shared/";

async function build(
  opts: BuildOptions,
  strSize = "shiny.min.js".length
): Promise<BuildIncremental | BuildResult> {
  const outFileName = basename(opts.outfile);
  let printName = outFileName;

  while (printName.length < strSize) {
    printName = printName + " ";
  }

  const onRebuild = function (error?: string) {
    if (error) {
      console.error(printName, "watch build failed:\n", error);
    } else {
      console.log("√ -", printName, "-", new Date().toJSON());
    }
    return;
  };

  let incremental = false;
  let watch: false | { onRebuild: (error, result) => void } = false;

  if (process.argv.length >= 3 && process.argv[2] == "--watch") {
    incremental = true;
    watch = {
      onRebuild: onRebuild,
    };
  }

  console.log("Building " + outFileName);
  return esbuildBuild({
    incremental: incremental,
    watch: watch,
    target: "es5",
    ...opts,
  }).then((x) => {
    onRebuild();
    return x;
  });
}

export { outDir, build };
