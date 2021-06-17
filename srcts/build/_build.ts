import {
  build as esbuildBuild,
  BuildIncremental,
  BuildOptions,
  BuildResult,
} from "esbuild";
import readcontrol from "readcontrol";
import process from "process";
import { basename } from "path";

const outDir = "./inst/www/shared/";

type ShinyDesc = { version: string; package: string; license: string };
const shinyDesc = readcontrol.readSync("./DESCRIPTION") as ShinyDesc;

const bannerTxt = [
  `/*! ${shinyDesc.package} ${shinyDesc.version}`,
  `(c) 2012-${new Date().getFullYear()} RStudio, PBC.`,
  `License: ${shinyDesc.license} */`,
].join(" | ");
const banner = {
  js: bannerTxt,
  css: bannerTxt,
};

async function build(
  opts: BuildOptions
): Promise<BuildIncremental | BuildResult> {
  const outFileNames = opts.outfile
    ? [basename(opts.outfile)]
    : (opts.entryPoints as string[]).map((entry) => basename(entry));

  const strSize = "shiny.min.js".length;
  const printNames = outFileNames;

  for (let i = 0; i < printNames.length; i++) {
    while (printNames[i].length < strSize) {
      printNames[i] = printNames[i] + " ";
    }
  }

  const onRebuild = function (error?: string) {
    if (error) {
      console.error(printNames.join(", "), "watch build failed:\n", error);
    } else {
      printNames.map((printName) => {
        console.log("√ -", printName, "-", new Date().toJSON());
      });
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

  outFileNames.map((outFileName) => {
    console.log("Building " + outFileName);
  });
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

export { outDir, build, shinyDesc, banner };
