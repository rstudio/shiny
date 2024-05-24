import type {
  BuildFailure,
  BuildIncremental,
  BuildOptions,
  BuildResult,
  WatchMode,
} from "esbuild";
import { build as esbuildBuild } from "esbuild";

import { basename } from "path";
import process from "process";

// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore; Type definitions are not found. This occurs when `strict: true` in tsconfig.json
import readcontrol from "readcontrol";

const outDir = "./inst/www/shared/";

type ShinyDesc = { version: string; package: string; license: string };
const shinyDesc = readcontrol.readSync("./DESCRIPTION") as ShinyDesc;

const bannerTxt = [
  `/*! ${shinyDesc.package} ${shinyDesc.version}`,
  `(c) 2012-${new Date().getFullYear()} Posit, PBC.`,
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

  const strSizes = outFileNames.map((outFileName) => outFileName.length);

  strSizes.push("shiny.min.js".length);
  const strSize = Math.max(...strSizes);
  const printNames = outFileNames;

  for (let i = 0; i < printNames.length; i++) {
    while (printNames[i].length < strSize) {
      printNames[i] = printNames[i] + " ";
    }
  }

  const onRebuild = function (error: BuildFailure | null) {
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
  let watch: WatchMode | false = false;

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
    target: "es2020",
    preserveSymlinks: true,
    ...opts,
  }).then((x) => {
    onRebuild(null);
    return x;
  });
}

export { outDir, build, shinyDesc, banner };
