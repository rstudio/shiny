import type { BuildOptions } from "esbuild";
import esbuild from "esbuild";

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
  `(c) 2012-${new Date().getFullYear()} Posit Software, PBC.`,
  `License: ${shinyDesc.license} */`,
].join(" | ");
const banner = {
  js: bannerTxt,
  css: bannerTxt,
};

async function build(opts: BuildOptions): Promise<void> {
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

  const onRebuild = function (error: any | null) {
    if (error) {
      console.error(printNames.join(", "), "watch build failed:\n", error);
    } else {
      printNames.map((printName) => {
        console.log("√ -", printName, "-", new Date().toJSON());
      });
    }
    return;
  };

  const watch = process.argv.length >= 3 && process.argv[2] == "--watch";

  outFileNames.map((outFileName) => {
    console.log("Building " + outFileName);
  });

  const ctx = await esbuild.context({
    target: "es2021",
    preserveSymlinks: true,
    ...opts,
  });

  if (watch) {
    await ctx.watch();
    onRebuild(null);
  } else {
    await ctx.rebuild();
    onRebuild(null);
    await ctx.dispose();
  }
}

export { banner, build, outDir, shinyDesc };
