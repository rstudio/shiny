import type { BuildIncremental, BuildOptions, BuildResult } from "esbuild";
declare const outDir = "./inst/www/shared/";
type ShinyDesc = {
    version: string;
    package: string;
    license: string;
};
declare const shinyDesc: ShinyDesc;
declare const banner: {
    js: string;
    css: string;
};
declare function build(opts: BuildOptions): Promise<BuildIncremental | BuildResult>;
export { outDir, build, shinyDesc, banner };
