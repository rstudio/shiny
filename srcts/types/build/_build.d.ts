import type { BuildOptions } from "esbuild";
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
declare function build(opts: BuildOptions): Promise<void>;
export { banner, build, outDir, shinyDesc };
