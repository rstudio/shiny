/*
Make sure all `*.ts` files contain `"jquery"` import statements to properly scope `jquery`.

Prior behavior:
- Use a patch file to remove globally declared `$` variable
- PR: https://github.com/rstudio/shiny/pull/3296/commits/169318382d1d00927d0148a16fde4c96a291a602
Prior reasoning:
  - Only allow for jQuery type definitions to exist if imported.
  - This is problematic as it can lead to improperly scoped values of `$`
  - Ex:
    * If `a.ts` imports jquery, then `b.ts` can see the global definition of `$`
      even though `b.ts` does not import jquery.
    * If `$` is not scoped / encapsulated, then it is possible to have
      inconsistent versions of jquery executing within the shiny bundle.
Related:
- Open Issue ('16): https://github.com/DefinitelyTyped/DefinitelyTyped/issues/11187
- Closed stale PR ('19): https://github.com/DefinitelyTyped/DefinitelyTyped/pull/40295/files
- Unsolved Issue ('14): https://github.com/DefinitelyTyped/DefinitelyTyped/issues/1564
- Multiple `$` conflicts ('22): https://github.com/DefinitelyTyped/DefinitelyTyped/discussions/60443
  - Bandaid PR ('22): https://github.com/DefinitelyTyped/DefinitelyTyped/pull/60444

Approach within this file:
* For every `*.ts` file:
  * Do not use `jQuery` (use `$`!)
  * If utilizing `$`
  * Require `jquery` import statement
- Advantages:
  - Does not require a yarn patch file or separate GitHub repo containing the patched type definitions.
- Disadvantages:
  - Variable reassignment isn't caught `jq = $; jq(divs)`. (This should not happen, but it is possible.)
  - Only tested when bundling is started, not on every file change.
- PR: https://github.com/rstudio/shiny/pull/3710
*/

import fs from "fs";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore; Type definitions are not found
import readdirRecursive from "fs-readdir-recursive";
import { isUndefined } from "lodash";

const verifyJqueryUsage = async function (filename: string): Promise<void> {
  const contents = await fs.promises.readFile(filename, "utf8");
  const lines = contents.toString().split("\n");

  // Find if using `jQuery` in the file
  const jqueryMatch = lines.find((line) => {
    return line.includes("jQuery.") || line.includes("jQuery(");
  });

  if (!isUndefined(jqueryMatch)) {
    throw (
      `Using \`jQuery\` in file: ${filename}\n` +
      `Match:\n${jqueryMatch}\n` +
      "Please use `$` instead of `jQuery`\n" +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  // Find if using `$` in the file
  const dollarMatch = lines.find((line) => {
    return line.includes("$.") || line.includes("$(");
  });

  if (isUndefined(dollarMatch)) {
    // No match found. Not using jquery
    return;
  }

  // Using jquery, find that it is being imported
  const importJquery = 'import $ from "jquery";';
  const hasJqueryImport = lines.includes(importJquery);

  if (!hasJqueryImport) {
    // Not importing jquery, yell
    throw (
      `Using \`$\` in file: ${filename}\n` +
      `Match:\n${dollarMatch}\n` +
      `Please call \`${importJquery}\` at the top of the file.\n` +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  // Using jquery and importing it;
  return;
};

const verifyJqueryImport = async function (dir = "."): Promise<void> {
  const tsFiles = (readdirRecursive(dir) as string[])
    .filter((path: string) => path.endsWith(".ts"))
    .map((path) => dir + "/" + path);

  // Run all checks in parallel
  await Promise.all(tsFiles.map((file) => verifyJqueryUsage(file)));
  return;
};

export { verifyJqueryImport };
