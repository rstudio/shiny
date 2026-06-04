/*
Make sure all `*.ts` files contain `"jquery"` import statements to properly
scope `jquery`, AND that jQuery imports / `JQuery<>` type references only
appear in srcts/src/dom/{module}/jquery.ts adapter files.

See `.claude/specs/2026-06-03-jquery-wrapper-design.md` section 4 for context.

Escape hatch: any file containing a single line of the form
  // shiny:jquery-allowed -- <reason>
is exempted from the adapter-file restriction. The reason is recorded in
an audit summary printed at the end of every build.
*/

import fs from "fs";
// eslint-disable-next-line @typescript-eslint/ban-ts-comment
// @ts-ignore; Type definitions are not found
import readdirRecursive from "fs-readdir-recursive";
import { isUndefined } from "lodash";

// eslint-disable-next-line prefer-regex-literals
const ADAPTER_PATH_RE = new RegExp(
  "(^|/)srcts/src/dom/[^/]+/jquery\\.ts$",
);
// eslint-disable-next-line prefer-regex-literals
const EXEMPTION_RE = new RegExp(
  "^//\\s*shiny:jquery-allowed\\s*--\\s*(.+)$",
  "m",
);
const IMPORT_JQUERY = 'import $ from "jquery";';
// eslint-disable-next-line prefer-regex-literals
const JQUERY_TYPE_RE = new RegExp("\\bJQuery\\s*<");

type Exemption = { file: string; reason: string };

async function verifyJqueryUsage(
  filename: string,
  exemptions: Exemption[],
): Promise<void> {
  const contents = await fs.promises.readFile(filename, "utf8");
  const lines = contents.split("\n");

  // ---- Rule 1 (existing): files that use $ must import it ---------------

  const jqueryWordMatch = lines.find(
    (line) => line.includes("jQuery.") || line.includes("jQuery("),
  );
  if (!isUndefined(jqueryWordMatch)) {
    throw (
      `Using \`jQuery\` in file: ${filename}\n` +
      `Match:\n${jqueryWordMatch}\n` +
      "Please use `$` instead of `jQuery`\n" +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  const dollarMatch = lines.find(
    (line) => line.includes("$.") || line.includes("$("),
  );
  const usesDollar = !isUndefined(dollarMatch);
  const hasJqueryImport = lines.includes(IMPORT_JQUERY);
  const inAdapter = ADAPTER_PATH_RE.test(filename);

  // Adapter files may define `const $ = getJQuery()` rather than importing it.
  if (usesDollar && !hasJqueryImport && !inAdapter) {
    throw (
      `Using \`$\` in file: ${filename}\n` +
      `Match:\n${dollarMatch}\n` +
      `Please call \`${IMPORT_JQUERY}\` at the top of the file.\n` +
      "See file ./srcts/build/_jquery.ts for more details"
    );
  }

  // ---- Rule 2 (new): jquery is fenced inside adapter files --------------

  const usesJqueryType = JQUERY_TYPE_RE.test(contents);
  const exemptMatch = contents.match(EXEMPTION_RE);

  if ((hasJqueryImport || usesJqueryType) && !inAdapter) {
    if (exemptMatch) {
      exemptions.push({ file: filename, reason: exemptMatch[1].trim() });
    } else {
      throw (
        `jQuery imports and \`JQuery<>\` types are only allowed in ` +
        `srcts/src/dom/*/jquery.ts adapter files.\n` +
        `Offending file: ${filename}\n` +
        `Use a dom/* dispatch function instead, or add a ` +
        `\`// shiny:jquery-allowed -- <reason>\` comment if the exception ` +
        `is intentional.\n` +
        "See file ./srcts/build/_jquery.ts for more details"
      );
    }
  }
}

const verifyJqueryImport = async function (dir = "."): Promise<void> {
  const tsFiles = (readdirRecursive(dir) as string[])
    .filter((path: string) => path.endsWith(".ts"))
    .map((path) => dir + "/" + path);

  const exemptions: Exemption[] = [];
  await Promise.all(
    tsFiles.map((file) => verifyJqueryUsage(file, exemptions)),
  );

  if (exemptions.length > 0) {
    // Print a stable, sorted audit so reviewers notice drift.
    exemptions.sort((a, b) => a.file.localeCompare(b.file));
    // eslint-disable-next-line no-console
    console.log(
      `\n[_jquery audit] ${exemptions.length} file(s) carry ` +
        `\`shiny:jquery-allowed\` exemptions:`,
    );
    for (const ex of exemptions) {
      // eslint-disable-next-line no-console
      console.log(`  - ${ex.file}: ${ex.reason}`);
    }
  }
};

export { verifyJqueryImport };
