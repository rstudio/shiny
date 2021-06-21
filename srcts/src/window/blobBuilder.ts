import type { BlobBuilderConstructor } from "../utils/blob";

function windowBlobBuilder(): BlobBuilderConstructor {
  const blob =
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.BlobBuilder ||
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.WebKitBlobBuilder ||
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.MozBlobBuilder ||
    window.MSBlobBuilder;

  return blob;
}

export { windowBlobBuilder };
