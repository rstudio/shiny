function windowBlobBuilder(): MSBlobBuilder {
  const blob: MSBlobBuilder =
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
