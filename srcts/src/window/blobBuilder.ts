function windowBlobBuilder(): MSBlobBuilder {
  const blob =
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.BlobBuilder ||
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.WebKitBlobBuilder ||
    // @ts-expect-error; Using legacy definitions of Blob builders
    window.MozBlobBuilder ||
    window.MSBlobBuilder;

  return blob as MSBlobBuilder;
}

export { windowBlobBuilder };
