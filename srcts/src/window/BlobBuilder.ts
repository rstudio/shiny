function windowBlobBuilder(): MSBlobBuilder {
  const blob =
    /* eslint "@typescript-eslint/ban-ts-comment": 0 */
    // @ts-ignore
    window.BlobBuilder ||
    // @ts-ignore
    window.WebKitBlobBuilder ||
    // @ts-ignore
    window.MozBlobBuilder ||
    window.MSBlobBuilder;

  return blob as MSBlobBuilder;
}

export { windowBlobBuilder };
