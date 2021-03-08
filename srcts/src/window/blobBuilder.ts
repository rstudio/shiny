function windowBlobBuilder(): MSBlobBuilder {
  const blob =
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    window.BlobBuilder ||
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    window.WebKitBlobBuilder ||
    // eslint-disable-next-line @typescript-eslint/ban-ts-comment
    // @ts-ignore
    window.MozBlobBuilder ||
    window.MSBlobBuilder;

  return blob as MSBlobBuilder;
}

export { windowBlobBuilder };
