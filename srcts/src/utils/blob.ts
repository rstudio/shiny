import $ from "jquery";

let BlobBuilder;

function setBlobBuilder(BlobBuilder_: MSBlobBuilder): void {
  BlobBuilder = BlobBuilder_;
  return;
}

function makeBlob(parts: BlobPart[]): Blob {
  // Browser compatibility is a mess right now. The code as written works in
  // a variety of modern browsers, but sadly gives a deprecation warning
  // message on the console in current versions (as of this writing) of
  // Chrome.

  // Safari 6.0 (8536.25) on Mac OS X 10.8.1:
  // Has Blob constructor but it doesn't work with ArrayBufferView args

  // Google Chrome 21.0.1180.81 on Xubuntu 12.04:
  // Has Blob constructor, accepts ArrayBufferView args, accepts ArrayBuffer
  // but with a deprecation warning message

  // Firefox 15.0 on Xubuntu 12.04:
  // Has Blob constructor, accepts both ArrayBuffer and ArrayBufferView args

  // Chromium 18.0.1025.168 (Developer Build 134367 Linux) on Xubuntu 12.04:
  // No Blob constructor. Has WebKitBlobBuilder.

  try {
    return new Blob(parts);
  } catch (e) {
    const blobBuilder = new BlobBuilder();

    $.each(parts, function (i, part) {
      blobBuilder.append(part);
    });
    return blobBuilder.getBlob();
  }
}

export { makeBlob, setBlobBuilder };
