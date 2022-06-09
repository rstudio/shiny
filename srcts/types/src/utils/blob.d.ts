declare type BlobBuilderConstructor = typeof window.MSBlobBuilder;
declare function setBlobBuilder(blobBuilderClass_: BlobBuilderConstructor): void;
declare function makeBlob(parts: BlobPart[]): Blob;
export { makeBlob, setBlobBuilder };
export type { BlobBuilderConstructor };
