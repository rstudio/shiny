import { setBlobBuilder } from "../utils/blob";
import { windowBlobBuilder } from "../window/blobBuilder";

function initBlobBuilder(): void {
  setBlobBuilder(windowBlobBuilder());
}

export { initBlobBuilder };
