import { setBlobBuilder } from "../utils/blob";
import { windowBlobBuilder } from "../window/blobBuilder";

function initBlobBuilder() {
  setBlobBuilder(windowBlobBuilder());
}

export { initBlobBuilder };
