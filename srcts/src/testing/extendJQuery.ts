// Used to avoid isolated module warning
import "jquery";

declare global {
  interface JQuery {
    // used for testing only
    internalTest: () => void;
  }
}
