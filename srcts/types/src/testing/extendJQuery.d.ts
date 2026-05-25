import "jquery";
declare global {
    interface JQuery {
        internalTest: () => void;
    }
}
