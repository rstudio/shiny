// srcts/PROTOCOL.md, "unload": a page leaving for good can only come back
// fresh, so the server may drop its saved state. A page entering the
// back/forward cache (`persisted`) may come back as it is.
function unloadOnPageHide(persisted: boolean, token: string | null): boolean {
  return !persisted && token !== null;
}

export { unloadOnPageHide };
