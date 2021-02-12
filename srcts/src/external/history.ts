function main(): void {
  const origPushState = window.history.pushState;

  window.history.pushState = function (...args) {
    const result = origPushState.apply(this, args);

    $(document).trigger("pushstate");
    return result;
  };
}

export { main };
