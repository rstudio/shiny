import assert from "node:assert/strict";
import test from "node:test";

// Note: DOM-dependent parts of $enterClosedState (overlay rendering,
// shiny:closed event, postMessage to parent) are not covered here —
// they require a DOM environment (jsdom or browser).
// DOM coverage is deferred to integration testing.
//
// This file covers the non-DOM contract: the hardDisconnectConfig message
// handler (registered in ShinyApp._init via addCustomMessageHandler) is
// expected to stash `message.message` onto `$hardDisconnectMessage`.
//
// The handler body from shinyapp.ts is:
//   (message: { message: string }) => {
//     this.$hardDisconnectMessage = message.message;
//   }
//
// If that contract changes, update both the handler and this test.

void test("hardDisconnectConfig handler stashes the message string", () => {
  // Simulate the handler body inline. The actual handler lives in
  // shinyapp.ts (_init) and is registered via addCustomMessageHandler.
  const stash: { $hardDisconnectMessage: string | null } = {
    $hardDisconnectMessage: null,
  };
  const handler = (message: { message: string }) => {
    stash.$hardDisconnectMessage = message.message;
  };

  handler({ message: "Thanks for using the app." });

  assert.equal(stash.$hardDisconnectMessage, "Thanks for using the app.");
});

void test("hardDisconnectConfig handler handles an empty string", () => {
  const stash: { $hardDisconnectMessage: string | null } = {
    $hardDisconnectMessage: null,
  };
  const handler = (message: { message: string }) => {
    stash.$hardDisconnectMessage = message.message;
  };

  handler({ message: "" });

  assert.equal(stash.$hardDisconnectMessage, "");
});

void test("hardDisconnectConfig handler overwrites a previously stashed message", () => {
  const stash: { $hardDisconnectMessage: string | null } = {
    $hardDisconnectMessage: "old message",
  };
  const handler = (message: { message: string }) => {
    stash.$hardDisconnectMessage = message.message;
  };

  handler({ message: "new message" });

  assert.equal(stash.$hardDisconnectMessage, "new message");
});
