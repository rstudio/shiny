import { JSDOM } from "jsdom";
import assert from "node:assert/strict";
import { afterEach, before, test } from "node:test";
import type * as reconnectDialogModule from "../reconnectDialog";

// jQuery binds to `window` when it is first imported, so the DOM has to exist
// before the module under test is loaded.
const dom = new JSDOM("<!doctype html><body></body>", {
  pretendToBeVisual: true,
});
Object.assign(globalThis, {
  window: dom.window,
  document: dom.window.document,
});
const { document } = dom.window;

let dialog: typeof reconnectDialogModule;

before(async () => {
  const { setShinyObj } = await import("../initedMethods");
  setShinyObj({
    bindAll: async () => {},
    unbindAll: () => {},
    initializeInputs: () => {},
  } as never);
  dialog = await import("../reconnectDialog");
});

afterEach(() => {
  dialog.hideReconnectDialog();
  document.body.innerHTML = "";
});

const text = (selector: string) =>
  document.querySelector(selector)?.textContent;
const wait = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

void test("a later attempt restarts the countdown in the notice already on screen", async () => {
  await dialog.showReconnectDialog(1500);
  assert.equal(text("#shiny-reconnect-time"), " in 1s");
  const notice = document.querySelector("#shiny-reconnect-text");

  await dialog.showReconnectDialog(6500);
  assert.equal(text("#shiny-reconnect-time"), " in 6s");
  assert.equal(document.querySelectorAll("#shiny-reconnect-text").length, 1);
  assert.equal(document.querySelector("#shiny-reconnect-text"), notice);
});

void test("the resume-ended notice is not removed by the retry notice fading out", async () => {
  await dialog.showReconnectDialog(1500);
  dialog.hideReconnectDialog();
  await dialog.showResumeEndedDialog();
  await wait(400);
  assert.equal(
    text("#shiny-resume-ended"),
    "This session could not be resumed.",
  );
  assert.equal(document.querySelectorAll("#shiny-reconnect-text").length, 0);
  assert.ok(document.querySelector("#shiny-reload-link"));
});

void test("the resume-lost notice says the work was not restored and can be dismissed", async () => {
  await dialog.showResumeLostNotice();
  assert.equal(
    text("#shiny-resume-lost"),
    "Reconnected, but your previous work could not be restored.",
  );
  const notice = document
    .querySelector("#shiny-resume-lost")
    ?.closest(".shiny-notification");
  assert.ok(notice?.querySelector(".shiny-notification-close"));
});
