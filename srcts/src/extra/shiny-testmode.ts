import { indirectEval } from "../utils/eval";

// Listen for messages from parent frame. This file is only added when the
// shiny.testmode option is TRUE.
window.addEventListener("message", function (e: { data: { code: string } }) {
  const message = e.data;

  if (message.code) indirectEval(message.code);
});
