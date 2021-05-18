/* eslint "@typescript-eslint/ban-ts-comment": 0 */
// @ts-nocheck
/* eslint "camelcase": 0 */
/* eslint "@typescript-eslint/no-unused-vars": 0 */
/* eslint "@typescript-eslint/no-this-alias": 0 */
/* eslint "@typescript-eslint/no-empty-function": 0 */
/* eslint "no-prototype-builtins": 0 */
/* eslint "prefer-const": 0 */
/* eslint "no-constant-condition": 0 */

import $ from "jquery";
jQuery = $;

import {
  escapeHTML,
  randomId,
  strToBool,
  getStyle,
  padZeros,
  roundSignif,
  parseDate,
  formatDateUTC,
  makeResizeFilter,
  pixelRatio,
  scopeExprToFunc,
  asArray,
  mergeSort,
  $escape,
  mapValues,
  isnan,
  _equal,
  equal,
  compareVersion,
  updateLabel,
  getComputedLinkColor,
  makeBlob,
  isBS3,
} from "./utils";

import { isQt, isIE, IEVersion } from "./utils/browser";

import { FileProcessor, FileUploader } from "./file/FileProcessor";

import { Shiny } from "./shiny";

import { inputBindings, outputBindings } from "./bindings";

function main(): void {
  // "_start.js"
  // √

  // "utils.js"
  // √

  // "browser.js"
  // √

  // "input_rate.js"
  // √; ./time/invoke.ts
  // √; ./time/debounce.ts
  // √; ./time/throttle.ts

  // √; ./inputPolicies/inputBatchSender.ts
  // √; ./inputPolicies/inputNoResendDecorator.ts
  // √; ./inputPolicies/inputEventDecorator.ts
  // √; ./inputPolicies/inputRateDecorator.ts
  // √; ./inputPolicies/inputDeferDecorator.ts
  // √; ./inputPolicies/inputValidateDecorator.ts

  // "shinyapp.js"
  // √; ./shiny/shinyapp.ts
  // √; ./shiny/reconnectDialog.ts

  // "notifications.js"
  // √; shiny/notifications.ts

  // "modal.js"
  // √; shiny/modal.ts

  // "file_processor.js"
  // √

  // "binding_registry.js"
  // √; ./bindings/registry.ts

  // √; ./bindings/input/index.ts
  // const inputBindings = (Shiny.inputBindings = new BindingRegistry());

  // √; ./bindings/output/index.ts
  // const outputBindings = (Shiny.outputBindings = new BindingRegistry());

  // "output_binding.js"
  // √; ./bindings/output/index.ts

  // "output_binding_text.js"
  // √; ./bindings/output/text.ts

  // "output_binding_image.js"
  // √; ./bindings/output/image.ts

  // √; ./imageutils/index.ts
  // √; ./imageutils/disableDrag.ts
  // √; ./imageutils/initPanelScales.ts
  // √; ./imageutils/initCoordmap.ts
  // √; ./imageutils/findbox.ts
  // √; ./imageutils/shiftToRange.ts
  // √; ./imageutils/createClickInfo.ts
  // √; ./imageutils/createHandlers.ts
  // √; ./imageutils/createBrush.ts
  // √; ./imageutils/resetBrush.ts

  // "output_binding_html.js"
  // √; ./bindings/output/html.ts

  // √; ./shiny/render.ts

  // "output_binding_downloadlink.js"
  // √; ./bindings/output/downloadlink.ts

  // "output_binding_datatable.js"
  // √; ./bindings/output/datatable.ts

  // "output_binding_adapter.js"
  // √; ./bindings/output_adapter.ts

  // "input_binding.js"
  // √; ./bindings/input/index.ts

  // "input_binding_text.js"
  // √; ./bindings/input/text.ts

  // "input_binding_textarea.js"
  // √; ./bindings/input/textarea.ts

  // "input_binding_password.js"
  // √; ./bindings/input/password.ts

  // "input_binding_number.js"
  // √; ./bindings/input/number.ts

  // "input_binding_checkbox.js"
  // √; ./bindings/input/checkbox.ts

  // "input_binding_slider.js"
  // √; ./bindings/input/slider.ts

  // "input_binding_date.js"
  // √; ./bindings/input/date.ts

  // "input_binding_daterange.js"
  // √; ./bindings/input/daterange.ts

  // "input_binding_select.js"

  // "input_binding_radio.js"
  // √; ./bindings/input/radio.ts

  // "input_binding_checkboxgroup.js"
  // √; ./bindings/input/checkboxgroup.ts

  // "input_binding_actionbutton.js"
  // √; ./bindings/input/actionbutton.ts

  // "input_binding_tabinput.js"
  // √; ./bindings/input/tabinput.ts

  // "input_binding_fileinput.js"
  // √; ./bindings/input/fileinput.ts

  // √; ./shiny/init.ts


  // "reactlog.js"
  // √; ./shiny/reactlog.ts

  // "_end.js"
  // √
}

export { main };
