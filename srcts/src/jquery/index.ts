// import { jQuery, $ } from "../window/globals";

const jQuery = (window as any)["jQuery"];
const $: JQueryStatic = jQuery;

export { jQuery, $ };
