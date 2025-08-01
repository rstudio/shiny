import type { HtmlDep } from "../shiny/render";
import type { MapValuesUnion, MapWithResult } from "./extraTypes";
import { asArray, hasDefinedProperty, hasOwnProperty } from "./object";
declare function escapeHTML(str: string): string;
declare function randomId(): string;
declare function strToBool(str: string): boolean | undefined;
declare function getStyle(el: Element, styleProp: string): string | undefined;
declare function padZeros(n: number, digits: number): string;
declare function roundSignif(x: number, digits?: number): number;
declare function parseDate(dateString: string): Date;
declare function formatDateUTC(x: Date): string;
declare function makeResizeFilter(el: HTMLElement, func: (width: HTMLElement["offsetWidth"], height: HTMLElement["offsetHeight"]) => void): () => void;
declare function pixelRatio(): number;
declare function getBoundingClientSizeBeforeZoom(el: HTMLElement): {
    width: number;
    height: number;
};
declare function scopeExprToFunc(expr: string): (scope: unknown) => unknown;
declare function mergeSort<Item>(list: Item[], sortfunc: (a: Item, b: Item) => boolean | number): Item[];
declare function $escape(val: undefined): undefined;
declare function $escape(val: string): string;
declare function mapValues<T extends {
    [key: string]: any;
}, R>(obj: T, f: (value: MapValuesUnion<T>, key: string, object: typeof obj) => R): MapWithResult<T, R>;
declare function isnan(x: unknown): boolean;
declare function _equal(x: unknown, y: unknown): boolean;
declare function equal(...args: unknown[]): boolean;
declare const compareVersion: (a: string, op: "<" | "<=" | "==" | ">" | ">=", b: string) => boolean;
declare function updateLabel(labelContent: string | {
    html: string;
    deps: HtmlDep[];
} | undefined, labelNode: JQuery<HTMLElement>): Promise<void>;
declare function getComputedLinkColor(el: HTMLElement): string;
declare function isBS3(): boolean;
declare function toLowerCase<T extends string>(str: T): Lowercase<T>;
declare function isShinyInDevMode(): boolean;
export { $escape, _equal, asArray, compareVersion, equal, escapeHTML, formatDateUTC, getBoundingClientSizeBeforeZoom, getComputedLinkColor, getStyle, hasDefinedProperty, hasOwnProperty, isBS3, isnan, isShinyInDevMode, makeResizeFilter, mapValues, mergeSort, padZeros, parseDate, pixelRatio, randomId, roundSignif, scopeExprToFunc, strToBool, toLowerCase, updateLabel, };
