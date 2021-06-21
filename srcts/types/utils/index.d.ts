import { makeBlob } from "./blob";
import { hasOwnProperty } from "./object";
declare function escapeHTML(str: string): string;
declare function randomId(): string;
declare function strToBool(str: string): boolean | undefined;
declare function getStyle(el: Element, styleProp: string): string | undefined;
declare function padZeros(n: number, digits: number): string;
declare function roundSignif(x: number, digits?: number): number;
declare function parseDate(dateString: string): Date;
declare function formatDateUTC(date: Date | null): string | null;
declare function makeResizeFilter(el: HTMLElement, func: (width: HTMLElement["offsetWidth"], height: HTMLElement["offsetHeight"]) => void): () => void;
declare function pixelRatio(): number;
declare function scopeExprToFunc(expr: string): (scope: unknown) => boolean;
declare function asArray<T>(value: T | T[] | null | undefined): T[];
declare function mergeSort<T>(list: T[], sortfunc: (a: T, b: T) => boolean | number): T[];
declare const $escape: (val: string) => string;
declare function mapValues<V, R>(obj: {
    [key: string]: V;
}, f: (value: V, key: string, obj: {
    [key: string]: V;
}) => R): {
    [key: string]: R;
};
declare function isnan(x: unknown): boolean;
declare function _equal(x: unknown, y: unknown): boolean;
declare function equal(...args: unknown[]): boolean;
declare const compareVersion: (a: string, op: "<" | "<=" | "==" | ">" | ">=", b: string) => boolean;
declare function updateLabel(labelTxt: string | undefined, labelNode: JQuery<HTMLElement>): void;
declare function getComputedLinkColor(el: HTMLElement): string;
declare function isBS3(): boolean;
declare function toLowerCase<T extends string>(str: T): Lowercase<T>;
export { escapeHTML, randomId, strToBool, getStyle, padZeros, roundSignif, parseDate, formatDateUTC, makeResizeFilter, pixelRatio, scopeExprToFunc, asArray, mergeSort, $escape, mapValues, isnan, _equal, equal, compareVersion, updateLabel, getComputedLinkColor, makeBlob, hasOwnProperty, isBS3, toLowerCase, };
