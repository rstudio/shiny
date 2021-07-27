import type { Bounds } from "./createBrush";
declare type Offset = {
    x: number;
    y: number;
};
declare function findBox(offset1: Offset, offset2: Offset): Bounds;
export type { Offset };
export { findBox };
