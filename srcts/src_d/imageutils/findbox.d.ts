import type { BoundsType } from "./createBrush";
declare type OffsetType = {
    x: number;
    y: number;
};
declare function findBox(offset1: OffsetType, offset2: OffsetType): BoundsType;
export type { OffsetType };
export { findBox };
