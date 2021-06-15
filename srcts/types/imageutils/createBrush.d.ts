import type { CoordmapType } from "./initCoordmap";
import type { PanelType } from "./initPanelScales";
import type { OffsetType } from "./findbox";
declare type BoundsType = {
    xmin: number;
    xmax: number;
    ymin: number;
    ymax: number;
};
declare type BoundsCss = BoundsType;
declare type BoundsData = BoundsType;
declare type ImageState = {
    brushing?: boolean;
    dragging?: boolean;
    resizing?: boolean;
    down?: OffsetType;
    up?: OffsetType;
    resizeSides?: {
        left: boolean;
        right: boolean;
        top: boolean;
        bottom: boolean;
    };
    boundsCss?: BoundsCss;
    boundsData?: BoundsData;
    panel?: PanelType;
    changeStartBounds?: BoundsType;
};
declare type BrushOptsType = {
    brushDirection: "x" | "y" | "xy";
    brushClip: boolean;
    brushFill: string;
    brushOpacity: string;
    brushStroke: string;
    brushDelayType?: "throttle" | "debounce";
    brushDelay?: number;
    brushResetOnNew?: boolean;
};
declare type BrushType = {
    reset: () => void;
    importOldBrush: () => void;
    isInsideBrush: (offsetCss: OffsetType) => boolean;
    isInResizeArea: (offsetCss: OffsetType) => boolean;
    whichResizeSides: (offsetCss: OffsetType) => ImageState["resizeSides"];
    onResize: () => void;
    boundsCss: {
        (boxCss: BoundsCss): void;
        (): BoundsCss;
    };
    boundsData: {
        (boxData: BoundsData): void;
        (): BoundsData;
    };
    getPanel: () => ImageState["panel"];
    down: {
        (): ImageState["down"];
        (offsetCss: any): void;
    };
    up: {
        (): ImageState["up"];
        (offsetCss: any): void;
    };
    isBrushing: () => ImageState["brushing"];
    startBrushing: () => void;
    brushTo: (offsetCss: OffsetType) => void;
    stopBrushing: () => void;
    isDragging: () => ImageState["dragging"];
    startDragging: () => void;
    dragTo: (offsetCss: OffsetType) => void;
    stopDragging: () => void;
    isResizing: () => ImageState["resizing"];
    startResizing: () => void;
    resizeTo: (offsetCss: OffsetType) => void;
    stopResizing: () => void;
};
declare function createBrush($el: JQuery<HTMLElement>, opts: BrushOptsType, coordmap: CoordmapType, expandPixels: number): BrushType;
export { createBrush };
export type { BoundsType, BrushOptsType, BoundsCss };
