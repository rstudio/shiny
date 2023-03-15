import type { Coordmap } from "./initCoordmap";
import type { Panel } from "./initPanelScales";
import type { Offset } from "./findbox";
type Bounds = {
    xmin: number;
    xmax: number;
    ymin: number;
    ymax: number;
};
type BoundsCss = Bounds;
type BoundsData = Bounds;
type ImageState = {
    brushing: boolean;
    dragging: boolean;
    resizing: boolean;
    down: Offset;
    up: Offset;
    resizeSides: {
        left: boolean;
        right: boolean;
        top: boolean;
        bottom: boolean;
    };
    boundsCss: BoundsCss;
    boundsData: BoundsData;
    panel: Panel | null;
    changeStartBounds: Bounds;
};
type BrushOpts = {
    brushDirection: "x" | "xy" | "y";
    brushClip: boolean;
    brushFill: string;
    brushOpacity: string;
    brushStroke: string;
    brushDelayType?: "debounce" | "throttle";
    brushDelay?: number;
    brushResetOnNew?: boolean;
};
type Brush = {
    reset: () => void;
    hasOldBrush: () => boolean;
    importOldBrush: () => void;
    isInsideBrush: (offsetCss: Offset) => boolean;
    isInResizeArea: (offsetCss: Offset) => boolean;
    whichResizeSides: (offsetCss: Offset) => ImageState["resizeSides"];
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
        (offsetCss: Offset): void;
    };
    up: {
        (): ImageState["up"];
        (offsetCss: Offset): void;
    };
    isBrushing: () => ImageState["brushing"];
    startBrushing: () => void;
    brushTo: (offsetCss: Offset) => void;
    stopBrushing: () => void;
    isDragging: () => ImageState["dragging"];
    startDragging: () => void;
    dragTo: (offsetCss: Offset) => void;
    stopDragging: () => void;
    isResizing: () => ImageState["resizing"];
    startResizing: () => void;
    resizeTo: (offsetCss: Offset) => void;
    stopResizing: () => void;
};
declare function createBrush($el: JQuery<HTMLElement>, opts: BrushOpts, coordmap: Coordmap, expandPixels: number): Brush;
export { createBrush };
export type { Bounds, BrushOpts, BoundsCss };
