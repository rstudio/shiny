/// <reference types="jquery" />
import type { BoundsCss, BoundsType, BrushOptsType } from "./createBrush";
import type { OffsetType } from "./findbox";
import type { CoordmapType } from "./initCoordmap";
import type { PanelType } from "./initPanelScales";
declare type CreateHandlerType = {
    mousemove?: (e: JQuery.MouseMoveEvent) => void;
    mouseout?: (e: JQuery.MouseOutEvent) => void;
    mousedown?: (e: JQuery.MouseDownEvent) => void;
    onResetImg: () => void;
    onResize?: () => void;
};
declare type BrushInfo = {
    xmin: number;
    xmax: number;
    ymin: number;
    ymax: number;
    coords_css?: BoundsCss;
    coords_img?: BoundsType;
    x?: number;
    y?: number;
    img_css_ratio?: OffsetType;
    mapping?: PanelType["mapping"];
    domain?: PanelType["domain"];
    range?: PanelType["range"];
    log?: PanelType["log"];
    direction?: BrushOptsType["brushDirection"];
    brushId?: string;
    outputId?: string;
};
declare type InputIdType = Parameters<CoordmapType["mouseCoordinateSender"]>[0];
declare type ClipType = Parameters<CoordmapType["mouseCoordinateSender"]>[1];
declare type NullOutsideType = Parameters<CoordmapType["mouseCoordinateSender"]>[2];
declare function createClickHandler(inputId: InputIdType, clip: ClipType, coordmap: CoordmapType): CreateHandlerType;
declare function createHoverHandler(inputId: InputIdType, delay: number, delayType: "throttle" | string, clip: ClipType, nullOutside: NullOutsideType, coordmap: CoordmapType): CreateHandlerType;
declare function createBrushHandler(inputId: InputIdType, $el: JQuery<HTMLElement>, opts: BrushOptsType, coordmap: CoordmapType, outputId: BrushInfo["outputId"]): CreateHandlerType;
export { createClickHandler, createHoverHandler, createBrushHandler };
export type { BrushInfo };
