/// <reference types="jquery" />
import type { BoundsCss, Bounds, BrushOpts } from "./createBrush";
import type { Offset } from "./findbox";
import type { Coordmap } from "./initCoordmap";
import type { Panel } from "./initPanelScales";
declare type CreateHandler = {
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
    coords_img?: Bounds;
    x?: number;
    y?: number;
    img_css_ratio?: Offset;
    mapping?: Panel["mapping"];
    domain?: Panel["domain"];
    range?: Panel["range"];
    log?: Panel["log"];
    direction?: BrushOpts["brushDirection"];
    brushId?: string;
    outputId?: string;
};
declare type InputId = Parameters<Coordmap["mouseCoordinateSender"]>[0];
declare type Clip = Parameters<Coordmap["mouseCoordinateSender"]>[1];
declare type NullOutside = Parameters<Coordmap["mouseCoordinateSender"]>[2];
declare function createClickHandler(inputId: InputId, clip: Clip, coordmap: Coordmap): CreateHandler;
declare function createHoverHandler(inputId: InputId, delay: number, delayType: string | "throttle", clip: Clip, nullOutside: NullOutside, coordmap: Coordmap): CreateHandler;
declare function createBrushHandler(inputId: InputId, $el: JQuery<HTMLElement>, opts: BrushOpts, coordmap: Coordmap, outputId: BrushInfo["outputId"]): CreateHandler;
export { createClickHandler, createHoverHandler, createBrushHandler };
export type { BrushInfo };
