/// <reference types="jquery" />
import type { Offset } from "./findbox";
import type { Bounds } from "./createBrush";
import type { Panel } from "./initPanelScales";
declare function findOrigin($el: JQuery<HTMLElement>): Offset;
declare type OffsetCss = Record<string, number>;
declare type OffsetImg = Record<string, number>;
declare type CoordmapInit = {
    panels: Array<Panel>;
    dims: {
        height: number;
        width: number;
    };
};
declare type Coordmap = {
    panels: Array<Panel>;
    dims: {
        height: number;
        width: number;
    };
    mouseOffsetCss: (evt: JQuery.MouseEventBase) => Offset;
    scaleCssToImg: {
        (offsetCss: Bounds): Bounds;
        (offsetCss: Offset): Offset;
        (offsetCss: OffsetCss): OffsetImg;
    };
    scaleImgToCss: {
        (offsetImg: Bounds): Bounds;
        (offsetImg: Offset): Offset;
        (offsetImg: OffsetImg): OffsetCss;
    };
    imgToCssScalingRatio: () => Offset;
    cssToImgScalingRatio: () => Offset;
    getPanelCss: (offsetCss: OffsetCss, expand?: number) => Panel;
    isInPanelCss: (offsetCss: OffsetCss, expand?: number) => boolean;
    mouseCoordinateSender: (inputId: string, clip?: boolean, nullOutside?: boolean) => (e: JQuery.MouseDownEvent) => void;
};
declare function initCoordmap($el: JQuery<HTMLElement>, coordmap_: CoordmapInit): Coordmap;
export type { Coordmap, CoordmapInit };
export { initCoordmap, findOrigin };
