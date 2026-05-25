import type { Bounds } from "./createBrush";
import type { Offset } from "./findbox";
import type { Panel, PanelInit } from "./initPanelScales";
declare function findOrigin($el: JQuery<HTMLElement>): Offset;
type OffsetCss = {
    [key: string]: number;
};
type OffsetImg = {
    [key: string]: number;
};
type CoordmapInit = {
    panels: PanelInit[];
    dims: {
        height: number;
        width: number;
    } | {
        height: null;
        width: null;
    };
};
type Coordmap = {
    panels: Panel[];
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
        (offsetImg: {
            [key: string]: number;
        }): {
            [key: string]: number | null;
        };
    };
    imgToCssScalingRatio: () => Offset;
    cssToImgScalingRatio: () => Offset;
    getPanelCss: (offsetCss: OffsetCss, expand?: number) => Panel | null;
    isInPanelCss: (offsetCss: OffsetCss, expand?: number) => boolean;
    mouseCoordinateSender: (inputId: string, clip?: boolean, nullOutside?: boolean) => (e: JQuery.MouseDownEvent | JQuery.MouseMoveEvent | null) => void;
};
declare function initCoordmap($el: JQuery<HTMLElement>, coordmap_: CoordmapInit): Coordmap;
export { findOrigin, initCoordmap };
export type { Coordmap, CoordmapInit };
