/// <reference types="jquery" />
import type { OffsetType } from "./findbox";
import type { BoundsType } from "./createBrush";
import type { PanelType } from "./initPanelScales";
declare function findOrigin($el: JQuery<HTMLElement>): OffsetType;
declare type OffsetCssType = Record<string, number>;
declare type OffsetImgType = Record<string, number>;
declare type CoordmapInitType = {
    panels: Array<PanelType>;
    dims: {
        height: number;
        width: number;
    };
};
declare type CoordmapType = {
    panels: Array<PanelType>;
    dims: {
        height: number;
        width: number;
    };
    mouseOffsetCss: (evt: JQuery.MouseEventBase) => OffsetType;
    scaleCssToImg: {
        (offsetCss: BoundsType): BoundsType;
        (offsetCss: OffsetType): OffsetType;
        (offsetCss: OffsetCssType): OffsetImgType;
    };
    scaleImgToCss: {
        (offsetImg: BoundsType): BoundsType;
        (offsetImg: OffsetType): OffsetType;
        (offsetImg: OffsetImgType): OffsetCssType;
    };
    imgToCssScalingRatio: () => OffsetType;
    cssToImgScalingRatio: () => OffsetType;
    getPanelCss: (offsetCss: OffsetCssType, expand?: number) => PanelType;
    isInPanelCss: (offsetCss: OffsetCssType, expand?: number) => boolean;
    mouseCoordinateSender: (inputId: string, clip?: boolean, nullOutside?: boolean) => (e: JQuery.MouseDownEvent) => void;
};
declare function initCoordmap($el: JQuery<HTMLElement>, coordmap_: CoordmapInitType): CoordmapType;
export type { CoordmapType, CoordmapInitType };
export { initCoordmap, findOrigin };
