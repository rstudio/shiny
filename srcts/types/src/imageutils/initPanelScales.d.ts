import type { Bounds } from "./createBrush";
import type { Offset } from "./findbox";
type PanelInit = {
    domain: {
        top: number;
        bottom: number;
        left: number;
        right: number;
    };
    range: {
        top: number;
        bottom: number;
        left: number;
        right: number;
    };
    log?: {
        x?: number;
        y?: number;
    };
    mapping: {
        [key: string]: string;
    };
    panel_vars?: {
        [key: string]: number | string;
    };
};
type Panel = PanelInit & {
    scaleDataToImg: {
        (val: Bounds, clip?: boolean): Bounds;
    };
    scaleImgToData: {
        (val: Offset, clip?: boolean): Offset;
    };
    clipImg: (offsetImg: {
        x: number;
        y: number;
    }) => {
        x: number;
        y: number;
    };
};
declare function initPanelScales(panels: PanelInit[]): Panel[];
export type { Panel, PanelInit };
export { initPanelScales };
