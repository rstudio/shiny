import { OffsetType } from "./findbox";
declare type PanelType = {
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
    mapping: Record<string, string>;
    panel_vars?: Record<string, number | string>;
    scaleDataToImg?: (val: Record<string, number>, clip?: boolean) => Record<string, number>;
    scaleImgToData?: {
        (val: OffsetType, clip?: boolean): OffsetType;
        (val: Record<string, number>, clip?: boolean): Record<string, number>;
    };
    clipImg?: (offsetImg: {
        x: number;
        y: number;
    }) => {
        x: number;
        y: number;
    };
};
declare function initPanelScales(panels: Array<PanelType>): void;
export type { PanelType };
export { initPanelScales };
