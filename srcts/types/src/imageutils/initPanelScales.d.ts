import type { Offset } from "./findbox";
declare type Panel = {
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
    scaleDataToImg?: (val: {
        [key: string]: number;
    }, clip?: boolean) => {
        [key: string]: number;
    };
    scaleImgToData?: {
        (val: Offset, clip?: boolean): Offset;
        (val: {
            [key: string]: number;
        }, clip?: boolean): {
            [key: string]: number;
        };
    };
    clipImg?: (offsetImg: {
        x: number;
        y: number;
    }) => {
        x: number;
        y: number;
    };
};
declare function initPanelScales(panels: Panel[]): void;
export type { Panel };
export { initPanelScales };
