// This centralized file is used to host all image types to avoid circular dependencies between modules

type BoundsType = {
  xmin: number;
  xmax: number;
  ymin: number;
  ymax: number;
};
type BoundsCss = BoundsType;
type BoundsData = BoundsType;

type ImageState = {
  brushing?: boolean;
  dragging?: boolean;
  resizing?: boolean;

  // Offset of last mouse down and up events (in CSS pixels)
  down?: OffsetType;
  up?: OffsetType;

  // Which side(s) we're currently resizing
  resizeSides?: {
    left: boolean;
    right: boolean;
    top: boolean;
    bottom: boolean;
  };

  boundsCss?: BoundsCss;
  boundsData?: BoundsData;

  // Panel object that the brush is in
  panel?: PanelType;

  // The bounds at the start of a drag/resize (in CSS pixels)
  changeStartBounds?: BoundsType;
};

type BrushOptsType = {
  brushDirection: "x" | "y" | "xy";
  brushClip: boolean;
  brushFill: string;
  brushOpacity: string;
  brushStroke: string;
  brushDelayType?: "throttle" | "debounce";
  brushDelay?: number;
  brushResetOnNew?: boolean;
};

type CreateHandlerType = {
  mousemove?: (e: JQuery.MouseMoveEvent) => void;
  mouseout?: (e: JQuery.MouseOutEvent) => void;
  mousedown?: (e: JQuery.MouseDownEvent) => void;
  onResetImg: () => void;
  onResize?: () => void;
};

type BrushInfo = {
  xmin: number;
  xmax: number;
  ymin: number;
  ymax: number;
  // eslint-disable-next-line camelcase
  coords_css?: BoundsCss;
  // eslint-disable-next-line camelcase
  coords_img?: BoundsType;
  x?: number;
  y?: number;
  // eslint-disable-next-line camelcase
  img_css_ratio?: OffsetType;
  mapping?: PanelType["mapping"];
  domain?: PanelType["domain"];
  range?: PanelType["range"];
  log?: PanelType["log"];
  direction?: BrushOptsType["brushDirection"];
  brushId?: string;
  outputId?: string;
};

type InputIdType = Parameters<CoordmapType["mouseCoordinateSender"]>[0];
type ClipType = Parameters<CoordmapType["mouseCoordinateSender"]>[1];
type NullOutsideType = Parameters<CoordmapType["mouseCoordinateSender"]>[2];

type OffsetType = {
  x: number;
  y: number;
};

type PanelType = {
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
  // eslint-disable-next-line camelcase
  panel_vars?: Record<string, number | string>;

  scaleDataToImg?: (
    val: Record<string, number>,
    clip?: boolean
  ) => Record<string, number>;
  scaleImgToData?: {
    (val: OffsetType, clip?: boolean): OffsetType;
    (val: Record<string, number>, clip?: boolean): Record<string, number>;
  };

  clipImg?: (offsetImg: { x: number; y: number }) => { x: number; y: number };
};

type OffsetCssType = Record<string, number>;
type OffsetImgType = Record<string, number>;

type Coords = {
  // eslint-disable-next-line camelcase
  coords_css: OffsetType;
  // eslint-disable-next-line camelcase
  coords_img: OffsetType;
  x?: number;
  y?: number;
  // eslint-disable-next-line camelcase
  img_css_ratio?: OffsetType;
  mapping?: PanelType["mapping"];
  domain?: PanelType["domain"];
  range?: PanelType["range"];
  log?: PanelType["log"];
};

type CoordmapType = {
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

  mouseCoordinateSender: (
    inputId: string,
    clip?: boolean,
    nullOutside?: boolean
  ) => (e: JQuery.MouseDownEvent) => void;
};

export type {
  BoundsType,
  BoundsCss,
  BoundsData,
  BrushOptsType,
  ImageState,
  CreateHandlerType,
  OffsetType,
  BrushInfo,
  InputIdType,
  ClipType,
  NullOutsideType,
  PanelType,
  OffsetCssType,
  OffsetImgType,
  Coords,
  CoordmapType,
};
