// @flow

import type { SomeGraphData } from "../graph/Graph";

type CytoOnEvent = (method: string, callback: (evt: CytoEvent) => any) => void;
// type CytoOnDblClick = (method: string, callback: (evt: CytoEvent, originalEvt: Event) => any) => void

type CytoData = {
  group: string,
  data: SomeGraphData,
};

type CytoscapeLRB = {
  left: CytoscapeElements,
  right: CytoscapeElements,
  both: CytoscapeElements,
};

type CytoscapeEdge = {
  id: () => string,
  // data: () => SomeGraphData,
  data: (info?: SomeGraphData) => any,
  flashClass: (className: string, timeout: number) => void,
  once: (string, (CytoEvent) => any) => any,
  trigger: (string, CytoEvent) => any,
  classes: string => CytoscapeEdge,
  style: Object => CytoscapeNode,
  removeStyle: () => CytoscapeNode,
};

// type SetEleData = (info: SomeGraphData) => CytoscapeElements;
// type GetEleData = () => CytoscapeElements;
type CytoscapeNode = {
  id: () => string,
  // data: (info: SomeGraphData) => CytoscapeElements,
  data: (info?: SomeGraphData) => any,
  flashClass: (className: string, timeout: number) => void,
  once: (string, (CytoEvent) => any) => any,
  trigger: (string, CytoEvent) => any,
  classes: string => CytoscapeNode,
  style: Object => CytoscapeNode,
  removeStyle: () => CytoscapeNode,
};
type CytoscapeElement = CytoscapeNode | CytoscapeEdge;
type CytoscapeElements = {
  $: (identifier?: string) => CytoscapeElements,
  $id: (id: string) => CytoscapeElement,
  length: number,
  data: (info?: SomeGraphData) => any,
  map: ((element: CytoscapeElement) => void) => void,
  diff: (other: CytoscapeElements) => CytoscapeLRB,
  sort: (
    (a: CytoscapeElement, b: CytoscapeElement) => number
  ) => CytoscapeElements,
};

type CytoscapeLayoutObject = {
  run: () => void,
};
type CytoscapeType = {
  $: (identifier?: string) => CytoscapeElements,
  $id: (id: string) => CytoscapeElement,
  add: (x: Array<CytoData> | CytoData | CytoscapeElement) => CytoscapeElement,
  on: CytoOnEvent, // | CytoOnDblClick
  startBatch: () => void,
  endBatch: () => void,
  nodes: () => CytoscapeElements,
  edges: () => CytoscapeElements,
  remove: (info: any) => CytoscapeType,
  animate: (animationInfo: { duration: number }) => CytoscapeType,
  layout: (layoutOptions: Object) => CytoscapeLayoutObject,
};

type CytoEvent = {
  target: CytoEventTarget,
};

type CytoEventTarget = EventTarget & {
  data: () => SomeGraphData,
  once: (string, (CytoEvent) => any) => any,
  trigger: (string, CytoEvent) => any,
};

type CytoscapeLibrary = {
  use: (lib: any) => void,
};

export type {
  CytoscapeType,
  CytoEvent,
  CytoscapeNode,
  CytoscapeEdge,
  CytoData,
  CytoscapeElement,
  CytoscapeLibrary,
};
