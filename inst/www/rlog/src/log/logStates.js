// @flow

let states = {
  asyncStart: "asyncStart",
  asyncStop: "asyncStop",
  define: "define",
  dependsOn: "dependsOn",
  dependsOnRemove: "dependsOnRemove",
  enter: "enter",
  exit: "exit",
  freeze: "freeze",
  invalidateEnd: "invalidateEnd",
  invalidateStart: "invalidateStart",
  isolateEnter: "isolateEnter",
  isolateExit: "isolateExit",
  isolateInvalidateEnd: "isolateInvalidateEnd",
  isolateInvalidateStart: "isolateInvalidateStart",
  mark: "markTime",
  queueEmpty: "queueEmpty",
  thaw: "thaw",
  updateNodeLabel: "updateNodeLabel",
  valueChange: "valueChange",
};

// type ActionsType = $Values<typeof states>;

type ReactIdType = string;
type CtxIdType = string;

// type EntryType = {
//   action: ActionsType,
//   session: ?string,
//   time: number,
//   step: number,
// };

// type TypeType =
//   | "observable"
//   | "observer"
//   | "reactiveVal"
//   | "reactiveValuesNames"
//   | "reactiveValuesAsList"
//   | "reactiveValuesAsListAll"
//   | "reactiveValuesKey";

// type EntryReactIdType = {
//   session: ?string,
//   time: number,
//   step: number,
//   reactId: ReactIdType,
// };
// type EntryContextType = {
//   session: ?string,
//   time: number,
//   step: number,
//   reactId: ReactIdType,
//   ctxId: CtxIdType,
// };

type LogEntryAsyncStartType = {
  action: "asyncStart",
  session: ?string,
  time: number,
  step: number,
};
type LogEntryAsyncStopType = {
  action: "asyncStop",
  session: ?string,
  time: number,
  step: number,
};

type LogEntryDefineType = {
  action: "define",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  type: string, // TypeType,
  label: string,
};

type LogEntryDependsOnType = {
  action: "dependsOn",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
  depOnReactId: ReactIdType,
};
type LogEntryDependsOnRemoveType = {
  action: "dependsOnRemove",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
  depOnReactId: ReactIdType,
};

type LogEntryEnterType = {
  action: "enter",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};
type LogEntryExitType = {
  action: "exit",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};

type LogEntryInvalidateStartType = {
  action: "invalidateStart",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
  type: string,
};
type LogEntryInvalidateEndType = {
  action: "invalidateEnd",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
  type: string,
};

type LogEntryIsolateEnterType = {
  action: "isolateEnter",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};
type LogEntryIsolateExitType = {
  action: "isolateExit",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};

type LogEntryIsolateInvalidateStartType = {
  action: "isolateInvalidateStart",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};
type LogEntryIsolateInvalidateEndType = {
  action: "isolateInvalidateEnd",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  ctxId: CtxIdType,
};

type LogEntryQueueEmptyType = {
  action: "queueEmpty",
  session: ?string,
  time: number,
  step: number,
};

type LogEntryUpdateNodeLabelType = {
  action: "updateNodeLabel",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  label: string,
};

type LogEntryValueChangeType = {
  action: "valueChange",
  session: ?string,
  time: number,
  step: number,
  reactId: ReactIdType,
  value: string,
};

type LogEntryAnyType =
  | LogEntryAsyncStartType
  | LogEntryAsyncStopType
  | LogEntryDefineType
  | LogEntryDependsOnType
  | LogEntryDependsOnRemoveType
  | LogEntryEnterType
  | LogEntryExitType
  | LogEntryInvalidateStartType
  | LogEntryInvalidateEndType
  | LogEntryIsolateEnterType
  | LogEntryIsolateExitType
  | LogEntryIsolateInvalidateStartType
  | LogEntryIsolateInvalidateEndType
  | LogEntryQueueEmptyType
  | LogEntryUpdateNodeLabelType
  | LogEntryValueChangeType;

type LogType = Array<LogEntryAnyType>;

export { states as LogStates };
export type {
  ReactIdType,
  CtxIdType,
  LogType,
  LogEntryAnyType,
  LogEntryAsyncStartType,
  LogEntryAsyncStopType,
  LogEntryDefineType,
  LogEntryDependsOnType,
  LogEntryDependsOnRemoveType,
  LogEntryEnterType,
  LogEntryExitType,
  LogEntryInvalidateStartType,
  LogEntryInvalidateEndType,
  LogEntryIsolateEnterType,
  LogEntryIsolateExitType,
  LogEntryIsolateInvalidateStartType,
  LogEntryIsolateInvalidateEndType,
  LogEntryQueueEmptyType,
  LogEntryUpdateNodeLabelType,
  LogEntryValueChangeType,
};
