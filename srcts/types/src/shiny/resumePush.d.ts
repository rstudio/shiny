import type { InputBinding } from "../bindings";
type PushedInputs = {
    [id: string]: unknown;
};
type BoundInput = {
    binding: InputBinding;
    el: HTMLElement;
    dataType: unknown;
};
type PushDeps = {
    lookup: (id: string) => BoundInput | null;
    remember: (nameType: string, value: unknown) => void;
    resendAll: () => void;
    log: (message: string, error: unknown) => void;
};
declare function applyPushedInputs(values: PushedInputs, deps: PushDeps): Promise<void>;
declare function adaptPushedValue(bindingName: string, dataType: unknown, value: unknown): unknown;
export { adaptPushedValue, applyPushedInputs };
export type { BoundInput, PushDeps, PushedInputs };
