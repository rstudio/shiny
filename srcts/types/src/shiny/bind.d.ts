import type { InputBinding, OutputBinding } from "../bindings";
import type { BindingRegistry } from "../bindings/registry";
import type { InputRateDecorator, InputValidateDecorator } from "../inputPolicies";
type BindScope = HTMLElement | JQuery<HTMLElement>;
type BindInputsCtx = {
    inputs: InputValidateDecorator;
    inputsRate: InputRateDecorator;
    inputBindings: BindingRegistry<InputBinding>;
    outputBindings: BindingRegistry<OutputBinding>;
    sendOutputHiddenState: () => void;
    maybeAddThemeObserver: (el: HTMLElement) => void;
    initDeferredIframes: () => void;
    outputIsRecalculating: (id: string) => boolean;
};
declare function bindInputs(shinyCtx: BindInputsCtx, scope?: BindScope): {
    [key: string]: {
        value: ReturnType<InputBinding["getValue"]>;
        opts: {
            immediate: boolean;
            binding: InputBinding;
            el: HTMLElement;
        };
    };
};
declare function _bindAll(shinyCtx: BindInputsCtx, scope: BindScope): Promise<ReturnType<typeof bindInputs>>;
declare function unbindAll(shinyCtx: BindInputsCtx, scope: BindScope, includeSelf?: boolean): void;
declare function bindAll(shinyCtx: BindInputsCtx, scope: BindScope): Promise<void>;
export { _bindAll, bindAll, unbindAll };
export type { BindInputsCtx, BindScope };
