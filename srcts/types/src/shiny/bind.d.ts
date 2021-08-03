import type { InputBinding, OutputBinding } from "../bindings";
import type { BindingRegistry } from "../bindings/registry";
import type { InputRateDecorator, InputValidateDecorator } from "../inputPolicies";
declare type BindScope = HTMLElement | JQuery<HTMLElement>;
declare type BindInputsCtx = {
    inputs: InputValidateDecorator;
    inputsRate: InputRateDecorator;
    inputBindings: BindingRegistry<InputBinding>;
    outputBindings: BindingRegistry<OutputBinding>;
    sendOutputHiddenState: () => void;
    maybeAddThemeObserver: (el: HTMLElement) => void;
    initDeferredIframes: () => void;
};
declare function bindInputs(shinyCtx: BindInputsCtx, scope?: BindScope): {
    [key: string]: {
        value: unknown;
        opts: {
            immediate: boolean;
            binding: InputBinding;
            el: HTMLElement;
        };
    };
};
declare function _bindAll(shinyCtx: BindInputsCtx, scope: BindScope): ReturnType<typeof bindInputs>;
declare function unbindAll(shinyCtx: BindInputsCtx, scope: BindScope, includeSelf?: boolean): void;
declare function bindAll(shinyCtx: BindInputsCtx, scope: BindScope): void;
export { unbindAll, bindAll, _bindAll };
export type { BindScope, BindInputsCtx };
