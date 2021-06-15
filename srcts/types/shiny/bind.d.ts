import type { InputBinding, OutputBinding } from "../bindings";
import type { BindingRegistry } from "../bindings/registry";
import type { InputRateDecorator, InputValidateDecorator } from "../inputPolicies";
declare type bindScope = HTMLElement | JQuery<HTMLElement>;
declare type bindInputsCtx = {
    inputs: InputValidateDecorator;
    inputsRate: InputRateDecorator;
    inputBindings: BindingRegistry<InputBinding>;
    outputBindings: BindingRegistry<OutputBinding>;
    sendOutputHiddenState: () => void;
    maybeAddThemeObserver: (el: HTMLElement) => void;
    initDeferredIframes: () => void;
};
declare function bindInputs(shinyCtx: bindInputsCtx, scope?: bindScope): Record<string, {
    value: unknown;
    opts: {
        immediate: boolean;
        binding: InputBinding;
        el: HTMLElement;
    };
}>;
declare function _bindAll(shinyCtx: bindInputsCtx, scope: bindScope): ReturnType<typeof bindInputs>;
declare function unbindAll(shinyCtx: bindInputsCtx, scope: bindScope, includeSelf?: boolean): void;
declare function bindAll(shinyCtx: bindInputsCtx, scope: bindScope): void;
export { unbindAll, bindAll, _bindAll };
export type { bindScope, bindInputsCtx };
