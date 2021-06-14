import { RatePolicyModes } from "../../inputPolicies/inputRateDecorator";
import { bindScope } from "../../shiny/bind";
declare class InputBinding {
    name: string;
    find(scope: bindScope): JQuery<HTMLElement>;
    getId(el: HTMLElement): string;
    getType(el: HTMLElement): string | false;
    getValue(el: HTMLElement): unknown;
    subscribe(el: HTMLElement, callback: (value: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
    receiveMessage(el: HTMLElement, data: unknown): void;
    getState(el: HTMLElement): unknown;
    getRatePolicy(el: HTMLElement): {
        policy: RatePolicyModes;
        delay: number;
    } | null;
    initialize(el: HTMLElement): void;
    dispose(el: HTMLElement): void;
}
export { InputBinding };
