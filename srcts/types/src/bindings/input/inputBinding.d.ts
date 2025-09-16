import type { EventPriority } from "../../inputPolicies/inputPolicy";
import type { RatePolicyModes } from "../../inputPolicies/inputRateDecorator";
import type { BindScope } from "../../shiny/bind";
type SubscribeEventPriority = EventPriority | boolean | {
    priority: EventPriority;
};
type InputSubscribeCallback = (value: SubscribeEventPriority) => void;
declare class InputBinding {
    name: string;
    find(scope: BindScope): JQuery<HTMLElement>;
    getId(el: HTMLElement): string;
    getType(el: HTMLElement): string | null;
    getValue(el: HTMLElement): any;
    subscribe(el: HTMLElement, callback: InputSubscribeCallback): void;
    unsubscribe(el: HTMLElement): void;
    receiveMessage(el: HTMLElement, data: unknown): Promise<void> | void;
    getState(el: HTMLElement): unknown;
    getRatePolicy(el: HTMLElement): {
        policy: RatePolicyModes;
        delay: number;
    } | null;
    initialize(el: HTMLElement): void;
    dispose(el: HTMLElement): void;
}
export { InputBinding };
export type { InputSubscribeCallback, SubscribeEventPriority };
