import type { InputBinding } from "../bindings";
type JSONSchema = {
    [key: string]: unknown;
};
declare function checkValue(schema: JSONSchema, value: unknown, path?: string): void;
declare function valueSchema(el: HTMLElement, binding: InputBinding): JSONSchema | null;
export { checkValue, valueSchema };
export type { JSONSchema };
