import type { NotUndefined } from "./extraTypes";

// Inspriation from https://fettblog.eu/typescript-hasownproperty/
// But mixing with "NonNullable key of Obj" instead of "key to unknown values"
function hasOwnProperty<Prop extends keyof X, X extends { [key: string]: any }>(
  obj: X,
  prop: Prop,
): obj is X & { [key in NonNullable<Prop>]: X[key] } {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

// Return true if the key exists on the object and the value is not undefined.
//
// This method is mainly used in input bindings' `receiveMessage` method.
// Since we know that the values are sent by Shiny via `{jsonlite}`,
// then we know that there are no `undefined` values. `null` is possible, but not `undefined`.
function hasDefinedProperty<
  Prop extends keyof X,
  X extends { [key: string]: any },
>(
  obj: X,
  prop: Prop,
): obj is X & { [key in NonNullable<Prop>]: NotUndefined<X[key]> } {
  return (
    Object.prototype.hasOwnProperty.call(obj, prop) && obj[prop] !== undefined
  );
}

// Return type for non-null value
function ifUndefined<X extends NotUndefined<any>, Y>(
  value: X,
  alternate: Y,
): NotUndefined<X>;
// Return type for null value
function ifUndefined<X extends undefined, Y>(value: X, alternate: Y): Y;
// Logic
function ifUndefined<X, Y>(value: X, alternate: Y): X | Y {
  if (value === undefined) return alternate;
  return value;
}

export { hasDefinedProperty, hasOwnProperty, ifUndefined };
