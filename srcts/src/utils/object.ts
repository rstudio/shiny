// Inspriation from https://fettblog.eu/typescript-hasownproperty/
// But mixing with "NonNullable key of Obj" instead of "key to unknown values"
function hasOwnProperty<Prop extends keyof X, X extends { [key: string]: any }>(
  obj: X,
  prop: Prop
): obj is X & { [key in NonNullable<Prop>]: X[key] } {
  return Object.prototype.hasOwnProperty.call(obj, prop);
}

export { hasOwnProperty };
