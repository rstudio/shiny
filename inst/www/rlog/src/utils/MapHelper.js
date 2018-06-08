// @flow

function mapValues<T>(x: Map<string, T>): Array<T> {
  return Array.from(x.values());
}
function mapKeys<T>(x: Map<string, T>): Array<string> {
  return Array.from(x.keys());
}

export { mapValues, mapKeys };
