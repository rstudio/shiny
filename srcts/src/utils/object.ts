function hasOwnProperty(x: { [key: string]: unknown }, y: string): boolean {
  return Object.prototype.hasOwnProperty.call(x, y);
}

export { hasOwnProperty };
