function hasOwnProperty(x: Record<string, unknown>, y: string): boolean {
  return Object.prototype.hasOwnProperty.call(x, y);
}

export { hasOwnProperty };
