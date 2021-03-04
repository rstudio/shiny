function hasOwnProperty(x: Record<any, any>, y: string): boolean {
  return Object.prototype.hasOwnProperty.call(x, y);
}

export { hasOwnProperty };
