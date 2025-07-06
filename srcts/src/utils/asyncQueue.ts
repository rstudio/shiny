// Adapted from https://stackoverflow.com/a/47157945/412655

export class AsyncQueue<T> {
  private $promises: Array<Promise<T>> = [];
  private $resolvers: Array<(x: T) => void> = [];

  private _add() {
    const p: Promise<T> = new Promise((resolve) => {
      this.$resolvers.push(resolve);
    });

    this.$promises.push(p);
  }

  enqueue(x: T): void {
    if (!this.$resolvers.length) this._add();

    const resolve = this.$resolvers.shift()!;

    resolve(x);
  }

  async dequeue(): Promise<T> {
    if (!this.$promises.length) this._add();

    const promise = this.$promises.shift()!;

    return promise;
  }

  isEmpty(): boolean {
    return !this.$promises.length;
  }

  isBlocked(): boolean {
    return !!this.$resolvers.length;
  }

  get length(): number {
    return this.$promises.length - this.$resolvers.length;
  }
}
