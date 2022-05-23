type Cb = {
  once: boolean;
  fn: () => void;
};

type Cbs = {
  [key: string]: Cb;
};

class Callbacks {
  callbacks: Cbs = {};
  id = 0;

  register(fn: () => void, once = true): () => void {
    this.id += 1;
    const id = this.id;

    this.callbacks[id] = { fn, once };
    return () => {
      delete this.callbacks[id];
    };
  }

  invoke(): void {
    for (const id in this.callbacks) {
      const cb = this.callbacks[id];

      try {
        cb.fn();
      } finally {
        if (cb.once) delete this.callbacks[id];
      }
    }
  }

  count(): number {
    return Object.keys(this.callbacks).length;
  }
}

export { Callbacks };
