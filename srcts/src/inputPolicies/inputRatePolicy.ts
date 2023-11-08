import type { AnyVoidFunction } from "../utils/extraTypes";
import type { InputPolicy } from "./inputPolicy";

interface InputRatePolicy<X extends AnyVoidFunction> {
  target: InputPolicy | null;
  func: X;

  normalCall(...args: Parameters<X>): void;
  immediateCall(...args: Parameters<X>): void;
}

export type { InputRatePolicy };
