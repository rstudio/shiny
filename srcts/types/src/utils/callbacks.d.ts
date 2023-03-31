type Cb = {
    once: boolean;
    fn: () => void;
};
type Cbs = {
    [key: string]: Cb;
};
declare class Callbacks {
    callbacks: Cbs;
    id: number;
    register(fn: () => void, once?: boolean): () => void;
    invoke(): void;
    clear(): void;
    count(): number;
}
export { Callbacks };
