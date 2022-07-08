declare function show({ html, deps }?: {
    html?: string | undefined;
    deps?: never[] | undefined;
}): Promise<void>;
declare function remove(): void;
export { show as showModal, remove as removeModal };
