declare function show({ html, deps }?: {
    html?: string;
    deps?: any[];
}): void;
declare function remove(): void;
export { show as showModal, remove as removeModal };
