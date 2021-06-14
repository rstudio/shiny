import { ShinyApp } from "../shiny/shinyapp";
declare type JobId = string;
declare type UploadUrl = string;
declare type UploadInitValue = {
    jobId: JobId;
    uploadUrl: UploadUrl;
};
declare type UploadEndValue = never;
declare class FileProcessor {
    files: FileList;
    fileIndex: number;
    aborted: boolean;
    completed: boolean;
    constructor(files: FileList, exec$run?: boolean);
    onBegin(files: FileList, cont: () => void): void;
    onFile(file: File, cont: () => void): void;
    onComplete(): void;
    onAbort(): void;
    abort(): void;
    $getRun(): () => void;
    $run(): void;
}
declare class FileUploader extends FileProcessor {
    shinyapp: ShinyApp;
    id: string;
    el: HTMLElement;
    jobId: JobId;
    uploadUrl: UploadUrl;
    progressBytes: number;
    totalBytes: number;
    constructor(shinyapp: ShinyApp, id: string, files: FileList, el: HTMLElement);
    makeRequest(method: "uploadInit", args: Array<Array<{
        name: string;
        size: number;
        type: string;
    }>>, onSuccess: (value: UploadInitValue) => void, onFailure: Parameters<ShinyApp["makeRequest"]>[3], blobs: Parameters<ShinyApp["makeRequest"]>[4]): void;
    makeRequest(method: "uploadEnd", args: [string, string], onSuccess: (value: unknown) => void, onFailure: Parameters<ShinyApp["makeRequest"]>[3], blobs: Parameters<ShinyApp["makeRequest"]>[4]): void;
    onBegin(files: FileList, cont: () => void): void;
    onFile(file: File, cont: () => void): void;
    onComplete(): void;
    onError(message: string): void;
    onAbort(): void;
    onProgress(file: File | null, completed: number): void;
    $container(): JQuery<HTMLElement>;
    $bar(): JQuery<HTMLElement>;
    $setVisible(visible: boolean): void;
    $setError(error: string | null): void;
    $setActive(active: boolean): void;
}
export { FileUploader };
export type { UploadInitValue, UploadEndValue };
