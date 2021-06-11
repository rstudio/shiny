import { ShinyApp } from "../shiny/shinyapp";
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
    shinyapp: any;
    id: string;
    el: HTMLElement;
    jobId: string;
    uploadUrl: string;
    progressBytes: number;
    totalBytes: number;
    constructor(shinyapp: ShinyApp, id: string, files: FileList, el: HTMLElement);
    makeRequest(method: any, args: any, onSuccess: any, onFailure: any, blobs: any): void;
    onBegin(files: FileList, cont: () => void): void;
    onFile(file: File, cont: () => void): void;
    onComplete(): void;
    onError(message: string): void;
    onAbort(): void;
    onProgress(file: File | null, completed: number): void;
    $container(): JQuery<HTMLElement>;
    $bar(): JQuery<HTMLElement>;
    $setVisible(visible: boolean): void;
    $setError(error: any | null): void;
    $setActive(active: boolean): void;
}
export { FileUploader };
