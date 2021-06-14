import { OutputBindingAdapter } from "../bindings/output_adapter";
import type { UploadInitValue, UploadEndValue } from "../file/FileProcessor";
declare type ResponseValue = UploadInitValue | UploadEndValue;
declare type HandlerType = (msg: Record<string, unknown> | Array<unknown> | boolean | string) => void;
declare type ShinyWebSocket = WebSocket & {
    allowReconnect?: boolean;
};
declare type errorsMessageValue = {
    message: string;
    call: Array<string>;
    type?: Array<string>;
};
declare type OnSuccessRequest = (value: ResponseValue) => void;
declare type OnErrorRequest = (err: string) => void;
declare type InputValuesType = Record<string, unknown>;
declare function addCustomMessageHandler(type: string, handler: HandlerType): void;
declare class ShinyApp {
    $socket: ShinyWebSocket;
    config: {
        workerId: string;
        sessionId: string;
    };
    $inputValues: InputValuesType;
    $initialInput: InputValuesType;
    $bindings: Record<string, OutputBindingAdapter>;
    $values: {};
    $errors: Record<string, errorsMessageValue>;
    $conditionals: {};
    $pendingMessages: Array<string>;
    $activeRequests: Record<number, {
        onSuccess: OnSuccessRequest;
        onError: OnErrorRequest;
    }>;
    $nextRequestId: number;
    $allowReconnect: boolean | "force";
    constructor();
    connect(initialInput: InputValuesType): void;
    isConnected(): boolean;
    private scheduledReconnect;
    reconnect(): void;
    createSocket(): ShinyWebSocket;
    sendInput(values: InputValuesType): void;
    $notifyDisconnected(): void;
    $removeSocket(): void;
    $scheduleReconnect(delay: Parameters<typeof setTimeout>[1]): void;
    reconnectDelay: {
        next: () => number;
        reset: () => void;
    };
    onDisconnected(): void;
    onConnected(): void;
    makeRequest(method: string, args: Array<unknown>, onSuccess: OnSuccessRequest, onError: OnErrorRequest, blobs: Array<Blob | ArrayBuffer | string>): void;
    $sendMsg(msg: string): void;
    receiveError(name: string, error: errorsMessageValue): void;
    receiveOutput<T>(name: string, value: T): T;
    bindOutput(id: string, binding: OutputBindingAdapter): OutputBindingAdapter;
    unbindOutput(id: string, binding: OutputBindingAdapter): boolean;
    private narrowScopeComponent;
    private narrowScope;
    $updateConditionals(): void;
    private addMessageHandler;
    dispatchMessage(data: string | ArrayBufferLike): void;
    _sendMessagesToHandlers(msgObj: Record<string, unknown>, handlers: Record<string, HandlerType>, handlerOrder: Array<string>): void;
    private init;
    progressHandlers: {
        binding: (message: {
            id: string;
        }) => void;
        open: (message: {
            style: "notification" | "old";
            id: string;
        }) => void;
        update: (message: {
            style: "notification" | "old";
            id: string;
            message?: string;
            detail?: string;
            value?: number;
        }) => void;
        close: (message: {
            style: "notification";
            id: string;
        }) => void;
    };
    getTestSnapshotBaseUrl({ fullUrl }?: {
        fullUrl?: boolean;
    }): string;
}
export { ShinyApp, addCustomMessageHandler };
export type { HandlerType, errorsMessageValue };
