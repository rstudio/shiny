import type { OutputBindingAdapter } from "../bindings/outputAdapter";
import type { UploadInitValue, UploadEndValue } from "../file/fileProcessor";
declare type ResponseValue = UploadEndValue | UploadInitValue;
declare type Handler = (message: any) => void;
declare type ShinyWebSocket = WebSocket & {
    allowReconnect?: boolean;
};
declare type ErrorsMessageValue = {
    message: string;
    call: string[];
    type?: string[];
};
declare type OnSuccessRequest = (value: ResponseValue) => void;
declare type OnErrorRequest = (err: string) => void;
declare type InputValues = {
    [key: string]: unknown;
};
declare type MessageValue = Parameters<WebSocket["send"]>[0];
declare function addCustomMessageHandler(type: string, handler: Handler): void;
declare class ShinyApp {
    $socket: ShinyWebSocket | null;
    private $socketInMessageQueue;
    $postDispatchMessageFnQueue: Array<() => void>;
    isInDispatchMessage: boolean;
    config: {
        workerId: string;
        sessionId: string;
    } | null;
    $inputValues: InputValues;
    $initialInput: InputValues | null;
    $bindings: {
        [key: string]: OutputBindingAdapter;
    };
    $values: {
        [key: string]: any;
    };
    $errors: {
        [key: string]: ErrorsMessageValue;
    };
    $conditionals: {};
    $pendingMessages: MessageValue[];
    $activeRequests: {
        [key: number]: {
            onSuccess: OnSuccessRequest;
            onError: OnErrorRequest;
        };
    };
    $nextRequestId: number;
    $allowReconnect: boolean | "force";
    constructor();
    connect(initialInput: InputValues): void;
    isConnected(): boolean;
    private scheduledReconnect;
    reconnect(): void;
    createSocket(): ShinyWebSocket;
    startInMessageQueueLoop(): Promise<void>;
    sendInput(values: InputValues): void;
    $notifyDisconnected(): void;
    $removeSocket(): void;
    $scheduleReconnect(delay: Parameters<typeof setTimeout>[1]): void;
    reconnectDelay: {
        next: () => number;
        reset: () => void;
    };
    onDisconnected(): void;
    onConnected(): void;
    makeRequest(method: string, args: unknown[], onSuccess: OnSuccessRequest, onError: OnErrorRequest, blobs: Array<ArrayBuffer | Blob | string> | undefined): void;
    $sendMsg(msg: MessageValue): void;
    receiveError(name: string, error: ErrorsMessageValue): void;
    receiveOutput<T>(name: string, value: T): T | undefined;
    bindOutput(id: string, binding: OutputBindingAdapter): OutputBindingAdapter;
    unbindOutput(id: string, binding: OutputBindingAdapter): boolean;
    private _narrowScopeComponent;
    private _narrowScope;
    $updateConditionals(): void;
    dispatchMessage(data: ArrayBufferLike | string): Promise<void>;
    private _sendMessagesToHandlers;
    private _init;
    progressHandlers: {
        binding: (this: ShinyApp, message: {
            id: string;
        }) => void;
        open: (message: {
            style: "notification" | "old";
            id: string;
        }) => Promise<void>;
        update: (message: {
            style: "notification" | "old";
            id: string;
            message?: string | undefined;
            detail?: string | undefined;
            value?: number | undefined;
        }) => void;
        close: (message: {
            style: "notification";
            id: string;
        }) => void;
    };
    getTestSnapshotBaseUrl({ fullUrl }?: {
        fullUrl?: boolean | undefined;
    }): string;
}
export { ShinyApp, addCustomMessageHandler };
export type { Handler, ErrorsMessageValue };
