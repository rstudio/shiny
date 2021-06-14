/// <reference types="jquery" />
import { InputBinding } from "./InputBinding";
declare class FileInputBinding extends InputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    getId(el: HTMLInputElement): string;
    getValue(el: HTMLElement): {
        name?: string;
    } | null;
    setValue(el: HTMLElement, value: void): void;
    getType(el: HTMLElement): string;
    _zoneOf(el: HTMLElement | JQuery<HTMLElement>): JQuery<HTMLElement>;
    _enableDraghover(el: JQuery<HTMLElement>): JQuery<HTMLElement>;
    _disableDraghover(el: JQuery<HTMLElement>): JQuery<HTMLElement>;
    _enableDocumentEvents(): void;
    _disableDocumentEvents(): void;
    _canSetFiles(fileList: FileList): boolean;
    _handleDrop(e: JQuery.DragEventBase, el: HTMLInputElement): void;
    subscribe(el: HTMLInputElement, callback: (x: boolean) => void): void;
    unsubscribe(el: HTMLElement): void;
}
export { FileInputBinding };
