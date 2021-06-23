import { OutputBinding } from "./outputBinding";
declare class DownloadLinkOutputBinding extends OutputBinding {
    find(scope: HTMLElement): JQuery<HTMLElement>;
    renderValue(el: HTMLElement, data: string): void;
}
export { DownloadLinkOutputBinding };
