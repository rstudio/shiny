import type { JQueryEventHandlerBase } from "bootstrap";
import "jquery";
type EvtPrefix<T extends string> = `${T}.${string}`;
type EvtFn<T extends JQuery.Event> = ((evt: T) => void) | null | undefined;
declare global {
    interface JQuery {
        on(events: EvtPrefix<"change">, handler: EvtFn<JQuery.DragEvent>): this;
        on(events: EvtPrefix<"mousdown">, handler: EvtFn<JQuery.MouseDownEvent>): this;
        on(events: EvtPrefix<"dblclick">, handler: EvtFn<JQuery.DoubleClickEvent>): this;
        on(events: EvtPrefix<"dblclick2">, handler: EvtFn<JQuery.MouseDownEvent>): this;
        on(events: EvtPrefix<"mousemove">, handler: EvtFn<JQuery.MouseMoveEvent>): this;
        on(events: EvtPrefix<"mouseout">, handler: EvtFn<JQuery.MouseOutEvent>): this;
        on(events: EvtPrefix<"mousedown">, handler: EvtFn<JQuery.MouseDownEvent>): this;
        on(events: EvtPrefix<"mousedown2">, handler: EvtFn<JQuery.MouseDownEvent>): this;
        on(events: EvtPrefix<"mouseup">, handler: EvtFn<JQuery.MouseUpEvent>): this;
        on(events: EvtPrefix<"resize">, handler: EvtFn<JQuery.ResizeEvent>): this;
        on(events: `shown.bs.${string}.sendImageSize`, selector: string, handler: (this: HTMLElement, e: JQueryEventHandlerBase<HTMLElement, any>) => void): this;
    }
}
export {};
