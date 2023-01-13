import { randomId } from "../utils";
import type { HtmlDep } from "./render";
declare function show({ html, action, deps, duration, id, closeButton, type, }?: {
    html?: string;
    action?: string;
    deps?: HtmlDep[];
    duration?: number | null;
    id?: string | null;
    closeButton?: boolean;
    type?: string | null;
}): Promise<ReturnType<typeof randomId>>;
declare function remove(id: string): void;
export { show as showNotification, remove as removeNotification };
