import { randomId } from "../utils";
declare function show({ html, action, deps, duration, id, closeButton, type, }?: {
    html?: string;
    action?: string;
    deps?: any[];
    duration?: number;
    id?: any;
    closeButton?: boolean;
    type?: any;
}): ReturnType<typeof randomId>;
declare function remove(id?: string): void;
export { show as showNotification, remove as removeNotification };
