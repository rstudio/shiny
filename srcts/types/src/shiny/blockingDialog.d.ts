type DialogButton = {
    label: string;
    style: "primary" | "link";
    choice: string;
    onClick: () => void;
};
type DialogSpec = {
    id: string;
    title: string;
    body: string;
    detail?: string | null;
    buttons: DialogButton[];
};
declare function showBlockingDialog(spec: DialogSpec): void;
declare function hideBlockingDialog(id: string): void;
export { hideBlockingDialog, showBlockingDialog };
export type { DialogButton, DialogSpec };
