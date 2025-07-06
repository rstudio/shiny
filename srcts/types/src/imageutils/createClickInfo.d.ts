declare function createClickInfo($el: JQuery<HTMLElement>, dblclickId: string, dblclickDelay: number): {
    mousedown: (e: JQuery.MouseDownEvent) => void;
    dblclickIE8: (e: JQuery.DoubleClickEvent) => void;
};
export { createClickInfo };
