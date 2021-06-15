interface BindingInterface {
    name: string;
}
interface BindingObjType<BindingType> {
    binding: BindingType;
    priority: number;
    name?: string;
}
declare class BindingRegistry<BindingType extends BindingInterface> {
    bindings: Array<BindingObjType<BindingType>>;
    bindingNames: Record<string, BindingObjType<BindingType>>;
    register(binding: BindingType, bindingName: string, priority?: number): void;
    setPriority(bindingName: string, priority: number): void;
    getPriority(bindingName: string): number | false;
    getBindings(): Array<BindingObjType<BindingType>>;
}
export { BindingRegistry };
