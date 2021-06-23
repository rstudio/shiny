//esbuild.github.io/content-types/#direct-eval
//tl/dr;
// * Direct usage of `eval("x")` is bad with bundled code.
// * Instead, use indirect calls to `eval` such as `indirectEval("x")`
//   * Even just renaming the function works well enough.
// > This is known as "indirect eval" because eval is not being called directly, and so does not trigger the grammatical special case for direct eval in the JavaScript VM. You can call indirect eval using any syntax at all except for an expression of the exact form eval('x'). For example, var eval2 = eval; eval2('x') and [eval][0]('x') and window.eval('x') are all indirect eval calls.
// > When you use indirect eval, the code is evaluated in the global scope instead of in the inline scope of the caller.

/**
 * Evaluates JavaScript code and executes it via _Indirect Evaluation_
 * @param y A String value that contains valid JavaScript code.
 *
 * From [esbuild](esbuild.github.io/content-types/#direct-eval): Direct usage of `eval("x")` is bad with bundled code. Instead, use indirect calls to `eval` such as `indirectEval("x")`. Even just renaming the function works well enough.
 *
 * > This is known as "indirect eval" because eval is not being called directly, and so does not trigger the grammatical special case for direct eval in the JavaScript VM. You can call indirect eval using any syntax at all except for an expression of the exact form `eval('x')`. For example, `var eval2 = eval; eval2('x')` and `[eval][0]('x')` and window.`eval('x')` are all indirect eval calls.
 * > When you use indirect eval, the code is evaluated in the global scope instead of in the inline scope of the caller.
 *
 */
const indirectEval = eval;

export { indirectEval };
