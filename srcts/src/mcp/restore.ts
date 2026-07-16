/**
 * Encode MCP tool-call arguments into the bookmark restore format consumed by
 * Shiny's RestoreContext on the R side (`.clientdata_mcp_restore`).
 *
 * Example: `{n: 200, note: "hi"}` -> `"_inputs_&n=200&note=%22hi%22"`
 */
export function encodeMcpRestore(args: Record<string, unknown>): string {
  return (
    "_inputs_&" +
    Object.entries(args)
      .map(
        ([k, v]) =>
          `${encodeURIComponent(k)}=${encodeURIComponent(JSON.stringify(v))}`,
      )
      .join("&")
  );
}
