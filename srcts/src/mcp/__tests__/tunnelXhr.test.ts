import assert from "node:assert";
import test from "node:test";
import type { ToolCaller } from "../tunnelSocket";
import { createTunnelXhrClass } from "../tunnelXhr";

type HttpCall = { [key: string]: unknown };

function makeEnv(
  respond: (args: HttpCall) => {
    status: number;
    headers?: { [key: string]: string };
    body?: string; // base64
  },
) {
  const calls: HttpCall[] = [];
  const caller: ToolCaller = async (name, args) => {
    assert.equal(name, "_shiny_http");
    calls.push(args);
    return respond(args) as unknown as { [key: string]: unknown };
  };
  const xhrClass = createTunnelXhrClass({
    callTool: caller,
    getConnectionId: async () => "abc",
    pageOrigin: "http://sandbox.example",
  });
  return { xhrClass, calls };
}

function once(xhr: { onload: (() => void) | null }): Promise<void> {
  return new Promise((resolve) => {
    xhr.onload = () => resolve();
  });
}

void test("GET tunnels relative URLs with query strings", async () => {
  const { xhrClass, calls } = makeEnv(() => ({
    status: 200,
    // eslint-disable-next-line @typescript-eslint/naming-convention
    headers: { "content-type": "text/plain", "X-Extra": "yes" },
    body: Buffer.from("hello").toString("base64"),
  }));
  const xhr = new xhrClass();
  const loaded = once(xhr);
  xhr.open("GET", "session/abc123/dataobj/tbl?w=&nonce=1");
  xhr.setRequestHeader("X-Requested-With", "XMLHttpRequest");
  xhr.send(null);
  await loaded;

  assert.equal(xhr.readyState, 4);
  assert.equal(xhr.status, 200);
  assert.equal(xhr.responseText, "hello");
  assert.equal(xhr.getResponseHeader("content-type"), "text/plain");
  assert.ok(xhr.getAllResponseHeaders().includes("x-extra: yes"));

  assert.equal(calls.length, 1);
  assert.equal(calls[0].method, "GET");
  assert.equal(calls[0].path, "/session/abc123/dataobj/tbl?w=&nonce=1");
  assert.equal(calls[0].connectionId, "abc");
});

void test("same-origin absolute URLs are tunneled with origin stripped", async () => {
  const { xhrClass, calls } = makeEnv(() => ({ status: 204 }));
  const xhr = new xhrClass();
  const loaded = once(xhr);
  xhr.open("GET", "http://sandbox.example/shared/shiny.min.css");
  xhr.send();
  await loaded;
  assert.equal(calls[0].path, "/shared/shiny.min.css");
  assert.equal(xhr.status, 204);
});

void test("POST tunnels string and binary bodies as base64", async () => {
  const { xhrClass, calls } = makeEnv(() => ({ status: 200 }));

  const xhr = new xhrClass();
  let loaded = once(xhr);
  xhr.open("POST", "session/x/upload/1");
  xhr.setRequestHeader("content-type", "text/plain");
  xhr.send("a=1&b=2");
  await loaded;
  assert.equal(
    Buffer.from(String(calls[0].body), "base64").toString(),
    "a=1&b=2",
  );
  const headers = calls[0].headers as { [key: string]: string };
  assert.equal(headers["content-type"], "text/plain");

  const xhr2 = new xhrClass();
  loaded = once(xhr2);
  xhr2.open("POST", "session/x/upload/2");
  xhr2.send(new Uint8Array([1, 2, 2, 2]).buffer);
  await loaded;
  assert.equal(calls[1].body, "AQICAg==");

  // Blob bodies (file uploads)
  const xhr3 = new xhrClass();
  loaded = once(xhr3);
  xhr3.open("POST", "session/x/upload/3");
  xhr3.send(new Blob([new Uint8Array([5, 6])]));
  await loaded;
  assert.equal(calls[2].body, Buffer.from([5, 6]).toString("base64"));
});

void test("arraybuffer responseType yields ArrayBuffer response", async () => {
  const { xhrClass } = makeEnv(() => ({
    status: 200,
    body: Buffer.from([9, 8, 7]).toString("base64"),
  }));
  const xhr = new xhrClass();
  const loaded = once(xhr);
  xhr.open("GET", "shared/thing.bin");
  xhr.responseType = "arraybuffer";
  xhr.send();
  await loaded;
  assert.deepEqual(
    Array.from(new Uint8Array(xhr.response as ArrayBuffer)),
    [9, 8, 7],
  );
});

void test("tool failure surfaces as onerror with status 0", async () => {
  const caller: ToolCaller = async () => {
    throw new Error("gone");
  };
  const xhrClass = createTunnelXhrClass({
    callTool: caller,
    getConnectionId: async () => "abc",
    pageOrigin: "http://sandbox.example",
  });
  const xhr = new xhrClass();
  const errored = new Promise<void>((resolve) => {
    xhr.onerror = () => resolve();
  });
  xhr.open("GET", "session/x/y");
  xhr.send();
  await errored;
  assert.equal(xhr.status, 0);
  assert.equal(xhr.readyState, 4);
});

void test("upload property exists for jQuery progress wiring", () => {
  const { xhrClass } = makeEnv(() => ({ status: 200 }));
  const xhr = new xhrClass();
  assert.ok(xhr.upload);
});
