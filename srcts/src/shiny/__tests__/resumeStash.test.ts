import assert from "node:assert/strict";
import test from "node:test";

import {
  CrashLoopTracker,
  decideOnLoad,
  initResumeOnLoad,
  nextStash,
  readStash,
  removeStash,
  startFresh,
  takeLoadDecision,
  takeResumeNotice,
  writeStash,
  type ResumeStash,
} from "../resumeStash";

function memoryStorage(): Storage {
  const items = new Map<string, string>();
  return {
    getItem: (key: string) => items.get(key) ?? null,
    setItem: (key: string, value: string) => {
      items.set(key, value);
    },
    removeItem: (key: string) => {
      items.delete(key);
    },
  } as unknown as Storage;
}

function throwingStorage(): Storage {
  const fail = (): never => {
    throw new Error("SecurityError");
  };
  return {
    getItem: fail,
    setItem: fail,
    removeItem: fail,
  } as unknown as Storage;
}

// initResumeOnLoad() reads the global `window`, which Node lacks.
function withWindow(
  stub: { href: string; storage?: Storage },
  run: () => void,
): void {
  Object.defineProperty(globalThis, "window", {
    configurable: true,
    value: {
      location: { href: stub.href, pathname: new URL(stub.href).pathname },
      addEventListener: () => undefined,
      sessionStorage: stub.storage ?? memoryStorage(),
    },
  });
  try {
    run();
  } finally {
    Reflect.deleteProperty(globalThis, "window");
  }
}

const app = "http://h/app/";
const stash: ResumeStash = {
  token: "a".repeat(32),
  url: app,
  reload: "ask",
  serverInitiated: false,
  failures: 0,
};

void test("no stash, or a URL that differs, loads fresh", () => {
  assert.deepEqual(decideOnLoad(null, app), { kind: "fresh", discard: null });
  assert.deepEqual(decideOnLoad(stash, app + "?other=1"), {
    kind: "fresh",
    discard: "url",
  });
});

void test("the reload setting decides: ask asks, resume resumes, fresh starts over", () => {
  assert.deepEqual(decideOnLoad(stash, app), {
    kind: "ask",
    token: stash.token,
  });
  assert.deepEqual(decideOnLoad({ ...stash, reload: "resume" }, app), {
    kind: "resume",
    token: stash.token,
  });
  assert.deepEqual(decideOnLoad({ ...stash, reload: "fresh" }, app), {
    kind: "fresh",
    discard: "reload-fresh",
  });
});

void test("a reload Shiny made resumes without asking, whatever the setting", () => {
  for (const reload of ["ask", "resume", "fresh"] as const) {
    assert.deepEqual(
      decideOnLoad({ ...stash, reload, serverInitiated: true }, app),
      { kind: "resume", token: stash.token },
    );
  }
});

void test("a stash whose resumes keep dying is discarded", () => {
  assert.deepEqual(
    decideOnLoad({ ...stash, failures: 2, serverInitiated: true }, app),
    { kind: "fresh", discard: "crash-loop" },
  );
  assert.equal(decideOnLoad({ ...stash, failures: 1 }, app).kind, "ask");
});

void test("nextStash writes only with a token, carries failures over, and clears the server-initiated flag", () => {
  assert.equal(nextStash(stash, null, "ask", app), null);
  assert.deepEqual(
    nextStash(
      { ...stash, failures: 1, serverInitiated: true },
      "b".repeat(32),
      "resume",
      app + "?q",
    ),
    {
      token: "b".repeat(32),
      url: app + "?q",
      reload: "resume",
      serverInitiated: false,
      failures: 1,
    },
  );
  assert.deepEqual(nextStash(null, "b".repeat(32), "fresh", app), {
    token: "b".repeat(32),
    url: app,
    reload: "fresh",
    serverInitiated: false,
    failures: 0,
  });
});

void test("the stash round-trips through storage and tolerates junk", () => {
  const storage = memoryStorage();
  writeStash(storage, "/app/", stash);
  assert.deepEqual(readStash(storage, "/app/"), stash);
  assert.equal(readStash(storage, "/other/"), null);
  storage.setItem("shiny-resume:/app/", "{not json");
  assert.equal(readStash(storage, "/app/"), null);
  storage.setItem(
    "shiny-resume:/app/",
    JSON.stringify({ token: stash.token, url: app, reload: "sideways" }),
  );
  assert.deepEqual(readStash(storage, "/app/"), { ...stash, reload: "ask" }); // unknown mode reads as the default
  removeStash(storage, "/app/");
  assert.equal(readStash(storage, "/app/"), null);
  assert.equal(readStash(throwingStorage(), "/app/"), null);
});

void test("startFresh discards and reloads once, when the server answers or when it does not", () => {
  const log: string[] = [];
  let timer: (() => void) | null = null;
  let answer: (() => void) | null = null;
  const deps = {
    request: (done: () => void) => {
      answer = done;
    },
    discardStash: () => log.push("discard"),
    reload: () => log.push("reload"),
    setTimer: (f: () => void) => {
      timer = f;
    },
  };
  startFresh(deps);
  answer!();
  timer!();
  assert.deepEqual(log, ["discard", "reload"]);
  log.length = 0;
  startFresh({
    ...deps,
    request: () => {
      throw new Error("socket gone");
    },
  });
  assert.deepEqual(log, ["discard", "reload"]);
});

void test("a close within ten seconds of a fresh resume is a failure; outliving them is survival", () => {
  let now = 0;
  const t = new CrashLoopTracker(() => now);
  assert.equal(t.closed(), false);
  t.resumed();
  now = 5000;
  assert.equal(t.closed(), true);
  t.resumed();
  now = 16000;
  assert.equal(t.survived(), true);
  assert.equal(t.closed(), false);
});

void test("initResumeOnLoad hands out the decision once, clears the server-initiated flag, and leaves a crash-loop notice", () => {
  const storage = memoryStorage();
  writeStash(storage, "/app/", { ...stash, serverInitiated: true });
  withWindow({ href: app, storage }, () => {
    initResumeOnLoad();
    assert.deepEqual(takeLoadDecision(), {
      kind: "resume",
      token: stash.token,
    });
    assert.equal(takeLoadDecision(), null);
    assert.equal(readStash(storage, "/app/")!.serverInitiated, false);
  });
  const looping = memoryStorage();
  writeStash(looping, "/app/", { ...stash, failures: 2 });
  withWindow({ href: app, storage: looping }, () => {
    initResumeOnLoad();
    assert.equal(takeLoadDecision()!.kind, "fresh");
    assert.equal(readStash(looping, "/app/"), null);
    assert.match(takeResumeNotice() ?? "", /loaded fresh/);
  });
  withWindow({ href: app, storage: throwingStorage() }, () => {
    initResumeOnLoad();
    assert.equal(takeLoadDecision()!.kind, "fresh");
  });
});
