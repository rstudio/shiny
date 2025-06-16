/* eslint-disable unicorn/filename-case */

document.documentElement.classList.add("autoreload-enabled");

const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
// Add trailing slash to path, if necessary, before appending "autoreload"
const defaultPath =
  window.location.pathname.replace(/\/?$/, "/") + "autoreload/";
const defaultUrl = `${protocol}//${window.location.host}${defaultPath}`;

// By default, use the defaultUrl. But if there's a data-ws-url attribute on our
// <script> tag, use that instead.
const wsUrl = document.currentScript?.dataset?.wsUrl || defaultUrl;

/**
 * Connects to an autoreload URL and waits for the server to tell us what to do.
 *
 * @param url The ws:// or wss:// URL to connect to.
 * @returns true if the server requests a reload, or false if the connection was
 * successfully established but then closed without the server requesting a
 * reload
 * @throws A nondescript error if the connection fails to be established.
 */
async function autoreload(url: string): Promise<boolean> {
  const ws = new WebSocket(url);

  let success = false;

  return new Promise((resolve, reject) => {
    ws.onopen = () => {
      success = true;
    };

    ws.onerror = (err) => {
      reject(err);
    };

    ws.onclose = () => {
      if (!success) {
        reject(new Error("WebSocket connection failed"));
      } else {
        resolve(false);
      }
    };

    ws.onmessage = function (event) {
      if (event.data === "autoreload") {
        resolve(true);
      }
    };
  });
}

async function sleep(ms: number) {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

async function initialize() {
  while (true) {
    try {
      if (await autoreload(wsUrl)) {
        window.location.reload();
        return;
      }
    } catch (err) {
      // It's possible for the autoreload() call to throw. If it does, that
      // means we tried but failed to connect to the autoreload socket. This
      // probably means that the entire `shiny run --reload` process was
      // restarted. As of today, the autoreload websocket port number is
      // randomly chosen for each `shiny run --reload` process, so it's
      // impossible for us to recover.
      console.debug("Giving up on autoreload");
      return;
    }
    // If we get here, the connection to the autoreload server was
    // successful but then got broken. Wait for a second, and then
    // try to re-establish the connection.
    await sleep(1000);
  }
}

initialize().catch((err) => {
  console.error(err);
});

export {};
