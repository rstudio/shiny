import type { UserAgent } from "../utils/userAgent";

function windowUserAgent(): UserAgent {
  return window.navigator.userAgent;
}

export { windowUserAgent };
