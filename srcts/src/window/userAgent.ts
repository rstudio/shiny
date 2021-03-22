import { UserAgentType } from "../utils/userAgent";

function windowUserAgent(): UserAgentType {
  return window.navigator.userAgent;
}

export { windowUserAgent };
