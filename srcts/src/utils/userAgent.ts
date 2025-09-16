type UserAgent = typeof window.navigator.userAgent;

let userAgent: UserAgent;

function setUserAgent(userAgent_: UserAgent): void {
  userAgent = userAgent_;
}

export { setUserAgent, userAgent };
export type { UserAgent };
