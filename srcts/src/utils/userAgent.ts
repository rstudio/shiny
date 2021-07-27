type UserAgent = typeof window.navigator.userAgent;

let userAgent: UserAgent;

function setUserAgent(userAgent_: UserAgent): void {
  userAgent = userAgent_;
}

export type { UserAgent };
export { userAgent, setUserAgent };
