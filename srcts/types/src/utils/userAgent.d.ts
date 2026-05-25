type UserAgent = typeof window.navigator.userAgent;
declare let userAgent: UserAgent;
declare function setUserAgent(userAgent_: UserAgent): void;
export { setUserAgent, userAgent };
export type { UserAgent };
