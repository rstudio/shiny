declare type UserAgentType = typeof window.navigator.userAgent;
declare let userAgentVal: UserAgentType;
declare function setUserAgent(userAgent: UserAgentType): void;
export type { UserAgentType };
export { userAgentVal as userAgent, setUserAgent };
