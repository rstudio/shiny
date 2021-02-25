type UserAgentType = typeof window.navigator.userAgent;

let userAgentVal: UserAgentType;

function setUserAgent(userAgent: UserAgentType): void {
  userAgentVal = userAgent;
}

export type { UserAgentType };
export { userAgentVal as userAgent, setUserAgent };
