const browsers = ["chrome", "firefox", "safari", "edge"] as const;
// Infer the type from the array
type Browsers = (typeof browsers)[number];

type BrowserVersions = {
  [K in Browsers]?: number;
};

/**
 * Returns whether the user agent string is from a browser that meets the required version.
 *
 * This will be helpful to check whether a functionality is supported in the current user agent.
 * E.g. Webauthn with Related Origin Requests.
 *
 * @param userAgent The user agent string to check.
 * @param versions The required versions for each browser.
 * @returns {boolean}
 */
export const checkRequiredBrowserVersion = (
  userAgent: string,
  versions: BrowserVersions,
): boolean => {
  const browserMatches: Record<Browsers, RegExp> = {
    chrome: /Chrome\/(\d+)/,
    firefox: /Firefox\/(\d+)/,
    safari: /Version\/(\d+).*Safari/,
    edge: /Edg\/(\d+)/,
  };

  for (const browser of browsers) {
    const requiredVersion = versions[browser];
    const browserRegex = browserMatches[browser];
    const userAgentMatch = userAgent.match(browserRegex);
    if (userAgentMatch !== null) {
      const version = Number(userAgentMatch[1]);
      if (requiredVersion !== undefined && version >= requiredVersion) {
        // We found the browser and the version is greater than or equal to the required version
        return true;
      }
    }
  }

  return false;
};

/**
 * Checks if the user agent string supports Webauthn with Related Origin Requests.
 *
 * Safari >=18: https://developer.apple.com/documentation/safari-release-notes/safari-18-release-notes
 * Firefox: Not supported yet
 * Chrome >=128: https://web.dev/articles/webauthn-related-origin-requests#browser_support
 * Edge >=128: https://passkeys.dev/device-support/#ror
 *
 * @param userAgent
 * @returns {boolean}
 */
export const supportsWebauthRoR = (userAgent: string): boolean =>
  checkRequiredBrowserVersion(userAgent, {
    chrome: 128,
    edge: 128,
    safari: 18,
  });
