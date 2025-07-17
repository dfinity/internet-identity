/* Global configuration for II and constants */

// The official URL, without protocol
export const OFFICIAL_II_URL_NO_PROTOCOL = "identity.internetcomputer.org";

// The URL where the official, production II is served
export const OFFICIAL_II_URL = "https://" + OFFICIAL_II_URL_NO_PROTOCOL;

// The legacy production II URL
export const LEGACY_II_URL = "https://identity.ic0.app";

export const PORTAL_II_URL = "https://internetcomputer.org/internet-identity";

// Default support page URL
export const SUPPORT_URL = "https://identitysupport.dfinity.org";

// Default support page URL for when error is shown to user
export const ERROR_SUPPORT_URL = `${SUPPORT_URL}/hc/en-us/articles/32301362727188`;

// Default source code URL
export const SOURCE_CODE_URL = "https://github.com/dfinity/internet-identity";

// Internet Computer URL
export const INTERNET_COMPUTER_URL = "https://internetcomputer.org";

// Pin is disallowed by default unless this query parameter is set.
// This is used for testing purposes because we still support logging in with PIN but not registering with it.
export const ENABLE_PIN_QUERY_PARAM_KEY = "enablePin";
