import { CredentialData } from "./credential-devices";

const DEFAULT_DOMAIN = "https://identity.ic0.app";

/**
 * Helper to extract the top and secondary level domain from a URL.
 *
 * Example: "https://identity.ic0.app" -> "ic0.app"
 *
 * @param url {string} The URL to extract the domain from.
 * @returns {string} The top and secondary level domain.
 *
 * @throws {Error} If the URL is invalid.
 * @throws {Error} If the URL does not contain a top and secondary level domain.
 */
const getTopAndSecondaryLevelDomain = (url: string): string => {
  const parts = new URL(url).hostname.split(".");

  if (parts.length < 2) {
    throw new Error("Invalid URL: Unable to extract domain.");
  }

  return parts.slice(-2).join(".");
};

/**
 * Helper to count devices for a domain, defaulting to "ic0.app" if origin is empty
 *
 * @param devices - The list of devices registered for the user.
 * @param domain - The domain to check for devices. It must be a top and secondary level domain e.g. "ic0.app"
 * We need this to support the beta domains with the same functions: beta.identity.ic0.app, beta.identity.internetcomputer.org
 * @returns {DeviceData[]} The list of devices registered for the domain.
 */
const getDevicesForDomain = (
  devices: CredentialData[],
  domain: string
): CredentialData[] =>
  devices.filter(
    (d) => getTopAndSecondaryLevelDomain(d.origin ?? DEFAULT_DOMAIN) === domain
  );

/**
 * Returns the domain to use as the RP ID for WebAuthn registration.
 *
 * The algorithm is as follows:
 * 1. If there is a device registered for the current domain, return undefined.
 * 2. If there is no device registered for the current domain, check if there is a device registered for one of the preferred domains.
 *    If there is, return the first preferred domain that has a device registered.
 * 3. If there is no device registered for the current domain and none of the preferred domains.
 *    Raise an error because the devices should be registered in one of the preferred domains.
 *
 * @param currentUrl - The current URL of the page.
 * @param devices - The list of devices registered for the user.
 * @param preferredDomains - Optional list of domains in order or preference to use as the RP ID.
 * @returns {string | undefined} The RP ID to use for WebAuthn registration.
 * `undefined` when the RP ID is the same as the current domain and is not needed.
 *
 * @throws {Error} If devices are not registered for any of the preferred domains.
 * @throws {Error} If no devices exist or the current domain is invalid.
 * @throws {Error} If the current domain is invalid.
 */
export const findWebAuthnRpId = (
  currentUrl: string,
  devices: CredentialData[],
  preferredDomains: string[] = ["ic0.app", "internetcomputer.org", "icp0.io"]
): string | undefined => {
  const currentDomain = getTopAndSecondaryLevelDomain(currentUrl);

  if (devices.length === 0) {
    throw new Error(
      "Not possible. Every registered user has at least one device."
    );
  }

  const getFirstDomain = (devices: CredentialData[]): string => {
    if (devices[0] === undefined) {
      throw new Error(
        "Not possible. Call this function only if devices exist."
      );
    }
    return devices[0].origin ?? DEFAULT_DOMAIN;
  };

  // Try current domain first if devices exist
  if (getDevicesForDomain(devices, currentDomain).length > 0) {
    return undefined;
  }

  // Check based on the order of preferred domains if there is no device with the current domain.
  for (const domain of preferredDomains) {
    const devicesForDomain = getDevicesForDomain(devices, domain);
    if (devicesForDomain.length > 0) {
      return getFirstDomain(devicesForDomain);
    }
  }

  throw new Error(
    "Not possible. Devices must be registered for at least one of the following domains: ic0.app, internetcomputer.org, icp0.io"
  );
};
