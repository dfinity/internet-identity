import { II_LEGACY_ORIGIN } from "$src/constants";
import { CredentialData } from "./credential-devices";

export const hasCredentialsFromMultipleOrigins = (
  credentials: CredentialData[]
): boolean =>
  new Set(credentials.map(({ origin }) => origin ?? II_LEGACY_ORIGIN)).size > 1;

/**
 * Filters out credentials from specific origins.
 *
 * This function takes a list of credentials and removes any that match the provided origins.
 * If a credential has no origin (undefined), it is treated as if it had the `DEFAULT_DOMAIN`.
 * Two origins match if they have the same hostname (domain).
 *
 * @param credentials - List of credential devices to filter
 * @param rpIds - Set of origins to exclude (undefined values are treated as `currentOrigin`)
 * @param currentOrigin - The current origin to use when comparing against undefined origins
 * @returns Filtered list of credentials, excluding those from the specified origins
 */
export const excludeCredentialsFromOrigins = (
  credentials: CredentialData[],
  rpIds: Set<string | undefined>,
  currentOrigin: string
): CredentialData[] => {
  if (rpIds.size === 0) {
    return credentials;
  }
  // Change `undefined` to the current origin.
  const originsToExclude = Array.from(rpIds).map((origin) =>
    origin === undefined ? currentOrigin : `https://${origin}`
  );
  return credentials.filter(
    (credential) =>
      originsToExclude.filter((originToExclude) =>
        sameDomain(credential.origin ?? II_LEGACY_ORIGIN, originToExclude)
      ).length === 0
  );
};

const sameDomain = (url1: string, url2: string): boolean =>
  new URL(url1).hostname === new URL(url2).hostname;

const hostname = (url: string): string => new URL(url).hostname;

const getFirstHostname = (devices: CredentialData[]): string => {
  if (devices[0] === undefined) {
    throw new Error("Not possible. Call this function only if devices exist.");
  }
  return hostname(devices[0].origin ?? II_LEGACY_ORIGIN);
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
  devices.filter((d) => sameDomain(d.origin ?? II_LEGACY_ORIGIN, domain));

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
 * @param relatedDomains - Optional list of domains in order or preference to use as the RP ID.
 * @returns {string | undefined}
 * `string` The RP ID (as hostname without schema) to use for WebAuthn registration.
 * `undefined` when the RP ID is the same as the current domain and is not needed.
 * `undefined` when there are no devices registered for any of the preferred domains.
 * `undefined` when there are no devices.
 * `undefined` when the devices domain is not a valid URL.
 *
 * `undefined` means to continue with the default flow.
 * In case of inconsistent state, let the default flow be.
 */
export const findWebAuthnRpId = (
  currentUrl: string,
  devices: CredentialData[],
  relatedDomains: string[]
): string | undefined => {
  // If there are no related domains, RP ID should not be set.
  if (relatedDomains.length === 0) {
    return undefined;
  }
  if (devices.length === 0) {
    return undefined;
  }

  try {
    // Try current domain first if devices exist
    if (getDevicesForDomain(devices, currentUrl).length > 0) {
      return undefined;
    }

    // Check based on the order of preferred domains if there is no device with the current domain.
    for (const domain of relatedDomains) {
      const devicesForDomain = getDevicesForDomain(devices, domain);
      if (devicesForDomain.length > 0) {
        return getFirstHostname(devicesForDomain);
      }
    }
  } catch (err: unknown) {
    // This could happen if the devices domain is not a valid URL.
    // In that case, let the default flow be.
    console.error(err);
    return undefined;
  }

  return undefined;
};
