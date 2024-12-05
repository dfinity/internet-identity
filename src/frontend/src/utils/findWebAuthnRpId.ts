import { DeviceData } from "$generated/internet_identity_types";

const defaultDomain = "https://identity.ic0.app";

const getTopAndSecondaryLevelDomain = (url: string): string => {
  const parts = new URL(url).hostname.split(".").reverse();

  if (parts.length < 2) {
    throw new Error("Invalid URL: Unable to extract domain.");
  }

  return `${parts[1]}.${parts[0]}`;
};

// Helper to count devices for a domain, defaulting to "ic0.app" if origin is empty
// The domain must be a top and secondary level domain.
const getDevicesForDomain = (
  devices: DeviceData[],
  domain: string
): DeviceData[] =>
  devices.filter((d) => {
    if (d.origin.length === 0)
      return domain === getTopAndSecondaryLevelDomain(defaultDomain);
    return d.origin.some((o) => getTopAndSecondaryLevelDomain(o) === domain);
  });

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
 * @param currentUrl {string} The current URL of the page.
 * @param devices {DeviceData[]} The list of devices registered for the user.
 * @param preferredDomains {string[]} The list of domains in order or preference to use as the RP ID.
 * @returns {string | undefined} The RP ID to use for WebAuthn registration.
 * `undefined` when the RP ID is the same as the current domain and is not needed.
 * @throws {Error} If devices are not registered for any of the preferred domains, no devices exist or the current domain is invalid.
 */
export const findWebAuthnRpId = (
  currentUrl: string,
  devices: DeviceData[],
  preferredDomains: string[] = ["ic0.app", "internetcomputer.org", "icp0.io"]
): string | undefined => {
  try {
    const currentDomain = getTopAndSecondaryLevelDomain(currentUrl);

    if (devices.length === 0) {
      throw new Error(
        "Not possible. Every registered user has at least one device."
      );
    }

    const getFirstDomain = (devices: DeviceData[]): string => {
      if (devices[0] === undefined) {
        throw new Error(
          "Not possible. Call this function only if devices exist."
        );
      }
      return devices[0].origin[0] ?? defaultDomain;
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
  } catch (error) {
    console.error(error);
    throw error;
  }
};
