import { II_LEGACY_ORIGIN } from "$lib/legacy/constants";
import { isNullish, nonNullish } from "@dfinity/utils";
import { CredentialData } from "./credential-devices";

export type WebAuthnFlow = {
  useIframe: boolean;
  rpId: string | undefined;
};
type Parameters = {
  // Does the user support Related Origin Requests?
  // Two sources are checked: the user agent and whether the user uses a third party provider for passkweys.
  // Not used at the moment.
  supportsRor: boolean;
  devices: CredentialData[];
  currentOrigin: string;
  relatedOrigins: string[];
};

/**
 * Function that returns the ordered steps to try to perform the webauthn authentication.
 *
 * There are two dimensions in the steps:
 * - Use iframe for the webauthn authentication or not.
 * - Which RP ID to use. This is used for the iframe or for Related Origin Requests.
 *
 * Logic:
 * - To calculate the RP IDs, we look for all RP IDs within the devices
 * - At the moment, we only use non-iframe if the RP ID matches the current origin. to avoid bad UX, if the RP ID doesn't match the current origin, the iframe will be used.
 *
 * @param {Parameters} params - The parameters to find the webauthn steps.
 * @returns {WebAuthnFlow[]} The ordered steps to try to perform the webauthn authentication.
 */
export const findWebAuthnFlows = ({
  devices,
  currentOrigin,
  relatedOrigins,
}: Parameters): WebAuthnFlow[] => {
  const currentRpId = new URL(currentOrigin).hostname;
  const relatedRpIds = relatedOrigins.map(
    (relatedOrigin) => new URL(relatedOrigin).hostname,
  );

  // The devices are expected to be ordered by recently used already
  const orderedDeviceRpIds = [
    ...new Set(
      devices
        // Device origin to RP ID (hostname)
        .map((device) =>
          device.origin === currentOrigin ||
          (currentOrigin === II_LEGACY_ORIGIN && isNullish(device.origin))
            ? undefined
            : new URL(device.origin ?? II_LEGACY_ORIGIN).hostname,
        )
        // Filter out RP IDs that are not within `relatedRpIds`
        .filter((rpId) => isNullish(rpId) || relatedRpIds.includes(rpId)),
    ),
  ];

  // Create steps from `deviceRpIds`, currently that's one step per RP ID
  const steps: WebAuthnFlow[] = orderedDeviceRpIds.map((rpId) => ({
    rpId,
    useIframe: nonNullish(rpId) && rpId !== currentRpId,
  }));

  // If there are no steps, add a default step.
  if (steps.length === 0) {
    steps.push({ useIframe: false, rpId: undefined });
  }
  return steps;
};
