import { II_LEGACY_ORIGIN } from "$lib/legacy/constants";
import { isNullish, nonNullish } from "@dfinity/utils";
import { CredentialData } from "./credential-devices";
import { canisterConfig } from "$lib/globals";
import { isSameOrigin } from "./urlUtils";

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
  // Move devices registered on the new flow origins to the end using toSorted (preserving relative order within groups)
  const newFlowOrigins = canisterConfig.new_flow_origins[0] ?? [];
  const isInNewFlow = (credentialData: CredentialData): boolean => {
    const origin = credentialData.origin ?? II_LEGACY_ORIGIN;
    return newFlowOrigins.some((o) => isSameOrigin(o, origin));
  };
  const sortNewFlowOriginsToEnd = (
    a: CredentialData,
    b: CredentialData,
  ): number => {
    const aIn = isInNewFlow(a);
    const bIn = isInNewFlow(b);
    // Keep the order if both are in the new flow or both are not
    if (aIn === bIn) return 0;
    // Move the one that is in the new flow to the end
    return aIn ? 1 : -1;
  };

  const orderedDeviceRpIds: (string | undefined)[] = [...devices]
    .sort(sortNewFlowOriginsToEnd)
    // Device origin to RP ID (hostname)
    .map((device: CredentialData) =>
      device.origin === currentOrigin ||
      (currentOrigin === II_LEGACY_ORIGIN && isNullish(device.origin))
        ? undefined
        : new URL(device.origin ?? II_LEGACY_ORIGIN).hostname,
    )
    // Filter out RP IDs that are not within `relatedRpIds`
    .filter(
      (rpId: string | undefined) =>
        isNullish(rpId) || relatedRpIds.includes(rpId),
    )
    // Remove duplicates
    .reduce((rpIds: Array<string | undefined>, rpId: string | undefined) => {
      if (rpIds.includes(rpId)) {
        return rpIds;
      }
      return [...rpIds, rpId];
    }, []);

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
