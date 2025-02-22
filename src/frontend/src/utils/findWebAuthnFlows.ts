import { CredentialData } from "./credential-devices";
import {
  excludeCredentialsFromOrigins,
  findWebAuthnRpId,
} from "./findWebAuthnRpId";

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
 * - To calculate the RP ID, we use the `findWebAuthnRpId` function.
 *   - Calculate the RP ID first with all the credentials.
 *   - For the subsequent RP IDs, the credentials' origin that matches the previous RP ID will be excluded.
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
  const steps: WebAuthnFlow[] = [];
  let filteredCredentials = [...devices];
  const rpIds = new Set<string | undefined>();

  while (filteredCredentials.length > 0) {
    const rpId = findWebAuthnRpId(
      currentOrigin,
      filteredCredentials,
      relatedOrigins
    );

    // EXCEPTION: At the moment, to avoid bad UX, if the RP ID doesn't match the current origin, the iframe will be used.
    // This is because it's hard to find out whether a user's credentials come from a third party password manager or not.
    // The iframe workaround works for all users.
    const useIframe =
      rpId !== undefined && rpId !== new URL(currentOrigin).hostname;

    const isRepeatedStep = steps.some(
      (step) => step.rpId === rpId && step.useIframe === useIframe
    );
    // Exit when the flow is the same to avoid infinite loops.
    if (isRepeatedStep) {
      break;
    }

    steps.push({ useIframe, rpId });
    rpIds.add(rpId);
    filteredCredentials = excludeCredentialsFromOrigins(
      filteredCredentials,
      rpIds,
      currentOrigin
    );
  }

  // One that doesn't use any new domain.
  const defaultStep = { useIframe: false, rpId: undefined };
  // If there are no steps, add a default step.
  if (steps.length === 0) {
    steps.push(defaultStep);
  }

  return steps;
};
