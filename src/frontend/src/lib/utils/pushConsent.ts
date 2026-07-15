/**
 * Push-notification consent: the set of dApp origins the identity has
 * granted permission to send push notifications to, via `push_grant_consent`
 * during `/authorize`. Read via `push_list_consented_origins` and revoked via
 * `push_revoke_consent`, both authenticated as the identity so only the user
 * (never the dApp) can change what's granted. The Settings UI is the only
 * place this list is surfaced.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";

/** List the origins this identity has granted push-notification consent to. */
export const listConsentedOrigins = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<string[]> => actor.push_list_consented_origins(identityNumber);

/** Revoke a previously-granted consent for `origin`. */
export const revokeConsent = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  origin: string,
): Promise<void> => {
  await actor
    .push_revoke_consent(identityNumber, origin)
    .then(throwTextCanisterError);
};
