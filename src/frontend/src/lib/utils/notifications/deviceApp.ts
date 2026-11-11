import type { ClaimNotificationAppError } from "$lib/generated/internet_identity_types";
import {
  registeredIdentityNumbers,
  signNotificationAppClaim,
} from "$lib/stores/browser-key.store";
import {
  isCanisterError,
  throwCanisterError,
  type CanisterError,
} from "$lib/utils/utils";
import { browserKeyActor } from "./browserActor";

/** Every arm the variant carries, so an arm added to it is a compile error here: `CanisterError` is constructed with no message. */
const refusal = (error: CanisterError<ClaimNotificationAppError>): string => {
  switch (error.type) {
    case "InvalidBrowserKey":
    case "InvalidClaimToken":
    case "ClaimTokenExpired":
    case "NoNotificationApp":
      return error.type;
    case "InternalCanisterError":
      return `${error.type}: ${error.value("InternalCanisterError")}`;
  }
};

/** Why a claim did not happen: for a refusal the variant the canister sent, otherwise what the browser or the transport said. */
const describe = (error: unknown): string => {
  if (isCanisterError<ClaimNotificationAppError>(error)) {
    return refusal(error);
  }
  return error instanceof Error ? error.message : String(error);
};

/** Deliberately well under the 30 minutes `CLAIM_TOKEN_LIFETIME_NS` in `main.rs` allows:
 *  the expiry is read off this device's clock and checked against the canister's, so
 *  asking for the whole window would refuse every phone whose clock runs even slightly
 *  fast. A claim is one call, so the rest of the window is headroom, not patience. */
export const CLAIM_TOKEN_LIFETIME_MS = 5 * 60 * 1000;

/** The token names both anchors and an expiry, so it is good for this pair alone and only for as long as the claim takes. */
const claimFrom = async (
  identityNumber: bigint,
  offeringIdentityNumber: bigint,
): Promise<boolean> => {
  const expiresAtNs =
    BigInt(Date.now() + CLAIM_TOKEN_LIFETIME_MS) * BigInt(1_000_000);
  try {
    const signature = await signNotificationAppClaim({
      offeringIdentityNumber,
      claimingIdentityNumber: identityNumber,
      expiresAtNs,
    });
    if (signature === undefined) {
      // No sign-in has placed this identity's key in an entry, so it has nothing to hand over.
      console.warn(
        `anchor ${offeringIdentityNumber} is not registered as a browser here`,
      );
      return false;
    }
    const actor = await browserKeyActor(identityNumber);
    await actor
      .claim_notification_app({
        anchor_number: identityNumber,
        offering_anchor_number: offeringIdentityNumber,
        expires_at_ns: expiresAtNs,
        signature,
      })
      .then(throwCanisterError);
    return true;
  } catch (error) {
    console.warn(
      `the app of anchor ${offeringIdentityNumber} was not claimed: ${describe(error)}`,
    );
    return false;
  }
};

/** `no-donor` is an answer about this device: nobody here has an app to hand over, so the
 *  install is what is left. `indeterminate` is the absence of one, and offering the
 *  install on it sends a user who already has the app through a second install. */
export type ClaimOutcome = "claimed" | "no-donor" | "indeterminate";

/** Whether another identity signed in here has an app a claim could take over, which is
 *  the same list the claim walks without writing anything.
 *
 *  `undefined` where the store would not open: nothing was asked, so nothing was learned,
 *  and that is not an answer that this device has no app. */
export const deviceHasDonorIdentity = async (
  identityNumber: bigint,
): Promise<boolean | undefined> => {
  try {
    return (await registeredIdentityNumbers()).some(
      (other) => other !== identityNumber,
    );
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

/** One call per candidate and no more: asking the same one again would get the same answer. */
export const claimDeviceApp = async (
  identityNumber: bigint,
): Promise<ClaimOutcome> => {
  let offering: bigint[];
  try {
    offering = (await registeredIdentityNumbers()).filter(
      (other) => other !== identityNumber,
    );
  } catch (error) {
    // Nothing was asked, so nothing was learned: a store that would not open says
    // nothing about whether this device has an app.
    console.error(error);
    return "indeterminate";
  }
  if (offering.length === 0) {
    console.warn("no other identity is signed in here to hand an app over");
    return "no-donor";
  }
  for (const offeringIdentityNumber of offering) {
    if (await claimFrom(identityNumber, offeringIdentityNumber)) {
      return "claimed";
    }
  }
  return "no-donor";
};
