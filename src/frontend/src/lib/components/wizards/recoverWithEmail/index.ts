export {
  default as RecoverWithEmailWizard,
} from "./RecoverWithEmailWizard.svelte";

import type { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { SignedDelegation } from "$lib/generated/internet_identity_types";

/**
 * The wizard hands this back via `onSignedIn` once the canister
 * has issued a `SignedDelegation` for the user's session keypair.
 * The host page builds a `DelegationIdentity` from the
 * `sessionIdentity` + delegation and proceeds with the rest of the
 * sign-in.
 */
export interface RecoverySuccess {
  sessionIdentity: ECDSAKeyIdentity;
  userKey: Uint8Array | number[];
  delegation: SignedDelegation;
}
