import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { withBrowserProof } from "$lib/stores/browser-key.store";
import { asBrowserKeyError } from "$lib/stores/channelHandlers/sessionDelegation";
import { describeBrowser } from "$lib/utils/describeBrowser";
import { toPermissionsArg } from "$lib/utils/accessLevel";
import { throwCanisterError } from "$lib/utils/utils";

/** The canister's own floor, so this asks for the shortest session it will store. */
const TEN_MINUTES_NS = BigInt(10 * 60) * BigInt(1_000_000_000);

/**
 * Signs the identity in at `origin` to mint the application a consent hangs off, and
 * hands nobody the delegation it creates.
 *
 * Read-only and as short as the canister allows, since nothing holds it. A later real
 * sign-in replaces it at no cost, because consent rides the per-app config rather than
 * the session.
 *
 * Only for an origin the identity has never reached: called for one it has, it would
 * replace a session the app is still using.
 */
export const mintApplicationSession = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  // Generated and then dropped: the session exists to be recorded, not used.
  const identity = await ECDSAKeyIdentity.generate({ extractable: false });
  const sessionKey = new Uint8Array(identity.getPublicKey().toDer());
  const browserDescription = await describeBrowser();

  await withBrowserProof(
    identityNumber,
    sessionKey,
    browserDescription,
    (browser) =>
      actor
        .prepare_account_session({
          identity_number: identityNumber,
          origin,
          // The identity's default account at this origin.
          account_number: [],
          session_key: sessionKey,
          browser_description: browserDescription,
          current_browser_key: browser.publicKey,
          next_browser_key: browser.nextPublicKey,
          current_browser_key_signature: browser.signature,
          next_browser_key_signature: browser.nextSignature,
          permissions: toPermissionsArg("read-only"),
          valid_for: [TEN_MINUTES_NS],
          max_idle: [],
        })
        .then(throwCanisterError)
        .catch((error: unknown) => {
          throw asBrowserKeyError(error);
        }),
    (prepared) => prepared.browser_id,
  );
};
