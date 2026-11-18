/**
 * Links an app's installed web app on this iPhone to this Safari, so the app's
 * notifications arrive once: in the installed app.
 *
 * The installed app registered a one-shot key on its own browser entry and opened this
 * page with the key in the fragment. Safari proves it holds the key by signing a claim
 * with it, bound to the identity and to the browser key this Safari signs the call with.
 */
import { z } from "zod";
import type { ClaimSameDeviceLinkError } from "$lib/generated/internet_identity_types";
import {
  browserKeyIdentity,
  registeredIdentityNumbers,
} from "$lib/stores/browser-key.store";
import { browserKeyActor } from "$lib/utils/notifications/browserActor";
import {
  importLinkKey,
  LINK_KEY_BYTES,
} from "$lib/utils/notifications/linkKey";
import {
  fromBase64URL,
  isCanisterError,
  throwCanisterError,
} from "$lib/utils/utils";

/** Must match `SAME_DEVICE_LINK_SIGNATURE_DOMAIN` in `browser_key.rs`. */
const SAME_DEVICE_LINK_SIGNATURE_DOMAIN = new TextEncoder().encode(
  "ii-same-device-link",
);

/** The bytes of the anchor number in the signed claim. */
const ANCHOR_NUMBER_BYTES = 8;

/** Whether the claim linked the installed app's entry to this Safari's, or no identity
 *  this Safari holds a key for had the link waiting. */
export type LinkOutcome = "linked" | "not-linked";

/** The fragment the installed app opens this page with: `key`, its one-shot link key as
 *  base64url. */
const LinkFragmentSchema = z.object({ key: z.string() });

/** The one-shot key the fragment carries as `key`, or `undefined` where it carries none
 *  of the right length. */
export const linkKeyOf = (hash: string): Uint8Array | undefined => {
  const parsed = LinkFragmentSchema.safeParse(
    Object.fromEntries(new URLSearchParams(hash.replace(/^#/, ""))),
  );
  if (!parsed.success) {
    return undefined;
  }
  let linkKey: Uint8Array;
  try {
    linkKey = fromBase64URL(parsed.data.key);
  } catch {
    return undefined;
  }
  return linkKey.length === LINK_KEY_BYTES ? linkKey : undefined;
};

/** Must match `same_device_link_message` in `browser_key.rs` byte for byte: the domain,
 *  the anchor number as a big-endian `u64`, the claimer's length as one byte, then the
 *  claimer's principal. */
export const claimMessage = (
  identityNumber: bigint,
  claimer: Uint8Array,
): Uint8Array<ArrayBuffer> => {
  const message = new Uint8Array(
    SAME_DEVICE_LINK_SIGNATURE_DOMAIN.length +
      ANCHOR_NUMBER_BYTES +
      1 +
      claimer.length,
  );
  message.set(SAME_DEVICE_LINK_SIGNATURE_DOMAIN);
  const anchorAt = SAME_DEVICE_LINK_SIGNATURE_DOMAIN.length;
  new DataView(message.buffer).setBigUint64(anchorAt, identityNumber, false);
  message[anchorAt + ANCHOR_NUMBER_BYTES] = claimer.length;
  message.set(claimer, anchorAt + ANCHOR_NUMBER_BYTES + 1);
  return message;
};

/**
 * Claims the link for whichever identity this Safari is signed in to holds it. The key
 * names no identity, so each one with a browser key here is asked in turn, and only the
 * one the installed app signed in as finds the key waiting.
 */
export const claimSameDeviceLink = async (
  linkKey: Uint8Array,
): Promise<LinkOutcome> => {
  const keyPair = await importLinkKey(linkKey);
  const linkPublicKey = new Uint8Array(
    await crypto.subtle.exportKey("spki", keyPair.publicKey),
  );
  for (const identityNumber of await registeredIdentityNumbers()) {
    const browserIdentity = await browserKeyIdentity(identityNumber);
    if (browserIdentity === undefined) {
      continue;
    }
    const signature = new Uint8Array(
      await crypto.subtle.sign(
        { name: "ECDSA", hash: "SHA-256" },
        keyPair.privateKey,
        claimMessage(
          identityNumber,
          browserIdentity.getPrincipal().toUint8Array(),
        ),
      ),
    );
    const actor = await browserKeyActor(identityNumber);
    try {
      await actor
        .claim_same_device_link({
          anchor_number: identityNumber,
          link_public_key: linkPublicKey,
          signature,
        })
        .then(throwCanisterError);
      return "linked";
    } catch (error) {
      if (!isCanisterError<ClaimSameDeviceLinkError>(error)) {
        throw error;
      }
      switch (error.type) {
        // Another identity, or a key this one has no use for: the next may hold it.
        case "NoPendingLink":
        case "InvalidBrowserKey":
          continue;
        case "InternalCanisterError":
          throw error;
      }
    }
  }
  return "not-linked";
};
