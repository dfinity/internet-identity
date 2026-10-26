/** A fragment rather than a query, so the key is never sent to the gateway; iOS bookmarks the whole URL, so the installed app launches holding it. */
import { fromBase64URL, toBase64URL } from "$lib/utils/utils";
import { LINK_KEY_BYTES } from "./linkKey";

export interface LinkToken {
  identityNumber: bigint;
  /** The private half of the one-shot link key, as `exportLinkKey` writes it. */
  linkKey: Uint8Array;
}

/** What the browser puts in the URL it tells the user to install from. */
export const encodeLinkToken = (token: LinkToken): string =>
  new URLSearchParams({
    anchor: token.identityNumber.toString(),
    key: toBase64URL(token.linkKey),
  }).toString();

/** `unreadable` is not `no-token`: a damaged install link has a browser waiting at the other end and needs a fresh link, not the steps for someone who has not started. */
export type FragmentLinkToken =
  | { kind: "token"; token: LinkToken }
  | { kind: "no-token" }
  | { kind: "unreadable"; reason: string };

/** What the browser puts in the URL, read back. */
export const decodeLinkToken = (fragment: string): FragmentLinkToken => {
  const params = new URLSearchParams(fragment.replace(/^#/, ""));
  const anchor = params.get("anchor");
  const key = params.get("key");
  if (anchor === null && key === null) {
    return { kind: "no-token" };
  }
  if (anchor === null || key === null) {
    return { kind: "unreadable", reason: "the token is missing a field" };
  }
  try {
    const bytes = fromBase64URL(key);
    if (bytes.length !== LINK_KEY_BYTES) {
      return {
        kind: "unreadable",
        reason: `the key is ${bytes.length} bytes, not ${LINK_KEY_BYTES}`,
      };
    }
    return {
      kind: "token",
      token: {
        identityNumber: BigInt(anchor),
        linkKey: bytes,
      },
    };
  } catch (error) {
    return {
      kind: "unreadable",
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};
