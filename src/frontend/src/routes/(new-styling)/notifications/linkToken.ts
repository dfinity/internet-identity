/**
 * The token that travels from the browser that installed this app to the app itself.
 *
 * It rides the URL fragment, because iOS bookmarks the whole URL when the user adds the
 * page to the Home Screen: the installed app launches holding a secret only whoever
 * performed the install ever saw. A fragment rather than a query so it is never sent to
 * the gateway and never reaches a server log.
 */

export interface LinkToken {
  identityNumber: bigint;
  expiresAtNs: bigint;
  signature: Uint8Array;
}

const base64UrlEncode = (bytes: Uint8Array): string =>
  btoa(String.fromCharCode(...bytes))
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/, "");

const base64UrlDecode = (value: string): Uint8Array =>
  Uint8Array.from(
    atob(value.replace(/-/g, "+").replace(/_/g, "/")),
    (character) => character.charCodeAt(0),
  );

/** What the browser puts in the URL it tells the user to install from. */
export const encodeLinkToken = (token: LinkToken): string =>
  new URLSearchParams({
    anchor: token.identityNumber.toString(),
    exp: token.expiresAtNs.toString(),
    sig: base64UrlEncode(token.signature),
  }).toString();

/** `undefined` for a fragment that carries no token, or one we cannot read: either way
 *  this launch has nothing to claim with, and the app says so rather than guessing. */
export const decodeLinkToken = (fragment: string): LinkToken | undefined => {
  try {
    const params = new URLSearchParams(fragment.replace(/^#/, ""));
    const anchor = params.get("anchor");
    const expiry = params.get("exp");
    const signature = params.get("sig");
    if (anchor === null || expiry === null || signature === null) {
      return undefined;
    }
    return {
      identityNumber: BigInt(anchor),
      expiresAtNs: BigInt(expiry),
      signature: base64UrlDecode(signature),
    };
  } catch {
    return undefined;
  }
};

/** How long a token is good for. Long enough to walk the install steps at a reading
 *  pace and open the app, short enough that one left in a browser history is of little
 *  use. A token refused for age is replaced by starting the steps again. */
export const LINK_TOKEN_LIFETIME_MS = 30 * 60 * 1000;
