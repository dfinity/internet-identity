import { KeyType } from "$generated/internet_identity_types";

/**
 * Converts a WebAuthn authenticator attachment to an II key type
 */
export function authenticatorAttachmentToKeyType(
  authenticatorAttachment: AuthenticatorAttachment | undefined
): KeyType {
  if (authenticatorAttachment === "cross-platform") {
    return { cross_platform: null };
  } else if (authenticatorAttachment === "platform") {
    return { platform: null };
  } else {
    return { unknown: null };
  }
}
