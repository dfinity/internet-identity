import { KeyType } from "../../generated/internet_identity_types";

/**
 * Converts a WebAuthn authenticator attachment to an II key type
 */
export function authenticatorAttachmentToKeyType(
  authenticatorAttachment: AuthenticatorAttachment | undefined
): KeyType {
  let keyType: KeyType = { unknown: null };
  switch (authenticatorAttachment) {
    case "cross-platform":
      keyType = { cross_platform: null };
      break;
    case "platform":
      keyType = { platform: null };
      break;
    default:
      break;
  }
  return keyType;
}
