import type { MetadataMapV2 } from "$lib/generated/internet_identity_types";
import { openIdLogo, openIdName } from "$lib/utils/openID";

/**
 * A parsed attribute key with its scope type, scope-specific values, and attribute name.
 *
 * The key format is: `scopeType:scopeValue1:...:scopeValueN:attributeName`
 * - First segment = scope type (e.g. "openid")
 * - Segments between first and last = scope-specific values (e.g. provider URL for OpenID)
 * - Last segment = attribute name (e.g. "email", "name")
 *
 * Unscoped keys have no scope type and only contain the attribute name.
 */
export interface ParsedAttributeKey {
  scopeType?: string;
  scopeValues: string[];
  attributeName: string;
}

export const parseAttributeKey = (key: string): ParsedAttributeKey => {
  // Split from the right: the last segment is always the attribute name.
  const lastColonIndex = key.lastIndexOf(":");
  if (lastColonIndex === -1) {
    // Unscoped key (e.g. "email")
    return { scopeValues: [], attributeName: key };
  }

  const attributeName = key.slice(lastColonIndex + 1);
  const scopePart = key.slice(0, lastColonIndex);

  // The first segment of the scope part is the scope type.
  const firstColonIndex = scopePart.indexOf(":");
  if (firstColonIndex === -1) {
    // Single scope segment (e.g. "openid:email" — scope type only, no scope values)
    return { scopeType: scopePart, scopeValues: [], attributeName };
  }

  const scopeType = scopePart.slice(0, firstColonIndex);
  const scopeValuesStr = scopePart.slice(firstColonIndex + 1);

  // For OpenID, the scope value is the full issuer URL (which itself contains colons).
  // Since the attribute name was already split off, the remaining string is the scope value.
  return {
    scopeType,
    scopeValues: [scopeValuesStr],
    attributeName,
  };
};

const IMPLICIT_CONSENT_ATTRIBUTES = ["name", "email", "verified_email"];

/**
 * Whether the given attribute key qualifies for implicit consent with the given OpenID issuer.
 * Implicit consent applies to name, email, and verified_email scoped to the authenticating issuer.
 */
export const isImplicitConsentAttribute = (
  key: string,
  issuer: string,
): boolean => {
  const parsed = parseAttributeKey(key);
  if (parsed.scopeType !== "openid") {
    return false;
  }
  if (parsed.scopeValues[0] !== issuer) {
    return false;
  }
  return IMPLICIT_CONSENT_ATTRIBUTES.includes(parsed.attributeName);
};

const ATTRIBUTE_LABELS: Record<string, string> = {
  email: "Email address",
  name: "Name",
  verified_email: "Verified email",
};

export const getAttributeLabel = (attributeName: string): string =>
  ATTRIBUTE_LABELS[attributeName] ?? attributeName;

/**
 * Whether the consent screen is needed for the given requested keys.
 * Returns false if all keys are implicit-consent for the given issuer.
 * Does NOT call list_available_attributes — this is a cheap client-side check.
 */
export const needsConsentScreen = (
  requestedKeys: string[],
  issuer?: string,
): boolean => {
  if (issuer === undefined) {
    // Explicit flow: always needs consent if there are any keys.
    return requestedKeys.length > 0;
  }
  // OpenID flow: needs consent if any key is NOT implicit.
  return requestedKeys.some((key) => !isImplicitConsentAttribute(key, issuer));
};

export interface ConsentAttribute {
  /** Fully scoped key, e.g. "openid:https://accounts.google.com:email" */
  scopedKey: string;
  /** Decoded string value, e.g. "user@gmail.com" */
  value: string;
  /** Provider display name, e.g. "Google" */
  providerName?: string;
  /** Provider logo SVG string */
  providerLogo?: string;
  /** Whether this attribute has implicit consent (pre-checked, for the OpenID issuer) */
  implicit: boolean;
}

export interface ConsentGroup {
  /** Unscoped attribute name, e.g. "email" */
  attributeName: string;
  /** Display label, e.g. "Email address" */
  label: string;
  /** Available options — 1 for scoped requests, potentially multiple for unscoped */
  options: ConsentAttribute[];
}

/**
 * Builds consent groups from the requested keys and available attributes.
 *
 * @param requestedKeys The attribute keys from the JSON-RPC request
 * @param availableAttributes Result of list_available_attributes (fully-scoped key + value bytes)
 * @param metadata OpenID metadata for resolving provider names/logos
 * @param issuer Optional issuer for marking implicit-consent attributes
 */
export const buildConsentGroups = (
  requestedKeys: string[],
  availableAttributes: [string, Uint8Array | number[]][],
  metadata: MetadataMapV2,
  issuer?: string,
): ConsentGroup[] => {
  const groups = new Map<string, ConsentGroup>();

  for (const key of requestedKeys) {
    const parsed = parseAttributeKey(key);
    const { attributeName } = parsed;

    if (!groups.has(attributeName)) {
      groups.set(attributeName, {
        attributeName,
        label: getAttributeLabel(attributeName),
        options: [],
      });
    }

    const group = groups.get(attributeName);
    if (group === undefined) {
      continue;
    }

    if (parsed.scopeType !== undefined) {
      // Scoped key: find the matching available attribute
      const match = availableAttributes.find(([k]) => k === key);
      if (match !== undefined) {
        const value = new TextDecoder().decode(new Uint8Array(match[1]));
        const scopeValue = parsed.scopeValues[0];
        group.options.push({
          scopedKey: key,
          value,
          providerName:
            scopeValue !== undefined
              ? openIdName(scopeValue, metadata)
              : undefined,
          providerLogo:
            scopeValue !== undefined
              ? openIdLogo(scopeValue, metadata)
              : undefined,
          implicit:
            issuer !== undefined && isImplicitConsentAttribute(key, issuer),
        });
      }
    } else {
      // Unscoped key: find ALL available attributes with this attribute name
      for (const [scopedKey, valueBytes] of availableAttributes) {
        const availableParsed = parseAttributeKey(scopedKey);
        if (availableParsed.attributeName !== attributeName) {
          continue;
        }
        const value = new TextDecoder().decode(new Uint8Array(valueBytes));
        const scopeValue = availableParsed.scopeValues[0];
        group.options.push({
          scopedKey,
          value,
          providerName:
            scopeValue !== undefined
              ? openIdName(scopeValue, metadata)
              : undefined,
          providerLogo:
            scopeValue !== undefined
              ? openIdLogo(scopeValue, metadata)
              : undefined,
          implicit:
            issuer !== undefined &&
            isImplicitConsentAttribute(scopedKey, issuer),
        });
      }
    }
  }

  // Remove groups with no available options
  return Array.from(groups.values()).filter(
    (group) => group.options.length > 0,
  );
};
