import {
  AuthnMethodData,
  DeviceData,
  MetadataMapV2,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { isNullish, nonNullish } from "@dfinity/utils";
import { isSameOrigin } from "./urlUtils";
import { canisterConfig } from "$lib/globals";
import { authnMethodEqual } from "./webAuthn";
import { LEGACY_II_URL } from "$lib/config";
import { findConfig, getMetadataString, isOpenIdConfig } from "./openID";

/**
 * Check if a `AuthnMethodData` or `OpenIdCredential` is a WebAuthn method
 */
export const isWebAuthnMetaData = (
  accessMethod: AuthnMethodData | OpenIdCredential,
): accessMethod is AuthnMethodData =>
  "authn_method" in accessMethod && "WebAuthn" in accessMethod.authn_method;

export const isOpenIdCredential = (
  accessMethod: AuthnMethodData | OpenIdCredential,
): accessMethod is OpenIdCredential => "iss" in accessMethod;

export const getLastUsedAccessMethod = (
  authnMethods: AuthnMethodData[],
  openIdCredentials: OpenIdCredential[],
): AuthnMethodData | OpenIdCredential | null => {
  if (authnMethods.length === 0 && openIdCredentials.length === 0) {
    return null;
  }
  const allMethods = [
    ...authnMethods,
    ...openIdCredentials.map((cred) => {
      return { ...cred, last_authentication: cred.last_usage_timestamp };
    }),
  ];

  return allMethods.sort((devA, devB) => {
    if (
      nonNullish(devA.last_authentication[0]) &&
      nonNullish(devB.last_authentication[0])
    ) {
      return (
        Number(devB.last_authentication[0]) -
        Number(devA.last_authentication[0])
      );
    } else if (
      isNullish(devA.last_authentication[0]) &&
      nonNullish(devB.last_authentication[0])
    ) {
      return 1;
    } else if (
      nonNullish(devA.last_authentication[0]) &&
      isNullish(devB.last_authentication[0])
    ) {
      return -1;
    } else {
      return 0;
    }
  })[0];
};

/**
 * Extract the origin from an AuthnMethodData's metadata
 */
export const getOrigin = (accessMethod: AuthnMethodData): string | undefined =>
  getMetadataString(accessMethod.metadata, "origin");

const getOpenIdCredentialName = (credential: OpenIdCredential) =>
  getMetadataString(credential.metadata, "name");

const getOpenIdCredentialEmail = (credential: OpenIdCredential) =>
  getMetadataString(credential.metadata, "email");

/**
 * Extract the RP ID (origin without protocol) from an AuthnMethodData's metadata
 */
export const getRpId = (accessMethod: AuthnMethodData): string | undefined => {
  const origin = getOrigin(accessMethod);
  if (nonNullish(origin)) {
    try {
      return new URL(origin).hostname;
    } catch {
      return undefined;
    }
  }
  return undefined;
};

/**
 * Check if there are multiple unique origins across authentication methods
 */
export const haveMultipleOrigins = (authnMethods: AuthnMethodData[]): boolean =>
  new Set(authnMethods.map(getOrigin)).size > 1;

const hasSomeOrigin = (
  accessMethod: AuthnMethodData,
  origins: string[],
): boolean => {
  const accessMethodOrigin = getOrigin(accessMethod);
  if (nonNullish(accessMethodOrigin)) {
    return origins.some((o) => isSameOrigin(o, accessMethodOrigin));
  }
  return false;
};

/**
 * Filters the access methods to only include the legacy ones.
 *
 * An access method is considered legacy if:
 * - It is a recovery method (not supported yet in new flow).
 * - It wasn't registered in the new flow's origin.
 *
 * TODO: Do not use new_flow_origins when old domains move to new flow.
 * TODO: Remove recovery once they are supported in new flow.
 *
 * @param accessMethod
 * @returns {boolean}
 */
export const isLegacyAuthnMethod = (accessMethod: AuthnMethodData): boolean => {
  return (
    !hasSomeOrigin(accessMethod, canisterConfig.new_flow_origins[0] ?? []) ||
    "Recovery" in accessMethod.security_settings.purpose
  );
};

/**
 * Returns true if the device's origin is one of the new flow origins.
 * As long as the device is not a recovery device (which are not yet supported in the new flow origins).
 *
 * TODO: Do not use new_flow_origins when old domains move to new flow.
 * TODO: Remove recovery once they are supported in new flow.
 *
 * @param device
 * @returns {boolean}
 */
export const isNewOriginDevice = (
  device: Omit<DeviceData, "alias">,
): boolean => {
  return (
    (canisterConfig.new_flow_origins[0] ?? []).some((new_origin) =>
      isSameOrigin(new_origin, device.origin[0] ?? LEGACY_II_URL),
    ) && !("Recovery" in device.purpose)
  );
};

/**
 * Check if an access method is the currently used access method for authentication
 */
export const isSameAccessMethod = (
  accessMethod1: AuthnMethodData | OpenIdCredential,
  accessMethod2: AuthnMethodData | OpenIdCredential,
): boolean => {
  if (isWebAuthnMetaData(accessMethod1) && isWebAuthnMetaData(accessMethod2)) {
    return authnMethodEqual(accessMethod1, accessMethod2);
  }

  if (isOpenIdCredential(accessMethod1) && isOpenIdCredential(accessMethod2)) {
    return (
      accessMethod2.iss === accessMethod1.iss &&
      accessMethod2.sub === accessMethod1.sub
    );
  }
  return false;
};

export const getOpenIdTitles = (
  credential: OpenIdCredential,
  metadata: MetadataMapV2,
): {
  title: { ellipsis: boolean; text: string };
  subtitle?: { ellipsis: boolean; text: string };
} => {
  const name = getOpenIdCredentialName(credential);
  const email = getOpenIdCredentialEmail(credential);
  const config = findConfig(credential.iss, metadata);
  const accountProvider = nonNullish(config)
    ? isOpenIdConfig(config)
      ? config.name
      : "Google"
    : "Unknown";
  if (nonNullish(name) && nonNullish(email)) {
    return {
      title: { ellipsis: false, text: name },
      subtitle: {
        ellipsis: true,
        text: `${accountProvider} Account - ${email}`,
      },
    };
  }
  if (nonNullish(name)) {
    return {
      title: { ellipsis: false, text: name },
      subtitle: { ellipsis: false, text: `${accountProvider} Account` },
    };
  }
  if (nonNullish(email)) {
    return {
      title: { ellipsis: true, text: email },
      subtitle: { ellipsis: false, text: `${accountProvider} Account` },
    };
  }
  return {
    title: { ellipsis: false, text: "Unknown Account" },
    subtitle: undefined,
  };
};
