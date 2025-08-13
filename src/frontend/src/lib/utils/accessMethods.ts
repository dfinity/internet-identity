import {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { isNullish, nonNullish } from "@dfinity/utils";
import { isSameOrigin } from "./urlUtils";
import { canisterConfig } from "$lib/globals";

/**
 * Check if a `AuthnMethodData` or `OpenIdCredential` is a WebAuthn method
 */
export const isWebAuthnMetaData = (
  accessMethod: AuthnMethodData | OpenIdCredential,
): accessMethod is AuthnMethodData =>
  "authn_method" in accessMethod && "WebAuthn" in accessMethod.authn_method;

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
export const getOrigin = (
  accessMethod: AuthnMethodData,
): string | undefined => {
  const metadataEntry = accessMethod.metadata.find(([key]) => key === "origin");
  const metadataValue = metadataEntry?.[1];
  if (nonNullish(metadataValue) && "String" in metadataValue) {
    return metadataValue.String;
  }
  return undefined;
};

/**
 * Check if there are multiple unique origins across authentication methods
 */
export const haveMultipleOrigins = (
  authnMethods: AuthnMethodData[],
): boolean => {
  const origins = new Set<string | undefined>();
  for (const method of authnMethods) {
    origins.add(getOrigin(method));
  }
  return origins.size > 1;
};

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
