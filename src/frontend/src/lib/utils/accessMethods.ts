import {
  AuthnMethodData,
  OpenIdCredential,
} from "$lib/generated/internet_identity_types";
import { isNullish, nonNullish } from "@dfinity/utils";

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
