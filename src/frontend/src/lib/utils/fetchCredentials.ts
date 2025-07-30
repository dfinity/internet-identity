import { anonymousActor } from "$lib/globals";
import { nonNullish } from "@dfinity/utils";
import { convertToValidCredentialData } from "./credential-devices";

/**
 * Fetches the credentials for a given identity number.
 * @param identityNumber The identity number to fetch credentials for.
 * @returns An array of credentials or undefined if no valid credentials are found.
 */
export const fetchIdentityCredentials = async (
  identityNumber: bigint,
): Promise<Uint8Array[] | undefined> => {
  try {
    const identityCredentials = await anonymousActor.lookup(identityNumber);
    const validCredentials = identityCredentials
      .filter((device) => "authentication" in device.purpose)
      .filter(({ key_type }) => !("browser_storage_key" in key_type))
      .map(convertToValidCredentialData)
      .filter(nonNullish);

    if (validCredentials.length > 0) {
      return validCredentials.map(
        (credential) => new Uint8Array(credential.credentialId),
      );
    }

    return undefined;
  } catch (error) {
    console.warn(
      `Error looking up identity ${identityNumber} credentials:`,
      error,
    );
    return undefined;
  }
};
