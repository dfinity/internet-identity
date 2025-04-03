/**
 * Check if registration is allowed based on the Internet Identity configuration and current origin.
 *
 * @param config - The InternetIdentityInit configuration
 * @param currentOrigin - The current origin from where the request is made
 * @returns Whether registration is allowed from the current origin
 */
import type { InternetIdentityInit } from "$lib/generated/internet_identity_types";
import { isNullish } from "@dfinity/utils";
import { isSameOrigin } from "./urlUtils";

export const isRegistrationAllowed = (
  config: InternetIdentityInit,
  currentOrigin: string,
): boolean => {
  // If there are no related origins defined or the related_origins list is empty, registration is allowed
  if (
    isNullish(config.related_origins[0]) ||
    config.related_origins[0].length === 0
  ) {
    return true;
  }

  // Get the related origins array
  const relatedOrigins = config.related_origins[0];

  // Check if the current origin matches any of the related origins
  return relatedOrigins.some((origin) => isSameOrigin(origin, currentOrigin));
};
