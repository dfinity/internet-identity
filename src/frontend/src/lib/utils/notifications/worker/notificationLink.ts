/**
 * Where a notification is allowed to take the user.
 *
 * The app names the link, so it is checked before it is ever opened: it must be on the
 * origin that sent the notification, or on one of the origins that origin publishes as
 * its own in `/.well-known/ii-alternative-origins`. Anything else, and anything
 * missing or unparsable, opens the sending origin itself where {@link appUrl} puts it,
 * so acting on a notification always lands on the app and never anywhere the app did
 * not vouch for.
 *
 * The origin that sent it has been remapped onto `ic0.app`, and an app names its link
 * on the domain it is actually served on, so both sides of that check are remapped the
 * same way. A link that passes opens as the app wrote it.
 */

import { fetchCapped } from "$lib/utils/appMetadata";
import {
  appUrl,
  MAX_ALTERNATIVE_ORIGINS,
  remapToLegacyDomain,
} from "$lib/utils/urlUtils";

const ALTERNATIVE_ORIGINS_PATH = "/.well-known/ii-alternative-origins";
/** The largest alternative origins document read, as Internet Identity's canister reads
 *  it (`ALTERNATIVE_ORIGINS_MAX_RESPONSE_BYTES` in
 *  `src/internet_identity/src/alternative_origins.rs`): a list the canister accepts a
 *  frontend from is one the link check can read too. */
const ALTERNATIVE_ORIGINS_MAX_BYTES = 40 * 1024;

/** The link to open, given the origins the app vouches for. */
export const allowedLink = ({
  url,
  origin,
  alternativeOrigins,
}: {
  url: string | undefined;
  origin: string;
  alternativeOrigins: string[];
}): string => {
  if (url === undefined) {
    return appUrl(origin);
  }
  let named: URL;
  try {
    named = new URL(url);
  } catch {
    return appUrl(origin);
  }
  const vouched = [origin, ...alternativeOrigins].map(remapToLegacyDomain);
  return vouched.includes(remapToLegacyDomain(named.origin))
    ? named.toString()
    : appUrl(origin);
};

/** The origins the app at `origin` publishes as its own, or none where it publishes
 *  nothing we can use. Read where {@link appUrl} puts the app, the gateway that serves
 *  every canister, through the same capped fetch as the app's metadata document, which
 *  stops the download at the cap rather than buffering whatever the origin sends. */
export const fetchAlternativeOrigins = async (
  origin: string,
): Promise<string[]> => {
  try {
    const result = await fetchCapped(
      new URL(ALTERNATIVE_ORIGINS_PATH, appUrl(origin)),
      "application/json",
      ALTERNATIVE_ORIGINS_MAX_BYTES,
    );
    if (!result.ok) {
      return [];
    }
    return parseAlternativeOrigins(await result.blob.text());
  } catch {
    return [];
  }
};

export const parseAlternativeOrigins = (body: string): string[] => {
  try {
    const document: unknown = JSON.parse(body);
    if (
      typeof document !== "object" ||
      document === null ||
      !("alternativeOrigins" in document)
    ) {
      return [];
    }
    const listed = (document as { alternativeOrigins: unknown })
      .alternativeOrigins;
    if (!Array.isArray(listed) || listed.length > MAX_ALTERNATIVE_ORIGINS) {
      return [];
    }
    return listed.filter(
      (entry): entry is string => typeof entry === "string" && isOrigin(entry),
    );
  } catch {
    return [];
  }
};

/** An origin and nothing else: a listed entry carrying a path or a query says the
 *  document was not written to be read this way. */
const isOrigin = (value: string): boolean => {
  try {
    return new URL(value).origin === value;
  } catch {
    return false;
  }
};
