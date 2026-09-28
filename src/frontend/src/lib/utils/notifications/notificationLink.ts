/**
 * Where a notification is allowed to take the user.
 *
 * The app names the link, so it is checked before it is ever opened: it must be on the
 * origin that sent the notification, or on one of the origins that origin publishes as
 * its own in `/.well-known/ii-alternative-origins`. Anything else — and anything
 * missing or unparsable — becomes the sending origin itself, so acting on a
 * notification always lands on the app and never anywhere the app did not vouch for.
 *
 * The origin that sent it has been remapped onto `ic0.app`, and an app names its link
 * on the domain it is actually served on, so both sides of that check are remapped the
 * same way. What opens is the app's link as it wrote it.
 */

import { remapToLegacyDomain } from "$lib/utils/urlUtils";

const ALTERNATIVE_ORIGINS_PATH = "/.well-known/ii-alternative-origins";

/** As many as II accepts elsewhere for the same document. */
const MAX_ALTERNATIVE_ORIGINS = 100;
const MAX_DOCUMENT_SIZE = 8_192;
const FETCH_TIMEOUT_MILLIS = 10_000;

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
    return origin;
  }
  let named: URL;
  try {
    named = new URL(url);
  } catch {
    return origin;
  }
  const vouched = [origin, ...alternativeOrigins].map(remapToLegacyDomain);
  return vouched.includes(remapToLegacyDomain(named.origin))
    ? named.toString()
    : origin;
};

/** The origins an app publishes as its own, or none where it publishes nothing we can
 *  use. Read the same way II reads the app's other well-known documents: no
 *  credentials, no redirects, a size cap and a timeout. */
export const fetchAlternativeOrigins = async (
  origin: string,
): Promise<string[]> => {
  const abort = new AbortController();
  const timeout = setTimeout(() => abort.abort(), FETCH_TIMEOUT_MILLIS);
  try {
    const response = await fetch(`${origin}${ALTERNATIVE_ORIGINS_PATH}`, {
      credentials: "omit",
      redirect: "error",
      signal: abort.signal,
    });
    if (!response.ok) {
      return [];
    }
    const length = Number(response.headers.get("content-length") ?? 0);
    if (length > MAX_DOCUMENT_SIZE) {
      return [];
    }
    const body = await response.text();
    if (body.length > MAX_DOCUMENT_SIZE) {
      return [];
    }
    return parseAlternativeOrigins(body);
  } catch {
    return [];
  } finally {
    clearTimeout(timeout);
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
