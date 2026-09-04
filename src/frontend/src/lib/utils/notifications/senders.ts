// Parses the `senders` array of a dApp's `/.well-known/ii-notification-senders`
// into the canister principals allowed to send for that origin. The document
// authorizes several canisters and any of them may own notification content, so
// the service worker pulls from all of them; list order carries no meaning.
//
// Pure and free of service-worker globals so it can be unit-tested; the worker
// wraps it with the fetch and the gateway fallback.

import { Principal } from "@icp-sdk/core/principal";

// Matches `MAX_SENDERS` in the backend's `notifications/well_known.rs`: the
// canister truncates the document to this many before binding, so a principal
// past it is never an authorized sender and there is nothing to pull from it.
export const MAX_SENDERS = 20;

/**
 * The valid, distinct sender principals named by a well-known document, in
 * document order, capped at {@link MAX_SENDERS}. Invalid or non-string entries
 * are skipped; a document with no `senders` array yields an empty list.
 */
export const parseSenders = (doc: unknown, max = MAX_SENDERS): string[] => {
  const senders =
    doc !== null && typeof doc === "object" && "senders" in doc
      ? (doc as { senders: unknown }).senders
      : undefined;
  if (!Array.isArray(senders)) {
    return [];
  }
  // Truncate first, then keep the valid ones, so the cap matches the backend's
  // `truncate(MAX_SENDERS)` rather than reaching further into a padded list.
  const seen = new Set<string>();
  for (const entry of senders.slice(0, max)) {
    if (typeof entry !== "string" || seen.has(entry)) {
      continue;
    }
    try {
      Principal.fromText(entry);
    } catch {
      continue;
    }
    seen.add(entry);
  }
  return [...seen];
};
