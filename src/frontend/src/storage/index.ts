/* Everything related to storage of user flow data, like anchor numbers, last used anchor, etc. */

import { parseUserNumber } from "$src/utils/userNumber";
import { nonNullish } from "@dfinity/utils";
import { z } from "zod";

/** The Anchor type as stored in storage, including hint of the frequency at which the anchor is used.
 * If you change this type, please add a migration */
type Anchor = z.infer<typeof Anchor>;
const Anchor = z
  .object({
    /** Timestamp (mills since epoch) of when anchor was last used */
    lastUsedTimestamp: z.number(),
  })
  .passthrough(); /* ensures keys not listed in schema are kept during parse */

/** The type of all anchors in storage. */
type Anchors = z.infer<typeof Anchors>;
const Anchors = z.record(Anchor);

/** We keep as many anchors as possible for two reasons:
 *  - we should design the app to discourage having many many anchors, but shouldn't prevent it
 *  - it's the only place where users may see all their anchors
 *
 *  we do still put a (rather high) limit to avoid storage blowing up
 */
export const MAX_SAVED_ANCHORS = 10;

/** Read saved anchors, sorted */
export const getAnchors = (): bigint[] => {
  const data = readAnchors();
  const anchors = Object.keys(data).map((ix) => BigInt(ix));

  // NOTE: This sort here is only used to ensure users see a stable ordering of anchors.
  anchors.sort();

  return anchors;
};

/** Set the specified anchor as used "just now" */
export const setAnchorUsed = (userNumber: bigint) => {
  void withAnchors((anchors) => {
    const ix = userNumber.toString();

    // Here we try to be as non-destructive as possible and we keep potentially unknown
    // fields
    anchors[ix] = { ...anchors[ix], lastUsedTimestamp: nowMillis() };
    return anchors;
  });
};

/** Accessing functions */

// Simply read the storage without updating it
const readAnchors = (): Anchors => {
  return updateStorage((anchors) => {
    return { ret: anchors, updated: false };
  });
};

// Read & update the storage
const withAnchors = (op: (anchors: Anchors) => Anchors): Anchors => {
  return updateStorage((anchors) => {
    const newAnchors = op(anchors);
    return { ret: newAnchors, updated: true };
  });
};

/** Building block for reading or updating anchors, ensuring anchors
 * are read correctly (found, migrated, or have a sensible default) and pruned
 * before being written back (if updated) */
const updateStorage = (
  op: (anchors: Anchors) => {
    ret: Anchors;
    updated: boolean /* iff true, anchors are updated in storage */;
  }
): Anchors => {
  let doWrite = false;

  const storedAnchors = readLocalStorage();

  const { ret: migratedAnchors, didMigrate } = nonNullish(storedAnchors)
    ? { ret: storedAnchors, didMigrate: false }
    : { ret: migrated() ?? {}, didMigrate: true };
  doWrite ||= didMigrate;

  const { ret: updatedAnchors, updated } = op(migratedAnchors);
  doWrite ||= updated;

  const { ret: prunedAnchors, pruned } = pruneAnchors(updatedAnchors);
  doWrite ||= pruned;

  if (doWrite) {
    writeLocalStorage(prunedAnchors);
  }

  return prunedAnchors;
};

/** Migration & Pruning of anchors */

/** Remove most unused anchors until there are MAX_SAVED_ANCHORS left */
const pruneAnchors = (anchors: Anchors): { ret: Anchors; pruned: boolean } => {
  // this is equivalent to while(anchors.length > MAX)
  // but avoids potential infinite loops if for some reason anchors can't
  // be deleted
  const nExtras = Math.max(Object.keys(anchors).length - MAX_SAVED_ANCHORS, 0);
  const filtered = { ...anchors };
  let pruned = false;
  for (let i = 0; i < nExtras; i++) {
    const unused = mostUnused(filtered);
    if (nonNullish(unused)) {
      delete filtered[unused];
      pruned = true;
    }
  }

  return { ret: filtered, pruned };
};

/** Figure out which anchor was used the least recently (if there are any) */
const mostUnused = (anchors: Anchors): string | undefined => {
  // First turn the { 123: { lastUsedTimestamp: ... } } into [{ ix: 123, lastUsedTimestamp: ... }],
  // then sort by last used and return first element
  const arr = Object.keys(anchors).map((ix) => ({
    ix,
    lastUsedTimestamp: anchors[ix].lastUsedTimestamp,
  }));

  arr.sort((a, b) => a.lastUsedTimestamp - b.lastUsedTimestamp);

  return arr[0]?.ix;
};

/** Best effort to migrate potentially old localStorage data to
 * the latest format */
const migrated = (): Anchors | undefined => {
  // Nothing to do if no 'userNumber's are stored
  const userNumberString = localStorage.getItem("userNumber");
  if (userNumberString === null) {
    return;
  }

  // Try to decode old data
  const userNumber = parseUserNumber(userNumberString);
  if (userNumber === null) {
    // The item may have been written by someone else so we don't remove it
    console.warn("Could not migrate unknown user number", userNumber);
    return;
  }

  const ix = userNumber.toString();

  const anchors = { [ix]: { lastUsedTimestamp: nowMillis() } };

  return anchors;
};

/** "Low-level" serialization functions to read and write anchors to local storage */

// Read localstorage stored anchors
const readLocalStorage = (): Anchors | undefined => {
  const raw = localStorage.getItem("anchors");

  // Abort
  if (raw === null) {
    return;
  }

  let item;
  try {
    item = JSON.parse(raw);
  } catch (e) {
    // If the parsing fail there isn't much we can do
    console.error("could not parse stored data as anchors", raw, e);
    item = {};
  }

  // Actually parse the JSON object
  const parsed = Anchors.safeParse(item);
  if (parsed.success !== true) {
    const message =
      `could not read saved identities: ignoring malformed localstorage data: ` +
      parsed.error;
    console.warn(message);
    return {};
  }

  return parsed.data;
};

// Write localstorage stored anchors
const writeLocalStorage = (anchors: Anchors) => {
  localStorage.setItem("anchors", JSON.stringify(anchors));
};

/** Helpers */

// Current timestamp, in milliseconds
const nowMillis = (): number => {
  return new Date().getTime();
};
