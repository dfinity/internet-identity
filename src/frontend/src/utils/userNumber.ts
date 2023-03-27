import { unknownToRecord } from "./utils";

/** The Anchor type as stored in local storage, including hint of the frequency
 * at which the anchor is used.
 * If you change this type, please add a migration */
type Anchor = {
  /** Timestamp (mills since epoch) of when anchor was last used */
  lastUsedTimestamp: number;
};

/** The type of all anchors in local storage. Because we deal with local storage we only
 * care about 'string's and not 'bigint's. */
type Anchors = Record<string, Anchor>;

/** We keep as many anchors as possible for two reasons:
 *  - we should design the app to discourage having many many anchors, but shouldn't prevent it
 *  - it's the only place where users may see all their anchors
 *
 *  we do still put a (rather high) limit to avoid storage blowing up
 */
export const MAX_SAVED_ANCHORS = 10;

/* Migrated version, see function below for implementation */
export { getAnchorsMigrated as getAnchors };
const getAnchorsMigrated = (): bigint[] => {
  migrate();
  return getAnchors();
};

/** Read saved anchors from local storage, returning at most
 * MAX_SAVED_ANCHORS */
const getAnchors = (): bigint[] => {
  const data = pruneAnchors(readAnchors());

  const anchors = Object.keys(data).map((ix) => BigInt(ix));

  // NOTE: this sort has nothing to do with the sorting we use to prune anchors.
  // This sort here is only used to ensure users see a stable ordering of anchors.
  anchors.sort();

  return anchors;
};

/* Migrated version, see function below for implementation */
export { setAnchorUsedMigrated as setAnchorUsed };
const setAnchorUsedMigrated = (anchor: bigint) => {
  migrate();
  return setAnchorUsed(anchor);
};

/** Set the most recently used anchor */
const setAnchorUsed = (anchor: bigint) => {
  const anchors = readAnchors();

  const ix = anchor.toString();
  // Here we try to be as non-destructive as possible and we keep potentially unknown
  // fields
  anchors[ix] = { ...anchors[ix], lastUsedTimestamp: new Date().getTime() };
  writeAnchors(pruneAnchors(anchors));
};

/** Remove most unused anchors until there are MAX_SAVED_ANCHORS left */
const pruneAnchors = (anchors: Anchors): Anchors => {
  // this is equivalent to while(anchors.length > MAX)
  // but avoids potential infinite loops if for some reason anchors can't
  // be deleted
  const extras = Math.max(Object.keys(anchors).length - MAX_SAVED_ANCHORS, 0);
  const filtered = { ...anchors };
  for (let i = 0; i < extras; i++) {
    const unused = mostUnused(filtered);
    if (unused !== undefined) {
      delete filtered[unused];
    }
  }

  return filtered;
};

/** Best effort to migrate potentially old localStorage data to
 * the latest format */
export const migrate = () => {
  // Exit early if we've already migrated/stored new anchors
  if (getAnchors().length !== 0) {
    return;
  }

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

  // Most likely the anchor was used before being written to local storage
  setAnchorUsed(userNumber);
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

/* "Low-level" serialization functions to read and write anchors to local storage */

const readAnchors = (): Anchors => {
  const raw = localStorage.getItem("anchors");

  if (raw === null) {
    return {};
  }

  let item;
  try {
    item = JSON.parse(raw);
  } catch (e) {
    // If the parsing fail there isn't much we can do
    console.error("could not parse stored data as anchors", raw, e);
    item = {};
  }

  if (typeof item !== "object") {
    console.warn("bad anchors", item, "type is", typeof item);
    return {};
  }

  // eslint-disable-next-line
  const tmp: {} = item;
  const objects: Record<string, unknown> = tmp;

  const anchors: Anchors = {};

  for (const ix in objects) {
    const anchor = asAnchor(objects[ix]);
    if (anchor === undefined) {
      console.warn("Could not read anchor", objects[ix]);
      continue;
    }

    anchors[ix] = anchor;
  }

  return anchors;
};

/** Try to interpret an unknown value as an anchor */
const asAnchor = (msg: unknown): Anchor | undefined => {
  const obj: Record<string, unknown> | undefined = unknownToRecord(msg);
  if (obj === undefined) {
    return undefined;
  }

  if ("lastUsedTimestamp" in obj && typeof obj.lastUsedTimestamp === "number") {
    return { ...obj, lastUsedTimestamp: obj.lastUsedTimestamp };
  }

  return undefined;
};

const writeAnchors = (anchors: Anchors) => {
  localStorage.setItem("anchors", JSON.stringify(anchors));
};

// We check that the user has entered a sequence of digits only,
// before attempting to parse
export const parseUserNumber = (s: string): bigint | null => {
  if (isUserNumber(s)) {
    try {
      return BigInt(s);
    } catch (err) {
      return null;
    }
  } else {
    return null;
  }
};

// This makes sure `s` is a viable user number; naively using BigInt
// on any string would also accept the following values, which are
// _not_ valid user numbers:
// - BigInt(whitespace) == 0
// - Hex/Octal formatted numbers
// - Scientific notation
export const isUserNumber = (s: string): boolean => {
  return /^\d+$/.test(s);
};
