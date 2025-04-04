/* Everything related to storage of user flow data, like anchor numbers, last used anchor, etc. */

import { parseUserNumber } from "$lib/utils/userNumber";
import { Principal } from "@dfinity/principal";
import { isNullish, nonNullish } from "@dfinity/utils";
import { get as idbGet, set as idbSet } from "idb-keyval";
import { ZodType, z } from "zod";

/** We keep as many anchors as possible for two reasons:
 *  - we should design the app to discourage having many many anchors, but shouldn't prevent it
 *  - it's the only place where users may see all their anchors
 *
 *  we do still put a (rather high) limit to avoid storage blowing up
 */
export const MAX_SAVED_ANCHORS = 10;

/** We don't keep an infinite number of principals to not bloat the storage. We do
 * keep a significant number however, since (depending on the max TTL for delegation)
 * a long time might have elapsed since we stored the principal (auth with RP) and the
 * moment the user tries to verify a credential. */
export const MAX_SAVED_PRINCIPALS = 40;

/** Read saved anchors, sorted */
export const getAnchors = async (): Promise<bigint[]> => {
  const data = await readStorage();
  const anchors = Object.keys(data.anchors).map((ix) => BigInt(ix));

  // NOTE: This sort here is only used to ensure users see a stable ordering of anchors.
  anchors.sort();

  return anchors;
};

/** Set the specified anchor as used "just now" */
export const setAnchorUsed = async (userNumber: bigint) => {
  await withStorage((storage) => {
    const ix = userNumber.toString();

    const anchors = storage.anchors;
    const defaultAnchor: Omit<Anchor, "lastUsedTimestamp"> = {
      knownPrincipals: [],
    };
    const oldAnchor = anchors[ix] ?? defaultAnchor;

    // Here we try to be as non-destructive as possible and we keep potentially unknown
    // fields
    storage.anchors[ix] = { ...oldAnchor, lastUsedTimestamp: nowMillis() };
    return storage;
  });
};

/** Look up an anchor by principal.
 * In reality the anchor is looked up by principal _digest_, see `computePrincipalDigest` for
 * more information.
 */
export const getAnchorByPrincipal = async ({
  principal,
}: {
  principal: Principal;
}): Promise<bigint | undefined> => {
  const storage = await readStorage();
  const anchors = storage.anchors;

  const principalDigest = await computePrincipalDigest({
    principal,
    hasher: storage.hasher,
  });

  for (const ix in anchors) {
    const anchor: Anchor = anchors[ix];

    if (
      anchor.knownPrincipals.some(
        (digest_) => digest_.principalDigest === principalDigest,
      )
    ) {
      return BigInt(ix);
    }
  }

  return;
};

/** Look up an anchor by principal, if it is the last used for the given origin.
 */
export const getAnchorIfLastUsed = async ({
  principal,
  origin,
}: {
  principal: Principal;
  origin: string;
}): Promise<bigint | undefined> => {
  const storage = await readStorage();
  const anchors = storage.anchors;

  const principalDigest = await computePrincipalDigest({
    principal,
    hasher: storage.hasher,
  });

  const originDigest = await computeOriginDigest({
    origin,
    hasher: storage.hasher,
  });

  // candidate anchors with their timestamp, principal digest -> identity number
  const candidates = [];
  for (const ix in anchors) {
    const anchor: Anchor = anchors[ix];

    // there is at most one principal known per anchor and origin.
    const lastUsed = anchor.knownPrincipals.filter(
      (knownPrincipal) => knownPrincipal.originDigest === originDigest,
    )[0];
    if (isNullish(lastUsed)) {
      continue;
    }
    candidates.push({
      lastUsedTimestamp: lastUsed.lastUsedTimestamp,
      principalDigest: lastUsed.principalDigest,
      identityNumber: ix,
    });
  }

  candidates.sort((a, b) => b.lastUsedTimestamp - a.lastUsedTimestamp);
  const mostRecent = candidates[0];
  if (isNullish(mostRecent)) {
    return;
  }

  if (mostRecent.principalDigest === principalDigest) {
    return BigInt(mostRecent.identityNumber);
  }

  return;
};

/** Set the principal as "known"; i.e. from which the anchor can be "looked up" */
export const setKnownPrincipal = async ({
  userNumber,
  origin,
  principal,
}: {
  userNumber: bigint;
  origin: string;
  principal: Principal;
}) => {
  await withStorage(async (storage) => {
    const defaultAnchor: AnchorV3 = {
      knownPrincipals: [],
      lastUsedTimestamp: nowMillis(),
    };

    const ix = userNumber.toString();
    const anchors = storage.anchors;
    const oldAnchor = anchors[ix] ?? defaultAnchor;

    const principalDigest = await computePrincipalDigest({
      principal,
      hasher: storage.hasher,
    });

    const principalData = {
      principalDigest,
      originDigest: await computeOriginDigest({
        origin,
        hasher: storage.hasher,
      }),
      lastUsedTimestamp: nowMillis(),
    };

    // Remove the principal, if we've encountered it already
    const dedupedPrincipals = oldAnchor.knownPrincipals.filter(
      (principalData_) =>
        principalData_.principalDigest !== principalData.principalDigest,
    );

    // Add the new principal and sort (most recently used is first)
    dedupedPrincipals.push(principalData);
    dedupedPrincipals.sort((a, b) => b.lastUsedTimestamp - a.lastUsedTimestamp);

    // Only keep the more recent N principals
    const prunedPrincipals = dedupedPrincipals.slice(0, MAX_SAVED_PRINCIPALS);

    // Here we try to be as non-destructive as possible and we keep potentially unknown
    // fields
    storage.anchors[ix] = { ...oldAnchor, knownPrincipals: prunedPrincipals };
    return storage;
  });
};

/** Accessing functions */

// Simply read the storage without updating it
const readStorage = (): Promise<Storage> => {
  return updateStorage((storage) => {
    return { ret: storage, updated: false };
  });
};

type Awaitable<T> = T | PromiseLike<T>;

// Read & update the storage
const withStorage = (
  op: (storage: Storage) => Awaitable<Storage>,
): Promise<Storage> => {
  return updateStorage(async (storage) => {
    const newStorage = await op(storage);
    return { ret: newStorage, updated: true };
  });
};

/** Building block for reading or updating anchors, ensuring anchors
 * are read correctly (found, migrated, or have a sensible default) and pruned
 * before being written back (if updated) */
const updateStorage = async (
  op: (storage: Storage) => Awaitable<{
    ret: Storage;
    updated: boolean /* iff true, anchors are updated in storage */;
  }>,
): Promise<Storage> => {
  let doWrite = false;

  const storedStorage = await readIndexedDB();

  const { ret: migratedStorage, didMigrate } = nonNullish(storedStorage)
    ? { ret: storedStorage, didMigrate: false }
    : {
        ret: (await migrated()) ?? { anchors: {}, hasher: await newHMACKey() },
        didMigrate: true,
      };
  doWrite ||= didMigrate;

  const { ret: updatedStorage, updated } = await op(migratedStorage);
  doWrite ||= updated;

  const { ret: prunedStorage, pruned } = pruneStorage(updatedStorage);
  doWrite ||= pruned;

  if (doWrite) {
    await writeIndexedDB(prunedStorage);
    // NOTE: we keep V3 up to date in case of rollback
    // (only for known identity numbers, _not_ known anchors)
    await writeIndexedDBV3(toV3(prunedStorage));
  }

  return prunedStorage;
};

/** Migration & Pruning of anchors */

/** Remove most unused anchors until there are MAX_SAVED_ANCHORS left */

const pruneStorage = (storage: Storage): { ret: Storage; pruned: boolean } => {
  const { ret: prunedAnchors, pruned } = pruneAnchors(storage.anchors);
  return { ret: { ...storage, anchors: prunedAnchors }, pruned };
};

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
const migrated = async (): Promise<Storage | undefined> => {
  // Try to read the "v3" (idb.anchors) storage and return that if found
  const v3 = await migratedV3();
  if (nonNullish(v3)) {
    return v3;
  }

  // Try to read the "v2" (idb.anchors) storage and return that if found
  const v2 = await migratedV2();
  if (nonNullish(v2)) {
    return v2;
  }

  // Try to read the "v1" (localStorage.anchors) storage and return that if found
  const v1 = await migratedV1();
  if (nonNullish(v1)) {
    return v1;
  }

  // Try to read the "v0" (localStorage.userNumber) storage and return that if found
  const v0 = await migratedV0();
  if (nonNullish(v0)) {
    return v0;
  }
};

/** Helpers */

// Current timestamp, in milliseconds
const nowMillis = (): number => {
  return new Date().getTime();
};

/**
 * This computes a digest for a principal.
 * Anchors store digests for every principal II generates for them. That way when a user is
 * returning to II with a known principal BUT unknown anchor (like is the case in the verifiable
 * credentials flow) we can know for sure what anchor they mean to use.
 *
 * The digest is derived from the actual principal but does not (in theory) allow figuring out
 * the actual principal from the digest. This is done to minimize the information leaked if the
 * user's browser is compromised: the principals associated with and origin of the visited dapps
 * should in principle not be leaked.
 *
 * The digest is a sha hash of the principal and some secret, i.e. a HMAC digest (with
 * non-extractable HMAC key).
 */
const computePrincipalDigest = async ({
  principal: principalObj,
  hasher,
}: {
  principal: Principal;
  hasher: CryptoKey;
}): Promise<string> => {
  // Encode each element & size
  const enc = new TextEncoder();
  const principal_ = principalObj.toText();

  const principal = enc.encode(principal_);
  const principalLen = Uint8Array.from([principal_.length]);

  // Create a buffer with all elements
  const buff = concatUint8Arrays([principal, principalLen]);

  // Create the digest
  const digestBytes = await crypto.subtle.sign("HMAC", hasher, buff);
  return arrayBufferToBase64(digestBytes);
};

/**
 * This computes a digest for an origin.
 *
 * Anchors store digests for every origin on which II generates principals for them. This information is required to
 * figure out if a specific principal was derived last for a specific origin.
 *
 * The digest is a sha hash of the origin and some secret, i.e. a HMAC digest (with
 * non-extractable HMAC key).
 */
const computeOriginDigest = async ({
  origin: origin_,
  hasher,
}: {
  origin: string;
  hasher: CryptoKey;
}): Promise<string> => {
  if (origin_.length > 255) {
    // Origins must not be longer than 255 bytes according to RFC1035.
    // See also here: https://stackoverflow.com/questions/32290167/what-is-the-maximum-length-of-a-dns-name
    // Given that by these limitations above it should not be possible to exceed the limit, we simply throw
    // to avoid the associated security issue.
    // Note: this is consistent with the backend, that also does not allow origins longer than 255 bytes
    // -> there cannot exist an RP for which the VC flow would be meaningful that has an origin longer than 255 bytes
    // since sign-in would not work in the first place.
    throw new Error("origin too long");
  }
  // Encode each element & size
  const enc = new TextEncoder();

  const origin = enc.encode(origin_);
  const originLen = Uint8Array.from([origin_.length]);

  // Create a buffer with all elements
  const buff = concatUint8Arrays([origin, originLen]);

  // Create the digest
  const digestBytes = await crypto.subtle.sign("HMAC", hasher, buff);
  return arrayBufferToBase64(digestBytes);
};

// Concat some byte arrays
const concatUint8Arrays = (buffers: Uint8Array[]): Uint8Array => {
  // Create a new byte array of the total size
  const totSize = buffers.reduce((acc, arr) => acc + arr.length, 0);
  const final = new Uint8Array(totSize);

  // For each of the arrays to concat: write it to the final array, then bump the write offset
  let offset = 0;
  buffers.forEach((arr) => {
    final.set(arr, offset);
    offset += arr.length;
  });

  return final;
};

/* Generate HMAC key */
const newHMACKey = async (): Promise<CryptoKey> => {
  const key = await crypto.subtle.generateKey(
    { name: "HMAC", hash: "SHA-512" },
    false /* not extractable */,
    ["sign"] /* only used to "sign" (e.g. produce a digest ) */,
  );

  return key;
};

/* Read an arraybuffer as a base64 string
 * https://stackoverflow.com/questions/9267899/arraybuffer-to-base64-encoded-string
 */
function arrayBufferToBase64(buffer: ArrayBuffer): string {
  let binary = "";
  const bytes = new Uint8Array(buffer);
  const len = bytes.byteLength;
  for (let i = 0; i < len; i++) {
    binary += String.fromCharCode(bytes[i]);
  }
  return window.btoa(binary);
}

/** Versions */

/** V0, localstorage["userNumber"] = 10000 */

const migratedV0 = async (): Promise<Storage | undefined> => {
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

  const anchors = {
    [ix]: { lastUsedTimestamp: nowMillis(), knownPrincipals: [] },
  };

  const hasher = await newHMACKey();
  return { anchors, hasher };
};

/**
 * V1, localstorage["anchors"] = { 10000: ... }
 * */

type AnchorV1 = z.infer<typeof AnchorV1>;
const AnchorV1 = z.object({
  /** Timestamp (mills since epoch) of when anchor was last used */
  lastUsedTimestamp: z.number(),
});

/** The type of all anchors in storage. */
type AnchorsV1 = z.infer<typeof AnchorsV1>;
const AnchorsV1 = z.record(AnchorV1);

/* Read localStorage data as ls["anchors"] = { "10000": {...}} */
const migratedV1 = async (): Promise<Storage | undefined> => {
  // NOTE: we do not wipe local storage but keep it around in case of rollback
  const anchors: AnchorsV1 | undefined = readLocalStorageV1();
  if (isNullish(anchors)) {
    return undefined;
  }

  const migratedAnchors: Anchors = {};
  for (const userNumber in anchors) {
    const oldAnchor = anchors[userNumber];
    migratedAnchors[userNumber] = { ...oldAnchor, knownPrincipals: [] };
  }

  const hasher = await newHMACKey();
  return { anchors: migratedAnchors, hasher };
};

// Read localstorage stored anchors
const readLocalStorageV1 = (): AnchorsV1 | undefined => {
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
  const parsed = AnchorsV1.safeParse(item);
  if (parsed.success !== true) {
    const message =
      `could not read saved identities: ignoring malformed localstorage data: ` +
      parsed.error;
    console.warn(message);
    return {};
  }

  return parsed.data;
};

/**
 * V2, indexeddb["anchors"] = { 10000: ... }
 * */

type AnchorsV2 = z.infer<typeof AnchorsV2>;
type AnchorV2 = z.infer<typeof AnchorV2>;

const AnchorV2 = z.object({
  /** Timestamp (mills since epoch) of when anchor was last used */
  lastUsedTimestamp: z.number(),
});
const AnchorsV2 = z.record(AnchorV2);

const migratedV2 = async (): Promise<Storage | undefined> => {
  const readAnchors = await readIndexedDBV2();

  if (isNullish(readAnchors)) {
    return undefined;
  }

  // No known principals
  const migratedAnchors: Anchors = {};

  for (const userNumber in readAnchors) {
    const oldAnchor = readAnchors[userNumber];
    migratedAnchors[userNumber] = { ...oldAnchor, knownPrincipals: [] };
  }

  const hasher = await newHMACKey();

  return { anchors: migratedAnchors, hasher };
};

const readIndexedDBV2 = (): Promise<AnchorsV2 | undefined> => {
  return readGeneric({ idbKey: "anchors", storageType: AnchorsV2 });
};

/**
 * V3, indexeddb["ii-storage-v3"] = { anchors: { 20000: ... } }
 * */

type StorageV3 = z.infer<typeof StorageV3>;
type AnchorsV3 = z.infer<typeof AnchorsV3>;
type AnchorV3 = z.infer<typeof AnchorV3>;
type PrincipalDataV3 = z.infer<typeof PrincipalDataV3>;

const IDB_KEY_V3 = "ii-storage-v3";

const PrincipalDataV3 = z.object({
  /** The actual digest */
  digest: z.string(),

  /** The last time the user authenticated with the principal */
  lastUsedTimestamp: z.number(),
});

const AnchorV3 = z.object({
  /** Timestamp (mills since epoch) of when anchor was last used */
  lastUsedTimestamp: z.number(),

  knownPrincipals: z.array(PrincipalDataV3),
});
const AnchorsV3 = z.record(AnchorV3);

/** The type of all anchors in storage. */
const StorageV3 = z.object({
  anchors: AnchorsV3,
  hasher: z.instanceof(CryptoKey),
});

const readIndexedDBV3 = (): Promise<StorageV3 | undefined> => {
  return readGeneric({ idbKey: IDB_KEY_V3, storageType: StorageV3 });
};

const writeIndexedDBV3 = async (storage: StorageV3) => {
  await idbSet(IDB_KEY_V3, storage);
};

const migratedV3 = async (): Promise<Storage | undefined> => {
  const readStorage = await readIndexedDBV3();

  if (isNullish(readStorage)) {
    return undefined;
  }

  // No known principals
  const migratedAnchors: Anchors = {};

  for (const userNumber in readStorage.anchors) {
    const oldAnchor = readStorage.anchors[userNumber];
    migratedAnchors[userNumber] = { ...oldAnchor, knownPrincipals: [] };
  }

  return { anchors: migratedAnchors, hasher: readStorage.hasher };
};

const toV3 = (storage: Storage): StorageV3 => {
  const migratedAnchors: AnchorsV3 = {};
  for (const userNumber in storage.anchors) {
    const oldAnchor = storage.anchors[userNumber];
    migratedAnchors[userNumber] = { ...oldAnchor, knownPrincipals: [] };
  }

  return { anchors: migratedAnchors, hasher: storage.hasher };
};

/**
 * V4, indexeddb["ii-storage-v4"] = { anchors: { 20000: ... } }
 * */
type StorageV4 = z.infer<typeof StorageV4>;
type AnchorsV4 = z.infer<typeof AnchorsV4>;
type AnchorV4 = z.infer<typeof AnchorV4>;
const IDB_KEY_V4 = "ii-storage-v4";

const PrincipalDataV4 = z.object({
  principalDigest: z.string(),
  originDigest: z.string(),

  /** The last time the user authenticated with the principal */
  lastUsedTimestamp: z.number(),
});

const AnchorV4 = z.object({
  /** Timestamp (mills since epoch) of when anchor was last used */
  lastUsedTimestamp: z.number(),

  knownPrincipals: z.array(PrincipalDataV4),
});
const AnchorsV4 = z.record(AnchorV4);

/** The type of all anchors in storage. */
const StorageV4 = z.object({
  anchors: AnchorsV4,
  hasher: z.instanceof(CryptoKey),
});
const readIndexedDBV4 = (): Promise<StorageV4 | undefined> => {
  return readGeneric({ idbKey: IDB_KEY_V4, storageType: StorageV4 });
};

const writeIndexedDBV4 = async (storage: StorageV4) => {
  await idbSet(IDB_KEY_V4, storage);
};

const readGeneric = async <T extends ZodType>({
  idbKey,
  storageType,
}: {
  idbKey: IDBValidKey;
  storageType: T;
}): Promise<T["_output"] | undefined> => {
  const item: unknown = await idbGet(idbKey);

  if (isNullish(item)) {
    return;
  }

  // Read the object
  const parsed = storageType.safeParse(item);
  if (!parsed.success) {
    const message =
      `could not read saved identities: ignoring malformed IndexedDB data (key: "${idbKey}"): ` +
      parsed.error;
    console.warn(message);
    return { anchors: {}, hasher: await newHMACKey() };
  }

  return parsed.data;
};

/* Latest */

/* Always points to the latest storage & anchor types */
type Storage = StorageV4;
type Anchor = AnchorV4;
type Anchors = Storage["anchors"];
const readIndexedDB = readIndexedDBV4;
const writeIndexedDB = writeIndexedDBV4;
