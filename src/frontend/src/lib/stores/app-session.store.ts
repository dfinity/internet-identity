import type { AccessLevel } from "$lib/utils/accessLevel";
import {
  createStore,
  get as idbGet,
  set as idbSet,
  del as idbDel,
  entries as idbEntries,
} from "idb-keyval";

/**
 * A session held for one `(identity, account, origin)`, so returning to an app, or
 * arriving at a sibling of one, can re-issue without another ceremony.
 *
 * The keypair is non-extractable and never leaves this origin; the app receives a chain
 * extended to its own key, not this one.
 */
export interface AppSessionRecord {
  keyPair: CryptoKeyPair;
  chainJson: string;
  expiresAtMillis: number;
  createdAtNanos: bigint;
  /** What the user consented to when this session was created, so a later request for more
   *  is not answered with less. */
  accessLevel: AccessLevel;
  /** The principal apps see for this account, so a hint can select between sessions. */
  accountPrincipal: string;
}

const APP_SESSION_STORE = createStore("ii-app-sessions", "sessions");

// Treat the last 5 minutes as already expired, so a session is never served that
// expires between the check here and validation on the IC.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

/** Bytes read back from IndexedDB can arrive from another realm, where an
 *  `instanceof Uint8Array` check fails. */
const normalize = (record: AppSessionRecord): AppSessionRecord => ({
  ...record,
});

const sessionKey = ({
  identityNumber,
  accountNumber,
  origin,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
}): string =>
  `${identityNumber.toString()}:${accountNumber?.toString() ?? "default"}:${origin}`;

export const storeAppSession = async (
  key: { identityNumber: bigint; accountNumber?: bigint; origin: string },
  record: AppSessionRecord,
): Promise<void> => {
  await idbSet(sessionKey(key), record, APP_SESSION_STORE);
};

export const appSessionFor = async (key: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
}): Promise<AppSessionRecord | undefined> => {
  let record: AppSessionRecord | undefined;
  try {
    record = await idbGet<AppSessionRecord>(sessionKey(key), APP_SESSION_STORE);
  } catch {
    return undefined;
  }
  if (record === undefined) {
    return undefined;
  }
  if (record.expiresAtMillis - EXPIRY_MARGIN_MS <= Date.now()) {
    await discardAppSession(key);
    return undefined;
  }
  return normalize(record);
};

export const discardAppSession = async (key: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
}): Promise<void> => {
  try {
    await idbDel(sessionKey(key), APP_SESSION_STORE);
  } catch {
    // A session that cannot be discarded locally is still revocable canister-side.
  }
};

/** Every session this identity holds, for the sibling lookup and for sign-out. */
export const appSessionsForOrigin = async (
  origin: string,
): Promise<
  { identityNumber: bigint; accountNumber?: bigint; record: AppSessionRecord }[]
> => {
  let stored: [IDBValidKey, AppSessionRecord][];
  try {
    stored = await idbEntries<IDBValidKey, AppSessionRecord>(APP_SESSION_STORE);
  } catch {
    return [];
  }

  const now = Date.now();
  return stored.flatMap(([key, record]) => {
    if (typeof key !== "string") {
      return [];
    }
    const separator = key.indexOf(":");
    const accountSeparator = key.indexOf(":", separator + 1);
    if (separator === -1 || accountSeparator === -1) {
      return [];
    }
    if (key.slice(accountSeparator + 1) !== origin) {
      return [];
    }
    if (record.expiresAtMillis - EXPIRY_MARGIN_MS <= now) {
      return [];
    }
    const accountPart = key.slice(separator + 1, accountSeparator);
    return [
      {
        identityNumber: BigInt(key.slice(0, separator)),
        accountNumber:
          accountPart === "default" ? undefined : BigInt(accountPart),
        record: normalize(record),
      },
    ];
  });
};

export const purgeAppSessions = async (
  identityNumber: bigint,
): Promise<void> => {
  let stored: [IDBValidKey, AppSessionRecord][];
  try {
    stored = await idbEntries<IDBValidKey, AppSessionRecord>(APP_SESSION_STORE);
  } catch {
    return;
  }
  const prefix = `${identityNumber.toString()}:`;
  await Promise.all(
    stored
      .map(([key]) => key)
      .filter(
        (key): key is string =>
          typeof key === "string" && key.startsWith(prefix),
      )
      .map((key) => idbDel(key, APP_SESSION_STORE).catch(() => {})),
  );
};
