import type { AccessLevel } from "$lib/utils/accessLevel";
import {
  createStore,
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
  /** The session this record holds, as the canister named it. Ending one session is the
   *  app's own call; from settings the user signs a whole browser out. */
  sessionId: bigint;
  /** What the user consented to when this session was created. Recorded for display; the
   *  canister enforces it at every mint, and an app cannot request a level of its own. */
  accessLevel: AccessLevel;
}

/**
 * Which account of which identity an app's principal names.
 *
 * Kept apart from the session because it is not a credential: it is a derivation an app
 * already knows the answer to, and nothing here can sign. A sign-in that asked not to be
 * resumable stores this and no session, so a later `hint` still selects the right account
 * to sign in as — it just has to be signed in for.
 */
export interface AppAccountRecord {
  accountPrincipal: string;
}

// Two databases rather than two object stores, because idb-keyval gives a database one
// store and fixes it at creation.
const APP_SESSION_STORE = createStore("ii-app-sessions", "sessions");
const APP_ACCOUNT_STORE = createStore("ii-app-accounts", "accounts");

// A session with less than an app delegation's life left cannot back even one, so
// serving it would answer a silent request with a sign-in that dies unexplained a
// moment later. The canister's own APP_DELEGATION_TTL_NS is the same five minutes.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

interface SessionKey {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
}

const sessionKey = ({
  identityNumber,
  accountNumber,
  origin,
}: SessionKey): string =>
  `${identityNumber.toString()}:${accountNumber?.toString() ?? "default"}:${origin}`;

const parseKey = (key: IDBValidKey): SessionKey | undefined => {
  if (typeof key !== "string") {
    return undefined;
  }
  const separator = key.indexOf(":");
  const accountSeparator = key.indexOf(":", separator + 1);
  if (separator === -1 || accountSeparator === -1) {
    return undefined;
  }
  const accountPart = key.slice(separator + 1, accountSeparator);
  return {
    identityNumber: BigInt(key.slice(0, separator)),
    accountNumber: accountPart === "default" ? undefined : BigInt(accountPart),
    origin: key.slice(accountSeparator + 1),
  };
};

const readAll = async <T>(
  store: ReturnType<typeof createStore>,
): Promise<[IDBValidKey, T][]> => {
  try {
    return await idbEntries<IDBValidKey, T>(store);
  } catch {
    return [];
  }
};

export const storeAppSession = async (
  key: SessionKey,
  record: AppSessionRecord,
): Promise<void> => {
  await idbSet(sessionKey(key), record, APP_SESSION_STORE);
};

export const rememberAppAccount = async (
  key: SessionKey,
  record: AppAccountRecord,
): Promise<void> => {
  try {
    await idbSet(sessionKey(key), record, APP_ACCOUNT_STORE);
  } catch {
    // Losing the mapping costs a hint its shortcut, not the sign-in it belongs to.
  }
};

export const discardAppSession = async (key: SessionKey): Promise<void> => {
  try {
    await idbDel(sessionKey(key), APP_SESSION_STORE);
  } catch {
    // A session that cannot be discarded locally is still revocable canister-side.
  }
};

/** Every session this identity holds, for the sibling lookup and for sign-out.
 *
 *  Each carries the principal its account is known by, which lives in the other store
 *  and is joined back on here: a hint names a principal, and what it selects between is
 *  sessions. */
export const appSessionsForOrigin = async (
  origin: string,
): Promise<
  {
    identityNumber: bigint;
    accountNumber?: bigint;
    accountPrincipal?: string;
    record: AppSessionRecord;
  }[]
> => {
  const now = Date.now();
  const accounts = new Map(
    (await readAll<AppAccountRecord>(APP_ACCOUNT_STORE)).map(
      ([key, record]) => [key, record.accountPrincipal],
    ),
  );
  return (await readAll<AppSessionRecord>(APP_SESSION_STORE)).flatMap(
    ([key, record]) => {
      const parsed = parseKey(key);
      // A key `sessionKey` could not have produced — an older format, most likely. No
      // reader reaches it and `purgeSessionsOf` matches on a parsed identity, so it
      // would sit here forever.
      if (parsed === undefined) {
        void idbDel(key, APP_SESSION_STORE).catch(() => {});
        return [];
      }
      // Before the origin filter, and so for every key rather than this origin's: the
      // whole store is already in memory, and an origin the user never returns to would
      // otherwise keep its dead record for the life of the profile. Not awaited, the way
      // every other delete here is, so a read costs no write.
      if (record.expiresAtMillis - EXPIRY_MARGIN_MS <= now) {
        void idbDel(key, APP_SESSION_STORE).catch(() => {});
        return [];
      }
      if (parsed.origin !== origin) {
        return [];
      }
      return [
        {
          identityNumber: parsed.identityNumber,
          accountNumber: parsed.accountNumber,
          accountPrincipal: accounts.get(key),
          record,
        },
      ];
    },
  );
};

/** Every account this browser has seen at one origin, whether or not a session for it
 *  survived. */
export const appAccountsForOrigin = async (
  origin: string,
): Promise<
  { identityNumber: bigint; accountNumber?: bigint; record: AppAccountRecord }[]
> =>
  (await readAll<AppAccountRecord>(APP_ACCOUNT_STORE)).flatMap(
    ([key, record]) => {
      const parsed = parseKey(key);
      return parsed?.origin === origin
        ? [
            {
              identityNumber: parsed.identityNumber,
              accountNumber: parsed.accountNumber,
              record,
            },
          ]
        : [];
    },
  );

/** Every session this identity holds here, whatever origin it is at. */
const purgeSessionsOf = async (identityNumber: bigint): Promise<void> => {
  const keys = (await readAll<unknown>(APP_SESSION_STORE))
    .map(([key]) => key)
    .filter((key) => parseKey(key)?.identityNumber === identityNumber);
  await Promise.all(
    keys.map((key) => idbDel(key, APP_SESSION_STORE).catch(() => {})),
  );
};

/** The account mappings for the same identity, which are keyed the same way today but
 *  are not the same data — so re-keying one store cannot quietly re-key the other. */
const purgeAccountsOf = async (identityNumber: bigint): Promise<void> => {
  const keys = (await readAll<unknown>(APP_ACCOUNT_STORE))
    .map(([key]) => key)
    .filter((key) => parseKey(key)?.identityNumber === identityNumber);
  await Promise.all(
    keys.map((key) => idbDel(key, APP_ACCOUNT_STORE).catch(() => {})),
  );
};

export const purgeAppSessions = async (
  identityNumber: bigint,
): Promise<void> => {
  await purgeSessionsOf(identityNumber);
  await purgeAccountsOf(identityNumber);
};
