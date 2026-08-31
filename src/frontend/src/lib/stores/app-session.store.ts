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
  /** Names this session to `revoke_account_session`, which is how one session is revoked
   *  once a surface exists that lists them. */
  createdAtNanos: bigint;
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

// Treat the last 5 minutes as already expired, so a session is never served that
// expires between the check here and validation on the IC.
const EXPIRY_MARGIN_MS = 5 * 60 * 1000;

interface SessionKey {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
}

/** Returns a copy, so a caller mutating the record cannot write back into the store
 *  through the object IndexedDB handed us. */
const normalize = <T extends object>(record: T): T => ({ ...record });

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

/** Every session this identity holds, for the sibling lookup and for sign-out. */
export const appSessionsForOrigin = async (
  origin: string,
): Promise<
  { identityNumber: bigint; accountNumber?: bigint; record: AppSessionRecord }[]
> => {
  const now = Date.now();
  return (await readAll<AppSessionRecord>(APP_SESSION_STORE)).flatMap(
    ([key, record]) => {
      const parsed = parseKey(key);
      if (parsed?.origin !== origin) {
        return [];
      }
      if (record.expiresAtMillis - EXPIRY_MARGIN_MS <= now) {
        return [];
      }
      return [
        {
          identityNumber: parsed.identityNumber,
          accountNumber: parsed.accountNumber,
          record: normalize(record),
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
              record: normalize(record),
            },
          ]
        : [];
    },
  );

export const purgeAppSessions = async (
  identityNumber: bigint,
): Promise<void> => {
  const prefix = `${identityNumber.toString()}:`;
  await Promise.all(
    [APP_SESSION_STORE, APP_ACCOUNT_STORE].map(async (store) =>
      Promise.all(
        (await readAll<unknown>(store))
          .map(([key]) => key)
          .filter(
            (key): key is string =>
              typeof key === "string" && key.startsWith(prefix),
          )
          .map((key) => idbDel(key, store).catch(() => {})),
      ),
    ),
  );
};
