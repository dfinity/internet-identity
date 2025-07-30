import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, Readable, writable } from "svelte/store";
import { writableStored } from "./writable.store";
import { isNullish, nonNullish } from "@dfinity/utils";
import { AccountInfo } from "$lib/generated/internet_identity_types";
import { anonymousActor } from "$lib/globals";
import { convertToValidCredentialData } from "$lib/utils/credential-devices";

const fetchIdentityCredentials = async (
  identityNumber: bigint,
): Promise<Uint8Array[] | undefined> => {
  try {
    const identityCredentials = await anonymousActor.lookup(identityNumber);
    const validCredentials = identityCredentials
      .filter((device) => "authentication" in device.purpose)
      .filter(({ key_type }) => !("browser_storage_key" in key_type))
      .map(convertToValidCredentialData)
      .filter(nonNullish);

    if (validCredentials.length > 0) {
      return validCredentials.map(
        (credential) => new Uint8Array(credential.credentialId),
      );
    }

    return undefined;
  } catch (error) {
    console.warn(
      `Error looking up identity ${identityNumber} credentials:`,
      error,
    );
    return undefined;
  }
};

export type LastUsedAccount = {
  identityNumber: bigint;
  accountNumber: bigint | undefined;
  origin: string;
  name?: string;
  lastUsedTimestampMillis: number;
};
export type LastUsedAccounts = {
  [origin: string]: {
    [accountNumber: string]: LastUsedAccount;
  };
};
export type LastUsedIdentity = {
  identityNumber: bigint;
  name?: string;
  authMethod:
    | { passkey: { credentialId: Uint8Array } }
    | { openid: { iss: string; sub: string } };
  // Field populated on store initialization
  credentialIds?: Uint8Array[];
  accounts?: LastUsedAccounts;
  lastUsedTimestampMillis: number;
};
export type LastUsedIdentities = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<{
  identities: LastUsedIdentities;
  selected?: LastUsedIdentity;
}> & {
  addLastUsedIdentity: (
    params: Pick<LastUsedIdentity, "identityNumber" | "name" | "authMethod">,
  ) => void;
  addLastUsedAccount: (
    params: Omit<LastUsedAccount, "lastUsedTimestampMillis">,
  ) => void;
  removeIdentity: (identityNumber: bigint) => void;
  syncLastUsedAccounts: (
    identityNumber: bigint,
    origin: string,
    accounts: AccountInfo[],
  ) => AccountInfo[];
  selectIdentity: (identityNumber: bigint) => void;
  reset: () => void;
};

export const PRIMARY_ACCOUNT_KEY = "primary";

export const initLastUsedIdentitiesStore = (): LastUsedIdentitiesStore => {
  const lastUsedStore = writableStored<LastUsedIdentities>({
    key: storeLocalStorageKey.LastUsedIdentities,
    defaultValue: {},
    version: 4,
  });
  const selectedStore = writable<bigint | undefined>(
    Object.values(get(lastUsedStore)).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0]?.identityNumber,
  );
  const { subscribe } = derived(
    [lastUsedStore, selectedStore],
    ([identities, selected]) => ({
      identities,
      selected: nonNullish(selected)
        ? Object.values(identities).find(
            (identity) => identity.identityNumber === selected,
          )
        : undefined,
    }),
  );

  // Fetch credentials for all passkey identities
  Object.values(get(lastUsedStore)).forEach(async (identity) => {
    // Only fetch if the identity was used with passkey
    if ("passkey" in identity.authMethod) {
      const identityNumber = identity.identityNumber;
      const credentials = await fetchIdentityCredentials(identityNumber);
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[identityNumber.toString()];
        identity.credentialIds = credentials;
        return lastUsedIdentities;
      });
    }
  });

  return {
    subscribe,
    addLastUsedIdentity: (params) => {
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        lastUsedIdentities[params.identityNumber.toString()] = {
          accounts: identity?.accounts,
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    addLastUsedAccount: (params) => {
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        if (isNullish(identity)) {
          return lastUsedIdentities;
        }
        if (isNullish(identity.accounts)) {
          identity.accounts = {};
        }
        if (isNullish(identity.accounts[params.origin])) {
          identity.accounts[params.origin] = {};
        }
        identity.accounts[params.origin][
          isNullish(params.accountNumber)
            ? PRIMARY_ACCOUNT_KEY
            : params.accountNumber.toString()
        ] = {
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    removeIdentity(identityNumber) {
      lastUsedStore.update((lastUsedIdentities) => {
        delete lastUsedIdentities[identityNumber.toString()];
        return lastUsedIdentities;
      });
    },
    // TODO: Update this method once we store usage timestamps in the canister,
    //       additionally the tests should be updated to include this method.
    syncLastUsedAccounts: (identityNumber, origin, accounts) => {
      let sortedAccounts: AccountInfo[] = [];
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[identityNumber.toString()];
        if (isNullish(identity)) {
          return lastUsedIdentities;
        }
        if (isNullish(identity.accounts)) {
          identity.accounts = {};
        }
        if (isNullish(identity.accounts[origin])) {
          identity.accounts[origin] = {};
        }
        const originAccounts = identity.accounts[origin];
        accounts.forEach((account) => {
          const key = isNullish(account.account_number[0])
            ? PRIMARY_ACCOUNT_KEY
            : account.account_number[0].toString();
          originAccounts[key] = {
            identityNumber,
            accountNumber: account.account_number[0],
            origin: account.origin,
            name: account.name[0],
            lastUsedTimestampMillis:
              originAccounts[key]?.lastUsedTimestampMillis ?? 0,
          };
        });
        sortedAccounts = Object.values(originAccounts).map((account) => ({
          name: nonNullish(account.name) ? [account.name] : [],
          origin,
          account_number: nonNullish(account.accountNumber)
            ? [account.accountNumber]
            : [],
          last_used: [BigInt(account.lastUsedTimestampMillis)],
        }));
        return lastUsedIdentities;
      });
      return sortedAccounts;
    },
    selectIdentity: (identityNumber: bigint) => {
      selectedStore.set(identityNumber);
    },
    reset: () => {
      selectedStore.set(undefined);
      lastUsedStore.set({});
    },
  };
};

export const lastUsedIdentitiesStore = initLastUsedIdentitiesStore();

export const lastUsedIdentityStore: Readable<LastUsedIdentity | undefined> =
  derived(lastUsedIdentitiesStore, ({ identities }) => {
    return Object.values(identities).sort(
      (a, b) => b.lastUsedTimestampMillis - a.lastUsedTimestampMillis,
    )[0];
  });
