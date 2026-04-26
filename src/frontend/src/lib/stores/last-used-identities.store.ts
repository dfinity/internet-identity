import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { derived, get, Readable, writable } from "svelte/store";
import { writableStored } from "./writable.store";
import {
  AccountInfo,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";

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
    | {
        openid: {
          iss: string;
          sub: string;
          loginHint?: string;
          metadata?: MetadataMapV2;
        };
      }
    | {
        // SSO credentials are resolved via the two-hop discovery chain
        // (`add_discoverable_oidc_config` + `discoverSsoConfig`), not via
        // the static `openid_configs` list — so we only need the discovery
        // domain (and the optional friendly name from hop 1) to rebuild
        // the request config at sign-in time. `iss`/`sub` aren't stored
        // because a fresh JWT gives us new ones on every re-auth.
        // `email` (when the IdP publishes one) is kept purely for the
        // identity row's secondary text — display fallback is
        // email → name → domain.
        sso: {
          domain: string;
          name?: string;
          email?: string;
          loginHint?: string;
        };
      };
  accounts?: LastUsedAccounts;
  lastUsedTimestampMillis: number;
  createdAtMillis?: number;
};
export type LastUsedIdentities = {
  [identityNumber: string]: LastUsedIdentity;
};
type LastUsedIdentitiesStore = Readable<{
  identities: LastUsedIdentities;
  selected?: LastUsedIdentity;
}> & {
  addLastUsedIdentity: (
    params: Pick<
      LastUsedIdentity,
      "identityNumber" | "name" | "authMethod" | "createdAtMillis"
    >,
  ) => void;
  addLastUsedIdentityIfMissing: (
    params: Pick<
      LastUsedIdentity,
      "identityNumber" | "name" | "authMethod" | "createdAtMillis"
    >,
  ) => void;
  addLastUsedAccount: (
    params: Omit<LastUsedAccount, "lastUsedTimestampMillis">,
  ) => void;
  restoreIdentity: (identity: LastUsedIdentity) => void;
  removeIdentity: (identityNumber: bigint) => void;
  syncLastUsedAccounts: (
    identityNumber: bigint,
    origin: string,
    accounts: AccountInfo[],
  ) => void;
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
      selected:
        selected !== undefined
          ? Object.values(identities).find(
              (identity) => identity.identityNumber === selected,
            )
          : undefined,
    }),
  );

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
    addLastUsedIdentityIfMissing: (params) => {
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        if (identity !== undefined) {
          return lastUsedIdentities;
        }
        lastUsedIdentities[params.identityNumber.toString()] = {
          accounts: undefined,
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    addLastUsedAccount: (params) => {
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[params.identityNumber.toString()];
        if (identity === undefined) {
          return lastUsedIdentities;
        }
        if (identity.accounts === undefined) {
          identity.accounts = {};
        }
        if (identity.accounts[params.origin] === undefined) {
          identity.accounts[params.origin] = {};
        }
        identity.accounts[params.origin][
          params.accountNumber === undefined
            ? PRIMARY_ACCOUNT_KEY
            : params.accountNumber.toString()
        ] = {
          ...params,
          lastUsedTimestampMillis: Date.now(),
        };
        return lastUsedIdentities;
      });
    },
    restoreIdentity(identity) {
      lastUsedStore.update((lastUsedIdentities) => {
        lastUsedIdentities[identity.identityNumber.toString()] = identity;
        return lastUsedIdentities;
      });
    },
    removeIdentity(identityNumber) {
      lastUsedStore.update((lastUsedIdentities) => {
        delete lastUsedIdentities[identityNumber.toString()];
        return lastUsedIdentities;
      });
    },
    syncLastUsedAccounts: (identityNumber, origin, accounts) => {
      lastUsedStore.update((lastUsedIdentities) => {
        const identity = lastUsedIdentities[identityNumber.toString()];
        if (identity === undefined) {
          return lastUsedIdentities;
        }
        if (identity.accounts === undefined) {
          identity.accounts = {};
        }
        if (identity.accounts[origin] === undefined) {
          identity.accounts[origin] = {};
        }
        identity.accounts[origin] = Object.fromEntries(
          accounts.map((account) => [
            account.account_number[0] === undefined
              ? PRIMARY_ACCOUNT_KEY
              : account.account_number[0].toString(),
            {
              identityNumber,
              accountNumber: account.account_number[0],
              origin: account.origin,
              name: account.name[0],
              lastUsedTimestampMillis:
                account.last_used.map(nanosToMillis)[0] ?? 0,
            },
          ]),
        );
        return lastUsedIdentities;
      });
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
