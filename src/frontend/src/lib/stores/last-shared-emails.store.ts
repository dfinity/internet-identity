import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { get } from "svelte/store";
import { writableStored } from "./writable.store";

/** Per-anchor, per-RP "last email the user shared on the consent
 *  screen". Drives picker pre-selection on the next consent for the
 *  same (anchor, RP) pair so the user doesn't re-pick the same address
 *  on every visit. Stored client-side — the canister has no need for
 *  it, and keeping it off-chain avoids persisting per-RP email choices
 *  in stable memory. Lost on a new browser; cost is one extra click. */
export type LastSharedEmails = {
  [identityNumber: string]: { [origin: string]: string };
};

const store = writableStored<LastSharedEmails>({
  key: storeLocalStorageKey.LastSharedEmails,
  defaultValue: {},
  version: 1,
});

export const lastSharedEmailsStore = {
  get(identityNumber: bigint, origin: string): string | undefined {
    return get(store)[identityNumber.toString()]?.[origin];
  },
  set(identityNumber: bigint, origin: string, email: string): void {
    store.update((current) => {
      const key = identityNumber.toString();
      current[key] = { ...current[key], [origin]: email };
      return current;
    });
  },
  reset(): void {
    store.set({});
  },
};
