import { get, type Writable, writable } from "svelte/store";
import { lastUsedIdentityStore } from "$lib/stores/last-used-identities.store";

export const currentIdentityNumberStore: Writable<bigint | undefined> =
  writable(get(lastUsedIdentityStore)?.identityNumber);
