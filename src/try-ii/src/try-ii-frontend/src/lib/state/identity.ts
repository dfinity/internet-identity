import { SignIdentity } from "@dfinity/agent";
import { Ed25519KeyIdentity, type DelegationIdentity } from "@dfinity/identity";
import { writable } from "svelte/store";

export let identity = writable<DelegationIdentity | undefined>(undefined);
export let localIdentity = writable<SignIdentity>(
  Ed25519KeyIdentity.generate()
);
