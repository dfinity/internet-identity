import { DelegationIdentity } from "@dfinity/identity";

export const authenticationState = $state<{
  authenticated?: {
    identity: DelegationIdentity;
    anchorNumber: bigint;
  };
}>({});
