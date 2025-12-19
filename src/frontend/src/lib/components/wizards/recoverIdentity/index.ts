import type { IdentityInfo } from "$lib/generated/internet_identity_types";
export { default as RecoverIdentityWizard } from "./RecoverIdentityWizard.svelte";

export interface FoundIdentity {
  identityNumber: bigint;
  identityInfo: IdentityInfo;
}
