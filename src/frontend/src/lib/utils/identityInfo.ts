import { IdentityAnchorInfo } from "$lib/generated/internet_identity_types";
import { nonNullish } from "@dfinity/utils";

export const createdAtMillis = (
  identityInfo: IdentityAnchorInfo,
): number | undefined => {
  return nonNullish(identityInfo.created_at[0])
    ? Number(identityInfo.created_at[0] / BigInt(1_000_000))
    : undefined;
};
