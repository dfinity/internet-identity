import { toast } from "$src/components/toast";
import { PinIdentityMaterial } from "$src/crypto/pinIdentity";
import { createStore, get, set } from "idb-keyval";

export {
  PinIdentityMaterial,
  constructPinIdentity,
  reconstructPinIdentity,
} from "$src/crypto/pinIdentity";

/* IndexedDB-specific storage for browser identities
 * (indexed by user number) */

export const idbIdentitiesStore = createStore(
  "browser-identities",
  "identities"
);

export const idbStorePinIdentityMaterial = async ({
  userNumber,
  pinIdentityMaterial,
}: {
  userNumber: bigint;
  pinIdentityMaterial: PinIdentityMaterial;
}): Promise<void> => {
  await set(userNumber.toString(), pinIdentityMaterial, idbIdentitiesStore);
};

export const idbRetrievePinIdentityMaterial = async ({
  userNumber,
}: {
  userNumber: bigint;
}): Promise<PinIdentityMaterial | undefined> => {
  const retrieved = await get(userNumber.toString(), idbIdentitiesStore);

  if (retrieved === undefined) {
    return undefined;
  }
  const result = PinIdentityMaterial.safeParse(retrieved);
  if (!result.success) {
    const message =
      `Unexpected error: malformed browser identity for identity ${userNumber}: ` +
      result.error;
    console.error(message);
    toast.error(message);
    throw new Error(message);
  }
  return result.data;
};
