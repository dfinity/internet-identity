/** Dropping the registration is not enough: the reconcile on the next authenticated boot registers this browser again, so the switch leaves this behind for it to read.
 * A storage that cannot be read reads as off, since a reconcile that does not run is something the user can undo; a storage that cannot be written throws. */
import {
  createStore,
  del as idbDel,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

const TURNED_OFF_STORE = createStore(
  "ii-notification-turned-off",
  "turned-off",
);

/** Throws where it could not be stored, so the switch can say it did not take. */
export const recordNotificationsOff = (identityNumber: bigint): Promise<void> =>
  idbSet(identityNumber.toString(), true, TURNED_OFF_STORE);

/** Answers `true` where this browser could not be read: what cannot be known is treated as the answer the user cannot undo. */
export const notificationsTurnedOff = async (
  identityNumber: bigint,
): Promise<boolean> => {
  try {
    return (
      (await idbGet<boolean>(identityNumber.toString(), TURNED_OFF_STORE)) ===
      true
    );
  } catch (error) {
    // A reconcile skipped for a storage that cannot be read looks from the outside exactly like one skipped because the user asked for it.
    console.error(error);
    return true;
  }
};

/** Forgets it, so the reconcile keeps this browser registered again. Throws where it could not be forgotten. */
export const clearNotificationsOff = (identityNumber: bigint): Promise<void> =>
  idbDel(identityNumber.toString(), TURNED_OFF_STORE);
