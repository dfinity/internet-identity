/** Whether a phone already has the notification app is the one thing nothing here can see: the canister records which browser installed it, so a second browser on the same phone finds none of its own.
 * Kept per identity, because an app is linked under one. */
import {
  createStore,
  del as idbDel,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

export const ALREADY_INSTALLED_STORE = createStore(
  "ii-notification-install",
  "already-installed",
);

/** Never throws: a browser that refuses storage just asks again. Reported, because what the user told us is then lost. */
export const recordAlreadyInstalled = async (
  identityNumber: bigint,
): Promise<void> => {
  try {
    await idbSet(identityNumber.toString(), true, ALREADY_INSTALLED_STORE);
  } catch (error) {
    console.error(error);
  }
};

/** `false` where storage would not answer: answering the other way would hide the install from a user who has no app at all. */
export const saidAlreadyInstalled = async (
  identityNumber: bigint,
): Promise<boolean> => {
  try {
    return (
      (await idbGet<boolean>(
        identityNumber.toString(),
        ALREADY_INSTALLED_STORE,
      )) === true
    );
  } catch (error) {
    console.error(error);
    return false;
  }
};

/** Forgets it, so the install is offered again. Throws where it could not be forgotten, or the switch would report off while the storage still says installed. */
export const forgetAlreadyInstalled = (identityNumber: bigint): Promise<void> =>
  idbDel(identityNumber.toString(), ALREADY_INSTALLED_STORE);
