/**
 * What the user told this browser about their own device.
 *
 * Whether a phone already has the notification app is the one thing nothing here can
 * see. The app has its own storage partition, and the canister records which *browser*
 * installed it, so a second browser on the same phone finds no app of its own and would
 * offer the install again however many times the user signs in.
 *
 * Asking anchor-wide instead would be worse: an identity whose phone has the app would
 * stop the same user's iPad being offered it at all, and a denial is worse than being
 * asked twice. So the question goes to the person who can answer it, and the answer is
 * kept in the browser it was given in, which is the browser that would otherwise keep
 * asking.
 *
 * Per identity, because an app is linked under one: another identity in this browser has
 * no app and is still owed the install.
 */
import {
  createStore,
  del as idbDel,
  get as idbGet,
  set as idbSet,
} from "idb-keyval";

const ALREADY_INSTALLED_STORE = createStore(
  "ii-notification-install",
  "already-installed",
);

/** Never throws: a browser that refuses storage just asks again, which is the state
 *  this started in. */
export const recordAlreadyInstalled = async (
  identityNumber: bigint,
): Promise<void> => {
  try {
    await idbSet(identityNumber.toString(), true, ALREADY_INSTALLED_STORE);
  } catch {
    // Nothing to do: the install is offered again next time.
  }
};

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
  } catch {
    return false;
  }
};

/** Forgets it, so the install is offered again. The way back from having said it by
 *  mistake: turning the switch in settings off and on again clears this. */
export const forgetAlreadyInstalled = async (
  identityNumber: bigint,
): Promise<void> => {
  try {
    await idbDel(identityNumber.toString(), ALREADY_INSTALLED_STORE);
  } catch {
    // Nothing to do: it stays said, and the switch is the way to say otherwise.
  }
};
