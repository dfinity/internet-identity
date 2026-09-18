/**
 * Which origins a sign-in has created, or is creating, a session for on this page.
 *
 * Anything that would create a session of its own asks here first, because
 * `create_session` drops every session the same browser already holds at an origin.
 *
 * Entries are kept after the sign-in finishes, not only while it runs: what a caller
 * needs to know is whether a sign-in has put this origin in the application registry,
 * which one that finished a moment ago answers too.
 *
 * Not a Svelte store: nothing renders from it, and callers need the promise rather than
 * a value to subscribe to.
 */
const signIns = new Map<string, Promise<unknown>>();

/**
 * Registers a sign-in's session creation so a consent ceremony waits for it instead of
 * creating a second one. Returns `creation` unchanged, to be wrapped around a call.
 */
export const trackSessionCreation = <T>(
  origin: string,
  creation: Promise<T>,
): Promise<T> => {
  signIns.set(origin, creation);
  // Forgotten on failure: a sign-in that stored no session put nothing in the registry
  // for a consent to find.
  void creation.catch(() => {
    // Only if it is still ours: a later sign-in may have replaced the entry.
    if (signIns.get(origin) === creation) {
      signIns.delete(origin);
    }
  });
  return creation;
};

/**
 * Waits for any sign-in at `origin` and reports whether there was one, which tells a
 * caller whether asking the canister again is worth a call.
 *
 * A failed sign-in settles this too and answers `true`, having already dropped its
 * entry. What resolving promises is that no sign-in at this origin is still in flight.
 */
export const awaitSessionCreation = async (
  origin: string,
): Promise<boolean> => {
  const creation = signIns.get(origin);
  if (creation === undefined) {
    return false;
  }
  await creation.catch(() => undefined);
  return true;
};
