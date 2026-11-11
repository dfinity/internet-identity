export type SilentDenial = "login_required" | "account_selection_required";

export type SilentOutcome<T> = { session: T } | { denial: SilentDenial };

/**
 * Picks which of the origin's held sessions a silent request re-issues from.
 *
 * A hint is a preference, not a credential: it can only select from what this browser
 * already holds for the origin being authorized, and holding the session is what confers
 * anything. Picking the wrong persona for the user is worse than asking, so several
 * candidates with nothing to choose between them is a denial rather than a guess.
 */
export const chooseSilentSession = <T extends { accountPrincipal?: string }>({
  held,
  hint,
}: {
  held: T[];
  hint?: string;
}): SilentOutcome<T> => {
  if (held.length === 0) {
    return { denial: "login_required" };
  }

  if (hint !== undefined) {
    const matched = held.filter((entry) => entry.accountPrincipal === hint);
    if (matched.length === 1) {
      return { session: matched[0] };
    }
    return {
      denial:
        matched.length === 0 ? "login_required" : "account_selection_required",
    };
  }

  if (held.length === 1) {
    return { session: held[0] };
  }
  return { denial: "account_selection_required" };
};
