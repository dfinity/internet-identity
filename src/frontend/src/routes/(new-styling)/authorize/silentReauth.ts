import type { AppSessionRecord } from "$lib/stores/app-session.store";

export type SilentDenial = "login_required" | "account_selection_required";

export type SilentOutcome =
  { record: AppSessionRecord } | { denial: SilentDenial };

/**
 * Picks which of the origin's held sessions a silent request re-issues from.
 *
 * A hint is a preference, not a credential: it can only select from what this browser
 * already holds for the origin being authorized, and holding the session is what confers
 * anything. Picking the wrong persona for the user is worse than asking, so several
 * candidates with nothing to choose between them is a denial rather than a guess.
 */
export const chooseSilentSession = ({
  held,
  hint,
}: {
  held: { record: AppSessionRecord }[];
  hint?: string;
}): SilentOutcome => {
  if (held.length === 0) {
    return { denial: "login_required" };
  }

  if (hint !== undefined) {
    const matched = held.filter(
      (entry) => entry.record.accountPrincipal === hint,
    );
    if (matched.length === 1) {
      return { record: matched[0].record };
    }
    return {
      denial:
        matched.length === 0 ? "login_required" : "account_selection_required",
    };
  }

  if (held.length === 1) {
    return { record: held[0].record };
  }
  return { denial: "account_selection_required" };
};
