import type { AccessLevel as CandidAccessLevel } from "$lib/generated/internet_identity_types";

/** The access level a session delegation grants. `"read-only"` restricts the
 *  delegation to query calls (it carries `permissions = "queries"`), so the
 *  Internet Computer rejects update calls authenticated through it —
 *  enforcement is protocol-level, not up to the app or canister.
 *  `"full-access"` is an unrestricted, update-capable delegation.
 *
 *  Typed as a string union (rather than a boolean) so call sites are
 *  self-describing: `authorize(account, "read-only")`, not
 *  `authorize(account, true)`. */
export type AccessLevel = "read-only" | "full-access";

/** The candid `access : opt AccessLevel` argument for this access level.
 *  Always explicit (never `[]`) so first-party calls don't rely on the
 *  backend's omitted-argument default. */
export const toAccessLevelArg = (
  accessLevel: AccessLevel,
): [CandidAccessLevel] => [
  accessLevel === "read-only" ? { read_only: null } : { full_access: null },
];

/** Whether an access-level toggle's checkbox is ticked: the box offers
 *  `prompt` as the opt-in, so it is ticked exactly when the current level
 *  *is* the prompted one. */
export const isToggleChecked = (
  accessLevel: AccessLevel,
  prompt: AccessLevel,
): boolean => accessLevel === prompt;

/** The access level after (un)ticking an access-level toggle: ticking
 *  selects the prompted level, unticking falls back to the other one. */
export const toggledAccessLevel = (
  checked: boolean,
  prompt: AccessLevel,
): AccessLevel =>
  checked ? prompt : prompt === "read-only" ? "full-access" : "read-only";
