import type { Permissions } from "$lib/generated/internet_identity_types";

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

/** The candid `permissions : opt Permissions` argument for this access level,
 *  mapping to the ICP protocol's permission values (`queries` / `all`).
 *  Always explicit (never `[]`) so first-party calls don't rely on the
 *  backend's omitted-argument default. */
export const toPermissionsArg = (accessLevel: AccessLevel): [Permissions] => [
  accessLevel === "read-only" ? { queries: null } : { all: null },
];

/** The sign-in flows that offer an access-level choice. The stored preference
 *  is kept separate per flow because the flows differ in kind: an app connect
 *  (`continue`), a linked CLI (`cli`) and a cross-app MCP connector (`mcp`)
 *  each warrant their own remembered default — a user who links a read-only
 *  CLI shouldn't have that leak into how they connect an app. */
export type AccessLevelFlow = "continue" | "cli" | "mcp";

// Browser-local, per-flow persistence for the access-level choice. Global
// (not per-anchor): the read-only/full-access decision is a mental-mode
// preference for a kind of connection, not something a user re-decides per
// identity. First-time (no stored value) leaves the selector unselected so the
// user makes an explicit choice; thereafter the last choice pre-fills.
const ACCESS_LEVEL_STORAGE_PREFIX = "ii:access-level:";

const isAccessLevel = (value: string | null): value is AccessLevel =>
  value === "read-only" || value === "full-access";

/** The remembered access level for a flow, or `undefined` when the user hasn't
 *  chosen one yet (first-time sign in) — in which case the selector shows
 *  nothing pre-selected. */
export const readAccessLevelPreference = (
  flow: AccessLevelFlow,
): AccessLevel | undefined => {
  if (typeof localStorage === "undefined") {
    return undefined;
  }
  const value = localStorage.getItem(`${ACCESS_LEVEL_STORAGE_PREFIX}${flow}`);
  return isAccessLevel(value) ? value : undefined;
};

/** Persist the access level the user connected with, so it pre-fills the next
 *  time they use this flow. */
export const writeAccessLevelPreference = (
  flow: AccessLevelFlow,
  accessLevel: AccessLevel,
): void => {
  if (typeof localStorage === "undefined") {
    return;
  }
  localStorage.setItem(`${ACCESS_LEVEL_STORAGE_PREFIX}${flow}`, accessLevel);
};
