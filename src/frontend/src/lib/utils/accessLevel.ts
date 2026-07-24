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
