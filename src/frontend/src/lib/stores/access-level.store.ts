import { storeLocalStorageKey } from "$lib/constants/store.constants";
import { get } from "svelte/store";
import { writableStored } from "./writable.store";
import type { AccessLevel } from "$lib/utils/accessLevel";

/** The sign-in flows that offer an access-level choice. The remembered choice
 *  is kept separate per flow because the flows differ in kind: an app connect
 *  (`continue`), a linked CLI (`cli`) and a cross-app MCP connector (`mcp`)
 *  each warrant their own default — a user who links a read-only CLI shouldn't
 *  have that leak into how they connect an app. */
export type AccessLevelFlow = "continue" | "cli" | "mcp";

/** Per-flow, per-anchor "access level the user last connected with". Drives
 *  the pre-selection of the access-level selector on the next sign-in for the
 *  same (flow, anchor) so the user doesn't re-pick every time. Keyed per anchor
 *  (not globally) because the browser may be shared — on a public/kiosk device
 *  several people sign in with different anchors, and one person's choice must
 *  not become another's default. Stored client-side; lost on a new browser,
 *  at the cost of one extra pick. */
export type AccessLevelPreferences = {
  [flow in AccessLevelFlow]?: { [identityNumber: string]: AccessLevel };
};

const store = writableStored<AccessLevelPreferences>({
  key: storeLocalStorageKey.AccessLevel,
  defaultValue: {},
  version: 1,
});

export const accessLevelStore = {
  /** The remembered access level for a flow and anchor, or `undefined` when
   *  that anchor hasn't chosen one yet (first-time sign in) — in which case
   *  the selector shows nothing pre-selected. */
  getPreference(
    flow: AccessLevelFlow,
    identityNumber: bigint,
  ): AccessLevel | undefined {
    return get(store)[flow]?.[identityNumber.toString()];
  },
  /** Persist the access level the anchor connected with, so it pre-fills the
   *  next time that anchor uses this flow. */
  setPreference(
    flow: AccessLevelFlow,
    identityNumber: bigint,
    accessLevel: AccessLevel,
  ): void {
    store.update((current) => {
      current[flow] = {
        ...current[flow],
        [identityNumber.toString()]: accessLevel,
      };
      return current;
    });
  },
  reset(): void {
    store.set({});
  },
};
