import type { ActorSubclass } from "@icp-sdk/core/agent";
import { purgeAppSessions } from "$lib/stores/app-session.store";
import { currentBrowserId } from "$lib/stores/browser-key.store";
import type {
  _SERVICE,
  BrowserInfo,
} from "$lib/generated/internet_identity_types";
import { nanosToMillis } from "$lib/utils/time";

export interface Browser {
  id: number;
  name: string;
  createdAtMillis: number;
  lastUsedMillis: number;
  /** Several browsers report the same name, so the list marks the one being read from. */
  isCurrent: boolean;
}

export const fromCanisterBrowsers = (
  browsers: [] | [BrowserInfo[]],
  currentBrowserId?: number,
): Browser[] =>
  (browsers[0] ?? [])
    .map((browser) => ({
      id: browser.id,
      name: browser.name,
      createdAtMillis: nanosToMillis(browser.created_at),
      lastUsedMillis: nanosToMillis(browser.last_used),
      isCurrent: browser.id === currentBrowserId,
    }))
    .sort((a, b) => b.lastUsedMillis - a.lastUsedMillis);

/**
 * Ends every session this browser holds, across every app it is signed into.
 *
 * The browser record itself survives, so a browser that has been signed out is still one
 * the user recognises and signing back in from it reuses the same entry.
 *
 * Signing *this* browser out also discards the session chains it holds locally. The
 * canister has already stopped honouring them, and leaving them would have the next
 * silent request offer a chain that cannot mint.
 *
 * Which browser this is comes from the key record here rather than from a caller: the
 * list renders that flag from a promise, so a click landing before it resolves would
 * otherwise pass `false` for the user's own browser and leave exactly those chains
 * behind.
 */
export const signOutBrowser = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  browserId: number,
): Promise<void> => {
  const result = await actor.revoke_browser_sessions({
    identity_number: identityNumber,
    browser_id: browserId,
  });
  if ("Err" in result) {
    throw new Error(
      "Unauthorized" in result.Err
        ? "Not authorized to end this browser's sessions"
        : result.Err.InternalCanisterError,
    );
  }
  if ((await currentBrowserId(identityNumber)) === browserId) {
    await purgeAppSessions(identityNumber);
  }
};
